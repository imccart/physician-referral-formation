/* ------------------------------------------------------------ */
/* TITLE:         PCP identification via 365-day carrier lookback*/
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 3 of 6                                       */
/* INPUT:         {Prefix}Encounters_{year} from Step 1,         */
/*                {Prefix}Carrier_{year} from Step 2              */
/* OUTPUT:        {Prefix}PCP_{year}                             */
/* ------------------------------------------------------------ */



/* ============================================================ */
/* PCP Assignment                                                */
/* ============================================================ */

/* For each specialist encounter, identify the PCP using a       */
/* 365-day lookback on carrier E&M visits.                       */
/*                                                               */
/* Logic:                                                        */
/* 1. Stack carrier claims from year Y and Y-1                   */
/* 2. Join to encounters where visit_date is within 365 days     */
/*    before the encounter and exclude the specialist             */
/* 3. Count visits per candidate physician                       */
/* 4. Rank by visit count (desc), then most recent visit (desc)  */
/* 5. Pick rank = 1 as PCP                                       */

%MACRO assign_pcp(prefix);

    %DO year_data = &year_start %TO &year_end;
        %LET year_lag = %EVAL(&year_data - 1);

        /* Unique encounters for this year */
        PROC SQL;
            DROP TABLE WORK.Encounters_&year_data;
            CREATE TABLE WORK.Encounters_&year_data AS
            SELECT DISTINCT
                BENE_ID,
                Specialist_NPI,
                Encounter_Date,
                Facility_ID
            FROM PL027710.&prefix.Encounters_&year_data;
        QUIT;

        /* Stack carrier claims from current and prior year */
        DATA WORK.Carrier_Stacked;
            SET PL027710.&prefix.Carrier_&year_data
                PL027710.&prefix.Carrier_&year_lag;
        RUN;

        /* Join carrier to encounters: 365-day lookback, exclude specialist */
        PROC SQL;
            DROP TABLE WORK.PreEncounter_Visits;
            CREATE TABLE WORK.PreEncounter_Visits AS
            SELECT
                a.BENE_ID,
                a.Specialist_NPI,
                a.Encounter_Date,
                a.Facility_ID,
                b.Physician_ID,
                b.Visit_Date,
                b.Phy_Tax_ID
            FROM WORK.Encounters_&year_data AS a
            LEFT JOIN WORK.Carrier_Stacked AS b
                ON a.BENE_ID = b.BENE_ID
            WHERE b.Visit_Date <= a.Encounter_Date
              AND b.Visit_Date > (a.Encounter_Date - 365)
              AND b.Physician_ID NE ''
              AND b.Physician_ID NE a.Specialist_NPI;
        QUIT;

        /* Aggregate: count visits and track most recent visit per candidate */
        PROC SQL;
            DROP TABLE WORK.PCP_Candidates;
            CREATE TABLE WORK.PCP_Candidates AS
            SELECT
                BENE_ID,
                Specialist_NPI,
                Encounter_Date,
                Facility_ID,
                Physician_ID,
                Phy_Tax_ID,
                COUNT(*) AS N_Visits,
                MAX(Visit_Date) AS Max_Visit_Date FORMAT=DATE9.,
                MIN(Visit_Date) AS Min_Visit_Date FORMAT=DATE9.
            FROM WORK.PreEncounter_Visits
            GROUP BY BENE_ID, Specialist_NPI, Encounter_Date, Facility_ID,
                     Physician_ID, Phy_Tax_ID;
        QUIT;

        /* Rank candidates: most visits, then most recent, pick rank 1 */
        PROC SQL;
            DROP TABLE PL027710.&prefix.PCP_&year_data;
            CREATE TABLE PL027710.&prefix.PCP_&year_data AS
            SELECT *
            FROM (
                SELECT
                    BENE_ID,
                    Specialist_NPI,
                    Encounter_Date,
                    Facility_ID,
                    Physician_ID AS PCP_NPI,
                    Phy_Tax_ID AS PCP_Tax_ID,
                    N_Visits,
                    Max_Visit_Date,
                    Min_Visit_Date,
                    MONOTONIC() AS _row_
                FROM WORK.PCP_Candidates
                ORDER BY BENE_ID, Specialist_NPI, Encounter_Date,
                         N_Visits DESC, Max_Visit_Date DESC
            )
            GROUP BY BENE_ID, Specialist_NPI, Encounter_Date
            HAVING _row_ = MIN(_row_);
        QUIT;

        /* Clean up */
        PROC DELETE DATA=WORK.Encounters_&year_data; RUN;
        PROC DELETE DATA=WORK.Carrier_Stacked; RUN;
        PROC DELETE DATA=WORK.PreEncounter_Visits; RUN;
        PROC DELETE DATA=WORK.PCP_Candidates; RUN;

        %PUT NOTE: &prefix PCP assignment complete for &year_data;

    %END;

%MEND assign_pcp;


/* ============================================================ */
/* Execute: Assign PCPs for all specialties                      */
/* ============================================================ */

%assign_pcp(&ortho_prefix);
%assign_pcp(&cardio_prefix);
%assign_pcp(&derm_prefix);
%assign_pcp(&cardioem_prefix);
