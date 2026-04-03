/* ------------------------------------------------------------ */
/* TITLE:         Specialist quality measures                    */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 5 of 6                                       */
/* INPUT:         {Prefix}Encounters_{year} from Step 1,         */
/*                Inpatient RIF for readmission lookforward       */
/* OUTPUT:        {Prefix}Quality_All                            */
/* ------------------------------------------------------------ */



/* ============================================================ */
/* Inpatient Outcomes: Unplanned Readmission Rate                */
/* ============================================================ */

/* For inpatient-anchored specialties (ortho, cardio), compute   */
/* unplanned readmission rate within outcome_days of discharge.  */
/* spec_qual = 1 - readmit_rate (higher = better).               */
/*                                                               */
/* Simplified from old 2_Outcomes.sas: no ICD complication logic.*/
/* Readmission = any non-elective inpatient admission within     */
/* the outcome window after initial discharge.                   */

%MACRO compute_readmission_quality(prefix, outcome_days);

    %DO year_data = &year_start %TO &year_end;
        %LET year_post = %EVAL(&year_data + 1);

        /* Initial encounters for this year */
        PROC SQL;
            DROP TABLE WORK.IP_Initial;
            CREATE TABLE WORK.IP_Initial AS
            SELECT DISTINCT
                BENE_ID,
                Specialist_NPI,
                Encounter_Date,
                Discharge_Date,
                CLM_ID
            FROM PL027710.&prefix.Encounters_&year_data;
        QUIT;

        /* Stack inpatient claims from current year + first 3 months of next */
        /* to capture readmissions near year end                              */
        DATA WORK.IP_Lookforward;
            SET RIF&year_data..INPATIENT_CLAIMS_01
                RIF&year_data..INPATIENT_CLAIMS_02
                RIF&year_data..INPATIENT_CLAIMS_03
                RIF&year_data..INPATIENT_CLAIMS_04
                RIF&year_data..INPATIENT_CLAIMS_05
                RIF&year_data..INPATIENT_CLAIMS_06
                RIF&year_data..INPATIENT_CLAIMS_07
                RIF&year_data..INPATIENT_CLAIMS_08
                RIF&year_data..INPATIENT_CLAIMS_09
                RIF&year_data..INPATIENT_CLAIMS_10
                RIF&year_data..INPATIENT_CLAIMS_11
                RIF&year_data..INPATIENT_CLAIMS_12
                RIF&year_post..INPATIENT_CLAIMS_01
                RIF&year_post..INPATIENT_CLAIMS_02
                RIF&year_post..INPATIENT_CLAIMS_03;
        RUN;

        /* Find unplanned readmissions within outcome window */
        PROC SQL;
            DROP TABLE WORK.Readmissions_&year_data;
            CREATE TABLE WORK.Readmissions_&year_data AS
            SELECT DISTINCT
                a.BENE_ID,
                a.Specialist_NPI,
                a.Encounter_Date,
                b.CLM_ADMSN_DT AS Readmit_Date
            FROM WORK.IP_Initial AS a
            INNER JOIN WORK.IP_Lookforward AS b
                ON a.BENE_ID = b.BENE_ID
            WHERE b.NCH_CLM_TYPE_CD = "60"
              AND b.CLM_IP_ADMSN_TYPE_CD NE "3"     /* non-elective */
              AND b.CLM_ADMSN_DT > a.Discharge_Date
              AND b.CLM_ADMSN_DT <= (a.Discharge_Date + &outcome_days);
        QUIT;

        /* Per-encounter readmission indicator */
        PROC SQL;
            DROP TABLE WORK.&prefix.Outcomes_&year_data;
            CREATE TABLE WORK.&prefix.Outcomes_&year_data AS
            SELECT
                a.Specialist_NPI,
                a.BENE_ID,
                a.Encounter_Date,
                CASE WHEN b.Readmit_Date IS NOT NULL THEN 1 ELSE 0 END AS Readmit
            FROM WORK.IP_Initial AS a
            LEFT JOIN WORK.Readmissions_&year_data AS b
                ON a.BENE_ID = b.BENE_ID
                AND a.Specialist_NPI = b.Specialist_NPI
                AND a.Encounter_Date = b.Encounter_Date;
        QUIT;

        /* Clean up */
        PROC DELETE DATA=WORK.IP_Initial; RUN;
        PROC DELETE DATA=WORK.IP_Lookforward; RUN;
        PROC DELETE DATA=WORK.Readmissions_&year_data; RUN;

    %END;

    /* Stack all years and compute specialist-level quality */
    DATA WORK.&prefix.Outcomes_All;
        SET
        %DO y = &year_start %TO &year_end;
            WORK.&prefix.Outcomes_&y
        %END;
        ;
    RUN;

    PROC SQL;
        DROP TABLE PL027710.&prefix.Quality_All;
        CREATE TABLE PL027710.&prefix.Quality_All AS
        SELECT
            Specialist_NPI AS Specialist_ID,
            COUNT(*) AS Total_Spec_Patients,
            1 - (SUM(Readmit) / COUNT(*)) AS Spec_Qual
        FROM WORK.&prefix.Outcomes_All
        GROUP BY Specialist_NPI;
    QUIT;

    /* Clean up per-year outcomes */
    %DO y = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.&prefix.Outcomes_&y; RUN;
    %END;
    PROC DELETE DATA=WORK.&prefix.Outcomes_All; RUN;

%MEND compute_readmission_quality;


/* ============================================================ */
/* Carrier Outcomes: Volume-Based (derm)                         */
/* ============================================================ */

/* For carrier-anchored specialties, no natural outcome metric.  */
/* Compute total patient volume as the only quality proxy.       */
/* spec_qual is set to missing.                                  */

%MACRO compute_volume_quality(prefix);

    PROC SQL;
        DROP TABLE PL027710.&prefix.Quality_All;
        CREATE TABLE PL027710.&prefix.Quality_All AS
        SELECT
            Specialist_NPI AS Specialist_ID,
            COUNT(DISTINCT BENE_ID) AS Total_Spec_Patients,
            . AS Spec_Qual
        FROM (
            %DO y = &year_start %TO &year_end;
                %IF &y > &year_start %THEN UNION ALL;
                SELECT Specialist_NPI, BENE_ID
                FROM PL027710.&prefix.Encounters_&y
            %END;
        )
        GROUP BY Specialist_NPI;
    QUIT;

%MEND compute_volume_quality;


/* ============================================================ */
/* Execute: Compute quality for all specialties                  */
/* ============================================================ */

/* Ortho: 90-day readmission */
%compute_readmission_quality(&ortho_prefix, &ortho_outcome_days);

/* Cardio: 30-day readmission */
%compute_readmission_quality(&cardio_prefix, &cardio_outcome_days);

/* Derm: volume only */
%compute_volume_quality(&derm_prefix);

/* CardioEM: volume only */
%compute_volume_quality(&cardioem_prefix);
