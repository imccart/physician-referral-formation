/* ------------------------------------------------------------ */
/* TITLE:         Aggregate referral pairs and export CSV         */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 6 of 6                                       */
/* INPUT:         {Prefix}ValidPairs_{year} from Step 4,         */
/*                {Prefix}Quality_All from Step 5                 */
/* OUTPUT:        ReferralPairs_{Prefix}.csv                     */
/* ------------------------------------------------------------ */



/* ============================================================ */
/* Aggregate and Export                                           */
/* ============================================================ */

/* Stack valid pairs across years, aggregate to                  */
/* Practice_ID x Specialist_ID x Year, join quality measures,    */
/* and export one CSV per specialty.                              */
/*                                                               */
/* Output columns:                                               */
/*   Practice_ID      - PCP NPI                                  */
/*   Specialist_ID    - Specialist NPI                            */
/*   Year             - Calendar year                             */
/*   Shared_Patients  - Count of unique patients linking pair     */
/*   Spec_Qual        - 1 - readmit_rate (missing for derm)      */
/*   Total_Spec_Patients - Specialist total patient volume        */
/*                                                               */
/* NOTE: Phy_Tax_ID is used internally but NOT exported (CMS     */
/* rule: no raw TIN/EIN in extracts).                            */

%MACRO aggregate_and_export(prefix);

    /* Stack valid pairs across all years */
    DATA WORK.&prefix.AllPairs;
        SET
        %DO y = &year_start %TO &year_end;
            PL027710.&prefix.ValidPairs_&y
        %END;
        ;
    RUN;

    /* Aggregate to Practice_ID x Specialist_ID x Year */
    PROC SQL;
        DROP TABLE WORK.&prefix.Aggregated;
        CREATE TABLE WORK.&prefix.Aggregated AS
        SELECT
            Practice_ID,
            Specialist_ID,
            Year,
            COUNT(DISTINCT BENE_ID) AS Shared_Patients
        FROM WORK.&prefix.AllPairs
        GROUP BY Practice_ID, Specialist_ID, Year;
    QUIT;

    /* Join specialist quality measures */
    PROC SQL;
        DROP TABLE WORK.&prefix.Final;
        CREATE TABLE WORK.&prefix.Final AS
        SELECT
            a.Practice_ID,
            a.Specialist_ID,
            a.Year,
            a.Shared_Patients,
            b.Spec_Qual,
            b.Total_Spec_Patients
        FROM WORK.&prefix.Aggregated AS a
        LEFT JOIN PL027710.&prefix.Quality_All AS b
            ON a.Specialist_ID = b.Specialist_ID
        ORDER BY a.Practice_ID, a.Specialist_ID, a.Year;
    QUIT;

    /* ---- Diagnostics before export ---- */
    TITLE "Diagnostic: &prefix referral pairs";

    PROC SQL;
        SELECT
            COUNT(*) AS Total_Rows,
            COUNT(DISTINCT Practice_ID) AS Unique_PCPs,
            COUNT(DISTINCT Specialist_ID) AS Unique_Specialists,
            MIN(Year) AS Min_Year,
            MAX(Year) AS Max_Year
        FROM WORK.&prefix.Final;
    QUIT;

    PROC SQL;
        SELECT Year, COUNT(*) AS Rows, COUNT(DISTINCT Practice_ID) AS PCPs,
               COUNT(DISTINCT Specialist_ID) AS Specialists
        FROM WORK.&prefix.Final
        GROUP BY Year
        ORDER BY Year;
    QUIT;

    TITLE;

    /* ---- Save to permanent library ---- */
    DATA PL027710.ReferralPairs_&prefix;
        SET WORK.&prefix.Final;
    RUN;

    %PUT NOTE: Saved PL027710.ReferralPairs_&prefix;

    /* Clean up */
    PROC DELETE DATA=WORK.&prefix.AllPairs; RUN;
    PROC DELETE DATA=WORK.&prefix.Aggregated; RUN;
    PROC DELETE DATA=WORK.&prefix.Final; RUN;

%MEND aggregate_and_export;


/* ============================================================ */
/* Execute: Aggregate all specialties                            */
/* ============================================================ */

%aggregate_and_export(&ortho_prefix);
%aggregate_and_export(&cardio_prefix);
%aggregate_and_export(&derm_prefix);
%aggregate_and_export(&cardioem_prefix);


/* ============================================================ */
/* Export: Full and Large versions per specialty                  */
/* ============================================================ */

/* Full = all pairs. Large = cell-size masked (>10 patients per  */
/* PCP and per specialist) for CMS export approval.              */

%MACRO export_referrals(prefix, include_qual);

    /* PCP total patients per year */
    PROC SQL;
        CREATE TABLE WORK.&prefix._PCP_Tot AS
        SELECT Practice_ID, Year,
               SUM(Shared_Patients) AS total_pcp_patients
        FROM PL027710.ReferralPairs_&prefix
        GROUP BY Practice_ID, Year;
    QUIT;

    /* Build main table with PCP totals attached */
    PROC SQL;
        CREATE TABLE WORK.&prefix._Main AS
        SELECT
            a.Practice_ID,
            a.Specialist_ID,
            a.Year,
            a.Shared_Patients,
            %IF &include_qual = 1 %THEN %DO;
            a.Spec_Qual,
            %END;
            a.Total_Spec_Patients,
            b.total_pcp_patients
        FROM PL027710.ReferralPairs_&prefix AS a
        LEFT JOIN WORK.&prefix._PCP_Tot AS b
            ON a.Practice_ID = b.Practice_ID
            AND a.Year = b.Year
        ORDER BY Practice_ID, Specialist_ID, Year;
    QUIT;

    /* Export 1: Full — all pairs, no masking */
    DATA PL027710.ReferralPairs_Full_&prefix;
        SET WORK.&prefix._Main;
    RUN;

    /* Export 2: Large — cell-size masked for CMS export */
    DATA PL027710.ReferralPairs_Large_&prefix;
        SET WORK.&prefix._Main;
        WHERE total_pcp_patients > 10 AND Total_Spec_Patients > 10;
    RUN;

    /* Clean up */
    PROC DELETE DATA=WORK.&prefix._PCP_Tot; RUN;
    PROC DELETE DATA=WORK.&prefix._Main; RUN;

%MEND export_referrals;

%export_referrals(&ortho_prefix, 1);
%export_referrals(&cardio_prefix, 1);
%export_referrals(&derm_prefix, 0);
%export_referrals(&cardioem_prefix, 0);


/* ============================================================ */
/* Final summary across all specialties                          */
/* ============================================================ */

TITLE "Pipeline complete: row counts per specialty";
%MACRO final_summary;
    %LET prefixes = Ortho Cardio Derm CardioEM;
    %DO i = 1 %TO 4;
        %LET p = %SCAN(&prefixes, &i);
        PROC SQL;
            SELECT "&p" AS Specialty, COUNT(*) AS Rows
            FROM PL027710.ReferralPairs_Large_&p;
        QUIT;
    %END;
%MEND final_summary;
%final_summary;
TITLE;
