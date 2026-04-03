/* ------------------------------------------------------------ */
/* TITLE:         Identify specialist-patient anchor encounters  */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 1 of 6                                       */
/* INPUT:         Inpatient RIF (ortho, cardio), Carrier RIF     */
/*                (derm), MDPPAS (derm specialist filter)         */
/* OUTPUT:        {Prefix}Encounters_{year},                     */
/*                {Prefix}Patients_Unique                        */
/* ------------------------------------------------------------ */

/* ============================================================ */
/* Inpatient Anchor: DRG-based (ortho, cardio)                   */
/* ============================================================ */

/* Extract inpatient stays matching specialty DRGs.              */
/* Filters: elective admission, physician/clinic/HMO referral,  */
/*          hospital claim type.                                 */
/* Specialist = operating physician (OP_PHYSN_NPI).              */

%MACRO extract_inpatient_anchor(prefix, drg_list_var);

    %DO year_data = &year_start %TO &year_end;

        /* Stack all monthly inpatient files for this year */
        %stack_inpatient(&year_data);

        /* Filter to specialty DRGs with standard admission criteria */
        PROC SQL;
            DROP TABLE PL027710.&prefix.Encounters_&year_data;
            CREATE TABLE PL027710.&prefix.Encounters_&year_data AS
            SELECT
                BENE_ID,
                CLM_ID,
                OP_PHYSN_NPI AS Specialist_NPI,
                CLM_ADMSN_DT AS Encounter_Date,
                NCH_BENE_DSCHRG_DT AS Discharge_Date,
                CLM_FROM_DT,
                CLM_THRU_DT,
                ORG_NPI_NUM AS Facility_ID,
                CLM_DRG_CD,
                CLM_PMT_AMT,
                CLM_TOT_CHRG_AMT,
                PTNT_DSCHRG_STUS_CD AS Discharge_Status
            FROM WORK.InpatientStays_&year_data
            WHERE &admsn_source_filter
              AND &claim_type_filter
              AND &elective_filter
              AND CLM_DRG_CD IN (&&&drg_list_var)
              AND OP_PHYSN_NPI NE ''
              AND OP_PHYSN_NPI NE '0000000000';
        QUIT;

        /* Unique patients for this year (for carrier extraction) */
        PROC SQL;
            DROP TABLE WORK.&prefix.Patient_&year_data;
            CREATE TABLE WORK.&prefix.Patient_&year_data AS
            SELECT DISTINCT BENE_ID, &year_data AS Year
            FROM PL027710.&prefix.Encounters_&year_data;
        QUIT;

        /* Clean up WORK */
        PROC DELETE DATA=WORK.InpatientStays_&year_data; RUN;

    %END;

    /* Stack patient lists across all years */
    DATA PL027710.&prefix.Patients_ByYear;
        SET
        %DO y = &year_start %TO &year_end;
            WORK.&prefix.Patient_&y
        %END;
        ;
    RUN;

    /* Deduplicate to unique patients (for carrier extraction) */
    PROC SQL;
        DROP TABLE PL027710.&prefix.Patients_Unique;
        CREATE TABLE PL027710.&prefix.Patients_Unique AS
        SELECT DISTINCT BENE_ID
        FROM PL027710.&prefix.Patients_ByYear;
    QUIT;

    /* Clean up per-year patient tables */
    %DO y = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.&prefix.Patient_&y; RUN;
    %END;

%MEND extract_inpatient_anchor;


/* ============================================================ */
/* Carrier Anchor: CPT-based (derm)                              */
/* ============================================================ */

/* For specialties without a natural inpatient procedure (e.g.,  */
/* dermatology), use new patient E&M visits on carrier claims    */
/* as the anchor event. Specialist identified via MDPPAS.        */

%MACRO extract_carrier_anchor(prefix, cpt_list_var, spec_names_var);

    %DO year_data = &year_start %TO &year_end;

        /* Get specialist NPIs from MDPPAS for this year */
        PROC SQL;
            DROP TABLE WORK.&prefix.Specialists_&year_data;
            CREATE TABLE WORK.&prefix.Specialists_&year_data AS
            SELECT DISTINCT NPI
            FROM MD_PPAS.MDPPAS_V24_&&mdppas_&year_data
            WHERE SPEC_PRIM_1_NAME IN (&&&spec_names_var);
        QUIT;

        /* Extract new patient E&M visits performed by specialists */
        /* Loop over monthly carrier files, stack results           */
        %DO m = 1 %TO 12;
            PROC SQL;
                CREATE TABLE WORK.&prefix.Anchor_m&m._&year_data AS
                SELECT DISTINCT
                    a.PRF_PHYSN_NPI AS Specialist_NPI,
                    a.BENE_ID,
                    a.CLM_THRU_DT AS Encounter_Date,
                    a.HCPCS_CD
                FROM RIF&year_data..BCARRIER_LINE_%SYSFUNC(PUTN(&m, Z2.)) AS a
                INNER JOIN WORK.&prefix.Specialists_&year_data AS b
                    ON a.PRF_PHYSN_NPI = b.NPI
                WHERE a.HCPCS_CD IN (&&&cpt_list_var);
            QUIT;
        %END;

        /* Stack monthly results */
        DATA WORK.&prefix.Anchor_Raw_&year_data;
            SET
            %DO m = 1 %TO 12;
                WORK.&prefix.Anchor_m&m._&year_data
            %END;
            ;
        RUN;

        /* Deduplicate: keep first visit per patient-specialist-year */
        PROC SORT DATA=WORK.&prefix.Anchor_Raw_&year_data;
            BY BENE_ID Specialist_NPI Encounter_Date;
        RUN;

        DATA PL027710.&prefix.Encounters_&year_data;
            SET WORK.&prefix.Anchor_Raw_&year_data;
            BY BENE_ID Specialist_NPI;
            IF FIRST.Specialist_NPI;
            /* Add placeholder columns to match inpatient schema */
            LENGTH CLM_ID $15 Facility_ID $10 CLM_DRG_CD $3;
            CLM_ID = '';
            Discharge_Date = Encounter_Date;
            Facility_ID = '';
            CLM_DRG_CD = '';
            CLM_PMT_AMT = .;
            CLM_TOT_CHRG_AMT = .;
            Discharge_Status = '';
            DROP HCPCS_CD;
        RUN;

        /* Unique patients for this year */
        PROC SQL;
            DROP TABLE WORK.&prefix.Patient_&year_data;
            CREATE TABLE WORK.&prefix.Patient_&year_data AS
            SELECT DISTINCT BENE_ID, &year_data AS Year
            FROM PL027710.&prefix.Encounters_&year_data;
        QUIT;

        /* Clean up monthly WORK tables */
        %DO m = 1 %TO 12;
            PROC DELETE DATA=WORK.&prefix.Anchor_m&m._&year_data; RUN;
        %END;
        PROC DELETE DATA=WORK.&prefix.Anchor_Raw_&year_data; RUN;
        PROC DELETE DATA=WORK.&prefix.Specialists_&year_data; RUN;

    %END;

    /* Stack patient lists across all years */
    DATA PL027710.&prefix.Patients_ByYear;
        SET
        %DO y = &year_start %TO &year_end;
            WORK.&prefix.Patient_&y
        %END;
        ;
    RUN;

    /* Deduplicate to unique patients */
    PROC SQL;
        DROP TABLE PL027710.&prefix.Patients_Unique;
        CREATE TABLE PL027710.&prefix.Patients_Unique AS
        SELECT DISTINCT BENE_ID
        FROM PL027710.&prefix.Patients_ByYear;
    QUIT;

    /* Clean up per-year patient tables */
    %DO y = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.&prefix.Patient_&y; RUN;
    %END;

%MEND extract_carrier_anchor;


/* ============================================================ */
/* Execute: Extract anchor events for all specialties            */
/* ============================================================ */

/* Orthopedic Surgery */
%extract_inpatient_anchor(&ortho_prefix, ortho_drg_list);

/* Cardiology (pacemaker) */
%extract_inpatient_anchor(&cardio_prefix, cardio_drg_list);

/* Dermatology (new patient E&M) */
%extract_carrier_anchor(&derm_prefix, derm_cpt_list, derm_spec_names);

/* Cardiology E&M (new patient visits) */
%extract_carrier_anchor(&cardioem_prefix, cardioem_cpt_list, cardioem_spec_names);
