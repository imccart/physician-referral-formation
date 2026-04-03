/* ------------------------------------------------------------ */
/* TITLE:         Configuration and shared macros                */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 0 of 6                                       */
/* PURPOSE:       Global parameters, library refs, utility macros*/
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Library References (verify in VRDC before running)            */
/* ============================================================ */
LIBNAME PL027710 "/workspace/pl027710";


/* ============================================================ */
/* Year Range                                                    */
/* ============================================================ */
%LET year_start = 2013;
%LET year_end   = 2018;
%LET carrier_start = 2012;  /* one year before year_start for lookback */
/* MDPPAS year mapping (2018 not available; fall back to 2017) */
%LET mdppas_2009 = 2009;
%LET mdppas_2010 = 2010;
%LET mdppas_2011 = 2011;
%LET mdppas_2012 = 2012;
%LET mdppas_2013 = 2013;
%LET mdppas_2014 = 2014;
%LET mdppas_2015 = 2015;
%LET mdppas_2016 = 2016;
%LET mdppas_2017 = 2017;
%LET mdppas_2018 = 2017;  /* fallback */


/* ============================================================ */
/* Specialty-Specific Parameters                                 */
/* ============================================================ */

/* Orthopedic Surgery: major joint replacement (inpatient DRG)   */
%LET ortho_drg_list = "469","470","461","462","480","481","482","483",
                      "485","486","487","488","489","507","508","510","511","512";
%LET ortho_spec_names = "Orthopedic Surgery";
%LET ortho_prefix = Ortho;
%LET ortho_outcome_days = 90;
%LET ortho_anchor = inpatient;

/* Cardiology: pacemaker implantation (inpatient DRG)            */
%LET cardio_drg_list = "242","243","244";
%LET cardio_spec_names = "Cardiology", "Clinical Cardiac Electrophysiology";
%LET cardio_prefix = Cardio;
%LET cardio_outcome_days = 30;
%LET cardio_anchor = inpatient;

/* Dermatology: new patient E&M visits (carrier CPT)             */
%LET derm_cpt_list = "99201","99202","99203","99204","99205";
%LET derm_spec_names = "Dermatology";
%LET derm_prefix = Derm;
%LET derm_outcome_days = 0;   /* volume-based only */
%LET derm_anchor = carrier;

/* Cardiology E&M: new patient visits (carrier CPT)              */
%LET cardioem_cpt_list = "99201","99202","99203","99204","99205";
%LET cardioem_spec_names = "Cardiology", "Clinical Cardiac Electrophysiology";
%LET cardioem_prefix = CardioEM;
%LET cardioem_outcome_days = 0;   /* volume-based only */
%LET cardioem_anchor = carrier;


/* ============================================================ */
/* Common Inpatient Admission Filters                            */
/* ============================================================ */

/* Admission source: physician referral (1), clinic (2), HMO (3) */
%LET admsn_source_filter = CLM_SRC_IP_ADMSN_CD IN ("1","2","3");

/* Claim type: inpatient hospital only */
%LET claim_type_filter = NCH_CLM_TYPE_CD = "60";

/* Admission type: elective */
%LET elective_filter = CLM_IP_ADMSN_TYPE_CD = "3";


/* ============================================================ */
/* Provider Exclusion Macro                                      */
/* ============================================================ */

/* Exclude rehab, psych, long-term care, children's hospitals.   */
/* Applied via PRVDR_NUM prefix/range checks (CMS provider IDs). */
/* Rehab: provider 3025-3099 range (3rd digit = 'R')             */
/* Psych: provider starts with certain codes                     */
/* LTC:   provider range                                         */
/* Children's: provider range                                    */
/* For simplicity, filter on claim type = 60 (short-term acute)  */
/* which already excludes most of these.                         */

%MACRO provider_exclusions;
    AND NCH_CLM_TYPE_CD IN ("60", "61")
%MEND provider_exclusions;


/* ============================================================ */
/* Utility: Stack Inpatient Claims for a Year                    */
/* ============================================================ */

%MACRO stack_inpatient(year);
    DATA WORK.InpatientStays_&year;
        SET RIF&year..INPATIENT_CLAIMS_01
            RIF&year..INPATIENT_CLAIMS_02
            RIF&year..INPATIENT_CLAIMS_03
            RIF&year..INPATIENT_CLAIMS_04
            RIF&year..INPATIENT_CLAIMS_05
            RIF&year..INPATIENT_CLAIMS_06
            RIF&year..INPATIENT_CLAIMS_07
            RIF&year..INPATIENT_CLAIMS_08
            RIF&year..INPATIENT_CLAIMS_09
            RIF&year..INPATIENT_CLAIMS_10
            RIF&year..INPATIENT_CLAIMS_11
            RIF&year..INPATIENT_CLAIMS_12;
    RUN;
%MEND stack_inpatient;


/* ============================================================ */
/* Utility: Extract Carrier Claims for One Month                 */
/* ============================================================ */

/* Extracts E&M carrier claims (BETOS starts with "M") for       */
/* patients in {patient_table}. Outputs to {out_table}.          */

%MACRO carrier_month_extract(year, month, patient_table, out_table);
    PROC SQL;
        CREATE TABLE &out_table AS
        SELECT DISTINCT
            PRF_PHYSN_NPI AS Physician_ID,
            a.BENE_ID,
            CLM_THRU_DT AS Visit_Date,
            TAX_NUM AS Phy_Tax_ID
        FROM RIF&year..BCARRIER_LINE_%SYSFUNC(PUTN(&month, Z2.)) AS a
        INNER JOIN &patient_table AS b
            ON a.BENE_ID = b.BENE_ID
        WHERE SUBSTR(BETOS_CD, 1, 1) = "M"
        ORDER BY a.BENE_ID, CLM_THRU_DT, PRF_PHYSN_NPI;
    QUIT;
%MEND carrier_month_extract;


/* ============================================================ */
/* Utility: Stack Monthly Carrier Tables into Annual             */
/* ============================================================ */

%MACRO stack_carrier_months(prefix, year);
    DATA PL027710.&prefix.Carrier_&year;
        SET WORK.&prefix.Carrier_m1_&year
            WORK.&prefix.Carrier_m2_&year
            WORK.&prefix.Carrier_m3_&year
            WORK.&prefix.Carrier_m4_&year
            WORK.&prefix.Carrier_m5_&year
            WORK.&prefix.Carrier_m6_&year
            WORK.&prefix.Carrier_m7_&year
            WORK.&prefix.Carrier_m8_&year
            WORK.&prefix.Carrier_m9_&year
            WORK.&prefix.Carrier_m10_&year
            WORK.&prefix.Carrier_m11_&year
            WORK.&prefix.Carrier_m12_&year;
    RUN;
%MEND stack_carrier_months;


/* ============================================================ */
/* MDPPAS Library Reference (verify version in VRDC)             */
/* ============================================================ */
/* MDPPAS tables named: MD_PPAS.MDPPAS_V24_{year}               */
/* Field: SPEC_PRIM_1_NAME for specialty name                    */
/* Field: SPEC_BROAD for broad specialty (1 = primary care)      */
LIBNAME MD_PPAS "/workspace/md_ppas";
