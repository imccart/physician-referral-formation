/* ------------------------------------------------------------ */
/* TITLE:   Patient-level outcomes over a 2-year post-encounter  */
/*          window, per specialist encounter (the index event).  */
/* RUNS IN THE VRDC. Run 0_config_outcomes.sas first.            */
/*                                                               */
/* INPUT:   PL027710.{Prefix}Encounters_&y (BENE_ID,             */
/*          Specialist_NPI, Encounter_Date) -- built by the      */
/*          referral pipeline in ../../data-build/sas/.          */
/* OUTPUT:  PL027710.{Prefix}PatientOutcomes (one row per        */
/*          BENE_ID x Specialist_NPI x Encounter_Date):          */
/*          n_admit, n_admit_ne, death_2yr, spend_total          */
/*                                                               */
/* Token/pattern sources (all confirmed, not invented):          */
/*   admissions window  -> ../../data-build/sas/5_outcomes.sas    */
/*   spending formulas   -> healthcare-vi/.../physician-hospital-VI:*/
/*      1_InpatientStays.sas:47, 3_OutpatientStays.sas:79,        */
/*      4_PhysicianData.sas:52,116                                */
/*   death (BENE_DEATH_DT) -> physician-variation/3_beneficiary.sas:33*/
/* ------------------------------------------------------------ */


/* Stack the 12 monthly BCARRIER_LINE files for a year.          */
/* (BCARRIER_LINE confirmed: denials/1-build-denials.sas:59,      */
/*  physician-variation/4_carrier_cardiologist.sas:93)            */
%MACRO stack_carrier_out(year, out);
    DATA &out;
        SET RIF&year..BCARRIER_LINE_01 RIF&year..BCARRIER_LINE_02
            RIF&year..BCARRIER_LINE_03 RIF&year..BCARRIER_LINE_04
            RIF&year..BCARRIER_LINE_05 RIF&year..BCARRIER_LINE_06
            RIF&year..BCARRIER_LINE_07 RIF&year..BCARRIER_LINE_08
            RIF&year..BCARRIER_LINE_09 RIF&year..BCARRIER_LINE_10
            RIF&year..BCARRIER_LINE_11 RIF&year..BCARRIER_LINE_12;
    RUN;
%MEND stack_carrier_out;


%MACRO patient_outcomes(prefix);

  %DO y = &enc_start %TO &enc_end;

    %LET y1 = %EVAL(&y + 1);
    %LET y2 = %EVAL(&y + 2);   /* 2-yr window can reach into y+2 */

    /* --- index encounters (one row per bene x specialist x date) --- */
    PROC SQL;
        CREATE TABLE WORK.idx AS
        SELECT DISTINCT BENE_ID, Specialist_NPI, Encounter_Date
        FROM PL027710.&prefix.Encounters_&y;
    QUIT;

    /* ---------- 1) Inpatient: admissions + spending in window ---------- */
    /* Stack inpatient claims for y, y+1, y+2 to cover the window.         */
    %stack_inpatient_out(&y,  WORK.ip0);
    %stack_inpatient_out(&y1, WORK.ip1);
    %stack_inpatient_out(&y2, WORK.ip2);
    DATA WORK.ip;
        SET WORK.ip0 WORK.ip1 WORK.ip2;
        WHERE NCH_CLM_TYPE_CD = "60";                 /* short-term acute */
        nonelective  = (CLM_IP_ADMSN_TYPE_CD NE "3"); /* unplanned        */
        ip_spend     = SUM(CLM_PMT_AMT, NCH_PRMRY_PYR_CLM_PD_AMT, NCH_IP_TOT_DDCTN_AMT);
        KEEP BENE_ID CLM_ADMSN_DT nonelective ip_spend;
    RUN;
    PROC DELETE DATA=WORK.ip0 WORK.ip1 WORK.ip2; RUN;

    PROC SQL;
        CREATE TABLE WORK.admit AS
        SELECT a.BENE_ID, a.Specialist_NPI, a.Encounter_Date,
               SUM( CASE WHEN b.BENE_ID IS NOT NULL THEN 1 ELSE 0 END )              AS n_admit,
               SUM( CASE WHEN b.nonelective = 1     THEN 1 ELSE 0 END )              AS n_admit_ne,
               SUM( COALESCE(b.ip_spend, 0) )                                        AS ip_spend
        FROM WORK.idx AS a
        LEFT JOIN WORK.ip AS b
          ON a.BENE_ID = b.BENE_ID
         AND b.CLM_ADMSN_DT >  a.Encounter_Date
         AND b.CLM_ADMSN_DT <= a.Encounter_Date + &window_days
        GROUP BY a.BENE_ID, a.Specialist_NPI, a.Encounter_Date;
    QUIT;

    /* ---------- 2) Outpatient spending in window ---------- */
    %stack_outpatient_out(&y,  WORK.op0);
    %stack_outpatient_out(&y1, WORK.op1);
    %stack_outpatient_out(&y2, WORK.op2);
    DATA WORK.op;
        SET WORK.op0 WORK.op1 WORK.op2;
        op_spend = SUM(CLM_PMT_AMT, NCH_PRMRY_PYR_CLM_PD_AMT,
                       NCH_BENE_PTB_DDCTBL_AMT, NCH_BENE_PTB_COINSRNC_AMT);
        KEEP BENE_ID CLM_THRU_DT op_spend;
    RUN;
    PROC DELETE DATA=WORK.op0 WORK.op1 WORK.op2; RUN;

    PROC SQL;
        CREATE TABLE WORK.op_w AS
        SELECT a.BENE_ID, a.Specialist_NPI, a.Encounter_Date,
               SUM( COALESCE(b.op_spend, 0) ) AS op_spend
        FROM WORK.idx AS a
        LEFT JOIN WORK.op AS b
          ON a.BENE_ID = b.BENE_ID
         AND b.CLM_THRU_DT >  a.Encounter_Date
         AND b.CLM_THRU_DT <= a.Encounter_Date + &window_days
        GROUP BY a.BENE_ID, a.Specialist_NPI, a.Encounter_Date;
    QUIT;

    /* ---------- 3) Carrier (Part B) spending in window ---------- */
    %stack_carrier_out(&y,  WORK.ca0);
    %stack_carrier_out(&y1, WORK.ca1);
    %stack_carrier_out(&y2, WORK.ca2);
    DATA WORK.ca;
        SET WORK.ca0 WORK.ca1 WORK.ca2;
        ca_spend = SUM(LINE_NCH_PMT_AMT, LINE_BENE_PRMRY_PYR_PD_AMT,
                       LINE_COINSRNC_AMT, LINE_BENE_PTB_DDCTBL_AMT);
        KEEP BENE_ID CLM_THRU_DT ca_spend;
    RUN;
    PROC DELETE DATA=WORK.ca0 WORK.ca1 WORK.ca2; RUN;

    PROC SQL;
        CREATE TABLE WORK.ca_w AS
        SELECT a.BENE_ID, a.Specialist_NPI, a.Encounter_Date,
               SUM( COALESCE(b.ca_spend, 0) ) AS ca_spend
        FROM WORK.idx AS a
        LEFT JOIN WORK.ca AS b
          ON a.BENE_ID = b.BENE_ID
         AND b.CLM_THRU_DT >  a.Encounter_Date
         AND b.CLM_THRU_DT <= a.Encounter_Date + &window_days
        GROUP BY a.BENE_ID, a.Specialist_NPI, a.Encounter_Date;
    QUIT;

    /* ---------- 4) Death within 2 years (MBSF) ---------- */
    /* Stack MBSF for y..y+2 and take earliest non-missing death date.     */
    DATA WORK.death;
        SET MBSF.MBSF_ABCD_&y (KEEP=BENE_ID BENE_DEATH_DT)
            MBSF.MBSF_ABCD_&y1(KEEP=BENE_ID BENE_DEATH_DT)
            MBSF.MBSF_ABCD_&y2(KEEP=BENE_ID BENE_DEATH_DT);
        WHERE BENE_DEATH_DT IS NOT NULL;
    RUN;
    PROC SQL;
        CREATE TABLE WORK.death_w AS
        SELECT a.BENE_ID, a.Specialist_NPI, a.Encounter_Date,
               MAX( CASE WHEN b.BENE_DEATH_DT > a.Encounter_Date
                          AND b.BENE_DEATH_DT <= a.Encounter_Date + &window_days
                         THEN 1 ELSE 0 END ) AS death_2yr
        FROM WORK.idx AS a
        LEFT JOIN WORK.death AS b ON a.BENE_ID = b.BENE_ID
        GROUP BY a.BENE_ID, a.Specialist_NPI, a.Encounter_Date;
    QUIT;

    /* ---------- Combine and append to the specialty-level table ---------- */
    PROC SQL;
        CREATE TABLE WORK.out_&y AS
        SELECT a.BENE_ID, a.Specialist_NPI, a.Encounter_Date,
               a.n_admit, a.n_admit_ne,
               COALESCE(d.death_2yr, 0)                                    AS death_2yr,
               SUM(a.ip_spend, COALESCE(o.op_spend,0), COALESCE(c.ca_spend,0)) AS spend_total
        FROM WORK.admit  AS a
        LEFT JOIN WORK.op_w    AS o ON a.BENE_ID=o.BENE_ID AND a.Specialist_NPI=o.Specialist_NPI AND a.Encounter_Date=o.Encounter_Date
        LEFT JOIN WORK.ca_w    AS c ON a.BENE_ID=c.BENE_ID AND a.Specialist_NPI=c.Specialist_NPI AND a.Encounter_Date=c.Encounter_Date
        LEFT JOIN WORK.death_w AS d ON a.BENE_ID=d.BENE_ID AND a.Specialist_NPI=d.Specialist_NPI AND a.Encounter_Date=d.Encounter_Date;
    QUIT;

    PROC DELETE DATA=WORK.ip WORK.op WORK.ca WORK.death
                     WORK.admit WORK.op_w WORK.ca_w WORK.death_w WORK.idx; RUN;

  %END;

  /* Stack encounter years into one specialty outcome table */
  DATA PL027710.&prefix.PatientOutcomes;
      SET %DO y = &enc_start %TO &enc_end; WORK.out_&y %END; ;
  RUN;
  %DO y = &enc_start %TO &enc_end; PROC DELETE DATA=WORK.out_&y; RUN; %END;

%MEND patient_outcomes;


/* Run for each specialty prefix (Ortho Cardio Derm CardioEM). */
%patient_outcomes(Ortho);
%patient_outcomes(Cardio);
%patient_outcomes(Derm);
%patient_outcomes(CardioEM);
