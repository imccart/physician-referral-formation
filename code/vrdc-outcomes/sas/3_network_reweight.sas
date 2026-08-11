/* ------------------------------------------------------------ */
/* TITLE:   Reweight risk-adjusted outcomes by the actual vs.    */
/*          counterfactual referral network, per PCP.            */
/* RUNS IN THE VRDC. Run 1_patient_outcomes.sas and              */
/*          2_risk_adjust.sas first, and import the uploaded     */
/*          weights file into PL027710 (see below).              */
/*                                                               */
/* INPUTS:                                                        */
/*   PL027710.NetworkWeights  <- import of                       */
/*      data/output/vrdc_referral_network_weights.csv            */
/*      (pcp_npi, specialist_npi, hrr, specialty,                */
/*       w_actual, w_cf_noorg). Import with PROC IMPORT and keep  */
/*      pcp_npi / specialist_npi as CHARACTER (they are NPIs).   */
/*   PL027710.{Prefix}Residualized  <- from 2_risk_adjust.sas    */
/*                                                               */
/* OUTPUT:  PL027710.NetworkContrast -- one row per PCP with the  */
/*          actual-network and counterfactual-network expected    */
/*          risk-adjusted outcome, and their difference, for each */
/*          of the four outcomes. Review cell sizes (n_patients)  */
/*          before exporting from the enclave.                    */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Step 1. Stack residualized outcomes across specialties        */
/* ============================================================ */
DATA WORK.resid_all;
    SET PL027710.OrthoResidualized
        PL027710.CardioResidualized
        PL027710.DermResidualized
        PL027710.CardioEMResidualized;
    KEEP BENE_ID Specialist_NPI r_admit_ne r_admit r_death r_spend;
RUN;


/* ============================================================ */
/* Step 2. Per-specialist mean residual (join intermediate only) */
/* ============================================================ */
/* This is a grouping used to attach the network weight to        */
/* patients, NOT a reported specialist quality measure. n_pat is   */
/* carried for cell-size review.                                   */
PROC SQL;
    CREATE TABLE WORK.spec_mean AS
    SELECT Specialist_NPI,
           COUNT(*)         AS n_pat,
           MEAN(r_admit_ne) AS m_admit_ne,
           MEAN(r_admit)    AS m_admit,
           MEAN(r_death)    AS m_death,
           MEAN(r_spend)    AS m_spend
    FROM WORK.resid_all
    GROUP BY Specialist_NPI;
QUIT;


/* ============================================================ */
/* Step 3. Attach network weights to specialist means            */
/* ============================================================ */
PROC SQL;
    CREATE TABLE WORK.wl AS
    SELECT w.pcp_npi, w.hrr, w.specialty,
           w.w_actual, w.w_cf_noorg,
           s.n_pat,
           s.m_admit_ne, s.m_admit, s.m_death, s.m_spend
    FROM PL027710.NetworkWeights AS w
    INNER JOIN WORK.spec_mean AS s
      ON w.specialist_npi = s.Specialist_NPI;
QUIT;


/* ============================================================ */
/* Step 4. Per-PCP weighted averages: actual vs counterfactual   */
/* ============================================================ */
/* For each PCP and each outcome:                                 */
/*   actual = sum(w_actual * m) / sum(w_actual)                   */
/*   cf     = sum(w_cf     * m) / sum(w_cf)                       */
/*   contrast = cf - actual                                       */
PROC SQL;
    CREATE TABLE PL027710.NetworkContrast AS
    SELECT pcp_npi, hrr, specialty,
           COUNT(*)      AS n_specialists,
           SUM(n_pat)    AS n_patients,

           SUM(w_actual*m_admit_ne)/SUM(w_actual) AS act_admit_ne,
           SUM(w_cf_noorg*m_admit_ne)/SUM(w_cf_noorg) AS cf_admit_ne,
           CALCULATED cf_admit_ne - CALCULATED act_admit_ne AS d_admit_ne,

           SUM(w_actual*m_admit)/SUM(w_actual) AS act_admit,
           SUM(w_cf_noorg*m_admit)/SUM(w_cf_noorg) AS cf_admit,
           CALCULATED cf_admit - CALCULATED act_admit AS d_admit,

           SUM(w_actual*m_death)/SUM(w_actual) AS act_death,
           SUM(w_cf_noorg*m_death)/SUM(w_cf_noorg) AS cf_death,
           CALCULATED cf_death - CALCULATED act_death AS d_death,

           SUM(w_actual*m_spend)/SUM(w_actual) AS act_spend,
           SUM(w_cf_noorg*m_spend)/SUM(w_cf_noorg) AS cf_spend,
           CALCULATED cf_spend - CALCULATED act_spend AS d_spend
    FROM WORK.wl
    GROUP BY pcp_npi, hrr, specialty;
QUIT;


/* ============================================================ */
/* Step 5. Market (HRR) summary for export review                */
/* ============================================================ */
/* Per-PCP rows may be thin in some HRRs; an HRR-level mean of the */
/* contrasts is the cell-safer object to export and mirrors the    */
/* per-market welfare table in the paper.                          */
PROC SQL;
    CREATE TABLE PL027710.NetworkContrast_HRR AS
    SELECT specialty, hrr,
           COUNT(*)        AS n_pcps,
           SUM(n_patients) AS n_patients,
           MEAN(d_admit_ne) AS d_admit_ne,
           MEAN(d_admit)    AS d_admit,
           MEAN(d_death)    AS d_death,
           MEAN(d_spend)    AS d_spend
    FROM PL027710.NetworkContrast
    GROUP BY specialty, hrr;
QUIT;

PROC DELETE DATA=WORK.resid_all WORK.spec_mean WORK.wl; RUN;
