/* ------------------------------------------------------------ */
/* TITLE:   Patient-outcome build for the referral-network       */
/*          welfare extension -- configuration and shared macros */
/* AUTHOR:  Ian McCarthy, Emory University                       */
/* PURPOSE: Build patient-level Medicare outcomes (admissions,   */
/*          ED visits, 2-year mortality, spending), risk-adjust  */
/*          them, and reweight by uploaded actual vs.            */
/*          counterfactual referral-network weights.             */
/*                                                               */
/* RUNS INSIDE THE CMS VRDC. Extends the existing referral build */
/* in ../../data-build/sas/ (uses its {Prefix}Encounters_{year}  */
/* index tables in PL027710).                                    */
/*                                                               */
/* TOKEN SOURCES (every claim variable/file name below is copied */
/* from working sibling code, not invented):                     */
/*   - Local:  ../../data-build/sas/0_config.sas, 3_pcp_assignment.sas, 5_outcomes.sas */
/*   - physician-variation: 1_nstemi_episodes.sas, 3_beneficiary.sas, 5_residualize.sas */
/*   - referrals-and-learning: P1-patient-cci.sas, 2_Outcomes.sas    */
/*   - denials: 1-build-denials.sas                                  */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Libraries                                                     */
/* ============================================================ */
/* RIF<yyyy>, MBSF, and PL027710 are AUTO-MOUNTED on the seat.   */
/* Do NOT define any workspace LIBNAME. All built tables live in  */
/* PL027710; the uploaded network-weights file is imported into   */
/* PL027710, and outputs are exported from there manually.        */
/* MedPAR is available, but inpatient events come from            */
/* INPATIENT_CLAIMS (the source feeding MedPAR), matching the      */
/* existing referral build. This outcome build does not use       */
/* MDPPAS, so no MD_PPAS libref is needed.                         */


/* ============================================================ */
/* Parameters                                                    */
/* ============================================================ */
/* The paper sample spans 2010-2018 (draft-paper.tex: "Our analysis   */
/* sample therefore spans 2010--2018"); df_logit_movers confirms       */
/* Year 2010-2018 for all three specialties.                           */
%LET enc_start  = 2010;   /* first encounter year (matches the paper sample)        */
%LET enc_end    = 2016;   /* last encounter year with a full 2-yr window in 2018    */
%LET claim_end  = 2018;   /* last claim year available for follow-up                */

/* Uniform 2-year post-encounter window for ALL outcomes         */
/* (admissions, ED visits, spending, death). Two years is the    */
/* practical ceiling given a 2018 data end; report robustness at */
/* a shorter (e.g. 1-year) window on the larger encounter sample.*/
%LET window_days = 730;

/* Specialty prefixes, as built by the referral pipeline.        */
%LET prefix_list = Ortho Cardio Derm CardioEM;


/* ============================================================ */
/* CONFIRMED claim tokens used downstream (evidence in headers)  */
/* ============================================================ */
/*  Bene / claim keys : BENE_ID, CLM_ID                          */
/*  Inpatient (12 mo)  : RIF&year..INPATIENT_CLAIMS_01-12         */
/*     admit / dschg   : CLM_ADMSN_DT, NCH_BENE_DSCHRG_DT         */
/*     claim type      : NCH_CLM_TYPE_CD ("60" short-term acute)  */
/*     admit type      : CLM_IP_ADMSN_TYPE_CD ("3" elective)      */
/*     payment / charge: CLM_PMT_AMT, CLM_TOT_CHRG_AMT            */
/*     hospital (CCN)  : PRVDR_NUM ; facility NPI: ORG_NPI_NUM    */
/*     dx array (25)   : ICD_DGNS_CD1-ICD_DGNS_CD25               */
/*  Outpatient (12 mo) : RIF&year..OUTPATIENT_CLAIMS_01-12        */
/*     payment         : CLM_PMT_AMT ; svc dates: CLM_FROM_DT/CLM_THRU_DT */
/*  Carrier line       : RIF&year..BCARRIER_LINE_01-12           */
/*     rendering NPI   : PRF_PHYSN_NPI ; procedure: HCPCS_CD      */
/*     svc date        : CLM_THRU_DT ; tax id: TAX_NUM           */
/*  MBSF (denominator) : MBSF.MBSF_ABCD_&year                     */
/*     death           : BENE_DEATH_DT                            */
/*     FFS enrollment  : BENE_SMI_CVRAGE_TOT_MONS (=12),          */
/*                       BENE_HMO_CVRAGE_TOT_MONS (=0),           */
/*                       DUAL_ELGBL_MONS, BENE_ENROLLMT_REF_YR    */
/*  Encounters (built) : PL027710.{Prefix}Encounters_&year with   */
/*                       BENE_ID, Specialist_NPI, Encounter_Date, */
/*                       Discharge_Date, Facility_ID, CLM_ID      */


/* ============================================================ */
/* TWO ITEMS TO VERIFY IN THE VRDC BEFORE RUNNING (not confirmed */
/* in any sibling build -- do NOT assume a name):                */
/* ============================================================ */
/* (A) ED visits. Identified below via HCPCS_CD in 99281-99285   */
/*     (HCPCS_CD is confirmed; physician-variation/4_carrier_...  */
/*     :148-150). The facility-side revenue-center alternative    */
/*     (REV_CNTR in 0450-0459, 0981) is NOT confirmed -- no       */
/*     sibling references REV_CNTR. Confirm REV_CNTR before using */
/*     the outpatient/revenue-code definition instead.            */
/* (B) Part B (carrier) PAYMENT -- RESOLVED, included in spend.   */
/*     1_patient_outcomes.sas sums carrier dollars from           */
/*     LINE_NCH_PMT_AMT + LINE_BENE_PRMRY_PYR_PD_AMT +            */
/*     LINE_COINSRNC_AMT + LINE_BENE_PTB_DDCTBL_AMT, so           */
/*     spend_total is IP + OP + carrier, on a total-cost basis    */
/*     (Medicare payment plus primary-payer and beneficiary       */
/*     cost-sharing). NOT institutional-only.                     */


/* ============================================================ */
/* Utility: stack the 12 monthly INPATIENT files for a year      */
/* (copied from ../../data-build/sas/0_config.sas %stack_inpatient)*/
/* ============================================================ */
%MACRO stack_inpatient_out(year, out);
    DATA &out;
        SET RIF&year..INPATIENT_CLAIMS_01 RIF&year..INPATIENT_CLAIMS_02
            RIF&year..INPATIENT_CLAIMS_03 RIF&year..INPATIENT_CLAIMS_04
            RIF&year..INPATIENT_CLAIMS_05 RIF&year..INPATIENT_CLAIMS_06
            RIF&year..INPATIENT_CLAIMS_07 RIF&year..INPATIENT_CLAIMS_08
            RIF&year..INPATIENT_CLAIMS_09 RIF&year..INPATIENT_CLAIMS_10
            RIF&year..INPATIENT_CLAIMS_11 RIF&year..INPATIENT_CLAIMS_12;
    RUN;
%MEND stack_inpatient_out;

%MACRO stack_outpatient_out(year, out);
    DATA &out;
        SET RIF&year..OUTPATIENT_CLAIMS_01 RIF&year..OUTPATIENT_CLAIMS_02
            RIF&year..OUTPATIENT_CLAIMS_03 RIF&year..OUTPATIENT_CLAIMS_04
            RIF&year..OUTPATIENT_CLAIMS_05 RIF&year..OUTPATIENT_CLAIMS_06
            RIF&year..OUTPATIENT_CLAIMS_07 RIF&year..OUTPATIENT_CLAIMS_08
            RIF&year..OUTPATIENT_CLAIMS_09 RIF&year..OUTPATIENT_CLAIMS_10
            RIF&year..OUTPATIENT_CLAIMS_11 RIF&year..OUTPATIENT_CLAIMS_12;
    RUN;
%MEND stack_outpatient_out;
