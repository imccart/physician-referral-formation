/* ------------------------------------------------------------ */
/* TITLE:   Risk-adjust the patient outcomes.                    */
/* RUNS IN THE VRDC. Run 0_config_outcomes.sas and              */
/*          1_patient_outcomes.sas first.                        */
/*                                                               */
/* A) Charlson from ALL of a patient's claims dated BEFORE their */
/*    first encounter (inpatient + outpatient + carrier). Charlson*/
/*    is a patient characteristic, so any pre-encounter claim     */
/*    that carries the code counts. Pre-encounter only, so we do  */
/*    not pick up conditions caused by the outcome.               */
/* B) MBSF demographics + Part B FFS filter.                     */
/* C) Per-outcome OLS residual (the risk-adjusted value).        */
/*                                                               */
/* Confirmed diagnosis-array lengths (ResDAC, Version J, 2011+):  */
/*   inpatient  ICD_DGNS_CD1-25   outpatient ICD_DGNS_CD1-25       */
/*   carrier header (BCARRIER_CLAIMS) ICD_DGNS_CD1-12              */
/* Scoring macros below are verbatim from referrals-and-learning  */
/* P1-patient-cci.sas (Quan 2005). Demographics follow physician- */
/* variation 3_beneficiary.sas.                                   */
/* ------------------------------------------------------------ */


/* ===== Charlson condition macros (verbatim, RL P1-patient-cci.sas:27-85) ===== */
%macro score_icd9;
    if dx{i} in: ('410','412') then cci_mi=1;
    if dx{i} in: ('39891','40201','40211','40291','40401','40403','40411','40413','40491','40493',
        '4254','4255','4256','4257','4258','4259','428') then cci_chf=1;
    if dx{i} in: ('0930','4373','440','441','4431','4432','4433','4434','4435','4436','4437','4438','4439',
        '4471','5571','5579','V434') then cci_pvd=1;
    if dx{i} in: ('36234','430','431','432','433','434','435','436','437','438') then cci_cvd=1;
    if dx{i} in: ('290','2941','3312') then cci_dem=1;
    if dx{i} in: ('4168','4169','490','491','492','493','494','495','496','497','498','499','500','501',
        '502','503','504','505','5064','5081','5088') then cci_cpd=1;
    if dx{i} in: ('4465','7100','7101','7102','7103','7104','7140','7141','7142','7148','725') then cci_rheum=1;
    if dx{i} in: ('531','532','533','534') then cci_ulcer=1;
    if dx{i} in: ('07022','07023','07032','07033','07044','07054','0706','0709','570','571','5733','5734',
        '5738','5739','V427') then cci_mldld=1;
    if dx{i} in: ('2500','2501','2502','2503','2508','2509') then cci_dm=1;
    if dx{i} in: ('2504','2505','2506','2507') then cci_dmcc=1;
    if dx{i} in: ('3341','342','343','3440','3441','3442','3443','3444','3445','3446','3449') then cci_hemi=1;
    if dx{i} in: ('40301','40311','40391','40402','40403','40412','40413','40492','40493','582','5830',
        '5831','5832','5833','5834','5835','5836','5837','585','586','588','V420','V451','V56') then cci_renal=1;
    if dx{i} in: ('14','15','16','170','171','172','174','175','176','177','178','179','18','190','191',
        '192','193','194','195','200','201','202','203','204','205','206','207','208','2386') then cci_malig=1;
    if dx{i} in: ('4560','4561','4562','5722','5723','5724','5725','5726','5727','5728') then cci_sevld=1;
    if dx{i} in: ('196','197','198','199') then cci_meta=1;
    if dx{i} in: ('042','043','044') then cci_hiv=1;
%mend score_icd9;

%macro score_icd10;
    if dx{i} in: ('I21','I22','I252') then cci_mi=1;
    if dx{i} in: ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429',
        'I43','I50','P290') then cci_chf=1;
    if dx{i} in: ('I70','I71','I731','I738','I739','I771','I790','I792','K551','K558','K559',
        'Z958','Z959') then cci_pvd=1;
    if dx{i} in: ('G45','G46','H340','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') then cci_cvd=1;
    if dx{i} in: ('F00','F01','F02','F03','F051','G30','G311') then cci_dem=1;
    if dx{i} in: ('I278','I279','J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62',
        'J63','J64','J65','J66','J67','J684','J701','J703') then cci_cpd=1;
    if dx{i} in: ('M05','M06','M315','M32','M33','M34','M351','M353','M360') then cci_rheum=1;
    if dx{i} in: ('K25','K26','K27','K28') then cci_ulcer=1;
    if dx{i} in: ('B18','K700','K701','K702','K703','K709','K713','K714','K715','K717','K73','K74',
        'K760','K762','K763','K764','K768','K769','Z944') then cci_mldld=1;
    if dx{i} in: ('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119',
        'E120','E121','E126','E128','E129','E130','E131','E136','E138','E139',
        'E140','E141','E146','E148','E149') then cci_dm=1;
    if dx{i} in: ('E102','E103','E104','E105','E107','E112','E113','E114','E115','E117',
        'E122','E123','E124','E125','E127','E132','E133','E134','E135','E137',
        'E142','E143','E144','E145','E147') then cci_dmcc=1;
    if dx{i} in: ('G041','G114','G801','G802','G81','G82','G830','G831','G832','G833','G834','G839') then cci_hemi=1;
    if dx{i} in: ('I120','I131','N032','N033','N034','N035','N036','N037','N052','N053','N054',
        'N055','N056','N057','N18','N19','N250','Z490','Z491','Z492','Z940','Z992') then cci_renal=1;
    if dx{i} in: ('C0','C1','C2','C30','C31','C32','C33','C34','C37','C38','C39','C40','C41',
        'C43','C45','C46','C47','C48','C49','C50','C51','C52','C53','C54','C55',
        'C56','C57','C58','C6','C70','C71','C72','C73','C74','C75','C76','C81',
        'C82','C83','C84','C85','C88','C90','C91','C92','C93','C94','C95','C96','C97') then cci_malig=1;
    if dx{i} in: ('I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767') then cci_sevld=1;
    if dx{i} in: ('C77','C78','C79','C80') then cci_meta=1;
    if dx{i} in: ('B20','B21','B22','B24') then cci_hiv=1;
%mend score_icd10;


/* ===== Step A. Charlson from pre-encounter claims (once, all benes) ===== */

/* Earliest encounter per patient across all specialties = the cutoff. */
PROC SQL;
    CREATE TABLE WORK.enc AS
    SELECT BENE_ID, MIN(Encounter_Date) AS first_enc FORMAT=DATE9.
    FROM (
        SELECT BENE_ID, Encounter_Date FROM PL027710.OrthoPatientOutcomes    UNION ALL
        SELECT BENE_ID, Encounter_Date FROM PL027710.CardioPatientOutcomes   UNION ALL
        SELECT BENE_ID, Encounter_Date FROM PL027710.DermPatientOutcomes     UNION ALL
        SELECT BENE_ID, Encounter_Date FROM PL027710.CardioEMPatientOutcomes )
    GROUP BY BENE_ID;
QUIT;

/* Score one claim file-type/year: keep sample benes, pre-encounter claims,
   flag Charlson conditions on the wide claim (mirrors RL:121-160). */
%MACRO score_file(filestub, ndx, tag);
  %DO yr = 2008 %TO &enc_end;
    DATA WORK.cl;
        SET RIF&yr..&filestub._01 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._02 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._03 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._04 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._05 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._06 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._07 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._08 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._09 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._10 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._11 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx)
            RIF&yr..&filestub._12 (KEEP=BENE_ID CLM_FROM_DT ICD_DGNS_CD1-ICD_DGNS_CD&ndx);
    RUN;
    PROC SQL;
        CREATE TABLE WORK.clr AS
        SELECT c.* FROM WORK.cl AS c INNER JOIN WORK.enc AS e ON c.BENE_ID = e.BENE_ID
        WHERE c.CLM_FROM_DT < e.first_enc;
    QUIT;
    DATA WORK.sc_&tag._&yr;
        SET WORK.clr;
        LENGTH cci_mi cci_chf cci_pvd cci_cvd cci_dem cci_cpd cci_rheum cci_ulcer
               cci_mldld cci_dm cci_dmcc cci_hemi cci_renal cci_malig cci_sevld
               cci_meta cci_hiv 3;
        ARRAY dx{&ndx} ICD_DGNS_CD1-ICD_DGNS_CD&ndx;
        cci_mi=0; cci_chf=0; cci_pvd=0; cci_cvd=0; cci_dem=0; cci_cpd=0; cci_rheum=0;
        cci_ulcer=0; cci_mldld=0; cci_dm=0; cci_dmcc=0; cci_hemi=0; cci_renal=0;
        cci_malig=0; cci_sevld=0; cci_meta=0; cci_hiv=0;
        do i = 1 to &ndx;
            if missing(dx{i}) then continue;
            if CLM_FROM_DT < '01OCT2015'd then do; %score_icd9;  end;
            else                               do; %score_icd10; end;
        end;
        keep BENE_ID cci_mi cci_chf cci_pvd cci_cvd cci_dem cci_cpd cci_rheum
             cci_ulcer cci_mldld cci_dm cci_dmcc cci_hemi cci_renal cci_malig
             cci_sevld cci_meta cci_hiv;
    RUN;
    PROC DELETE DATA=WORK.cl WORK.clr; RUN;
  %END;
%MEND score_file;

%score_file(INPATIENT_CLAIMS, 25, ip);
%score_file(OUTPATIENT_CLAIMS, 25, op);
%score_file(BCARRIER_CLAIMS,   12, ca);

/* Stack every scored claim-level table (SET idiom, cf. 5_outcomes.sas).  */
/* %DO is not valid in open code, so wrap the stack in a macro.           */
%MACRO stack_scored;
    DATA WORK.sc_all;
        SET %DO yr = 2008 %TO &enc_end;
              WORK.sc_ip_&yr WORK.sc_op_&yr WORK.sc_ca_&yr
            %END; ;
    RUN;
%MEND stack_scored;
%stack_scored;

/* Patient-level "ever" flags, then hierarchies + weighted score */
PROC SQL;
    CREATE TABLE PL027710.Charlson_All AS
    SELECT BENE_ID,
           MAX(cci_mi) AS cci_mi, MAX(cci_chf) AS cci_chf, MAX(cci_pvd) AS cci_pvd,
           MAX(cci_cvd) AS cci_cvd, MAX(cci_dem) AS cci_dem, MAX(cci_cpd) AS cci_cpd,
           MAX(cci_rheum) AS cci_rheum, MAX(cci_ulcer) AS cci_ulcer, MAX(cci_mldld) AS cci_mldld,
           MAX(cci_dm) AS cci_dm, MAX(cci_dmcc) AS cci_dmcc, MAX(cci_hemi) AS cci_hemi,
           MAX(cci_renal) AS cci_renal, MAX(cci_malig) AS cci_malig, MAX(cci_sevld) AS cci_sevld,
           MAX(cci_meta) AS cci_meta, MAX(cci_hiv) AS cci_hiv
    FROM WORK.sc_all
    GROUP BY BENE_ID;
QUIT;

DATA PL027710.Charlson_All;
    SET PL027710.Charlson_All;
    if cci_dmcc=1  then cci_dm=0;            /* hierarchies, RL:146-148 */
    if cci_sevld=1 then cci_mldld=0;
    if cci_meta=1  then cci_malig=0;
    cci_score = cci_mi + cci_chf + cci_pvd + cci_cvd + cci_dem + cci_cpd
              + cci_rheum + cci_ulcer + cci_mldld + cci_dm
              + 2*(cci_dmcc + cci_hemi + cci_renal + cci_malig)
              + 3*cci_sevld + 6*(cci_meta + cci_hiv);
RUN;


/* ===== Step B. MBSF demographics + Part B FFS filter (physician-variation 3_beneficiary.sas) ===== */
%MACRO read_bene(year);
    PROC SQL;
        CREATE TABLE WORK.b_&year AS
        SELECT BENE_ID, BENE_ENROLLMT_REF_YR, AGE_AT_END_REF_YR, BENE_RACE_CD,
               SEX_IDENT_CD, DUAL_ELGBL_MONS
        FROM MBSF.MBSF_ABCD_&year
        WHERE BENE_SMI_CVRAGE_TOT_MONS = 12    /* full-year Part B */
          AND BENE_HMO_CVRAGE_TOT_MONS = 0;    /* FFS only         */
    QUIT;
%MEND read_bene;
/* Build every per-year bene table first, then stack. A PROC SQL step     */
/* cannot sit inside a DATA-step SET, and %DO is not valid in open code,   */
/* so both the read loop and the stack live in one macro.                  */
%MACRO build_bene;
    %DO yr = 2010 %TO &claim_end;
        %read_bene(&yr);
    %END;
    DATA WORK.bene_all;
        SET %DO yr = 2010 %TO &claim_end; WORK.b_&yr %END; ;
        Age    = AGE_AT_END_REF_YR;
        Age_Sq = Age*Age;
        D_Black        = (BENE_RACE_CD = "2");
        D_Hisp         = (BENE_RACE_CD = "5");
        D_Asian        = (BENE_RACE_CD = "4");
        D_Race_Missing = (BENE_RACE_CD = "0");
        D_Female       = (SEX_IDENT_CD = "2");
        D_Dual         = (DUAL_ELGBL_MONS > 0);
    RUN;
%MEND build_bene;
%build_bene;


/* ===== Step C. Assemble regression frame per specialty and residualize ===== */
%MACRO residualize_one(yvar, out_resid);
    PROC GLM DATA=WORK.reg NOPRINT;
        CLASS enc_year;
        MODEL &yvar = Age Age_Sq D_Female D_Black D_Hisp D_Asian D_Race_Missing
                      D_Dual DUAL_ELGBL_MONS cci_score enc_year / SOLUTION;
        OUTPUT OUT=WORK.ro PREDICTED=p_ RESIDUAL=&out_resid;
    RUN; QUIT;
    PROC SORT DATA=WORK.ro; BY BENE_ID Specialist_NPI Encounter_Date; RUN;
    DATA WORK.reg; MERGE WORK.reg WORK.ro(KEEP=BENE_ID Specialist_NPI Encounter_Date &out_resid);
        BY BENE_ID Specialist_NPI Encounter_Date; RUN;
%MEND residualize_one;

%MACRO risk_adjust(prefix);
    PROC SQL;
        CREATE TABLE WORK.reg AS
        SELECT o.BENE_ID, o.Specialist_NPI, o.Encounter_Date,
               YEAR(o.Encounter_Date) AS enc_year,
               o.n_admit, o.n_admit_ne, o.death_2yr, o.spend_total,
               b.Age, b.Age_Sq, b.D_Female, b.D_Black, b.D_Hisp, b.D_Asian,
               b.D_Race_Missing, b.D_Dual, b.DUAL_ELGBL_MONS,
               COALESCE(c.cci_score, 0) AS cci_score
        FROM PL027710.&prefix.PatientOutcomes AS o
        INNER JOIN WORK.bene_all AS b
           ON o.BENE_ID = b.BENE_ID AND YEAR(o.Encounter_Date) = b.BENE_ENROLLMT_REF_YR
        LEFT JOIN PL027710.Charlson_All AS c ON o.BENE_ID = c.BENE_ID;
    QUIT;
    PROC SORT DATA=WORK.reg; BY BENE_ID Specialist_NPI Encounter_Date; RUN;

    %residualize_one(n_admit_ne,  r_admit_ne);
    %residualize_one(n_admit,     r_admit);
    %residualize_one(death_2yr,   r_death);
    %residualize_one(spend_total, r_spend);

    DATA PL027710.&prefix.Residualized;
        SET WORK.reg (KEEP=BENE_ID Specialist_NPI Encounter_Date
                           r_admit_ne r_admit r_death r_spend);
    RUN;
%MEND risk_adjust;

%risk_adjust(Ortho);
%risk_adjust(Cardio);
%risk_adjust(Derm);
%risk_adjust(CardioEM);
