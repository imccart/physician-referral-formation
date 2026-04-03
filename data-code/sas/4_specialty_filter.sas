/* ------------------------------------------------------------ */
/* TITLE:         MDPPAS specialty validation                    */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 4 of 6                                       */
/* INPUT:         {Prefix}PCP_{year} from Step 3,                */
/*                MD_PPAS.MDPPAS_V24_{year}                      */
/* OUTPUT:        {Prefix}ValidPairs_{year}                      */
/* ------------------------------------------------------------ */

/* ============================================================ */
/* Specialty Filter                                              */
/* ============================================================ */

/* Validates that: (1) assigned PCP is primary care              */
/* (SPEC_BROAD = 1), and (2) specialist matches the target       */
/* specialty in MDPPAS (SPEC_PRIM_1_NAME).                       */
/*                                                               */
/* This replaces the old Stata-based specialty filter that       */
/* used a separate step with MDPPAS merges.                      */

%MACRO filter_specialties(prefix, spec_names_var);

    %DO year_data = &year_start %TO &year_end;

        /* Join PCP assignments to MDPPAS: validate PCP and specialist */
        PROC SQL;
            DROP TABLE PL027710.&prefix.ValidPairs_&year_data;
            CREATE TABLE PL027710.&prefix.ValidPairs_&year_data AS
            SELECT
                a.PCP_NPI AS Practice_ID,
                a.Specialist_NPI AS Specialist_ID,
                &year_data AS Year,
                a.BENE_ID,
                a.Encounter_Date,
                a.Facility_ID
            FROM PL027710.&prefix.PCP_&year_data AS a

            /* PCP must be primary care */
            INNER JOIN MD_PPAS.MDPPAS_V24_&&mdppas_&year_data AS pcp
                ON a.PCP_NPI = pcp.NPI
                AND pcp.SPEC_BROAD = 1

            /* Specialist must match target specialty */
            INNER JOIN MD_PPAS.MDPPAS_V24_&&mdppas_&year_data AS spec
                ON a.Specialist_NPI = spec.NPI
                AND spec.SPEC_PRIM_1_NAME IN (&&&spec_names_var);
        QUIT;

        %PUT NOTE: &prefix specialty filter complete for &year_data;

    %END;

%MEND filter_specialties;


/* ============================================================ */
/* Execute: Filter for all specialties                           */
/* ============================================================ */

%filter_specialties(&ortho_prefix, ortho_spec_names);
%filter_specialties(&cardio_prefix, cardio_spec_names);
%filter_specialties(&derm_prefix, derm_spec_names);
%filter_specialties(&cardioem_prefix, cardioem_spec_names);
