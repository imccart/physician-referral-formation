/* ------------------------------------------------------------ */
/* TITLE:         Extract carrier E&M claims for anchor patients */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  2/19/2026                                      */
/* CODE FILE ORDER: 2 of 6                                       */
/* INPUT:         Carrier RIF (BCARRIER_LINE),                   */
/*                {Prefix}Patients_Unique from Step 1             */
/* OUTPUT:        {Prefix}Carrier_{year} for carrier_start to    */
/*                year_end (2008-2018)                            */
/* ------------------------------------------------------------ */



/* ============================================================ */
/* Carrier E&M Extraction                                        */
/* ============================================================ */

/* For each specialty's patient pool, extract all medical E&M    */
/* visits from carrier claims. These are the candidate PCP       */
/* visits used in the 365-day lookback (Step 3).                 */
/*                                                               */
/* Runs from carrier_start (2008) through year_end (2018) to     */
/* ensure lookback coverage for the first analysis year (2009).  */

%MACRO extract_carrier_em(prefix);

    %DO year_data = &carrier_start %TO &year_end;

        /* Extract E&M visits month by month, inner join to patients */
        %DO m = 1 %TO 12;
            %carrier_month_extract(
                &year_data,
                &m,
                PL027710.&prefix.Patients_Unique,
                WORK.&prefix.Carrier_m&m._&year_data
            );
        %END;

        /* Stack monthly tables into annual */
        %stack_carrier_months(&prefix, &year_data);

        /* Clean up monthly WORK tables */
        %DO m = 1 %TO 12;
            PROC DELETE DATA=WORK.&prefix.Carrier_m&m._&year_data; RUN;
        %END;

        %PUT NOTE: &prefix carrier extraction complete for &year_data;

    %END;

%MEND extract_carrier_em;


/* ============================================================ */
/* Execute: Extract carrier E&M for all specialties              */
/* ============================================================ */

%extract_carrier_em(&ortho_prefix);
%extract_carrier_em(&cardio_prefix);
%extract_carrier_em(&derm_prefix);
%extract_carrier_em(&cardioem_prefix);
