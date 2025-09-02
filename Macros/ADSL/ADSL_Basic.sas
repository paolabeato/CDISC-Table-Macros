/*******************************************************************************
* MACRO: ADSL_Basic.sas
* PURPOSE: Generate basic CDISC-compliant demographic baseline characteristics
*          tables (Tables 14.1.1, 14.1.2, 14.1.3) following SDTM/ADaM standards
* 
* PARAMETERS:
*   pop           = Population flag variable (e.g., ITTFL, SAFFL, FASFL)
*   popdesc       = Population description for titles (e.g., Intent-to-Treat)
*   popval        = Value indicating inclusion in population (typically 'Y')
*   trtvar        = Treatment variable for grouping (e.g., ARM, TRT01P)
*   outrtf        = Output RTF file path (required)
*   tablenum      = Table number for title (e.g., 14.1.1)
*   denom_mode    = Denominator calculation method: FULL | NONMISS
*   show_missing  = Display missing categories: Y | N
*   dostat        = Calculate p-values: Y | N
*   test_type     = Statistical test for continuous vars: ANOVA | KW (Kruskal-Wallis)
*   dooverall     = Include Overall column: Y | N
*   trt_order     = Explicit treatment order (e.g., Placebo DrugA)
*   tab_title     = Main table title
*   tab_subtitle  = Optional subtitle
*   src           = Source footnote text
*   output_type   = Output type (RTF | PDF)
* 
* DEFAULT VARIABLES PROCESSED:
*   Categorical: SEX, RACE, AGEGR1_, BMIGRP, ETHNIC, REGION, SMOKER
*   Continuous: AGE, WEIGHT, HEIGHT, BMI
* 
* STATISTICAL METHODS:
*   - Categorical: Chi-square or Fisher's exact test
*   - Continuous: ANOVA or Kruskal-Wallis test
*   - P-values formatted as <0.001 when appropriate
* 
* DEPENDENCIES:
*   - Requires ADSL dataset with standard CDISC variables
*   - Requires pre-defined formats for SEX, RACE, AGEGR1, BMIGRP
*   - Requires derived ADSL_ dataset with AGEGR1_ and BMIGRP variables
* 
* OUTPUT: CDISC-compliant RTF table with:
*   - n(%) for categorical variables
*   - Mean (SD) for continuous variables
*   - Column headers with treatment group N values
*   - Optional p-values column
*   - Proper title and footnote structure
* 
* CDISC COMPLIANCE: 
*   - Follows ADaM implementation guide v1.3
*   - Uses standard population flags (ITTFL, SAFFL, FASFL)
*   - Implements proper variable naming and formatting conventions
*   - Generates tables matching FDA submission requirements
* 
* EXAMPLE USAGE:
*   %adsl_basic(pop=ITTFL, popdesc=Intent-to-Treat, popval=Y, trtvar=ARM, 
*           outrtf=/output/table_14_1_1.rtf, tablenum=14.1.1,
*           denom_mode=NONMISS, show_missing=Y, dostat=N, 
*           test_type=ANOVA, dooverall=N, trt_order=Placebo DrugA,
*           tab_title=Summary of Demographic and Baseline Characteristics,
*           src=ADSL);
*
* AUTHOR: Paola Beato
* DATE: 09/02/2025
* VERSION: 1.0
*******************************************************************************/
/* =========================
   0) Output wrappers & formats
   ========================= */
%macro open_output(outfile=, style=journal, output_type=RTF);
  ods _all_ close;
  ods escapechar='^';
  options orientation=portrait;
  
  %if %upcase(&output_type)=RTF %then %do;
    ods rtf file="&outfile" style=&style startpage=no bodytitle;
  %end;
  %else %if %upcase(&output_type)=PDF %then %do;
    ods pdf file="&outfile" style=&style startpage=no;
  %end;
%mend;

%macro close_output(output_type=RTF);
  %if %upcase(&output_type) = PDF %then %do;
    ods pdf close;
  %end;
  %else %if %upcase(&output_type) = RTF %then %do;
    ods rtf close;
  %end;
  title; footnote;
%mend;

/* Display formats */
proc format;
  value $sex   'F'='Female' 'M'='Male' other='Missing';
  value $race  'White'='White' 'Black'='Black' 'Asian'='Asian' 'Other'='Other' other='Missing';
  value agegrp  low-<65 = '<65'  
                65-high = '>=65';
  value bmigrp  low-<18.5='Underweight'
                18.5-<25 ='Normal'
                25-<30   ='Overweight'
                30-high  ='Obese';
  value $paramdesc
    'SEX'        = 'Sex - n (%)'
    'RACE_STD'   = 'Race - n (%)'
    'AGEGR1_'    = 'Age Group - n (%)'
    'BMIGRP'     = 'BMI Category - n (%)'
    'ETHNIC_STD' = 'Ethnicity - n (%)'
    'REGION'     = 'Region - n (%)'
    'SMOKER'     = 'Smoker - n (%)'
    'AGE'        = 'Age (years)'
    'WEIGHT'     = 'Weight (kg)'
    'HEIGHT'     = 'Height (cm)'
    'BMI'        = 'BMI (kg/m²)';
run;

/* =========================
   1) Derivations in ADSL
   ========================= */
proc datasets library=work nolist; 
  delete adsl_; 
quit;

data adsl_;
  set adsl;
  length Overall $7;
  Overall='Overall';

  if not missing(AGEGR1) then do;
    AGEGR1_ = strip(AGEGR1);
  end;
  else if not missing(AGE) then do;
    if AGE < 65 then AGEGR1_ = '<65';
    else AGEGR1_ = '^{unicode 2265}65';
  end;
  
  if missing(BMIGRP) then BMIGRP = put(BMI, bmigrp.);

  length ARMN 8;
  if upcase(strip(ARM))='PLACEBO' then ARMN=1;
  else if upcase(strip(ARM))='DRUGA' then ARMN=2;
  else ARMN=.;

  /* Normalize RACE to standard CDISC categories */
  length RACE_STD $200;
  select (upcase(strip(RACE)));
    when ('AMERICAN INDIAN OR ALASKA NATIVE') RACE_STD = 'American Indian or Alaska Native';
    when ('ASIAN') RACE_STD = 'Asian';
    when ('BLACK','BLACK OR AFRICAN AMERICAN') RACE_STD = 'Black';
    when ('NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER') RACE_STD = 'Native Hawaiian or Other Pacific Islander';
    when ('WHITE') RACE_STD = 'White';
    when ('NOT REPORTED') RACE_STD = 'Not Reported';
    when ('UNKNOWN') RACE_STD = 'Unknown';
    when ('OTHER') RACE_STD = 'Other';
    otherwise do;
      if missing(RACE) then RACE_STD = 'Not Reported';
      else RACE_STD = 'Other';
    end;
  end;

  /* Normalize ETHNICITY to standard CDISC categories */
  length ETHNIC_STD $200;
  select (upcase(strip(ETHNIC)));
    when ('HISPANIC OR LATINO','HISPANIC/LATINO') ETHNIC_STD = 'Hispanic or Latino';
    when ('NOT HISPANIC OR LATINO','NON-HISPANIC') ETHNIC_STD = 'Not Hispanic or Latino';
    when ('NOT REPORTED') ETHNIC_STD = 'Not Reported';
    when ('UNKNOWN') ETHNIC_STD = 'Unknown';
    otherwise do;
      if missing(ETHNIC) then ETHNIC_STD = 'Not Reported';
      else ETHNIC_STD = 'Unknown';
    end;
  end;

  label AGEGR1_='Age group' BMIGRP='BMI category' 
        RACE_STD='Race' ETHNIC_STD='Ethnicity';
run;

/* =========================
   2) Main Table Macro
   ========================= */
%macro adsl_basic(
  pop=,                /* population flag variable */
  popdesc=,            /* e.g., Intent-to-Treat */
  popval=,             /* flag value (Y) */
  trtvar=,             /* treatment grouping variable */
  outrtf=,             /* RTF output file (required) */
  tablenum=,           /* table number tag in title */
  denom_mode=,         /* FULL | NONMISS  */
  show_missing=,       /* Y | N           */
  dostat=,             /* compute p-values (Y/N) */
  test_type=,          /* ANOVA | KW      */
  dooverall=,          /* include Overall column (Y/N) */
  trt_order=,          /* explicit order: e.g., Placebo DrugA */
  tab_title=,		   /* table title */
  tab_subtitle=,       /* optional second line beneath title */
  src=,                /* source footnote text */
  output_type=         /* output type (RTF | PDF) */
);

  /* ===== Validations ===== */
  %if %length(&outrtf)=0 %then %do; %put ERROR: outrtf parameter is required.; %return; %end;
  %if %length(&popdesc)=0 %then %do; %put ERROR: popdesc parameter is required.; %return; %end;
  %if %length(&popval)=0 %then %do; %put ERROR: popval parameter is required.; %return; %end;
  %if %length(&trtvar)=0 %then %do; %put ERROR: trtvar parameter is required.; %return; %end;

  %if %upcase(&dooverall) ne Y and %upcase(&dooverall) ne N %then %do;
    %put ERROR: dooverall must be Y or N (got &dooverall).; %return;
  %end;

  %if %upcase(&dostat) ne Y and %upcase(&dostat) ne N %then %do;
    %put ERROR: dostat must be Y or N (got &dostat).; %return;
  %end;

  %if %upcase(&test_type) ne ANOVA and %upcase(&test_type) ne KW %then %do;
    %put ERROR: test_type must be ANOVA or KW (got &test_type).; %return;
  %end;

  %if %upcase(&denom_mode) ne FULL and %upcase(&denom_mode) ne NONMISS %then %do;
    %put ERROR: denom_mode must be FULL or NONMISS (got &denom_mode).; %return;
  %end;

  %if %upcase(&show_missing) ne Y and %upcase(&show_missing) ne N %then %do;
    %put ERROR: show_missing must be Y or N (got &show_missing).; %return;
  %end;
  
  %if %upcase(&output_type) ne RTF and %upcase(&output_type) ne PDF %then %do;
    %put ERROR: output_type must be RTF or PDF (got &output_type).; %return;
  %end;

  %if %sysfunc(exist(adsl_))=0 %then %do; %put ERROR: adsl_ dataset not found.; %return; %end;

  /* Get maximum length of treatment variable */
  proc sql noprint;
    select max(length(&trtvar)) into :_maxlen from adsl_;
  quit;
  %let _maxlen=%sysfunc(max(&&_maxlen,7));

  /* ===== Population subset ===== */
  data work_population;
    set adsl_;
    where upcase(&pop)=upcase("&popval");
  run;

  proc sql noprint;
    select count(*) into :pop_count from work_population;
    select count(distinct &trtvar) into :trt_groups from work_population where not missing(&trtvar);
  quit;

  %if &pop_count=0 %then %do; %put ERROR: No subjects in &pop=&popval.; %return; %end;

  /* ===== Denominators (column N) ===== */
  proc sql;
    create table denom_full as
    select &trtvar as trt length=&_maxlen, count(*) as N_full
    from work_population
    group by &trtvar
    ;
  quit;

  /* Add Overall column N */
  %if %upcase(&dooverall)=Y %then %do;
    data denom_full;
      set denom_full end=last;
      output;
      if last then do; trt='Overall'; N_full=&pop_count; output; end;
    run;
  %end;

  /* ===== Treatment order mapping ===== */
  data trt_order_map;
    length trt $&_maxlen;
    stop;
  run;

  %if %length(&trt_order) %then %do;
    data trt_order_map;
      length trt $&_maxlen order 8;
      %local i tok;
      %let i=1;
      %do %while(%length(%scan(&trt_order,&i)));
        %let tok=%scan(&trt_order,&i);
        trt="&tok"; order=&i; output;
        %let i=%eval(&i+1);
      %end;
    run;
  %end;
  %else %do;
    /* Try to use ARMN if trtvar=ARM; else alphabetical */
    %if %upcase(&trtvar)=ARM %then %do;
      proc sql;
        create table trt_order_map as
        select distinct &trtvar as trt length=&_maxlen,
               min(ARMN) as order
        from work_population
        group by &trtvar
        order by calculated order, trt;
      quit;
    %end;
    %else %do;
      /* Create a list of distinct treatments and assign an order based on appearance */
      proc sql noprint;
          create table _distinct_trts as
          select distinct &trtvar as trt
          from work_population
          where not missing(&trtvar)
          order by trt; /* Alphabetical order */
      quit;

      data trt_order_map;
          set _distinct_trts;
          order = _N_; /* Sequential numbering */
      run;

      proc datasets lib=work nolist;
          delete _distinct_trts;
      quit;
    %end;
  %end;

  %if %upcase(&dooverall)=Y %then %do;
    data trt_order_map; 
      set trt_order_map end=last; 
      output;
      if last then do; trt='Overall'; order=999; output; end;
    run;
  %end;

  /* ===== P-values ===== */
  data pvalues;
      length param $50 pvalue_display $15 pvalue_num 8;
      stop;
  run;

  /* ===== Initialize cat_long with proper structure ===== */
  data cat_long; 
    length param $50 level $200 trt $&_maxlen value $25 n 8 denom 8 N_full 8 N_nonmiss 8; 
    stop; 
  run;

  /* ===== Categorical variables processing ===== */
  %let catvars=SEX RACE_STD AGEGR1_ BMIGRP ETHNIC_STD REGION SMOKER;

  %do _i=1 %to %sysfunc(countw(&catvars));
    %let var=%scan(&catvars,&_i);

    /* Check if variable exists and has non-missing values */
    proc sql noprint;
      select count(*) into :var_exists from work_population
      where not missing(&var);
    quit;

    %if &var_exists > 0 %then %do;

        /* Create complete category template for Race and Ethnicity */
        %if &var = RACE_STD %then %do;
          data _template_&var;
            length &var $200;
            &var = 'American Indian or Alaska Native'; output;
            &var = 'Asian'; output;
            &var = 'Black'; output;
            &var = 'Native Hawaiian or Other Pacific Islander'; output;
            &var = 'White'; output;
            &var = 'Not Reported'; output;
            &var = 'Unknown'; output;
            &var = 'Other'; output;
          run;
        %end;
        %else %if &var = ETHNIC_STD %then %do;
          data _template_&var;
            length &var $200;
            &var = 'Hispanic or Latino'; output;
            &var = 'Not Hispanic or Latino'; output;
            &var = 'Not Reported'; output;
            &var = 'Unknown'; output;
          run;
        %end;

        /* Variable-specific denominators if NONMISS */
        proc sql;
          create table _denom_nm_&var as
          select "&var" as var length=32, &trtvar as trt length=&_maxlen,
                 count(&var) as N_nonmiss
          from work_population
          where not missing(&var)  /* Only count non-missing for denominators */
          group by &trtvar;
        quit;

        /* Counts */
        data _cat_&var;
          length param $50 level $200 trt $&_maxlen n 8;
          set work_population;
          %if %upcase(&show_missing)=N %then %do;
            where not missing(&var);
          %end;
          
          param = put("&var", $paramdesc.);
          
          /* Handle different variable types appropriately */
          %if &var = SEX %then %do;
            level = put(&var, $sex.);
          %end;
          %else %if &var = RACE_STD %then %do;
            level = strip(&var);
          %end;
          %else %if &var = AGEGR1_ %then %do;
            level = strip(&var);
          %end;
          %else %if &var = BMIGRP %then %do;
            level = strip(&var);
          %end;
          %else %if &var = ETHNIC_STD %then %do;
            level = strip(&var);
          %end;
          %else %do;
            level = ifc(missing(&var), 'Missing', strip(&var));
          %end;
          
          trt = &trtvar;
          n = 1;
          
          keep param level trt n;
        run;

        /* Summarize counts */
        proc sql;
          create table _cat_sum_&var as
          select param, level, trt, count(*) as n
          from _cat_&var
          group by param, level, trt;
        quit;

        /* For Race and Ethnicity, ensure all categories are represented */
        %if &var = RACE_STD or &var = ETHNIC_STD %then %do;
          proc sql;
            create table _cat_complete_&var as
            select distinct 
                   put("&var", $paramdesc.) as param length=50,
                   t.&var as level length=200,
                   d.trt length=&_maxlen,
                   coalesce(s.n, 0) as n
            from _template_&var t
            cross join (select distinct trt from denom_full where trt ne 'Overall') d
            left join _cat_sum_&var s 
              on t.&var = s.level and d.trt = s.trt;
          quit;
          
          data _cat_sum_&var; 
            set _cat_complete_&var; 
          run;
        %end;

        /* Attach denominator per chosen mode */
        proc sql;
          create table _cat_disp_&var as
          select c.param, c.level, c.trt, c.n,
                 d.N_full,
                 nm.N_nonmiss,
                 /* denominator chosen */
                 %if %upcase(&denom_mode)=FULL %then %do; d.N_full %end;
                 %else %do; coalesce(nm.N_nonmiss, d.N_full) %end;
                 as denom
          from _cat_sum_&var c
          left join denom_full d on c.trt=d.trt
          left join _denom_nm_&var nm on c.trt=nm.trt and nm.var="&var";
        quit;

        data _cat_disp_&var;
          set _cat_disp_&var;
          length value $25;
          if n=0 or denom=0 then value = '-';
          else value = catx(' ', put(n, best.), cats('(', put(100*n/max(1,denom), 5.1), '%)'));
        run;

        proc append base=cat_long data=_cat_disp_&var force; run;

        /* --- P-values for categorical variable --- */
        %if %upcase(&dostat)=Y and &trt_groups>1 %then %do;
          /* Check if enough data for chi-square test */
          proc sql noprint;
            select count(distinct &trtvar) into :trt_levels from work_population 
            where not missing(&var) and not missing(&trtvar);
            select count(distinct &var) into :var_levels from work_population 
            where not missing(&var) and not missing(&trtvar);
          quit;

          %if &trt_levels > 1 and &var_levels > 1 %then %do;
            proc freq data=work_population;
              where not missing(&var) and not missing(&trtvar);
              tables &trtvar*&var / chisq fisher expected norow nocol nopercent;
              ods output ChiSq=_chi_&var(where=(Statistic='Chi-Square'))
                         FishersExact=_fis_&var(where=(Name1='XP2_FISH'));
            run;

            /* Check if output datasets were created */
            %if %sysfunc(exist(_chi_&var)) or %sysfunc(exist(_fis_&var)) %then %do;
              data _catp_&var;
                length param $50 pvalue_display $15 pvalue_num 8;
                /* Initialize variables */
                p_chi = .; p_fish = .;
                
                %if %sysfunc(exist(_chi_&var)) %then %do;
                  if _n_ = 1 then set _chi_&var(keep=Prob rename=(Prob=p_chi));
                %end;
                %if %sysfunc(exist(_fis_&var)) %then %do;
                  if _n_ = 1 then set _fis_&var(keep=nValue1 rename=(nValue1=p_fish));
                %end;
                
                param = put("&var",$paramdesc.);
                pvalue_num = coalesce(p_fish, p_chi);
                if missing(pvalue_num) then delete;
                if pvalue_num < 0.001 then pvalue_display='<0.001';
                else pvalue_display=strip(put(pvalue_num, 6.3));
                keep param pvalue_display pvalue_num;
                output;
                stop;
              run;

              proc append base=pvalues data=_catp_&var force; run;
            %end;
          %end;
        %end;

    %end;
    %else %do;
        %put WARNING: Variable &var has no non-missing values. Skipping.;
    %end;

  %end;
  
/* Manual ordering for categorical variables */
data cat_long_ordered;
  set cat_long;
  length order 8;
  
  select (param);
    /* Sex */
    when ('Sex - n (%)') do;
      select (level);
        when ('Female') order = 1;
        when ('Male') order = 2;
        when ('Missing') order = 3;
        otherwise order = 4;
      end;
    end;
    
    /* Race */
    when ('Race - n (%)') do;
      select (level);
        when ('American Indian or Alaska Native') order = 1;
        when ('Asian') order = 2;
        when ('Black') order = 3;
        when ('Native Hawaiian or Other Pacific Islander') order = 4;
        when ('White') order = 5;
        when ('Other') order = 6;
        when ('Not Reported') order = 7;
        when ('Unknown') order = 8;
        otherwise order = 9;
      end;
    end;
    
    /* Age Group */
    when ('Age Group - n (%)') do;
      select (level);
        when ('<65') order = 1;
        when ('^{unicode 2265}65') order = 2;
        when ('Missing') order = 3;
        otherwise order = 4;
      end;
    end;
    
    /* BMI Category */
    when ('BMI Category - n (%)') do;
      select (level);
        when ('Underweight') order = 1;
        when ('Normal') order = 2;
        when ('Overweight') order = 3;
        when ('Obese') order = 4;
        when ('Missing') order = 5;
        otherwise order = 6;
      end;
    end;
    
    /* Ethnicity */
    when ('Ethnicity - n (%)') do;
      select (level);
        when ('Hispanic or Latino') order = 1;
        when ('Not Hispanic or Latino') order = 2;
        when ('Not Reported') order = 3;
        when ('Unknown') order = 4;
        otherwise order = 5;
      end;
    end;
    
    /* Region */
    when ('Region - n (%)') do;
      select (level);
        when ('Africa') order = 1;
        when ('Asia') order = 2;
        when ('Europe') order = 3;
        when ('North America') order = 4;
        when ('South America') order = 5;
        when ('Oceania') order = 6;
        when ('Missing') order = 7;
        otherwise order = 8;
      end;
    end;
    
    /* Smoker */
    when ('Smoker - n (%)') do;
      select (level);
        when ('Former') order = 1;
        when ('Current') order = 2;
        when ('Never') order = 3;
        when ('Missing') order = 4;
        otherwise order = 5;
      end;
    end;
    
    /* Default for any unexpected parameters */
    otherwise order = 1;
  end;
run;

  /* ===== Continuous variables Processing ===== */
  %let contvars=AGE WEIGHT HEIGHT BMI;

  /* Enhanced continuous statistics (n, mean, SD, median, min, max) */
  proc means data=work_population nway noprint;
    class &trtvar / missing;
    var &contvars;
    output out=cont_stats(drop=_type_ _freq_)
           n=     n_AGE     n_WEIGHT     n_HEIGHT     n_BMI
           mean=  mean_AGE  mean_WEIGHT  mean_HEIGHT  mean_BMI
           std=   sd_AGE    sd_WEIGHT    sd_HEIGHT    sd_BMI
           median=med_AGE   med_WEIGHT   med_HEIGHT   med_BMI
           min=   min_AGE   min_WEIGHT   min_HEIGHT   min_BMI
           max=   max_AGE   max_WEIGHT   max_HEIGHT   max_BMI;
  run;

  /* Overall row */
  %if %upcase(&dooverall)=Y %then %do;
    proc means data=work_population nway noprint;
      var &contvars;
      output out=cont_overall(drop=_type_ _freq_)
             n=     n_AGE     n_WEIGHT     n_HEIGHT     n_BMI
             mean=  mean_AGE  mean_WEIGHT  mean_HEIGHT  mean_BMI
             std=   sd_AGE    sd_WEIGHT    sd_HEIGHT    sd_BMI
             median=med_AGE   med_WEIGHT   med_HEIGHT   med_BMI
             min=   min_AGE   min_WEIGHT   min_HEIGHT   min_BMI
             max=   max_AGE   max_WEIGHT   max_HEIGHT   max_BMI;
    run;

    data cont_overall;
      length &trtvar $&_maxlen;
      set cont_overall; &trtvar='Overall';
    run;

    data cont_all; set cont_stats cont_overall; run;
  %end;
  %else %do;
    data cont_all; set cont_stats; run;
  %end;

/* Create display with all statistics in order */
data cont_disp;
  length param $50 level $40 trt $&_maxlen value $25 order 8;
  set cont_all;
  trt = &trtvar;

  /* AGE rows */
  param = 'Age (years)'; 
  level = 'n'; value = strip(put(n_AGE, best.)); order = 1; output;
  level = 'Mean'; value = strip(put(mean_AGE, 6.1)); order = 2; output;
  level = 'SD'; value = strip(put(sd_AGE, 6.1)); order = 3; output;
  level = 'Median'; value = strip(put(med_AGE, 6.1)); order = 4; output;
  level = 'Minimum'; value = strip(put(min_AGE, 6.1)); order = 5; output;
  level = 'Maximum'; value = strip(put(max_AGE, 6.1)); order = 6; output;
  
  /* WEIGHT rows */
  param = 'Weight (kg)';
  level = 'n'; value = strip(put(n_WEIGHT, best.)); order = 1; output;
  level = 'Mean'; value = strip(put(mean_WEIGHT, 6.1)); order = 2; output;
  level = 'SD'; value = strip(put(sd_WEIGHT, 6.1)); order = 3; output;
  level = 'Median'; value = strip(put(med_WEIGHT, 6.1)); order = 4; output;
  level = 'Minimum'; value = strip(put(min_WEIGHT, 6.1)); order = 5; output;
  level = 'Maximum'; value = strip(put(max_WEIGHT, 6.1)); order = 6; output;
  
  /* HEIGHT rows */
  param = 'Height (cm)';
  level = 'n'; value = strip(put(n_HEIGHT, best.)); order = 1; output;
  level = 'Mean'; value = strip(put(mean_HEIGHT, 6.1)); order = 2; output;
  level = 'SD'; value = strip(put(sd_HEIGHT, 6.1)); order = 3; output;
  level = 'Median'; value = strip(put(med_HEIGHT, 6.1)); order = 4; output;
  level = 'Minimum'; value = strip(put(min_HEIGHT, 6.1)); order = 5; output;
  level = 'Maximum'; value = strip(put(max_HEIGHT, 6.1)); order = 6; output;
  
  /* BMI rows */
  param = 'BMI (kg/m²)';
  level = 'n'; value = strip(put(n_BMI, best.)); order = 1; output;
  level = 'Mean'; value = strip(put(mean_BMI, 6.2)); order = 2; output;
  level = 'SD'; value = strip(put(sd_BMI, 6.2)); order = 3; output;
  level = 'Median'; value = strip(put(med_BMI, 6.2)); order = 4; output;
  level = 'Minimum'; value = strip(put(min_BMI, 6.2)); order = 5; output;
  level = 'Maximum'; value = strip(put(max_BMI, 6.2)); order = 6; output;
run;

  /* Continuous p-values */
  %if %upcase(&dostat)=Y and &trt_groups>1 %then %do;
    %do _i=1 %to %sysfunc(countw(&contvars));
      %let var=%scan(&contvars,&_i);
      %if %upcase(&test_type)=ANOVA %then %do;
        proc glm data=work_population;
          class &trtvar; model &var = &trtvar;
          ods output ModelANOVA=_anova_&var;
        run; 
        quit;
        
        %if %sysfunc(exist(_anova_&var)) %then %do;
          /* Filter for treatment effect row */
          data _anova_filtered_&var;
            set _anova_&var;
            if upcase(Source) = upcase("&trtvar");
          run;
          
          data _contp_&var;
            length param $50 pvalue_display $15;
            set _anova_filtered_&var(keep=ProbF rename=(ProbF=pvalue_num));
            
            /* Only proceed if valid p-value */
            if not missing(pvalue_num);
            
            select (upcase("&var"));
              when ('AGE')    param='Age (years)';
              when ('WEIGHT') param='Weight (kg)';
              when ('HEIGHT') param='Height (cm)';
              when ('BMI')    param='BMI (kg/m²)';
              otherwise       param="&var";
            end;
            if pvalue_num < 0.001 then pvalue_display='<0.001';
            else pvalue_display=strip(put(pvalue_num,6.3));
            keep param pvalue_display pvalue_num;
          run;
          
          /* Only append if valid data */
          proc sql noprint;
            select count(*) into :valid_rows from _contp_&var where not missing(pvalue_num);
          quit;
          
          %if &valid_rows > 0 %then %do;
            proc append base=pvalues data=_contp_&var force; run;
          %end;
        %end;
      %end;
      %else %do; /* Kruskal–Wallis */
        proc npar1way data=work_population wilcoxon;
          class &trtvar; var &var;
          ods output KruskalWallisTest=_kw_&var;
        run;
        
        %if %sysfunc(exist(_kw_&var)) %then %do;
          /* Check dataset then filter appropriately */
          data _kw_filtered_&var;
            set _kw_&var;
            /* Look for row containing p-value */
            if index(upcase(Name1),'CHI-SQUARE') > 0 or 
               index(upcase(Label1),'CHI-SQUARE') > 0 or
               index(upcase(Name1),'PR') > 0 or
               _n_ = 1; /* fallback to first row if structure is different */
          run;
          
          data _contp_&var;
            length param $50 pvalue_display $15;
            set _kw_filtered_&var;
            
            /* Try different possible p-value variable names */
            if not missing(nValue1) then pvalue_num = nValue1;
            else if not missing(Prob) then pvalue_num = Prob;
            else if not missing(ProbChiSq) then pvalue_num = ProbChiSq;
            
            /* Only proceed if p-value found */
            if not missing(pvalue_num);
            
            select (upcase("&var"));
              when ('AGE')    param='Age (years)';
              when ('WEIGHT') param='Weight (kg)';
              when ('HEIGHT') param='Height (cm)';
              when ('BMI')    param='BMI (kg/m²)';
              otherwise       param="&var";
            end;
            if pvalue_num < 0.001 then pvalue_display='<0.001';
            else pvalue_display=strip(put(pvalue_num,6.3));
            keep param pvalue_display pvalue_num;
          run;
          
          /* Only append if valid data */
          proc sql noprint;
            select count(*) into :valid_rows from _contp_&var where not missing(pvalue_num);
          quit;
          
          %if &valid_rows > 0 %then %do;
            proc append base=pvalues data=_contp_&var force; run;
          %end;
        %end;
      %end;
    %end;
  %end;

/* ===== Stack cat + cont, then pivot wide ===== */
data stack;
  set cat_long_ordered(keep=param level trt value order) 
      cont_disp(keep=param level trt value order);
run;

/* Ensure stable trt and row ordering */
proc sort data=stack;
  by param order level;
run;

/* Add treatment order for final output ordering */
proc sql;
  create table stack_ord as
  select s.*, o.order as trt_order
  from stack s left join trt_order_map o
    on s.trt=o.trt
  order by param, order, level, trt_order;
quit;

/* Final sort for transpose */
proc sort data=stack_ord;
  by param level order trt;
run;

/* Transpose to one row per (param, level) with columns per trt */
proc transpose data=stack_ord out=wide_temp(drop=_name_) prefix=col_;
  by param level order;
  id trt;
  var value;
run;

data wide;
  set wide_temp;
run;

/* Sort by param and order to preserve desired sequence */
proc sort data=wide;
  by param order;
run;

/* Attach P-values (one per param) if applicable */
%if %upcase(&dostat)=Y and &trt_groups>1 %then %do;
  /* Identify first level for each parameter using order variable */
  proc sql;
    create table param_first_level as
    select param, min(order) as first_order
    from wide
    group by param;
  quit;
  
  /* Then merge p-values only to first level of each parameter */
  proc sql;
    create table wide_final as
    select w.*, 
           case when w.order = pfl.first_order then coalesce(p.pvalue_display,' ') 
                else ' ' end as pvalue length=15
    from wide w 
    left join param_first_level pfl on w.param = pfl.param
    left join pvalues p on w.param = p.param
    order by w.param, w.order;
  quit;
  
  data wide; set wide_final; run;
%end;
%else %do;
  /* If no p-values, add an empty column to maintain structure */
  data wide;
      set wide;
      length pvalue $15;
      pvalue = ' ';
  run;
%end;

/* Build ordered list of treatment columns from trt_order_map (excluding Overall) */
proc sql noprint;
  select trt into :_trt1-:_trt20
  from trt_order_map
  where trt ne 'Overall'
  order by order;
  %let _trtcount=&sqlobs;
quit;

/* Retain columns in desired order (+ Overall, + pvalue) */
data wide;
  retain param level order  /* Keep order for final sorting */
  %do _j=1 %to &_trtcount;
    col_&&_trt&_j
  %end;
  %if %upcase(&dooverall)=Y %then col_Overall;
  %if %upcase(&dostat)=Y and &trt_groups>1 %then pvalue;;
  set wide;
run;

/* Custom ordering for parameters */
proc format;
  value $paramorder
    'Age (years)'                 = 1
    'Age Group - n (%)'           = 2
    'Sex - n (%)'                 = 3
    'Race - n (%)'                = 4
    'Ethnicity - n (%)'           = 5
    'Region - n (%)'              = 6
    'Weight (kg)'                 = 7
    'Height (cm)'                 = 8
    'BMI (kg/m²)'                 = 9
    'BMI Category - n (%)'        = 10
    'Smoker - n (%)'              = 11
    other                         = 99;
run;

/* Add parameter order to wide dataset */
data wide;
  set wide;
  param_order = input(put(param, $paramorder.), 2.);
  if missing(param_order) then param_order = 99;
run;

/* Sort by custom parameter order first, then within-parameter order */
proc sort data=wide;
  by param_order order;
run;

  /* ===== Column headers with N= from denom_full (always FULL for headers) ===== */
  proc sql noprint;
    /* treatment headers in order (excluding Overall) */
    select trt, N_full into :_htrt1-:_htrt20, :_hN1-:_hN20
    from denom_full where trt ne 'Overall'
    order by (select order from trt_order_map t where t.trt=denom_full.trt);
    %let _hcount=&sqlobs;

    /* Overall header N */
    %if %upcase(&dooverall)=Y %then %do;
      select N_full into :_overallN trimmed from denom_full where trt='Overall';
    %end;
  quit;

  /* Making sure ≥ gets displayed in the final PDF outputs */
  data wide;
    set wide;
    array char_vars(*) _character_;
    do i = 1 to dim(char_vars);
      char_vars(i) = tranwrd(char_vars(i), '≥', '^{unicode 2265}');
    end;
    drop i;
  run;

  /* ===== Output ===== */
  %open_output(outfile=&outrtf, style=journal, output_type=&output_type);

  title "Table &tablenum";
  title2 "&tab_title";
  %if %length(&tab_subtitle) > 0 %then %do;
  	title3 "&tab_subtitle";
  	title4 "&popdesc Population";
  %end;
  %else %do;
  	title3 "&popdesc Population";
  %end;

footnote1 "Source: &src";
%if %upcase(&output_type) = PDF %then %do;
  footnote2 j=r "Page ^{thispage} of ^{lastpage}";
%end;
%else %if %upcase(&output_type) = RTF %then %do;
  footnote2 j=r "Page [Number] of [Total]";
%end;
footnote3 j=l "Generated: %sysfunc(today(), worddate.)";

  proc report data=wide nowd headline split='|'
              style(header)=[just=l]
              style(column)=[cellpadding=2];
    columns param level
    %do _j=1 %to &_hcount;
      col_&&_htrt&_j
    %end;
    %if %upcase(&dooverall)=Y %then %do; col_Overall %end;
    %if %upcase(&dostat)=Y and &trt_groups>1 %then %do; pvalue %end;
    ;

    define param / group "Parameter" style(column)=[cellwidth=2.2in] order=data;
    define level / group "Category/Statistic" style(column)=[cellwidth=2.2in] order=data;

    %do _j=1 %to &_hcount;
      define col_&&_htrt&_j / display "&&_htrt&_j|(N = &&_hN&_j)" center;
    %end;

    %if %upcase(&dooverall)=Y %then %do;
      define col_Overall / display "Overall|(N = &_overallN)" center;
    %end;

    %if %upcase(&dostat)=Y and &trt_groups>1 %then %do;
      define pvalue / display "P-value" center style(column)=[cellwidth=0.8in];
    %end;

    compute before _page_ / style=[just=l];
    endcomp;
  run;

  %close_output(output_type=&output_type);

%mend adsl_basic;

/* =========================
   MACRO CALLS - RTF
   ========================= */

/* Intent-to-Treat Population */
%adsl_basic(
  pop=ITTFL,
  popdesc=Intent-to-Treat,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Basic_ADSL_Tables/ITT_Population/Table_14_1_1_ITT.rtf,
  tablenum=14.1.1,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=RTF
);

/* Safety Population */
%adsl_basic(
  pop=SAFFL,
  popdesc=Safety,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Basic_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.rtf,
  tablenum=14.1.2,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=RTF
);

/* Full Analysis Population */
%adsl_basic(
  pop=FASFL,
  popdesc=Full Analysis,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Basic_ADSL_Tables/FAS_Population/Table_14_1_3_FAS.rtf,
  tablenum=14.1.3,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=RTF
);

/* =========================
   MACRO CALLS - PDF
   ========================= */

/* Intent-to-Treat Population */
%adsl_basic(
  pop=ITTFL,
  popdesc=Intent-to-Treat,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Basic_ADSL_Tables/ITT_Population/Table_14_1_1_ITT.pdf,
  tablenum=14.1.1,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=PDF
);

/* Safety Population */
%adsl_basic(
  pop=SAFFL,
  popdesc=Safety,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Basic_ADSL_Tables/SAF_Population/Table_14_1_2_SAF.pdf,
  tablenum=14.1.2,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=PDF
);

/* Full Analysis Population */
%adsl_basic(
  pop=FASFL,
  popdesc=Full Analysis,
  popval=Y,
  trtvar=ARM,
  outrtf=/home/&sysuserid/TLFs/Output/ADSL_Tables/Basic_ADSL_Tables/FAS_Population/Table_14_1_3_FAS.pdf,
  tablenum=14.1.3,
  denom_mode=NONMISS,
  show_missing=Y,
  dostat=N,
  test_type=KW,
  dooverall=N,
  trt_order=Placebo DrugA,
  tab_title=Summary of Demographic and Baseline Characteristics,
  tab_subtitle=,
  src=ADSL,
  output_type=PDF
);
