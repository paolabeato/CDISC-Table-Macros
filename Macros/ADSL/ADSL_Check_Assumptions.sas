/************************************************************************************************************
* MACRO: ADSL_Chec_Assumptions.sas
* PURPOSE: Automatically checks statistical assumptions (normality and equal variance) for continuous 
*          variables to guide appropriate statistical test selection between parametric (ANOVA) and 
*          non-parametric (Kruskal-Wallis) methods in demographic baseline tables.
*
* PARAMETERS:
*   data        = Input dataset containing analysis population (Default: work_population)
*   contvars    = Space-separated list of continuous variables to check
*                 (Default: AGE WEIGHT HEIGHT BMI)
*   trtvar      = Treatment variable for grouping (Required)
*   outdir      = Output directory for HTML report. If empty, creates temporary report in WORK
*   alpha       = Significance level for statistical tests (Default: 0.05)
*
* STATISTICAL TESTS PERFORMED:
*   - Normality: Shapiro-Wilk test by treatment group
*   - Equal Variance: Levene's test for homogeneity of variances
*
* OUTPUT:
*   - Detailed summary with recommendations printed to SAS log
*   - Comprehensive HTML report with:
*     * Statistical test results (p-values)
*     * Histograms with normal density curves by treatment group
*     * Normal quantile-quantile (QQ) plots by treatment group
*   - Clear recommendations for test_type parameter in table1 macro
*
* INTERPRETATION GUIDANCE:
*   - Use test_type=ANOVA if: ALL p-values > alpha (assumptions met)
*   - Use test_type=KW if: ANY p-value < alpha (assumption violation)
*
* DEPENDENCIES:
*   - Requires work_population dataset (typically created by %table1 with dostat=N)
*   - Treatment variable must exist in the input dataset
*   - Continuous variables should be numeric
*
* CDISC CONSIDERATIONS:
*   - Supports CDISC ADaM analysis datasets
*   - Provides statistical justification for test selection in regulatory submissions
*   - Creates audit trail for statistical assumption checking
*
* EXAMPLE USAGE:
*   %adsl_assumptions(data=work_population, contvars=AGE WEIGHT, trtvar=ARM, 
*                      outdir=/output/assumption_checks, alpha=0.05);
*
* AUTHOR: Paola Beato
* DATE: 09/02/2025
* VERSION: 1.0
***************************************************************************************************************/

%macro adsl_assumptions(
    data=work_population,
    contvars=AGE WEIGHT HEIGHT BMI,
    trtvar=, /* Making this required by not providing a default */
    outdir=,
    alpha=0.05
);

    /* Validation for required parameters */
    %local i var;
   
    %if %length(&trtvar)=0 %then %do;
        %put ERROR: trtvar parameter is required.;
        %return;
    %end;
    
    %if %sysfunc(exist(&data))=0 %then %do;
        %put ERROR: Dataset &data does not exist.;
        %return;
    %end;

    %if %sysfunc(varnum(&data, &trtvar))=0 %then %do;
        %put ERROR: Treatment variable &trtvar not found in &data.;
        %return;
    %end;

    /* Create an output directory */
    %if %length(&outdir) %then %do;
        %put NOTE: Saving HTML report to &outdir/assumption_checks.html;
        filename _assump "&outdir/ADSL_Assumption_Checks.html";
        ods html5 file=_assump style=journal;
    %end;
    %else %do;
        %put NOTE: Creating temporary HTML report in WORK directory.;
        ods html5 style=journal;
    %end;

    title "Assumption Checking for Parametric Tests";
    title2 "Data: &data | Treatment: &trtvar";
    footnote "Recommendation: ANOVA if assumptions are met (p > &alpha), else use Kruskal-Wallis.";

    /* Single loop for both validation and processing */
    %do i = 1 %to %sysfunc(countw(&contvars));
        %let var = %scan(&contvars, &i);
    
        /* Validation for each variable */
        %if %sysfunc(varnum(&data, &var))=0 %then %do;
            %put WARNING: Variable &var not found - skipping.;
            %continue;
        %end;
        %else %do;
            /* Check if variable is numeric */
            %if %sysfunc(vartype(&data, %sysfunc(varnum(&data, &var)))) = C %then %do;
                %put WARNING: Variable &var is character - skipping.;
                %continue;
            %end;
        %end;

        title3 "Variable: &var";

        /* Reset flags for each variable */
        %global normality_violation variance_violation;
        %let normality_violation=0;
        %let variance_violation=0;

        /* 1. Normality Test by Group (Shapiro-Wilk) */
        proc univariate data=&data normal;
            class &trtvar;
            var &var;
            ods output TestsForNormality = _normality;
        run;

        /* 2. Equal Variance Test (Levene's) */
        proc glm data=&data;
            class &trtvar;
            model &var = &trtvar;
            means &trtvar / hovtest=levene;
            ods output HOVFTest = _levene;
            ods output NObs = _nobs; /* Get N per group for the log */
        run;
        quit;

        /* Process normality results if dataset exists */
        %if %sysfunc(exist(_normality)) %then %do;
            data _null_;
                set _normality;
                by &trtvar;
                where TestLab = "Shapiro-Wilk";

                /* Flag if any group has significant non-normality */
                if pValue < &alpha then do;
                    call symputx("normality_violation", 1);
                    put "--- &trtvar=" &trtvar +(-1) ": Shapiro-Wilk W=" Statistic "p=" pValue " *VIOLATION*";
                end;
                else do;
                    put "--- &trtvar=" &trtvar +(-1) ": Shapiro-Wilk W=" Statistic "p=" pValue;
                end;
            run;
        %end;
        %else %do;
            %put WARNING: Normality tests could not be performed for &var;
        %end;

        /* Process variance results if dataset exists */
        %if %sysfunc(exist(_levene)) %then %do;
            data _null_;
                set _levene;
                /* Flag if variances are not equal */
                if probF < &alpha then do;
                    call symputx("variance_violation", 1);
                    put "--- Levene's Test for Homogeneity of Variance: p=" probF " *VIOLATION*";
                end;
                else do;
                    put "--- Levene's Test for Homogeneity of Variance: p=" probF;
                end;
            run;
        %end;
        %else %do;
            %put WARNING: Levene test could not be performed for &var;
        %end;

        /* Print Final Recommendation to Log */
        %put ---------------------------------------------------;
        %put VARIABLE: &var;
        %put ;
        %if &normality_violation=1 or &variance_violation=1 %then %do;
            %put **ASSUMPTION VIOLATION DETECTED**.;
            %if &normality_violation=1 %then %put - Normality assumption violated (p < &alpha in at least one group).;
            %if &variance_violation=1 %then %put - Equal variance assumption violated (p < &alpha).;
            %put RECOMMENDATION: Use test_type=KW in table1 macro.;
        %end;
        %else %do;
            %put Assumptions of normality and equal variance were not violated (p > &alpha).;
            %put RECOMMENDATION: Use test_type=ANOVA in table1 macro.;
        %end;
        %put ---------------------------------------------------;
        %put ;

        /* 3. Generate Visual Plots for Inspection (HTML Report) */
        proc sgpanel data=&data;
            panelby &trtvar / columns=4;
            histogram &var;
            density &var / type=normal;
        run;

        /* QQ plots using PROC UNIVARIATE */
        title4 "QQ Plots for &var by &trtvar";
        proc univariate data=&data;
            class &trtvar;
            var &var;
            qqplot / normal(mu=est sigma=est);
        run;
        title4;

        /* Cleanup datasets for next variable */
        proc datasets library=work nolist;
            delete _normality _levene _nobs;
        quit;

    %end; /* End of variable loop */

    %if %length(&outdir) %then %do;
        ods html5 close;
        filename _assump clear;
        %put NOTE: HTML report saved to &outdir/assumption_checks.html;
    %end;
    %else %do;
        ods html5 close;
    %end;

%mend adsl_assumptions;

/* Print log */
proc printto log="/home/&sysuserid/TLFs/Output/ADSL_Tables/ADSL_Assumptions/ADSL_Assumptions_Log.txt" new;
run;

%adsl_assumptions(
    data=work_population,
    contvars=AGE WEIGHT HEIGHT BMI,
    trtvar=ARM,
    outdir=/home/&sysuserid/TLFs/Output/ADSL_Tables/ADSL_Assumptions
);

/* Reset log back to SAS environment */
proc printto;
run;