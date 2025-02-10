/*-----------------------------------------------------------
 * FIN9013 Assignment1
 * Author: Ang Zhang
 * Date: 2025-02-01
 * Content: SAS code for method assignment
 *-----------------------------------------------------------
 */
libname mydata "/home/u64135951/sasuser.v94";
%let export_path = /home/u64135951/sasuser.v94;

/* Question A */
data firms;
	set mydata.ccm_1985_2020_cln;
	where fyear between 2005 and 2016;
run;


data firms;
	set firms;

	Debt=(dltt + dlc) / at;

	Size=log(sale);


	MB=(prcc_f * cshpri + dlc + dltt + pstkl - txditc) / at;
	if missing(xrd) then
		xrd=0;

	if xrd > 0 then
		D_RD=1;
	else
		D_RD=0;


	RD_ratio=xrd / sale;

	if RD_ratio=0 then
		RD_rank=1;
	else if RD_ratio > 0 and RD_ratio < 0.25 then
		RD_rank=2;
	else if RD_ratio >=0.25 and RD_ratio < 0.5 then
		RD_rank=3;
	else if RD_ratio >=0.5 and RD_ratio < 0.75 then
		RD_rank=4;
	else if RD_ratio >=0.75 then
		RD_rank=5;

	if Debt > 0.25 then
		D_Debt=1;
	else
		D_Debt=0;

	if month(ipodate)=12 then
		D_DecIPO=1;
	else
		D_DecIPO=0;
run;

proc univariate data=firms noprint;
	var Debt Size MB;
	output out=percentiles pctlpts=1 99 pctlpre=Debt_ Size_ MB_;
run;

data firms;
	set firms;
	if _n_ = 1 then set percentiles;

	if Debt < Debt_1 then
		Debt=Debt_1;

	if Debt > Debt_99 then
		Debt=Debt_99;

	if Size < Size_1 then
		Size=Size_1;

	if Size > Size_99 then
		Size=Size_99;

	if MB < MB_1 then
		MB=MB_1;

	if MB > MB_99 then
		MB=MB_99;
run;

ods output Summary=summary_stats;
proc means data=firms n mean std min p25 median p75 max stackods;
    var Debt Size MB D_RD RD_rank D_Debt D_DecIPO;
run;

ods output Summary=summary_stats_rd;
proc means data=firms(where=(D_RD=1)) n mean std min p25 median p75 max stackods;
    var Debt Size MB D_RD RD_rank D_Debt D_DecIPO;
run;

ods output Summary=summary_stats_no_rd;
proc means data=firms(where=(D_RD=0)) n mean std min p25 median p75 max stackods;
    var Debt Size MB D_RD RD_rank D_Debt D_DecIPO;
run;

ods output close;


data summary_stats_all;
    set summary_stats(in=a) summary_stats_rd(in=b) summary_stats_no_rd(in=c);
    if a then panel = "All Firms";
    else if b then panel = "Firms w RD";
    else if c then panel = "Firmsw/o RD";
run;


proc print data=summary_stats_all noobs;
    by panel;
    title "Table 1. Summary Statistics Table";
run;

/* save to mydata lib*/
data mydata.firm;
	set firms;
run;
	

/* Question B */
proc reg data=firms outest=model_1;
	model Debt=Size MB D_RD;
	ods output ParameterEstimates=param_estimates FitStatistics=fit_stats NObs=num_obs;
run;

/* Baseline Model */
proc glm data=firms outstat=model_baseline;
    model Debt = Size MB D_RD / solution;
    ods output ParameterEstimates=coef_baseline FitStatistics=fit_baseline NObs=nob_baseline;
	store out=baseline_store;
run;

/* Interaction Model */
proc glm data=firms outstat=model_interaction;
    model Debt = Size MB D_RD MB*D_RD / solution;
    ods output ParameterEstimates=coef_interaction FitStatistics=fit_interaction NObs=nob_interaction;
run;

/* RD Model */
proc glm data=firms outstat=model_rd;
    where D_RD = 1;
    model Debt = Size MB / solution;
    ods output ParameterEstimates=coef_rd FitStatistics=fit_rd NObs=nob_rd;
run;

/* No RD Model */
proc glm data=firms outstat=model_no_rd;
    where D_RD = 0;
    model Debt = Size MB / solution;
    ods output ParameterEstimates=coef_no_rd FitStatistics=fit_no_rd NObs=nob_no_rd;
run;

/* Augmented Model */
proc glm data=firms outstat=model_augmented;
    model Debt = Size MB D_RD Size*D_RD MB*D_RD / solution;
    ods output ParameterEstimates=coef_augmented FitStatistics=fit_augmented NObs=nob_augmented;
run;

/* RD Rank Model */
proc glm data=firms outstat=model_rd_rank;
    model Debt = Size MB RD_rank / solution;
    ods output ParameterEstimates=coef_rd_rank FitStatistics=fit_rd_rank NObs=nob_rd_rank;
run;

/* RD Class Model */
proc glm data=firms outstat=model_rd_class;
    class RD_rank;
    model Debt = Size MB RD_rank / solution;
    ods output ParameterEstimates=coef_rd_class FitStatistics=fit_rd_class NObs=nob_rd_class;
run;

%macro export_glm_stats(model_name);
    /* Extract coefficients, significance, and standard error */
    
    data coef_&model_name;
        set coef_&model_name;
        length Display $100;
        if Probt < 0.01 then star = '***';
        else if Probt < 0.05 then star = '**';
        else if Probt < 0.10 then star = '*';
        else star = '';
        
        Display = cats(put(Estimate, best8.4), star, " (", put(StdErr, best8.4), ")");
        keep Parameter Display;
    run;
    
    data fit_&model_name;
        set fit_&model_name;
        if _N_ = 1 then do;
        	Parameter = "RSquare";
            Display = put(RSquare, best8.4);
            output;
        end;
        keep Parameter Display;
    run;
    
    run;
    
    data nob_&model_name;
        set nob_&model_name;
        if Label1 = "Number of Observations Used" then do;
        	Parameter = "#Obs";
            Display = put(N, best8.);
            output;
        end;
        keep Parameter Display;
    run;
    
    data combined;
    	set coef_&model_name fit_&model_name nob_&model_name;
    run;
    
    /* Export the data to a CSV file */
    proc export data=combined
        outfile="&export_path./&model_name._stats.csv"
        dbms=csv
        replace;
        putnames=yes;
    run;

%mend export_glm_stats;

%export_glm_stats(baseline);
%export_glm_stats(interaction);
%export_glm_stats(rd);
%export_glm_stats(no_rd);
%export_glm_stats(augmented);
%export_glm_stats(rd_rank);
%export_glm_stats(rd_class);




%macro process_results(modelname);
    /* Add significance levels */
    data coef_&modelname;
        set coef_&modelname;
        if ProbT < 0.01 then Significance = "***";
        else if ProbT < 0.05 then Significance = "**";
        else if ProbT < 0.10 then Significance = "*";
        else Significance = "";
        format Estimate 8.4 StdErr 8.4;
        modelname = "&modelname";
    run;
    
    /* Extract R-Squared and number of observations */
    data fit_&modelname;
        set fit_&modelname;
        if Label2 = "R-Square" then do;
        Variable = "R-Squared"
        Estimate = nvalue2;
        output;
        end;
        if label1 = "Number of Observations" then N = nvalue2;
        keep R_Sq N modelname;
        modelname = "&modelname";
    run;
%mend;





data reg_summary;
    set param_estimates fit_stats_rows;
run;

proc means data=firms noprint;
    var Size MB D_RD;
    output out=means(drop=_TYPE_ _FREQ_) 
           mean=Size MB D_RD;
    output out=medians(drop=_TYPE_ _FREQ_) 
           median=Size MB D_RD;
run;

proc plm restore=baseline_store;
    score data=means out=pred_means;
    score data=medians out=pred_medians;
run;


%margins(v);

/* Logistic regression with D(Debt) as dependent variable */

/* */