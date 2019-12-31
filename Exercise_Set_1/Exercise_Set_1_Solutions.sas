/*Author: Bazil Muzaffar Kotriwala
Timestamp: 29 Dec 2019 */

* Set directory;
LIBNAME SASDATA "/folders/myfolders/Exercise_1";

/* ---- Printing / Importing data* ----*/

* Print Data;
/* PROC PRINT DATA= SASDATA.IVF; */
/* RUN; */

* Import .sas7bdat file from library;
DATA IVF;
	SET SASDATA.IVF;
RUN;

/* ----- Create Subset without IMP, PER and AGE variable ---
   ----- Remove Duplicate rows based on ID / PER (only single row for each child) *
   ----- Setting PER = 4, 8 or 12 works because each child is measured 3 times 
   		 (so rows for each child repeat 3 times) */

* Method 1;
DATA IVFSubset;
	SET IVF;
	WHERE PER = 4;
	DROP IMP PER AGE;
RUN;

* Method 2;
DATA IVFSubset;
	SET IVF;
	DROP IMP PER AGE;
RUN;

PROC SORT DATA=IVFSubset
 NODUPKEY;
 BY ID;
RUN;

** ---- Questions ---- **

/* Question 1.1 */

** 1. Total number of children in study (Count unique rows in IVFSubset)
   2. Total number of children belonging to different treatment groups (Group by TRT);

* Method 1;
PROC SQL;
	SELECT count(ID) AS tot_children
	FROM IVFSubset
QUIT;

PROC SQL;
	SELECT TRT, count(ID) AS tot_children
	FROM IVFSubset
	GROUP BY TRT;
QUIT;

* Method 2;
PROC FREQ DATA=IVFSubset;
	TABLES TRT;
RUN;


/* Question 1.2
   Assumption: Age of mothers (AGEM) is normally distributed */

* Part a);

** 95% Confidence interval;
ods select BasicIntervals;
PROC UNIVARIATE DATA=IVFSubset cibasic(alpha=0.05);
	VAR AGEM;
RUN;

** 95% Prediction Interval;

* Method 1;
PROC IML;
use IVFSubset;
read all var{agem};
close IVFSubset;

alpha = 0.05;

Ybar = mean(agem);
s = var(agem);
n = nrow(agem);

/* print(Ybar || s || n); */

qT = quantile('t', alpha/2, n-1);
/*  */
/* print(qT); */

UPL = Ybar - qT * sqrt((n+1) * s/n);
LPL = Ybar + qT * sqrt((n+1) * s/n);

A = Ybar || s || LPL || UPL;

/* print(A); */
/*  */
/* print(UCL || LCL); */

create DATA from A[colname = {'mean', 'variance', 'LPL', 'UPL'}];
append from A;
close DATA;
quit;

* Method 2;
PROC MEANS data=IVFSubset MEAN VAR n;
	var AGEM;
	output out=agem_sumstat;
run;

proc transpose data=agem_sumstat out=agem_PI(DROP= _TYPE_ _FREQ_ _NAME_ _LABEL_);
	by _type_ _freq_;
	id _stat_;
run;

* Print Data;
PROC PRINT DATA= agem_PI;
RUN;
	
data agem_PI;
	set agem_PI;
	T = QUANTILE("T", 1-0.05/2, N-1);
	LPL = MEAN -T * std*sqrt((N+1)/ N);
	UPL = MEAN + T * std*sqrt((N+1)/ N);
run;

proc print DATA=agem_PI;
var LPL UPL;
run;

* Method 3;
/* The REG procedure fits least-squares estimates to linear regression models. */
PROC REG data=IVFSubset;
	model AGEM = / cli alpha=0.05;
run;


* Part b);

/* Number of mothers which became pregnant after age 40 */

* Method 1;
PROC SQL;
	SELECT count(ID)
	FROM IVFSubset
	WHERE AGEM >= 40;
QUIT;

* Method 2;

PROC FREQ DATA=IVFSubset;
	TABLES AGEM;
	WHERE AGEM >= 40;

* Part c);
ods select BasicIntervals;
PROC UNIVARIATE DATA=IVFSubset cibasic(alpha=0.05);
	var AGEM;
RUN;

/* Question 1.3  
   Analyze the birth weight (BW)*/
  
** Part a - Compute Q1, Q3 and Interquartile Range);
* Method 1;
PROC UNIVARIATE DATA=IVFSubset cipctldf;
	var BW;
	
* Method 2;
PROC MEANS DATA=IVFSubset p25 p75 qrange;
	var BW;
	
** Part b - % of children whose birth weight differs at most one interquartilerange 
   from the median, median +- iq;

PROC SQL;
	SELECT count(ID) / (SELECT count(ID) FROM IVFSubset) * 100
	FROM IVFSubset
	WHERE BW BETWEEN (3365 - 870) AND (3365 + 870);
QUIT;

** Part c;

DATA IVFSubsetBOXCOX;
	SET IVFSubset;
	BWMINUS2= (-1/2)*(BW**-2 -1);
	BWMINUS1= (-1)*(BW**-1 -1);
	BWMINUS12= (-2)*(BW**-(0.5)-1);
	BW0= log(BW);
	BW13= (3)*(BW**(1/3) -1);
	BW12= (2)*(BW**(1/2) -1);
	BW2= (0.5)*(BW**(2) -1);
RUN;

ods SELECT histogram;
proc univariate data=IVFSubsetBOXCOX;
	histogram BWMINUS2 /normal;
	histogram BWMINUS1 /normal;
	histogram BWMINUS12 /normal;
	histogram BW0 /normal;
	histogram BW13 /normal;
	histogram BW12 /normal;
	histogram BW2 /normal;
	histogram BW /normal;
run;

** Part d;

** 95% Prediction Interval;

* Method 1;
PROC IML;
use IVFSubsetBOXCOX;
read all var{BW2};
close IVFSubsetBOXCOX;

alpha = 0.05;

Ybar = mean(BW2);
s = var(BW2);
n = nrow(BW2);

qT = quantile('t', alpha/2, n-1);

UPL = Ybar - qT * sqrt((n+1) * s/n);
LPL = Ybar + qT * sqrt((n+1) * s/n);

/* NOTE: For boxcox the BW2 have transformed the BW values */

/* Transform back to the original values */
LPL = sqrt(2*LPL + 1);
UPL = sqrt(2*UPL + 1);

A = Ybar || s || LPL || UPL;

create DATA from A[colname = {'mean', 'variance', 'LPL', 'UPL'}];
append from A;
close DATA;
quit;

* Method 2;
PROC MEANS data=IVFSubsetBOXCOX MEAN VAR n;
	var BW2;
	output out=BW2_sumstat;
run;

proc transpose data=BW2_sumstat out=BW2_PI(DROP= _TYPE_ _FREQ_ _NAME_ _LABEL_);
	by _type_ _freq_;
	id _stat_;
run;

* Print Data;
PROC PRINT DATA=BW2_PI;
RUN;
	
data BW2_PI;
	set BW2_PI;
	T = QUANTILE("T", 1-0.05/2, N-1);
	LPL = MEAN -T * std*sqrt((N+1)/ N);
	UPL = MEAN + T * std*sqrt((N+1)/ N);
	
	/* Transform back to the original values */
	LPL = sqrt(2*LPL + 1);
	UPL = sqrt(2*UPL + 1);
run;

proc print DATA=BW2_PI;
var LPL UPL;
run;

* Part e - Heaviest baby in data-set;
* Method 1;
PROC SQL; 
	SELECT *
	FROM IVFSubset
	HAVING BW = max(BW);
	
* Method 2;
PROC MEANS data = IVFSubset max;
	var BW;
RUN;

* Part f;
* % of boys and girls in data-set, mean, variance, skewness and kurtosis of birth weight;

PROC SQL;
	SELECT SEX, count(SEX) / (SELECT count(SEX) FROM IVFSubset) * 100 AS perc_gender, 
	avg(BW) AS mean_BW, VAR(BW) AS var_BW
	FROM IVFSubset
	GROUP BY SEX;
RUN;

PROC UNIVARIATE DATA=IVFSubset;
	CLASS SEX;
	VAR BW;
RUN;

* Part g;
/* Larger variance for boys, therefore a wider prediction interval for the boys */
/* Computing 95 % PI's using box-cox transformation from part (c) */

PROC IML;
use IVFSubsetBOXCOX;
read all var{BW2} into boy where(SEX=1);
read all var{BW2} into girl where(SEX=0);
close IVFSubsetBOXCOX;

/* Boy (SEX = 1) Group calculation */
alpha = 0.05;

Ybar_boy = mean(boy);
s_boy = var(boy);
n_boy = nrow(boy);

qT_boy = quantile('t', alpha/2, n_boy-1);

UPL_boy = Ybar_boy - qT_boy * sqrt((n_boy+1) * s_boy/n_boy);
LPL_boy = Ybar_boy + qT_boy * sqrt((n_boy+1) * s_boy/n_boy);

/* NOTE: For boxcox the BW2 have transformed the BW values */

/* Transform back to the original values */
LPL_boy = sqrt(2*LPL_boy + 1);
UPL_boy = sqrt(2*UPL_boy + 1);

/* Girl (SEX = 0) Group calculation */

Ybar_girl = mean(girl);
s_girl = var(girl);
n_girl = nrow(girl);

qT_girl = quantile('t', alpha/2, n_girl-1);

UPL_girl = Ybar_girl - qT_girl * sqrt((n_girl+1) * s_girl/n_girl);
LPL_girl = Ybar_girl + qT_girl * sqrt((n_girl+1) * s_girl/n_girl);

/* NOTE: For boxcox the BW2 have transformed the BW values */

/* Transform back to the original values */
LPL_girl = sqrt(2*LPL_girl + 1);
UPL_girl = sqrt(2*UPL_girl + 1);

A = Ybar_boy || s_boy || LPL_boy || UPL_boy || Ybar_girl || s_girl || LPL_girl || UPL_girl;

create DATA from A[colname = {'mean_boy', 'var_boy', 'LPL_boy', 'UPL_boy', 'mean_girl', 'var_girl', 'LPL_girl', 'UPL_girl'}];
append from A;
close DATA;
quit;

* Method 2 (add later)


/* Question 1.4 
   Create dataset, compute statistics, 95% conf and 99% prediction interval*/;

* Create dataset;
DATA qs_1_4_data; 
   INPUT Value@@; 
   DATALINES; 
	25.0
	27.4
	17.1
	22.1
	20.8
	21.3
	22.5
	29.2
	27.9
	25.7
	24.7
	18.8
	;
RUN; 
  
* Part a - basic statistics;
PROC UNIVARIATE DATA=qs_1_4_data;
	VAR Value;
RUN;

* Part b - 95% Confidence Interval;
ods select BasicIntervals;
PROC UNIVARIATE DATA=qs_1_4_data cibasic(alpha=0.05);
	VAR Value;
RUN;

* Part c - 99% Prediction Interval;
* Method 1;
PROC IML;
use qs_1_4_data;
read all var{Value};
close qs_1_4_data;

alpha = 0.01;

Ybar = mean(Value);
s = var(Value);
n = nrow(Value);

qT = quantile('t', alpha/2, n-1);

UPL = Ybar - qT * sqrt((n+1) * s/n);
LPL = Ybar + qT * sqrt((n+1) * s/n);

A = Ybar || s || LPL || UPL;

create DATA from A[colname = {'mean', 'variance', 'LPL', 'UPL'}];
append from A;
close DATA;
quit;

* Method 2;
PROC MEANS data=qs_1_4_data MEAN VAR n;
	var Value;
	output out=Value_sumstat;
run;

proc transpose data=Value_sumstat out=Value_PI(DROP= _TYPE_ _FREQ_ _NAME_ _LABEL_);
	by _type_ _freq_;
	id _stat_;
run;

* Print Data;
PROC PRINT DATA= Value_PI;
RUN;
	
data Value_PI;
	set Value_PI;
	T = QUANTILE("T", 1-0.01/2, N-1);
	LPL = MEAN -T * std*sqrt((N+1)/ N);
	UPL = MEAN + T * std*sqrt((N+1)/ N);
run;

proc print DATA=Value_PI;
var LPL UPL;
run;

/* Question 1.5 
   */;

* Part a;
DATA qs_1_5_interval; 
   INPUT log_LPL@@ log_UPL@@; 
   DATALINES; 
	-0.137 1.128
	;
RUN;
/*  */
/* DATA qs_1_5_interval; */
/* 	set qs_1_5_interval; */
/* 	LPL = 10 ** log_LPL; */
/* 	UPL = 10 ** log_UPL; */
/* RUN; */

/* Question 1.6 
   Gesta-tional age (GA) measures the duration in pregnancy in week 
   This is not normally distributed, LOG(44 - GA) does have an approximate normal distribution*/;
   
* Part a -  95% Prediction Interval;

DATA IVFSubset;
	set IVFSubset;
	log_GA = log(44 - GA);
RUN;

/* Plotting to see if LOG(44 - GA) does indeed approximate to a normal distribution */
ods select HISTOGRAM;
PROC UNIVARIATE data=IVFSubset;
   histogram log_GA/normal;
RUN;

* Method 1;
PROC IML;
use IVFSubset;
read all var{log_GA};
close IVFSubset;

alpha = 0.05;

Ybar = mean(log_GA);
s = var(log_GA);
n = nrow(log_GA);

qT = quantile('t', alpha/2, n-1);

/* Note for the loge the UPL mean is added and LPL is subtracted */
UPL = Ybar + qT * sqrt((n+1) * s/n);
LPL = Ybar - qT * sqrt((n+1) * s/n);

/* Transform back to the original values */
LPL = 44 - exp(LPL);
UPL = 44 - exp(UPL);

A = Ybar || s || LPL || UPL;

create DATA from A[colname = {'mean', 'variance', 'LPL', 'UPL'}];
append from A;
close DATA;
quit;

* Method 2;
PROC MEANS data=IVFSubset MEAN VAR n;
	var log_GA;
	output out=log_GA_sumstat;
run;

proc transpose data=log_GA_sumstat out=log_GA_PI(DROP= _TYPE_ _FREQ_ _NAME_ _LABEL_);
	by _type_ _freq_;
	id _stat_;
run;

* Print Data;
PROC PRINT DATA= log_GA_PI;
RUN;
	
data log_GA_PI;
	set log_GA_PI;
	T = QUANTILE("T", 1-0.05/2, N-1);
	LPL = MEAN + T * std*sqrt((N+1)/ N);
	UPL = MEAN - T * std*sqrt((N+1)/ N);

	/* Transform back to the original values */
	LPL = 44 - exp(LPL);
	UPL = 44 - exp(UPL);
run;

proc print DATA=log_GA_PI;
var LPL UPL;
run;

* Part b - 95% Confidence interval;
PROC UNIVARIATE DATA = IVFSubset cibasic(alpha=0.05);
	var log_GA;
RUN;
/* No, if we only have the 95% confidence interval for the mean of LOG(44 - GA), then we
cannot compute a 95% confidence interval for the mean of GA. There does not exist any  
function which maps u(f) to log(u(f))*/

* Part c - 95% confidence interval for median GA;

PROC UNIVARIATE DATA = IVFSubset CIPCTLDF;
	var GA;
RUN;
/* Because 39 is not contained in the confidence interval we conclude that the median of GA is a
significantly different from 39. */

* Part d - Testing whether preterm children (GA <= 38) have a greater chance to experience stress during delivery.;
DATA IVFSubset;
	set IVFSubset;
	if GA <= 38 then GA_preterm = 1;
	else GA_preterm = 0;
RUN;

* Part e - % of children in stress (FIS) for both preterm and non-preterm;

PROC PRINT Data=IVFSubset;

/* DATA IVFFIS1; */
/* 	SET IVFSubset; */
/* 	WHERE FIS = 1; */
/* RUN; */
/*  */
/* PROC SQL; */
/* 	SELECT count(ID) / (SELECT count(ID) FROM IVFSubset) * 100 */
/* 	FROM IVFSubset */
/* 	WHERE FIS = 1 AND GA_preterm = 1; */
/* RUN; */
/*  */
/* PROC SQL; */
/* 	SELECT GA_preterm, count(ID) */
/* 	FROM IVFSubset */
/* 	GROUP BY GA_preterm */
/* 	HAVING FIS = 1; */
/* RUN; */
/*  */
/* PROC SQL; */
/* 	SELECT avg(FIS) */
/* 	FROM Q1_6D */
/* 	WHERE PRETERM = 1; */
/* QUIT; */
/*  */
/*  */
/* DATA = IVFSubset; */
/* 	SET = IVFSubset; */
/* 	WHERE FIS = 1; */
/* 	output out=IVFSubsetFIS1; */
/* RUN; */
/*  */
/* PROC UNIVARIATE DATA=IVFSubset; */
/* 	CLASS GA_preterm; */
/* 	VAR BW; */
/* RUN; */
/*  */
/*  */
/* DATA IVFFIS1; */
/* 	SET IVFSubset; */
/* 	WHERE FIS = 1; */
/* RUN; */
/*  */
/* PROC SQL; */
/* 	SELECT GA_preterm, count(GA_preterm) */
/* 	FROM IVFFIS1 */
/* 	GROUP BY GA_preterm; */
/* RUN; */
/*  */
/* PROC SQL; */
/* 	SELECT GA_preterm, count(GA_preterm)  */
/* 	/ (SELECT count(GA_preterm) FROM IVFSubset) * 100 AS perc_child_in_stress */
/* 	FROM IVFSubset */
/* 	HAVING FIS = 1 */
/* 	GROUP BY GA_preterm; */
/* RUN; */

* Part f;


/* Question 1.8 */

* Part a - Two sided test H0: u = 3200;

/* t-stat = 2.20, p-val = 0.0285, reject the null hypothesis h0 as p-val < 0.05, 
thus the average birth weight is significantly different (alpha = 0.05) from 3200 */
proc ttest data=IVFSubset h0=3200 sides=2 alpha=0.05;
	var BW;
run;

* Part b - One sided test H0: u <= 3200;
/* t-stat = 2.20, p-val = 0.0143, reject the null hypothesis h0 as p-val < 0.05,
thus the average birth weight is significantly greater than (alpha = 0.05) 3200 */
proc ttest data=IVFSubset h0=3200 sides=U alpha=0.05;
	var BW;
run;

* Part c;
/* Results seem to be reliable as the averages converge to a norm dist due to CLT (therefore, n is large enough) */
%macro samples(dataset=,ns=,n=);
proc surveyselect data=&dataset NOPRINT
	method=urs n=&n out=FINAL;
run;

data FINAL;
	set FINAL;
	sampleno = 1;
run;

%do sn = 2 %to &ns;
	proc surveyselect data=&dataset NOPRINT
	method=urs n=&n out=SAMPLEI;
run;

data SAMPLEI;
	set SAMPLEI;
	sampleno = &sn;
run;

data FINAL;
	set Final SAMPLEI;
run;
%end;

proc datasets library=work NOPRINT;
	delete SAMPLEI;
run;
%mend;

PROC PRINT DATA=FINAL;
RUN;

%samples(dataset=IVFSubset, ns=100, n=253);
proc means data = FINAL mean NOPRINT;
	var BW;
	by sampleno;
	output out=MEANSBW mean=BW_MEAN;
run;

proc univariate data=MEANSBW;
hist BW_MEAN / normal;
RUN;
% mend;

* Part d;
/* Using a the t-test we judge whether an observed effect size, ¯y − 3200, is significantly
large taking in to account the variability in the shape of the standard error. Using
a sample with 3 identical copies of every observed value will not change the effect size,
while the standard error decreases with a factor 1 / sqrt(3). As a result, the p-value will be
smaller and H0 (if false) will be rejected faster.  */