/**VERMONT AUTOMOBILE CRASH DATA, 2016**/

*importing as .xlsx to retain variable formats;
proc import datafile = "D:\567\Project 1\VT crash data.xlsx"
out = work.rawdata
dbms = xlsx replace;
run;

*removing records with missing data, extracting date and time from combined variable, dropping unneeded variables;
data work.rawdata (drop = ACCIDENTDATE X Y REPORTINGA ReportingAgency STREETADDRESS INTERSECTIONWITH REPORTNUMB AOTROADWAYGROUPid 
RoadGroup AOTACTUALMILEPOINT NonReportableAddress VCSG_CITYORTOWNid CITYORTOWN EASTING NORTHING VCSG_AOTROUTEid AOTROUTE LRSNUMBER LAT_DD
VCSG_LONGITUDE LONG_DD HOWMAPPED LOC_ERROR RDFLNAME Route VCSG_LATITUDE LOCALID time date daten);
set work.rawdata;
format timen 8. dated weekday1.;
if InjuryType = "" then delete;
if DirOfCollision in ("","Other - Explain in Narrative") then delete;
if RoadCharacteristics in ("","Other - Explain in Narrative") then delete;
if Animal = "" then delete;
if Impairment = "" then delete;
if Involving = "" then delete;
if Weather in ("","Unknown") then delete;
if DayNight = "" then delete;
if SurfaceCondition in ("","Other - Explain in Narrative") then delete;
if RoadCondition in ("","Other - Explain in Narrative","Unknown") then delete;
date = substr(ACCIDENTDATE,1,10); *we just want the month;
time = substr(ACCIDENTDATE,12,2); *we just want the hour;
timen = time; *converting to numeric;
daten = input(date,yymmdd10.);
dated = daten;
run;

*creating a new data set with numeric values;
data work.trdata (drop = InjuryType Weather timen DayNight Impairment dated SurfaceCondition dark objectID /**/);
	set work.rawdata;
	length Outcome 8. wethsev 8. timen 8. dark 8. impair 8. datew 7. surfcond 8.;
	if InjuryType = "Property Damage Only" then Outcome = 0;
	if InjuryType = "Injury" then Outcome = 1;
	if InjuryType = "Fatal" then Outcome = 2;
	if Weather = "Clear" then wethsev = 0;
	if Weather = "Cloudy" then wethsev = 1;
	if Weather = "Wind" then wethsev = 2;
	if Weather = "Rain" then wethsev = 3;
	if Weather = "Freezing Precipitation" then wethsev = 4;
	if timen <= 6 then timeofday = 1;
	if (timen > 6 AND timen <= 12) then timeofday = 2;
	if (timen > 12 AND timen <= 18) then timeofday = 3;
	if timen > 18 then timeofday = 4;
	if DayNight = "Day" then dark = 0;
	if DayNight = "Night" then dark = 1;
	if Impairment = "None" then impair = 0;
	if Impairment in ("Alcohol","Drugs") then impair = 1;
	if Impairment = ("Alcohol and Drugs") then impair = 2;
	if mod(dated,7) = 0 then datew = 6;
	if mod(dated,7) = 6 then datew = 5;
	if mod(dated,7) = 5 then datew = 4;
	if mod(dated,7) = 4 then datew = 3;
	if mod(dated,7) = 3 then datew = 2;
	if mod(dated,7) = 2 then datew = 1;
	if mod(dated,7) = 1 then datew = 7;
	if SurfaceCondition in ("Unknown","Not Reported","") then delete;
	if SurfaceCondition = "Dry" then surfcond = 0;
	if SurfaceCondition = "Sand, mud, dirt, oil,gravel" then surfcond = 1;
	if SurfaceCondition in("Wet","Water (standing /moving)") then surfcond = 2;
	if SurfaceCondition in("Snow","Slush") then surfcond = 3;
	if surfcond = . then delete;
run;

/*PROJECT 1 STUFF*/

*exporting cleaned up data to a new document;

*ods _all_ close;

*ods excel file = "D:\567\Project 1\cleaned up data.xlsx";

*proc report data = work.trdata;
*run;

*ods _all_ close; 


*proc univariate data = work.trdata;
*	histogram / midpoints = 1,2; *change midpoint values here;
*run;

*proc univariate data=work.trdata;
*	class surfcond; *change here;
*	histogram outcome / nrows = 4 /*set this to number of levels*/midpoints = 1,2;
*run;

*proc sgplot data = work.trdata;
*	series x=outcome y=dark;
*run;

*proc corr data = work.trdata;
*	var outcome wethsev surfcond impair datew timeofday;
*run;
*variance-covariance matrix generated using excel;

*Chi-square test for Hypothesis 1;
*proc freq data = work.trdata;
*	tables datew*outcome / expected norow nocol chisq;
*run;

*proc hpprincomp data = work.trdata cov;
*	var outcome wethsev impair datew timeofday surfcond;
*run;

data work.trdata; set work.trdata;
	if outcome = 2 then outcome = 1;
run;

proc logistic data = work.trdata;
	model outcome = wethsev | timeofday | datew | impair | surfcond /selection = stepwise;
run;
	
proc factor data = work.trdata METHOD = prin NFACT = 5 ROTATE = VARIMAX plots = (scree) out = factor1;
var wethsev timeofday datew impair surfcond; 
run;

proc cluster data=work.factor1 method = ward outtree = work.cluster1 noprint;
	var Factor1 Factor2 Factor3 Factor4;
	copy outcome wethsev timeofday datew impair surfcond Factor1 Factor2 Factor3 Factor4;
run;
proc tree nClusters = 5 out = work.treecluster data = work.cluster1 noprint;
	copy outcome wethsev timeofday datew impair surfcond Factor1 Factor2 Factor3 Factor4;
run;
proc sort data = work.treecluster; by CLUSTER; run;
proc means data = work.treecluster;
var outcome;
by cluster;
run;
proc glm data = work.treecluster;
	class CLUSTER;
	model outcome = CLUSTER;
	contrast 'last cluster different'  CLUSTER -1 -1 -1 -1 4;
run;
data work.cluster5; 
	set work.treecluster;
	format sample $char12.;
	sample = "subset";
	if CLUSTER = 5 then output;
run;

/*
proc means data = work.cluster5 noprint; output out = meanscluster5; run;
proc means data = work.treecluster noprint; output out = meanstotal; run;
data work.meanscluster5;
	set work.meanscluster5(keep = _STAT_ Outcome wethsev timeofday datew impair surfcond);
run;
proc transpose data = work.meanscluster5 out = tmeanscluster5; run;
data work.tmeanscluster5; set work.tmeanscluster5;
		sample = "subset";
run;
data work.meanstotal;
	set work.meanstotal(keep = _STAT_ Outcome wethsev timeofday datew impair surfcond);
	sample = "total";
run;
proc transpose data = work.meanstotal out = tmeanstotal; run;
data work.tmeanstotal; set work.tmeanstotal;
		sample = "total";
run;
data datafinal;
	set work.tmeanscluster5 work.tmeanstotal;
run;

proc ttest data = work.datafinal;
	class sample;
run;
*/

data work.treecluster; set work.treecluster;
	format sample $char12.;
	sample = "total";
run;
data work.final (keep = Outcome wethsev timeofday datew impair surfcond sample); 
	set work.treecluster work.cluster5;
run;
proc ttest data = work.final;
	class sample;
run;
