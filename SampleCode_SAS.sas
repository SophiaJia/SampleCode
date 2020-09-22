// SAS sample code of analysis, Macro and outputs for basic survival analysis.


/*------------------------------------------------------------------*
// Analysis

data D;
set D;
age_d = (age > 70) + (age > 60) + (age > 50) + (age > 40) + 1;
run; 
%summtable (data = D, cat2 = age_d , class = isup_m_new , rtfout = map_6mg1);
%summtable (data = D, cat2 = age_d , class = FurhmanGrade_new , rtfout = map_6mg2);

%outsummtable (rtfin =  map_6mg1 map_6mg2,
              rtffile= "&basedir.\Table1_age.doc" );


%let BASEDIR = H:\Projects\P_Christopher Przybycin\New Grading (RCC)\1th;
%include "Z:\rlopez\SASmacros\summtable.sas" ;
 %include "Z:\rlopez\SASmacros\outsummtable.sas" ;
%summtable (data = D, con2 = age tumor_size , class = isup_m_new, rtfout = map_6mg1);
%summtable (data = D, cat2 = Sex stage_con_1 Necrosis_new Necrosis_new_gep10  SarcDiff tumor_size10_1 ccprcc_like_1Y rn_invasion_1Y PerinephFat_m mononuc_cell_infiltrate_m1 adrenal_m MicroVascInv , class = isup_m_new , rtfout = map_6mg2);

%outsummtable (rtfin =  map_6mg1 map_6mg2,
              rtffile= "&basedir.\Table1_ISUS.doc" );


%summtable (data = D, con2 = age tumor_size , class = FurhmanGrade_new, rtfout = map_6mg4);
%summtable (data = D, cat2 = Sex stage_con_1 Necrosis_new Necrosis_new_gep10 SarcDiff tumor_size10_1 ccprcc_like_1Y rn_invasion_1Y PerinephFat_m mononuc_cell_infiltrate_m1 adrenal_m MicroVascInv , class = FurhmanGrade_new, rtfout = map_6mg5);

%outsummtable (rtfin =  map_6mg4 map_6mg5,
              rtffile= "&basedir.\Table1_F.doc" );

**************additional survival ; 
%include   "C:\Users\jiax\Documents\GitHub\-SAS--Macros\survival_macro_uni.sas" ;
%surt_cat(data=D, var = Sex,            survtime = surdate, scensor = rcensor, sout=rs1);
%surt_cat(data=D, var = stage_con_1,    survtime = surdate, scensor = rcensor, sout=rs2);
%surt_cat(data=D, var = Necrosis_new,   survtime = surdate, scensor = rcensor, sout=rs3);
%surt_cat(data=D, var = SarcDiff,       survtime = surdate, scensor = rcensor, sout=rs4);
%surt_cat(data=D, var = tumor_size10_1, survtime = surdate, scensor = rcensor, sout=rs5);
%surt_cat(data=D, var = ccprcc_like_1Y, survtime = surdate, scensor = rcensor, sout=rs6);
%surt_cat(data=D, var = rn_invasion_1Y, survtime = surdate, scensor = rcensor, sout=rs7);
%surt_cat(data=D, var = PerinephFat_m,  survtime = surdate, scensor = rcensor, sout=rs8);
%surt_cat(data=D, var = mononuc_cell_infiltrate_m1, survtime = surdate, scensor = rcensor, sout=rs9);
%surt_cat(data=D, var = adrenal_m,      survtime = surdate, scensor = rcensor, sout=rs10);
%surt_cat(data=D, var = MicroVascInv,   survtime = surdate, scensor = rcensor, sout=rs11);
data  Sout_all_r;
set rs1 - rs11;
run;

%include "C:\Users\jiax\Documents\GitHub-SAS--Macros\survival_macro_multy.sas" ;
%surt(data = D, var = FurhmanGrade_new Sex stage_con_1 Necrosis_new SarcDiff tumor_size10_1 rn_invasion_1Y PerinephFat_m mononuc_cell_infiltrate_m1 MicroVascInv  , survtime = surdate, scensor = rcensor, sout=s1);
%surt(data = D, var = isup_m_new Sex stage_con_1 Necrosis_new SarcDiff tumor_size10_1 rn_invasion_1Y PerinephFat_m mononuc_cell_infiltrate_m1 MicroVascInv  , survtime = surdate, scensor = rcensor, sout=s2);

*output variables for input to stree ;
data _null_;
set D;
file 'H:\Projects\P_Christopher Przybycin\New Grading (RCC)\1th\stree_data_all.txt';
if _n_ = 1 then put @1 ' c     r    o   o   o   o o   o   o   o o   o   o   o ';
if isup_m_new ne . and FurhmanGrade_new ne . then put @1 censor_m 2.  +2  surdate 5.1 +2 isup_m_new 3.  +2 FurhmanGrade_new +2 Necrosis_new  +2 Necrosis_new_gep10 Sex stage_con_1 SarcDiff tumor_size10_1 rn_invasion_1Y PerinephFat_m mononuc_cell_infiltrate_m1 MicroVascInv;
run;


/*------------------------------------------------------------------*

// MACRO 

/*------------------------------------------------------------------*
   | MACRO NAME  : surv_uni
   | SHORT DESC  : General survival statistics for univariable analysis
   |               Table and data format are ready for bind or output by 
   |               calling %survival_macro_uni.sas;
   | VERSION     : 1.0
   *------------------------------------------------------------------*
   | CREATED BY  : Sophia Jia                             (07/13/2014)
   *------------------------------------------------------------------*
   | PURPOSE
   |
   | This macro will calculate the general survival statistics (Total 
   | number of patients by group, Number of deaths, Median estimated 
   | survival time, HR, and p(t)
   |
   *------------------------------------------------------------------*
   | MODIFIED BY : Name                                          (Date)
   |
   | Version;  Detail 
   *------------------------------------------------------------------*  
   | MACRO CALL
   |
   | %surv_uni(
   |            data = ,
   |            var = ,
   |            survtime = ,
   |            scensor = ,
   |            sout = ,
   |          );
   *------------------------------------------------------------------*
   | REQUIRED PARAMETERS
   |
   | Name      : var
   | Default   :
   | Type      : Variable Name (Single)
   | Purpose   : Group variable which are using for the analysis 
   |
   | Name      : survtime
   | Default   :
   | Type      : Variable Name (Single)
   | Purpose   : Variable containing time to event or last follow-up
   |             in any units
   |
   | Name      : scensor
   | Default   :
   | Type      : Variable Name (Single)
   | Purpose   : Event variable as a numeric two-valued variable (0,1). 
   |             The event value is coded as 1 and the censoring value 
   |             is coded as 0
   |
   | Name      : sout
   | Default   :
   | Type      : Dataset Name
   | Purpose   : Output dataset name
   |
   *------------------------------------------------------------------*
   | OPTIONAL PARAMETERS
   |
   | Name      : data
   | Default   : _LAST_
   | Type      : Dataset Name
   | Purpose   : Input dataset name (Default is the last dataset created)
   |
   *------------------------------------------------------------------*
 | ADDITIONAL NOTES
   |
   |
   |  1.  Under developing 
   |       a. multiply variables input
   |
   |
   |
   *------------------------------------------------------------------*
   | EXAMPLES
   |
   |
   | %surv_uni(data = D, var = d28_lt500, survtime = surv_from_ind, 
   |           scensor = scensor, sout = ss3);
   *------------------------------------------------------------------*/

%macro surv_uni(data = _LAST_, var=, survtime = , scensor = , sout = );

*Variables Checking;
%if &survtime =  %then %do;
   %put  ERROR: Variable <survtime> not defined;
   %LET  errorflg = 1;
   %end;

%if &scensor =  %then %do;
   %put  ERROR: Variable <scensor> not defined;
   %LET  errorflg = 1;
   %end;

*survival;
proc lifetest data=&data ;
  time &survtime * &scensor(0);
  strata &var;
  ods output Quartiles =_Median(where=(Percent=50));
  ods output CensoredSummary =_deathN;
  ods output HomTests =_Pv(where=(Test="Log-Rank"));
run;

data _deathN;
	set _deathN;
	Stratum1=put(Stratum,1.);
run;

proc sql;
	delete *
	from _deathN
	where Stratum1="T"
	;

proc sql;
	create table _sout as
	select _Median.&var as var1, _DeathN.Total as Total, _DeathN.Failed as Death, 100-_DeathN.PctCens as Pcent, _Median.Estimate as EMtime
	from _Median, _DeathN
	where _Median.STRATUM = _deathN.Stratum
	;

data _pv;
	length factor $20.;
	set _pv;
	Factor="&var";
run;

data _Surv_logR;
	merge _sout _Pv;
	drop Test ChiSq DF;
run;


**** HR and p-value;
proc phreg data=&data;
      model &survtime * &scensor(0)=&var /risklimits;
	  ods output ParameterEstimates=_PE;
run;

data _HR_out;
	length Parameter $20.;
	set _PE;
	HR=put(HazardRatio, 4.2)||"  ("||put(HRLowerCL,4.2)||","||put(HRUpperCL,4.2)||")";
	pvalue=ProbChiSq;
	keep Parameter HR pvalue;
run;

data &sout;
	merge _Surv_logR _HR_out;
run;

data &sout;
	set &sout;
	EMtime_2 = put(EMtime, 4.2);
	ND = put(Death,4.0)||"( "||put(Pcent, 4.2)||"% )";
run;

proc datasets library=work;
	delete _deathn _hr_out _median  _pe _pv _surv_logr _sout;
run;

%mend;

   *------------------------------------------------------------------*
// output

proc format;
   picture Pvaluef (round)
           0.985   -   high    = "0.99"    (NoEdit)
           0.10    -<  0.985   = "9.99"
           0.001   -<  0.10    = "9.999"
           0       -<  0.001   = "<0.001"  (NoEdit)
		    . = " ";
run;
ods rtf file="&basedir.\Table6_Uni_survival.doc" style=journal bodytitle;
proc report data=  Sout_all nowd
            style(report)={borderwidth=3 bordercolor=black cellpadding=3
                           font_size=11pt font_face=Times  FONTSTYLE= ROMAN}

            style(lines)={background=white foreground=black
                          font_size=9pt font_face=Times FONTSTYLE= ROMAN
                          protectspecialchars=off}

            style(column)={background=white foreground=black
                          font_size=11pt font_face=Times FONTSTYLE= ROMAN
                          font_weight=medium}

            style(header)={background=white foreground=black borderbottomstyle=double
                          font_weight=bold FONTSTYLE= ROMAN
                          font_size=11pt font_face=Times};
            column Parameter var1 Total ND EMtime_2 HR pvalue;
            

            ***** Title *****;
            compute before _PAGE_ /style = {font_size=11pt font_face=Times
                                  FONTSTYLE=ROMAN font_weight=bold
                                  just=left borderbottomwidth=3
                                  borderbottomcolor=black bordertopcolor=white};
                line "Table 6: survival compasion";
            endcomp;

            ***** Variable name column *****;
            
            define Parameter /"Factor"
                               style(header) = {just = left}
                               style(column) = {cellwidth = 1.5in font_weight=bold just = left};

            define var1/""
                               style(header) = {just = left}
                               style(column) = {cellwidth = 0.3in just = left};

            define Total/"Total" 
                               style(header) = {just = center}
                               style(column) = {cellwidth = 0.3in just = center};
            define ND/"Death" 
                               style(header) = {just = center}
                               style(column) = {cellwidth = 0.8in just = center};

            define EMtime_2/"Estimated Median Survival(Year)"
                               style(header) = {just = center}
                               style(column) = {cellwidth = 0.8in just = center};

            define HR/"Hazard Ratio (95% CI)"
                               style(header) = {just = left}
                               style(column) = {cellwidth = 1.5in just = left};

            define pvalue/"P-value" format=Pvaluef.
                               style(header) = {just = center}
                               style(column) = {cellwidth = 0.6in just = center};
run;
ods rtf close;
       ***** END MY PROC REPORT *****;

