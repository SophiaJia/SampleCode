libname tmp "H:\Projects\P_Patrick C Ma\Repeat biopsy";

Data D;
set tmp.data;
run;
proc contents data = D;run;

Data D_o;
set tmp.data;
run;
proc contents data = D; run;

Data D_o;
set Redcap;
run;

** Checks;
*** Tstage with M stage - if M1a or M1b should mostly be T3 or T4;
proc freq data = D_o;
   tables t_stage * m_stage / nopercent norow;
run;

*** select the date variables ;
proc sql;
create table tmp as
select d_tx_1 , d_diag, d_bio_1 
from D_o;

TITLE “Unformatted Constants”; 
PROC PRINT DATA = tmp; 
VAR d_tx_1  d_diag d_bio_1; 
run;

*** Date of initial diagnosis >= date of diagnosis  & date of first biopsy
**** Transform char to date;
data D_o;
set D_o;
a = scan(d_tx_1,1, "/");
b = scan(d_tx_1,2, "/");
c = scan(d_tx_1,3, "/");
D_tx_date = put(mdy(a,b,c),date9.) ;
initial_dt = input(D_tx_date,date9.);
format initial_dt YYMMDD10.;
run;

PROC PRINT data = D_o;
WHERE d_diag ge initial_dt and d_diag ^= initial_dt;
RUN;
PROC PRINT data = D_o;
WHERE d_bio_1 ge initial_dt and d_bio_1 ^= initial_dt;
RUN;

proc sql;
select  d_diag, d_tx_1, initial_dt
from D_o
where d_diag ge initial_dt and d_diag ^= initial_dt;
proc sql;
select  d_bio_1, d_tx_1, initial_dt
from D_o
where d_bio_1 ge initial_dt and d_bio_1 ^= initial_dt;


**** Age ;
data D_o;
set D_o;
age_dx = ( d_diag - dob ) / 365.25;
run;

proc sql;
select age_dx , record_id
from D_o
where  age_dx < 18.001 or age_dx > 89.999;

**** Date of first biopsy is within +- 30 days of initial diagnosis ;

proc sql;
select ( d_bio_1 - d_diag ) as D_diff, record_id
from D_o
where ( d_bio_1 - d_diag ) > 30.0001 or ( d_bio_1 - d_diag ) < - 29.9999 ;

**** progression date > = date of diagnosis , date first biopsy, initial treatmen;

data D_o;
set D_o;
ag = scan(d_prog,1, "/");
bg = scan(d_prog,2, "/");
cg = scan(d_prog,3, "/");
D_tx_date = put(mdy(ag,bg,cg),date9.) ;
prog_dt = input(D_tx_date,date9.);
format prog_dt YYMMDD10.;
run;
/*
PROC PRINT data = D_o;
WHERE initial_dt ge prog_dt and initial_dt ^= prog_dt;
RUN;

PROC PRINT data = D_o;
WHERE d_diag ge prog_dt and d_diag ^= prog_dt;
RUN;

PROC PRINT data = D_o;
WHERE d_bio_1 ge prog_dt and d_bio_1 ^= prog_dt;
RUN;
*/

proc sql;
select initial_dt,d_diag, d_bio_1,prog_dt, record_id
from D_o
where initial_dt ge prog_dt and initial_dt ^= prog_dt;

proc sql;
select d_diag, prog_dt, record_id
from D_o
where d_diag ge prog_dt and d_diag ^= prog_dt;

proc sql;
select d_bio_1, prog_dt, record_id
from D_o
where d_bio_1 ge prog_dt and d_bio_1 ^= prog_dt;

**** initial treatment and initial chemo; 

proc sql ;
select tx_1___01, chemo_1___01, chemo_1___02, chemo_1___03, chemo_1___04, chemo_1___05, chemo_1___06, chemo_1___07, chemo_1___09, chemo_1___10,  chemo_1___99, record_id
from D
where tx_1___01 ^= ( sum ( chemo_1___01, chemo_1___02, chemo_1___03, chemo_1___04, chemo_1___05, chemo_1___06, chemo_1___07, chemo_1___09, chemo_1___10,  chemo_1___99 ) > 0 ) * 1;

proc sql ;
select p_tx___01, record_id
from D
where p_tx___01 ^= ( sum ( chemo_2___01, chemo_2___02, chemo_2___03, chemo_2___04, chemo_2___05, chemo_2___06, chemo_2___07, chemo_2___09, chemo_2___10,  chemo_2___99 ) > 0 ) * 1;

proc sql ;
select tx_n___01, record_id
from D
where tx_n___01 ^= ( sum ( tx_n_c___01, tx_n_c___02, tx_n_c___03, tx_n_c___04, tx_n_c___05, tx_n_c___06, tx_n_c___07, tx_n_c___09, tx_n_c___10,  tx_n_c___99 ) > 0 ) * 1;


**** number rof repeat biopies ;

proc sql;
select n_bio, d_r_bio , d_r_bio2, d_r_bio3 ,d_r_bio4, record_id 
from D
where n_bio ^= (d_r_bio > 1) + (d_r_bio2 > 1 ) + (d_r_bio3 > 1) + (d_r_bio4 > 1 ) ; 

**** data of first bi0psy  larger than the initial binpsy ;
proc sql;
select  d_bio_1 , d_r_bio,record_id 
from D
where d_r_bio < d_bio_1; 
**** Bronchoscopy vs type;
data D;
set D; 
tmp_other = (r_bio_o ^= " " ) * 1 ;
tmp_sum = sum (bronc_1___01 , bronc_1___02 , bronc_1___03 ,  bronc_1___04 ,  bronc_1___05 , bronc_1___06 );

tmp_sum2 = sum (bronc_2___01 , bronc_2___02 , bronc_2___03 ,  bronc_2___04 ,  bronc_2___05 , bronc_2___06 );

tmp_sum3 = sum (bronc_3___01 , bronc_3___02 , bronc_3___03 ,  bronc_3___04 ,  bronc_3___05 , bronc_3___06 );

tmp_sum4 = sum (bronc_4___01 , bronc_4___02 , bronc_4___03 ,  bronc_4___04 ,  bronc_4___05 , bronc_4___06 );

run;

proc sql;
select r_bio_proc , bronc_1___01 , bronc_1___02 , bronc_1___03 ,  bronc_1___04 ,  bronc_1___05 , bronc_1___06 ,tmp_sum, record_id 
from D
where (r_bio_proc = 2 ) * 1 ^= (tmp_sum > 0 )* 1;
/*
proc sql;
select r_bio_proc , bronc_2___01 , bronc_2___02 , bronc_2___03 ,  bronc_2___04 ,  bronc_2___05 , bronc_2___06 ,tmp_sum, record_id 
from D
where (r_bio_proc2 = 2 ) * 1 ^= (tmp_sum2 > 0 )* 1;

proc sql;
select r_bio_proc , bronc_3___01 , bronc_3___02 , bronc_3___03 ,  bronc_3___04 ,  bronc_3___05 , bronc_3___06 ,tmp_sum, record_id 
from D
where (r_bio_proc3 = 2 ) * 1 ^= (tmp_sum3 > 0 )* 1;

proc sql;
select r_bio_proc , bronc_4___01 , bronc_4___02 , bronc_4___03 ,  bronc_4___04 ,  bronc_4___05 , bronc_4___06 ,tmp_sum, record_id 
from D
where (r_bio_proc4 = 2 ) * 1 ^= (tmp_sum4 > 0 )* 1;

*/

* 1st repeat biospy and PD;
data D;
set D;
tmp_sum = sum (r_bio_rx , r_bio_rx2 , r_bio_rx3 , r_bio_rx4);
run;

proc sql;
select r_bio_rx,  r_bio_ex, record_id 
from D
where ( r_bio_rx = 0 )* 1 ^= (r_bio_ex = " " )* 1 ;
*where r_bio_rx = 1 ;

proc sql;
select d_prog, r_bio_rx, r_bio_rx2, r_bio_rx3, r_bio_rx4, tmp_sum, record_id 
from D
where ( d_prog = " " ) * 1 ^= (tmp_sum > 0 ) * 1 ;  

proc sql;
select d_prog, r_bio_rx, r_bio_rx2, r_bio_rx3, r_bio_rx4, tmp_sum, record_id 
from D
where ( d_prog = " " ) * 1 ^= (r_bio_rx = 1 ) * 1 ;  

proc sql;
select d_prog, r_bio_rx, r_bio_rx2, r_bio_rx3, r_bio_rx4, tmp_sum, record_id 
from D
where d_prog = " "  ; 

****safety at lease one response have been selected;
proc sql;
select r_bio_safe___01, r_bio_safe___02, r_bio_safe___03, r_bio_safe___04, r_bio_safe___05, r_bio_safe___06, r_bio_safe___07, r_bio_safe___99, record_id 
from D
where sum (r_bio_safe___01, r_bio_safe___02, r_bio_safe___03, r_bio_safe___04, r_bio_safe___05, r_bio_safe___06, r_bio_safe___07, r_bio_safe___99) = 0; 

**** what is the difference between  adqm_1 and adqh_1;
proc sql;
select adqm_1, adqh_1, record_id 
from D;

****;
proc sql;
select hist_2 , d_prog, r_bio_rx , r_bio_rx2 , r_bio_rx3 , r_bio_rx4 , record_id 
from D;

**** date of repeat biopsy;
proc sql;
select n_bio, d_r_bio , d_r_bio2, d_r_bio3 ,d_r_bio4, record_id
from D
where d_r_bio > d_r_bio2 or d_r_bio2 > d_r_bio3 or d_r_bio3 >d_r_bio4 ; 

**** biopsy sample adequate for molecular diagnosis  and EGFR
ALK
KRAS
other ;
proc sql; 
select r_bio_adqm,  egfr_1, alk_1, kras_1 ,oth_1 , record_id
from D_o
where ( r_bio_adqm = 9 ) * 1 + ( r_bio_adqm = 0 ) * 1 ^=   (((egfr_1 =99) + (alk_1 =99) + (kras_1 =99) + (oth_1 =0) ) = 4) * 1  ;

****page 8;
proc sql;
create table D_tmp as 
select *
from D
where imp = 1;
* number = 30;

proc sql;
select hist, hist_o, hist_2,hist_2_o, hist_3, hist_3_o, hist_4,hist_5,record_id
from D_tmp;

*egfr_1, alk_1, kras_1;
proc sql; 
select kras_d, kras_1, kras_2, kras_3 ,kras_4 , record_id
from D_tmp
where record_id = "053" or record_id = "072" or record_id = "090";

proc sql;
select hist, hist_o, hist_2,hist_2_o, hist_3, hist_3_o, hist_4,hist_5,record_id
from D_tmp; 

proc sql; 
select kras_d, kras_1, kras_2, kras_3 ,kras_4 , record_id
from D;
where record_id = "177" ;

proc sql; 
select  alk_d,  alk_1,  alk_2,  alk_3 , alk_4 , record_id
from D;
where record_id = "177" ;

proc sql; 
select egfr_d, egfr_1, egfr_2, egfr_3 ,egfr_4 , record_id
from D;
where record_id = "177" ;

*n_bio, d_r_bio , d_r_bio2, d_r_bio3 ,d_r_bio4;

data D;
set D;
ag = scan(d_prog,1, "/");
bg = scan(d_prog,2, "/");
cg = scan(d_prog,3, "/");
D_tx_date = put(mdy(ag,bg,cg),date9.) ;
prog_dt = input(D_tx_date,date9.);
format prog_dt YYMMDD10.;
run;

Data D;
set D;

if r_bio_rx = 0 then bio_tmp = 1;
else if r_bio_rx2  = 0 then bio_tmp = 2;
else if r_bio_rx3  = 0 then bio_tmp = 3;
else if r_bio_rx4  = 0 then bio_tmp = 4;

tmp = prog_dt;
tmp1 = d_r_bio;
bio_d_tmp1 =  tmp - tmp1;
bio_d_tmp2 =  prog_dt - d_r_bio2 ;
bio_d_tmp3 =  prog_dt - d_r_bio3 ;
bio_d_tmp4 =  prog_dt - d_r_bio4 ;

bio_D_tmp =  ( bio_d_tmp4 > 0 )* 1 + ( bio_d_tmp3 > 0 ) * 1  + ( bio_d_tmp2 > 0 )* 1 + ( bio_d_tmp1 > 0 ) * 2 + ( bio_d_tmp1 < 0 ) * 1 +( bio_d_tmp1 = 0 ) * 1;
run;


proc sql;
select bio_tmp , bio_D_tmp , r_bio_rx,r_bio_rx2,r_bio_rx3, r_bio_rx4, bio_d_tmp1 ,  bio_d_tmp2,  bio_d_tmp3,  bio_d_tmp4,  n_bio, d_r_bio , d_r_bio2, d_r_bio3 ,d_r_bio4, d_prog,  record_id 
from D
where bio_tmp ^= bio_D_tmp and bio_tmp ^ = 0;

proc freq data= D;
tables bio_D_tmp ;
run;

proc sql;
select bio_tmp , bio_D_tmp , r_bio_rx,r_bio_rx2,r_bio_rx3,  bio_d_tmp1 ,  bio_d_tmp2,  bio_d_tmp3,  d_r_bio , d_r_bio2, d_r_bio3 , d_r_bio4, d_prog,  record_id 
from D
where bio_tmp < bio_D_tmp and bio_tmp ^= 0;

proc sql;
select bio_tmp , bio_D_tmp , r_bio_rx,r_bio_rx2,r_bio_rx3, d_bio_1, d_r_bio , d_r_bio2, d_r_bio3 , d_r_bio4, d_prog,  record_id 
from D
where bio_tmp < bio_D_tmp and d_prog ^= " " ; and bio_tmp ^= 0;

proc sql;
select bio_tmp , bio_D_tmp , r_bio_rx,r_bio_rx2,r_bio_rx3,  d_r_bio , d_r_bio2, d_r_bio3 , d_r_bio4, d_prog,  record_id 
from D
where bio_tmp > bio_D_tmp ;

****;
Proc sql; 
select hist_2, d_prog,  r_bio_rx,r_bio_rx2,r_bio_rx3 , r_bio_rx4, record_id 
from D 
where d_prog = " " ;
