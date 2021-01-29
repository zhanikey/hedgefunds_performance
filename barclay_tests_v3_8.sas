libname bc "...";

/* all strategies ROR dataset */

proc import datafile = '...\allstrat.xlsx'
 out = bc.allstrat
 dbms = xlsx
 replace;
run;

proc npar1way data=bc.allstrat;
   class strategy;
   var ror;
run;

/* event driven strategy dataset */

proc import datafile = '...\eventdriven.xlsx'
 out = bc.event
 dbms = xlsx
 replace;
run; 

/* during crise */

data bc.event_crise;
set bc.event;
if put(date,yymmn6. -l) > '200705' and put(date,yymmn6. -l) < '200904';
run;

/* beyond crise */

data bc.event_beyond;
set bc.event;
if put(date,yymmn6. -l) > '200903' and put(date,yymmn6. -l) < '202002';
run;

/* during covid */

data bc.event_covid;
set bc.event;
if put(date,yymmn6. -l) > '202001';
run;

title "during crise";
proc univariate data=bc.event_crise normaltest;
   var ROR;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title "beyond crise";
proc univariate data=bc.event_beyond normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title "covid crise";
proc univariate data=bc.event_covid normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title; 

/* S&P CRISIS */

title "all time";
proc univariate data=bc.event normaltest;
   var benchmark_SP;
   label benchmark_SP = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title "during crise";
proc univariate data=bc.event_crise normaltest;
   var benchmark_SP;
   label benchmark_SP = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title "beyond crise";
proc univariate data=bc.event_beyond normaltest;
   var benchmark_SP;
   label benchmark_SP = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title "covid crise";
proc univariate data=bc.event_covid normaltest;
   var benchmark_SP;
   label benchmark_SP = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title;

/* DJIA CRISIS */

title "all time";
proc univariate data=bc.event normaltest;
   var benchmark_DJ;
   label benchmark_DJ = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title "during crise";
proc univariate data=bc.event_crise normaltest;
   var benchmark_DJ;
   label benchmark_DJ = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title "beyond crise";
proc univariate data=bc.event_beyond normaltest;
   var benchmark_DJ;
   label benchmark_DJ = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title "covid crise";
proc univariate data=bc.event_covid normaltest;
   var benchmark_DJ;
   label benchmark_DJ = 'benchmark_SP';
   inset  mean std / format=6.4;
run;

title;

/* general test*/ 

proc univariate data=bc.event normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

/* runs event driven */

proc sgplot data=bc.event;
scatter x=Date y=ROR;
run;

proc stdize data=bc.event out=bc.event_norm method=median;
      var ROR;
   run;

data bc.runcount_event;
       	set bc.event_norm nobs=nobs;
        if ROR=0 then delete;
        if ROR>0 then n+1;
        if ROR<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(ROR);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( ROR > 0 );
          prevneg=( previous < 0 );
          currneg=( ROR < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_event;
          set bc.runcount_event end=last;
          if last;
        run;

data bc.waldwolf_event;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_event;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
        pvalue=2*(1-probnorm(abs(Z)));
        pvalue_one=(1-probnorm(abs(Z)));
        run;
      
     title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_event label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

/* runs event driven relative */

/* S&P relative */

proc sgplot data=bc.event;
scatter x=Date y=RROR_SP;
run;

proc stdize data=bc.event_norm out=bc.event_rel_norm method=median;
var RROR_SP;
run;

data bc.runcount_event_rel_SP;
       	set bc.event_rel_norm nobs=nobs;
        if RROR_SP=0 then delete;
        if RROR_SP>0 then n+1;
        if RROR_SP<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(RROR_SP);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( RROR_SP > 0 );
          prevneg=( previous < 0 );
          currneg=( RROR_SP < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_event_rel_SP;
          set bc.runcount_event_rel_SP end=last;
          if last;
        run;

data bc.waldwolf_event_rel_SP;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_event_rel_SP;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
      
        pvalue=2*(1-probnorm(abs(Z)));
		pvalue_one=(1-probnorm(abs(Z)));
        run;
      
     title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_event_rel_SP label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

/* Dow Jones relative */

proc sgplot data=bc.event;
scatter x=Date y=RROR_DJ;
run;

proc stdize data=bc.event_rel_norm out=bc.event_rel_norm method=median;
var RROR_DJ;
run;

data bc.runcount_event_rel_DJ;
       	set bc.event_rel_norm nobs=nobs;
        if RROR_DJ=0 then delete;
        if RROR_DJ>0 then n+1;
        if RROR_DJ<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(RROR_DJ);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( RROR_DJ > 0 );
          prevneg=( previous < 0 );
          currneg=( RROR_DJ < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_event_rel_DJ;
          set bc.runcount_event_rel_DJ end=last;
          if last;
        run;

data bc.waldwolf_event_rel_DJ;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_event_rel_DJ;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
      
        pvalue=2*(1-probnorm(abs(Z)));
		pvalue_one=(1-probnorm(abs(Z)));
        run;
      
      title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_event_rel_DJ label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

title;

/* ARIMA autocorrelation tests event driven strategy */

proc sgplot data=bc.event_norm;
scatter y=ROR x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.event_norm;
   identify var=ROR scan;
run;

proc arima data=bc.event_norm;
identify var=ROR stationarity=(adf=(1,2,3,4));/*en niveau*/
run;

/* residuals are autocorrelated */

/* ARIMA autocorrelation tests event driven relative strategy */

/* S&P Relative */

proc sgplot data=bc.event_rel_norm;
scatter y=RROR_SP x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.event_rel_norm;
   identify var=RROR_SP scan;
run;

proc arima data=bc.event_rel_norm;
identify var=RROR_SP stationarity=(adf=(1,2,3,4));/*en niveau*/
run;

/* Dow Jones Relative */

proc sgplot data=bc.event_rel_norm;
scatter y=RROR_DJ x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.event_rel_norm;
   identify var=RROR_DJ scan;
run;

/* Goodness of fit - Normality*/

/* event driven */

*/title 'ROR Analysis';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.event_norm normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

/* event driven relative */

/* S&P Relative */

/*title 'RROR Analysis S&P';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.event_rel_norm normaltest;
   var RROR_SP;
   probplot RROR_SP/ normal(mu=est sigma=est)
                        square;
   label RROR_SP = 'RROR S&P';
   inset  mean std / format=6.4;
run;

/* Dow Jones Relative */

/*title 'RROR Analysis S&P';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.event_rel_norm normaltest;
   var RROR_DJ;
   probplot RROR_DJ/ normal(mu=est sigma=est)
                        square;
   label RROR_DJ = 'RROR Dow Jones';
   inset  mean std / format=6.4;
run;

/* Sortino Stats */

/* Below Mean */

proc univariate data=bc.event_norm normaltest;
   var Sortino_BM;
   probplot Sortino_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.event_norm normaltest;
   var Sortino_BZ;
   probplot Sortino_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/*Benchmarks */

/* S&P */

/* Below Mean */

proc univariate data=bc.event_norm normaltest;
   var Sortino_SP_BM;
   probplot Sortino_SP_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_SP_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.event_norm normaltest;
   var Sortino_SP_BZ;
   probplot Sortino_SP_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_SP_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/* Dow Jones */

/* Below Mean */

proc univariate data=bc.event_norm normaltest;
   var Sortino_DJ_BM;
   probplot Sortino_DJ_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_DJ_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.event_norm normaltest;
   var Sortino_DJ_BZ;
   probplot Sortino_DJ_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_DJ_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/* Information Ratio */

/* S&P */

proc univariate data=bc.event_norm normaltest;
   var IR_SP;
   probplot IR_SP/ normal(mu=est sigma=est)
                        square;
   label IR_SP = 'Information Ratio S&P as Benchmark';
   inset  mean std / format=6.4;
run;

/* Dow Jones */

proc univariate data=bc.event_norm normaltest;
   var IR_DJ;
   probplot IR_DJ/ normal(mu=est sigma=est)
                        square;
   label IR_DJ = 'Information Ratio Dow Jones as Benchmark';
   inset  mean std / format=6.4;
run;

/*______________________________________________________________________________________________________________________________________
_______________________________________________________________________________________________________________________________________*/

/* fixed income strategy dataset */

proc import datafile = '...\fixedincome.xlsx'
 out = bc.fixed
 dbms = xlsx
 replace;
run;

/* during crise */

data bc.fixed_crise;
set bc.fixed;
if put(date,yymmn6. -l) > '200705' and put(date,yymmn6. -l) < '200904';
run;

/* beyond crise */

data bc.fixed_beyond;
set bc.fixed;
if put(date,yymmn6. -l) > '200903' and put(date,yymmn6. -l) < '202002';
run;

/* during covid */

data bc.fixed_covid;
set bc.fixed;
if put(date,yymmn6. -l) > '202001';
run;

title "during crise";
proc univariate data=bc.fixed_crise normaltest;
   var ROR;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title "beyond crise";
proc univariate data=bc.fixed_beyond normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title "covid crise";
proc univariate data=bc.fixed_covid normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title; 

/* general test */ 

proc univariate data=bc.fixed normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

/* runs fixed income */

proc sgplot data=bc.fixed;
scatter x=Date y=ROR;
run;

proc stdize data=bc.fixed out=bc.fixed_norm method=median;
      var ROR;
   run;

data bc.runcount_fixed;
       	set bc.fixed_norm nobs=nobs;
        if ROR=0 then delete;
        if ROR>0 then n+1;
        if ROR<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(ROR);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( ROR > 0 );
          prevneg=( previous < 0 );
          currneg=( ROR < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_fixed;
          set bc.runcount_fixed end=last;
          if last;
        run;

data bc.waldwolf_fixed;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_fixed;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
        pvalue=2*(1-probnorm(abs(Z)));
        pvalue_one=(1-probnorm(abs(Z)));
        run;
      
     title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_fixed label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

/* runs fixed income relative */

/* S&P relative */

proc sgplot data=bc.fixed;
scatter x=Date y=RROR_SP;
run;

proc stdize data=bc.fixed out=bc.fixed_rel_norm method=median;
var RROR_SP;
run;

data bc.runcount_fixed_rel_SP;
       	set bc.fixed_rel_norm nobs=nobs;
        if RROR_SP=0 then delete;
        if RROR_SP>0 then n+1;
        if RROR_SP<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(RROR_SP);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( RROR_SP > 0 );
          prevneg=( previous < 0 );
          currneg=( RROR_SP < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_fixed_rel_SP;
          set bc.runcount_fixed_rel_SP end=last;
          if last;
        run;

data bc.waldwolf_fixed_rel_SP;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_fixed_rel_SP;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
      
        pvalue=2*(1-probnorm(abs(Z)));
		pvalue_one=(1-probnorm(abs(Z)));
        run;
      
     title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_event_rel_SP label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

/* Dow Jones relative */

proc sgplot data=bc.fixed;
scatter x=Date y=RROR_DJ;
run;

proc stdize data=bc.fixed_rel_norm out=bc.fixed_rel_norm method=median;
var RROR_DJ;
run;

data bc.runcount_fixed_rel_DJ;
       	set bc.fixed_rel_norm nobs=nobs;
        if RROR_DJ=0 then delete;
        if RROR_DJ>0 then n+1;
        if RROR_DJ<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(RROR_DJ);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( RROR_DJ > 0 );
          prevneg=( previous < 0 );
          currneg=( RROR_DJ < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_fixed_rel_DJ;
          set bc.runcount_fixed_rel_DJ end=last;
          if last;
        run;

data bc.waldwolf_fixed_rel_DJ;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_fixed_rel_DJ;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
      
        pvalue=2*(1-probnorm(abs(Z)));
		pvalue_one=(1-probnorm(abs(Z)));
        run;
      
      title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_fixed_rel_DJ label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

title;

/* ARIMA autocorrelation tests fixed income strategy */

proc sgplot data=bc.fixed_norm;
scatter y=ROR x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.fixed_norm;
   identify var=ROR scan;
run;

proc arima data=bc.fixed_norm;
identify var=ROR stationarity=(adf=(1,2,3,4));/*en niveau*/
run;


/* residuals are autocorrelated */

/* ARIMA autocorrelation tests fixed income relative strategy */

/* S&P Relative */

proc sgplot data=bc.fixed_rel_norm;
scatter y=RROR_SP x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.fixed_rel_norm;
   identify var=RROR_SP scan;
run;

/* Dow Jones Relative */

proc sgplot data=bc.fixed_rel_norm;
scatter y=RROR_DJ x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.fixed_rel_norm;
   identify var=RROR_DJ scan;
run;

/* Goodness of fit - Normality*/

/* fixed income */

*/title 'ROR Analysis';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.fixed_norm normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

/* fixed income relative */

/* S&P Relative */

/*title 'RROR Analysis S&P';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.fixed_rel_norm normaltest;
   var RROR_SP;
   probplot RROR_SP/ normal(mu=est sigma=est)
                        square;
   label RROR_SP = 'RROR S&P';
   inset  mean std / format=6.4;
run;

/* Dow Jones Relative */

/*title 'RROR Analysis S&P';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.fixed_rel_norm normaltest;
   var RROR_DJ;
   probplot RROR_DJ/ normal(mu=est sigma=est)
                        square;
   label RROR_DJ = 'RROR Dow Jones';
   inset  mean std / format=6.4;
run;

/* Sortino Stats */

/* Below Mean */

proc univariate data=bc.fixed_norm normaltest;
   var Sortino_BM;
   probplot Sortino_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.fixed_norm normaltest;
   var Sortino_BZ;
   probplot Sortino_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/*Benchmarks */

/* S&P */

/* Below Mean */

proc univariate data=bc.fixed_norm normaltest;
   var Sortino_SP_BM;
   probplot Sortino_SP_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_SP_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.fixed_norm normaltest;
   var Sortino_SP_BZ;
   probplot Sortino_SP_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_SP_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/* Dow Jones */

/* Below Mean */

proc univariate data=bc.fixed_norm normaltest;
   var Sortino_DJ_BM;
   probplot Sortino_DJ_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_DJ_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.fixed_norm normaltest;
   var Sortino_DJ_BZ;
   probplot Sortino_DJ_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_DJ_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/* Information Ratio */

/* S&P */

proc univariate data=bc.fixed_norm normaltest;
   var IR_SP;
   probplot IR_SP/ normal(mu=est sigma=est)
                        square;
   label IR_SP = 'Information Ratio S&P as Benchmark';
   inset  mean std / format=6.4;
run;

/* Dow Jones */

proc univariate data=bc.fixed_norm normaltest;
   var IR_DJ;
   probplot IR_DJ/ normal(mu=est sigma=est)
                        square;
   label IR_DJ = 'Information Ratio Dow Jones as Benchmark';
   inset  mean std / format=6.4;
run;

/*______________________________________________________________________________________________________________________________________
_______________________________________________________________________________________________________________________________________*/

/* market neutral strategy dataset */

proc import datafile = '...\marketneutral.xlsx'
 out = bc.neutral
 dbms = xlsx
 replace;
run;

/* during crise */

data bc.neutral_crise;
set bc.neutral;
if put(date,yymmn6. -l) > '200705' and put(date,yymmn6. -l) < '200904';
run;

/* beyond crise */

data bc.neutral_beyond;
set bc.neutral;
if put(date,yymmn6. -l) > '200903' and put(date,yymmn6. -l) < '202002';
run;

/* during covid */

data bc.neutral_covid;
set bc.neutral;
if put(date,yymmn6. -l) > '202001';
run;

title "during crise";
proc univariate data=bc.neutral_crise normaltest;
   var ROR;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title "beyond crise";
proc univariate data=bc.neutral_beyond normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title "covid crise";
proc univariate data=bc.neutral_covid normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

title; 

/* general test */ 

proc univariate data=bc.neutral normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

/* runs market neutral */

proc sgplot data=bc.neutral;
scatter x=Date y=ROR;
run;

proc stdize data=bc.neutral out=bc.neutral_norm method=median;
      var ROR;
   run;

data bc.runcount_neutral;
       	set bc.neutral_norm nobs=nobs;
        if ROR=0 then delete;
        if ROR>0 then n+1;
        if ROR<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(ROR);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( ROR > 0 );
          prevneg=( previous < 0 );
          currneg=( ROR < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_neutral;
          set bc.runcount_neutral end=last;
          if last;
        run;

data bc.waldwolf_neutral;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_neutral;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
        pvalue=2*(1-probnorm(abs(Z)));
        pvalue_one=(1-probnorm(abs(Z)));
        run;
      
     title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_neutral label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

/* runs market neutral relative */

/* S&P relative */

proc sgplot data=bc.neutral;
scatter x=Date y=RROR_SP;
run;

proc stdize data=bc.neutral out=bc.neutral_rel_norm method=median;
var RROR_SP;
run;

data bc.runcount_neutral_rel_SP;
       	set bc.neutral_rel_norm nobs=nobs;
        if RROR_SP=0 then delete;
        if RROR_SP>0 then n+1;
        if RROR_SP<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(RROR_SP);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( RROR_SP > 0 );
          prevneg=( previous < 0 );
          currneg=( RROR_SP < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_neutral_rel_SP;
          set bc.runcount_neutral_rel_SP end=last;
          if last;
        run;

data bc.waldwolf_neutral_rel_SP;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_neutral_rel_SP;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
      
        pvalue=2*(1-probnorm(abs(Z)));
		pvalue_one=(1-probnorm(abs(Z)));
        run;
      
     title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_neutral_rel_SP label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

/* Dow Jones relative */

proc sgplot data=bc.neutral;
scatter x=Date y=RROR_DJ;
run;

proc stdize data=bc.neutral_rel_norm out=bc.neutral_rel_norm method=median;
var RROR_DJ;
run;

data bc.runcount_neutral_rel_DJ;
       	set bc.neutral_rel_norm nobs=nobs;
        if RROR_DJ=0 then delete;
        if RROR_DJ>0 then n+1;
        if RROR_DJ<0 then m+1;
        retain runs 0 numpos 0 numneg 0;
        previous=lag(RROR_DJ);

        if _n_=1 then do;
          runs=1;
          prevpos=.;
          currpos=.;
          prevneg=.;
          currneg=.;
          end;
        else do;
          prevpos=( previous > 0 );
          currpos=( RROR_DJ > 0 );
          prevneg=( previous < 0 );
          currneg=( RROR_DJ < 0 );
      
          if _n_=2 and (currpos and prevpos) then numpos+1;
            else if _n_=2 and (currpos and prevneg) then numneg+1;
            else if _n_=2 and (currneg and prevpos) then numpos+1;
            else if _n_=2 and (currneg and prevneg) then numneg+1;
              
	    if currpos and prevneg then do;
            runs+1;
            numpos+1;
            end;
      
          if currneg and prevpos then do;
            runs+1;
            numneg+1;
            end;
        end;
 
        run;
        data bc.runcount_neutral_rel_DJ;
          set bc.runcount_neutral_rel_DJ end=last;
          if last;
        run;

data bc.waldwolf_neutral_rel_DJ;
        label z='Wald-Wolfowitz Z'
              pvalue='Pr > |Z|'
			  pvalue_one='Pr > Z';
        set bc.runcount_fixed_rel_DJ;
        mu = ( (2*n*m) / (n + m) ) + 1;
        sigmasq = ( (2*n*m) * (2*n*m-(n+m)) ) / ( ((n+m)**2) * (n+m-1) );
        sigma=sqrt(sigmasq);
        drop sigmasq;
      
        if N GE 50 then Z = (Runs - mu) / sigma;
        else if Runs-mu LT 0 then Z = (Runs-mu+0.5)/sigma;
          else Z = (Runs-mu-0.5)/sigma;
      
        pvalue=2*(1-probnorm(abs(Z)));
		pvalue_one=(1-probnorm(abs(Z)));
        run;
      
      title  'Wald-Wolfowitz Test for Randomness';
      title2 'H0: The data are random';
      proc print data=bc.waldwolf_neutral_rel_DJ label noobs;
        var Runs n m Sigma Mu z pvalue pvalue_one;
        format pvalue pvalue.;
		format pvalue_one pvalue.;
        run;

title;

/* ARIMA autocorrelation tests market neutral strategy */

proc sgplot data=bc.neutral_norm;
scatter y=ROR x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.neutral_norm;
   identify var=ROR scan;
run;

proc arima data=bc.neutral_norm;
identify var=ROR stationarity=(adf=(1,2,3,4));/*en niveau*/
run;

/* residuals are autocorrelated */

/* ARIMA autocorrelation tests market neutral relative strategy */

/* S&P Relative */

proc sgplot data=bc.neutral_rel_norm;
scatter y=RROR_SP x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.neutral_rel_norm;
   identify var=RROR_SP scan;
run;

/* Dow Jones Relative */

proc sgplot data=bc.neutral_rel_norm;
scatter y=RROR_DJ x=date;
run;

/*-- Order Identification Diagnostic with SCAN Method --*/
proc arima data=bc.neutral_rel_norm;
   identify var=RROR_DJ scan;
run;

/* Goodness of fit - Normality*/

/* fixed income */

*/title 'ROR Analysis';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.neutral_norm normaltest;
   var ROR;
   probplot ROR/ normal(mu=est sigma=est)
                        square;
   label ROR = 'ROR';
   inset  mean std / format=6.4;
run;

/* market neutral relative */

/* S&P Relative */

/*title 'RROR Analysis S&P';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.neutral_rel_norm normaltest;
   var RROR_SP;
   probplot RROR_SP/ normal(mu=est sigma=est)
                        square;
   label RROR_SP = 'RROR S&P';
   inset  mean std / format=6.4;
run;

/* Dow Jones Relative */

/*title 'RROR Analysis S&P';*/
ods graphics on;
ods select Moments TestsForNormality ProbPlot;
proc univariate data=bc.neutral_rel_norm normaltest;
   var RROR_DJ;
   probplot RROR_DJ/ normal(mu=est sigma=est)
                        square;
   label RROR_DJ = 'RROR Dow Jones';
   inset  mean std / format=6.4;
run;

/* Sortino Stats */

/* Below Mean */

proc univariate data=bc.neutral_norm normaltest;
   var Sortino_BM;
   probplot Sortino_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.neutral_norm normaltest;
   var Sortino_BZ;
   probplot Sortino_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/*Benchmarks */

/* S&P */

/* Below Mean */

proc univariate data=bc.neutral_norm normaltest;
   var Sortino_SP_BM;
   probplot Sortino_SP_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_SP_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.neutral_norm normaltest;
   var Sortino_SP_BZ;
   probplot Sortino_SP_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_SP_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/* Dow Jones */

/* Below Mean */

proc univariate data=bc.neutral_norm normaltest;
   var Sortino_DJ_BM;
   probplot Sortino_DJ_BM/ normal(mu=est sigma=est)
                        square;
   label Sortino_DJ_BM = 'Sortino Below Mean';
   inset  mean std / format=6.4;
run;

/* Below Zero */ 

proc univariate data=bc.neutral_norm normaltest;
   var Sortino_DJ_BZ;
   probplot Sortino_DJ_BZ/ normal(mu=est sigma=est)
                        square;
   label Sortino_DJ_BZ = 'Sortino Below Zero';
   inset  mean std / format=6.4;
run;

/* Information Ratio */

/* S&P */

proc univariate data=bc.neutral_norm normaltest;
   var IR_SP;
   probplot IR_SP/ normal(mu=est sigma=est)
                        square;
   label IR_SP = 'Information Ratio S&P as Benchmark';
   inset  mean std / format=6.4;
run;

/* Dow Jones */

proc univariate data=bc.neutral_norm normaltest;
   var IR_DJ;
   probplot IR_DJ/ normal(mu=est sigma=est)
                        square;
   label IR_DJ = 'Information Ratio Dow Jones as Benchmark';
   inset  mean std / format=6.4;
run;
