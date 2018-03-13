/*********************************************
* Roy Pardee
* Group Health Research Institute
* (206) 287-2078
* pardee.r@ghc.org
*
* //groups/data/ctrhs/chs/pardre1/repos/lab/supporting_files/new_qa.sas
*
* Creates graphics by which the health of lab results data can be assessed.
*********************************************/
* ======================= begin edit section ======================= ;
* ======================= begin edit section ======================= ;
* ======================= begin edit section ======================= ;

* If roy forgets to comment this out, please do so.  Thanks/sorry! ;
%include "h:/SAS/Scripts/remoteactivate.sas" ;

options
  linesize  = 150
  msglevel  = i
  formchar  = '|-++++++++++=|-/|<>*'
  dsoptions = note2err
  nocenter
  noovp
  nosqlremerge
  mprint
;

* Please edit this to point to your local standard vars file. ;
%include "&GHRIDW_ROOT/Sasdata/CRN_VDW/lib/StdVars.sas" ;

* Where do you want the output (including interim dsets)? ;
%let out_folder = \\mlt1q0\C$\users\pardre1\Documents\vdw\lab-graphs\sample_output ;
* What style do you want for the output? ;
%let style = magnify ;

* Where will we find the SGD file? ;
%let sgd_file = \\mlt1q0\C$\users\pardre1\Documents\vdw\lab-graphs\lab_graphs.sgd ;

* ======================== end edit section ======================== ;
* ======================== end edit section ======================== ;
* ======================== end edit section ======================== ;

libname out "&out_folder" ;

%macro make_summary(inset = &_vdw_lab, outset = out.result_freqs) ;

  %* Makes a frequency dset of raw lab results by result type, month and value type, and value for use in the graphs. ;
  %* The resulting dset is still pretty big, but it is substantially smaller than raw, and contains some ;
  %* value-added vars used for the graphs. ;

  %local round_to ;
  %let round_to = .1 ;

  proc format ;
    value $mod
      "TX" = "char"
      other = "num"
    ;
  quit ;

  %local keepvars hashvars ;
  %let hashvars = 'test_type', 'result_type', 'lab_date', 'result_unit', 'char_result', 'num_result', 'local_cd' ;
  %let keepvars = test_type result_unit local_cd result_c result_dt lab_dt order_dt ;

  data toss_it ;
    length
      result_type $ 4
      char_result $ 8
    ;

    retain numpat ;

    set &inset (keep = &keepvars) end = alldone ;

    * where lab_source = 'C';

    if _n_ = 1 then do ;

      declare hash result_freqs() ;
      result_freqs.definekey(&hashvars) ;
      result_freqs.definedata(&hashvars., 'cnt') ;
      result_freqs.definedone() ;
      * We figure out numeric-or-not empirically rather than trusting to the MODIFIER var. ;
      numpat  = PRXPARSE("/^((-?\d*\.?\d+)|(-?\d+\.?\d*))\s*$/") ;

    end ;

    if prxmatch(numpat, result_c) then result_type = 'num' ;
    else result_type = 'char' ;

    * We just want *some* date to attach to records.  Truncate to first-of-month for aggregation. ;
    lab_date = intnx('month', coalesce(result_dt, lab_dt, order_dt), 0, 'beg') ;

    result_unit = coalescec(compress(upcase(result_unit)), '?unk?') ;
    label result_unit = 'Result Unit';

    select(result_type) ;
      when('char') char_result = coalescec(result_c, '?nul?') ;

      when('num') do ;
        num_result = input(result_c, ? best.) ;
        if n(num_result) then num_result = round(num_result, &round_to) ;
        else do ;
          * Should never happen, but just in case... ;
          char_result = strip(result_c) ;
          result_type = 'char' ;
        end ;
      end ;

      otherwise ; * <-- do nothing ;
    end ;

    if result_freqs.find() = 0 then do ;
      cnt = cnt + 1 ;
      result_freqs.replace() ;
    end ;
    else do ;
      cnt = 1 ;
      result_freqs.add() ;
    end ;

    if alldone then do ;
      result_freqs.output(dataset: "&outset") ;
    end ;
  run ;

  %removedset(dset = toss_it) ;

%mend make_summary ;

%macro make_rt_dsets(inset = out.result_freqs, tt = ALBUMIN, outlib = work) ;

  /*

    Takes the freq dset created by make_summary() and splits it into the 5
    dsets needed for the graph.
      monthly counts over time
      bars for modifier (so--how many are TX vs numeric) grouped by local_cd
      boxplot of numeric result values by unit
      bars of numeric result Ns by unit, grouped by local_cd
      bars of non-numeric result values
   */

   %local nr ;
   %let nr = sum(cnt) as num_results format = comma9.0 label = "No. of result records" ;

   proc sql ;

    create table gnu as
    select *
    from &inset
    where test_type = "&tt"
    ;

     create table &outlib..mo_counts as
     select lab_date format = mmddyy10., &nr
     from gnu
     group by lab_date
     ;

     create table &outlib..result_types as
     select result_type, local_cd, &nr
     from gnu
     group by result_type, local_cd
     ;

     create table &outlib..numeric_results as
     select num_result, result_unit, &nr
     from gnu
     group by num_result, result_unit
     ;

     create table &outlib..char_results as
     select char_result, local_cd, &nr
     from gnu
     where result_type = 'char'
     group by char_result, local_cd
     ;

     create table &outlib..result_units as
     select result_unit, local_cd, &nr
     from gnu
     where result_type = 'num'
     group by result_unit, local_cd
     ;

   quit ;

    * Add an infinitesimal amount to any 0 results, so the log scaling on the boxplots can work ;
    data &outlib..numeric_results ;
      set &outlib..numeric_results ;
      * if num_result = 0 then num_result = .0000001 ;
      if num_result = 0 then num_result = . ;
    run ;



  *  * For now we expand out the numeric_results dataset b/c it seems impossible to use the FREQ option with the BOXPLOT statement in an SGD file. :(;
  * Wait, no--figured it out! ;
  * data &outlib..numeric_results ;
  *   set &outlib..numeric_results ;
  *   do i = 1 to num_results ;
  *     output ;
  *   end ;
  *   drop i ;
  * run ;

%mend make_rt_dsets ;

%macro make_report(inset = out.result_freqs) ;

  proc sql noprint ;
    ** What test types do we have in our result freqs dset? ;
    select distinct test_type
    into :n1-:n9999
    from &inset
    where test_type in ('WBC', 'TSH', 'TRIGL_NS', 'TOT_PROT', 'TOT_CHOLES', 'K', 'SODIUM', 'ALBUMIN', 'ALT', 'INR', 'ANC')
    ;

    ** How many rows were there? ;
    %let num_rows = &SQLOBS ;

    %do i = 1 %to &num_rows ;
      %let this_one = &&n&i ;
      %put Working on name &this_one ;

      %make_rt_dsets(inset = &inset, tt = &this_one) ;

      ods html text = "<h1>Result Type: &this_one.</h1>" ;
      ods proclabel "&this_one" ;
      ods graphics / imagename = "&this_one" LOESSMAXOBS = 10000 ;
      proc sgdesign sgd = "&sgd_file" description = "&this_one" ;
        dynamic result_type = "&this_one" ;
      run ;
       ;

    %end ;
  quit ;
%mend make_report ;

* %let sumset = out.lab_freqs ;
%let sumset = lab_freqs ;

%make_summary(outset = &sumset) ;

* ods graphics / height = 9in width = 10in ;
 ods html path = "&out_folder" (URL=NONE)
          body   = "new_qa.html"
          contents = "toc.html"
          frame = "lab_graphs.html"
          (title = "Lab Results Graphs")
          style = &style
           ;

ods rtf file = "%sysfunc(pathname(out))./vdw_lab_graphs.rtf" device = sasemf style = &style ;

  %make_report(inset = &sumset) ;

run ;

ods _all_ close ;

/*
  proc template;
  define statgraph sgdesign;
  dynamic _LAB_DATE _NUM_RESULTS _RESULT_UNIT _NUM_RESULT _LOCAL_CD _NUM_RESULTS2 _RESULT_TYPE _NUM_RESULTS3 _RESULT_UNIT3 _LOCAL_CD2 _CHAR_RESULT _NUM_RESULTS4 _LOCAL_CD3;
  dynamic RESULT_TYPE;
  begingraph / designheight=1066 designwidth=1193;
  entrytitle _id='title' halign=center 'Result Type: ' RESULT_TYPE /;
  layout lattice _id='lattice' / column2datarange=data columndatarange=data columngutter=10 columns=2 rowdatarange=data rowgutter=10 rows=3;
           layout overlay _id='overlay' / xaxisopts=(label='Result Date' gridDisplay=on tickvalueattrs=GraphAnnoText) yaxisopts=(gridDisplay=on type=linear);
              loessplot _id='counts_over_time' x=_LAB_DATE y=_NUM_RESULTS / datatransparency=0.0 name='counts_over_time';
              entry _id='entry3' halign=center 'Record Counts over Time' / valign=top;
           endlayout;
           layout overlay _id='overlay3' / xaxisopts=(label='Numeric Result Values' gridDisplay=on) yaxisopts=(gridDisplay=off);
              boxplot _id='boxplot_numeric_values' x=_RESULT_UNIT y=_NUM_RESULT / name='boxplot_numeric_values' orient=horizontal;
              entry _id='entry4' halign=center 'Numeric Value Distributions by Unit' / valign=top;
           endlayout;
           layout overlay _id='overlay2' / yaxisopts=(gridDisplay=on);
              barchart _id='result_types_by_local_cd' x=_LOCAL_CD y=_NUM_RESULTS2 / group=_RESULT_TYPE name='result_types_by_local_cd';
              discretelegend _id='legend2' 'result_types_by_local_cd'  / across=1 border=true displayclipped=true halign=right location=inside opaque=false order=rowmajor valign=center;
              entry _id='entry5' halign=center 'Result Type by Local Code' / valign=top;
           endlayout;
           layout overlay _id='overlay4' / xaxisopts=(label='Result Unit') yaxisopts=(gridDisplay=on);
              barchart _id='units_by_local_cd' x=_RESULT_UNIT3 y=_NUM_RESULTS3 / group=_LOCAL_CD2 name='units_by_local_cd';
              entry _id='entry6' halign=center 'N Numeric Values by Unit' / valign=top;
           endlayout;
           layout overlay _id='overlay5' / xaxisopts=(label='Character Result Value');
              barchart _id='char_values' x=_CHAR_RESULT y=_NUM_RESULTS4 / group=_LOCAL_CD3 name='char_values';
              entry _id='entry7' halign=center 'N of Character Values' / valign=top;
           endlayout;
           layout overlay _id='overlay6' /;
              entry _id='dropsite6' halign=center '(drop a plot here...)' / valign=center;
           endlayout;
           sidebar / align=bottom spacefill=false;
              discretelegend _id='legend5' 'units_by_local_cd' 'char_values'  / border=true displayclipped=true down=2 halign=center opaque=true order=columnmajor valign=bottom;
           endsidebar;
  endlayout;
  endgraph;
  end;
  run;

  The graph uses multiple datasets. This is not supported by PROC SGRENDER. You need to run PROC SGDESIGN to generate the graph.
*/

