*&---------------------------------------------------------------------*
*& Report  ZPERF_EX_DB
*&---------------------------------------------------------------------*
*
* Runtime Measurements for DB Statements
*
*   Use Result-Overview to see all available testcases.
*
*   Use test_run, if you want to use a SQL Trace
*
* Last change:  2009/11/20
*----------------------------------------------------------------------*
REPORT zperf_db_ex    LINE-SIZE 220.

*----------------------------------------------------------
TYPES:
       tab_dd01l TYPE STANDARD TABLE OF dd01l,
       tab_dd02l TYPE STANDARD TABLE OF dd02l,
       tab_dd03l TYPE STANDARD TABLE OF dd03l,
       tab_dd09l TYPE STANDARD TABLE OF dd09l,
       tab_dd12t TYPE STANDARD TABLE OF dd12t,
       tab_dd25l TYPE STANDARD TABLE OF dd25l,
       tab_dd26s TYPE STANDARD TABLE OF dd26s,
       tab_dd35l TYPE STANDARD TABLE OF dd35l,
       tab_t006  TYPE STANDARD TABLE OF t006,
       wa_dd03l  LIKE dd03l,
       wa_dd02l  LIKE dd02l.

* key structure of dd02l
TYPES:
   BEGIN OF st_key_dd02l,
         tabname  LIKE dd02l-tabname,
         as4local LIKE dd02l-as4local,
         as4vers  LIKE dd02l-as4vers,
   END OF st_key_dd02l.

* key structure of dd03l (69 byte of 293 in all releases)
TYPES:
   BEGIN OF st_key_dd03l,
         tabname   LIKE dd03l-tabname,
         fieldname LIKE dd03l-fieldname,
         as4local  LIKE dd03l-as4local,
         as4vers   LIKE dd03l-as4vers,
         position  LIKE dd03l-position,
   END OF st_key_dd03l.

* key structure of dd03l (69 byte of 293 in all releases)
TYPES:
   BEGIN OF st_70_dd03l,
         tabname   LIKE dd03l-tabname,
         fieldname LIKE dd03l-fieldname,
         as4local  LIKE dd03l-as4local,
         as4vers   LIKE dd03l-as4vers,
         position  LIKE dd03l-position,
         keyflag   LIKE dd03l-keyflag,
   END OF st_70_dd03l.

* st139 structure of dd03l (139 byte of 293 in all releases)
TYPES:
   BEGIN OF st_139_dd03l,
         tabname    LIKE dd03l-tabname,
         fieldname  LIKE dd03l-fieldname,
         as4local   LIKE dd03l-as4local,
         as4vers    LIKE dd03l-as4vers,
         position   LIKE dd03l-position,
         keyflag    LIKE dd03l-keyflag,
         mandatory  LIKE dd03l-mandatory,
         rollname   LIKE dd03l-rollname,
         checktable LIKE dd03l-checktable,
         adminfield LIKE dd03l-adminfield,
         inttype    LIKE dd03l-inttype,
         intlen	    LIKE dd03l-intlen,
   END OF st_139_dd03l.


* st244 structure of dd03l (244 byte of 293 in all releases)
TYPES:
   BEGIN OF st_244_dd03l,
         tabname    LIKE dd03l-tabname,
         fieldname  LIKE dd03l-fieldname,
         as4local   LIKE dd03l-as4local,
         as4vers    LIKE dd03l-as4vers,
         position   LIKE dd03l-position,
         keyflag    LIKE dd03l-keyflag,
         mandatory  LIKE dd03l-mandatory,
         rollname   LIKE dd03l-rollname,
         checktable LIKE dd03l-checktable,
         adminfield LIKE dd03l-adminfield,
         inttype    LIKE dd03l-inttype,
         intlen	    LIKE dd03l-intlen,
         reftable	  LIKE dd03l-reftable,
         precfield  LIKE dd03l-precfield,
         reffield	  LIKE dd03l-reffield,
         conrout    LIKE dd03l-conrout,
         notnull    LIKE dd03l-notnull,
         datatype	  LIKE dd03l-datatype,
   END OF st_244_dd03l.

* st293 structure of dd03l (293 in all releases)
TYPES:
   BEGIN OF st_293_dd03l,
         tabname    LIKE dd03l-tabname,
         fieldname  LIKE dd03l-fieldname,
         as4local   LIKE dd03l-as4local,
         as4vers    LIKE dd03l-as4vers,
         position   LIKE dd03l-position,
         keyflag    LIKE dd03l-keyflag,
         mandatory  LIKE dd03l-mandatory,
         rollname   LIKE dd03l-rollname,
         checktable LIKE dd03l-checktable,
         adminfield LIKE dd03l-adminfield,
         inttype    LIKE dd03l-inttype,
         intlen	    LIKE dd03l-intlen,
         reftable	  LIKE dd03l-reftable,
         precfield  LIKE dd03l-precfield,
         reffield	  LIKE dd03l-reffield,
         conrout    LIKE dd03l-conrout,
         notnull    LIKE dd03l-notnull,
         datatype	  LIKE dd03l-datatype,
         leng	      LIKE dd03l-leng,
         decimals	  LIKE dd03l-decimals,
         domname    LIKE dd03l-domname,
         shlporigin LIKE dd03l-shlporigin,
         tabletype  LIKE dd03l-tabletype,
         depth      LIKE dd03l-depth,
         comptype	  LIKE dd03l-comptype,
         reftype    LIKE dd03l-reftype,
         languflag  LIKE dd03l-languflag,
   END OF st_293_dd03l.

TYPES:
  BEGIN OF st_dd02_03l,
         tabname   LIKE dd03l-tabname,
         as4local  LIKE dd03l-as4local,
         as4vers   LIKE dd03l-as4vers,
         fieldname LIKE dd03l-fieldname,
         position  LIKE dd03l-position,
         clidep    LIKE dd02l-clidep,
  END OF st_dd02_03l.

TYPES:
  BEGIN OF st_ex_names,
         a(3)   TYPE c,
         b(100) TYPE c,
  END OF st_ex_names.

TYPES:
       tab_key_dd03l TYPE STANDARD TABLE OF st_key_dd03l,
       tab_70_dd03l  TYPE STANDARD TABLE OF st_70_dd03l,
       tab_139_dd03l TYPE STANDARD TABLE OF st_139_dd03l,
       tab_244_dd03l TYPE STANDARD TABLE OF st_244_dd03l,
       tab_293_dd03l TYPE STANDARD TABLE OF st_293_dd03l,
       tab_dd02_03l  TYPE STANDARD TABLE OF st_dd02_03l,
       tab_ex_names  TYPE STANDARD TABLE OF st_ex_names.

*-------------------------------------------------------------
DATA:
      tx(140)     TYPE c,
      gt_ex_names TYPE tab_ex_names,

      test1(3) TYPE c,
      test2(3) TYPE c,
      test3(3) TYPE c,
      test4(3) TYPE c,
      test5(3) TYPE c,
      test6(3) TYPE c,

      form1(7) TYPE c,
      form2(7) TYPE c,
      form3(7) TYPE c,
      form4(7) TYPE c,
      form5(7) TYPE c,
      form6(7) TYPE c,

      x1     TYPE c,
      x2     TYPE c,
      x3     TYPE c,
      x4     TYPE c,
      x5     TYPE c,
      x6     TYPE c,
      sum1   TYPE p DECIMALS 1,
      sum2   TYPE p DECIMALS 1,
      sum3   TYPE p DECIMALS 1,
      sum4   TYPE p DECIMALS 1,
      sum5   TYPE p DECIMALS 1,
      sum6   TYPE p DECIMALS 1,
      t1_min TYPE p DECIMALS 1,
      t2_min TYPE p DECIMALS 1,
      t3_min TYPE p DECIMALS 1,
      t4_min TYPE p DECIMALS 1,
      t5_min TYPE p DECIMALS 1,
      t6_min TYPE p DECIMALS 1,
      t1     TYPE p DECIMALS 1,
      t2     TYPE p DECIMALS 1,
      t3     TYPE p DECIMALS 1,
      t4     TYPE p DECIMALS 1,
      t5     TYPE p DECIMALS 1,
      t6     TYPE p DECIMALS 1,
      t1_m   TYPE p DECIMALS 1,
      t2_m   TYPE p DECIMALS 1,
      t3_m   TYPE p DECIMALS 1,
      t4_m   TYPE p DECIMALS 1,
      t5_m   TYPE p DECIMALS 1,
      t6_m   TYPE p DECIMALS 1,
      t1_sum TYPE p DECIMALS 1,
      t2_sum TYPE p DECIMALS 1,
      t3_sum TYPE p DECIMALS 1,
      t4_sum TYPE p DECIMALS 1,
      t5_sum TYPE p DECIMALS 1,
      t6_sum TYPE p DECIMALS 1,
      t1_x   TYPE p DECIMALS 1,
      t2_x   TYPE p DECIMALS 1,
      t3_x   TYPE p DECIMALS 1,
      t4_x   TYPE p DECIMALS 1,
      t5_x   TYPE p DECIMALS 1,
      t6_x   TYPE p DECIMALS 1,


      test(6) TYPE c,
      cline   LIKE tx,
*   c_numb       LIKE tx,
      text206(88) TYPE c,
      nn(5)       TYPE c,

      min_avg(3) TYPE c,
      detail_1   TYPE c,
      detail_2   TYPE c,
      detail_3   TYPE c,

      n_type(4) TYPE c,
      n_init    TYPE i,
      n_inc     TYPE i,
      n_loops   TYPE i,
      s_loops   TYPE i,
      l_loops   TYPE i,
*   i_loops      TYPE i,


      info0 TYPE i,
      info1 TYPE i,
      info2 TYPE i,
      info3 TYPE i,
      info4 TYPE i,
      info5 TYPE i,
      error TYPE c,
      c3(3) TYPE c,
*   f            TYPE float,
      s     TYPE i,
      start TYPE i,
      stop  TYPE i,
      n     TYPE i,
      n_i   TYPE i,
      i     TYPE i,
      l_inc TYPE i,
      n1    TYPE i,
      l_i   TYPE i,

*   sum         TYPE i,
*   div         TYPE i,
   t_min       TYPE i,

      tab_search    LIKE dd03l-tabname,
      tab_search2   LIKE dd03l-tabname,
      field_search  LIKE dd03l-fieldname,
      field_search2 LIKE dd03l-fieldname,
      field_search3 LIKE dd03l-fieldname,
      field_search4 LIKE dd03l-fieldname,
      pos_search    LIKE dd03l-position,
      search        TYPE i,
      gv_n1         TYPE i,
      gv_l1         TYPE i,
      xx            TYPE c,
      tt            TYPE p DECIMALS 3,
      t             TYPE p DECIMALS 3.
*----------------------------------------------------------------------
* Parameter:
PARAMETERS:
 testcase(3)   TYPE n      DEFAULT '',
 test_run      AS CHECKBOX DEFAULT ''.

*----------------------------------------------------------------------
START-OF-SELECTION.

* parameters from testcases:
  PERFORM define_testcases USING error.
  IF ( error EQ 'X' ).
    EXIT.
  ENDIF.

  PERFORM fill_texts.
  PERFORM write_header.
*-----------------------------------------------------------------------
  n_i = 0.
  CLEAR text206.
  DO n_loops TIMES.
    n_i = n_i + 1.

    IF ( n_type = 'mult' ).
* multiplicative
      n_inc = n_inc * 10.
      n     = n_init * n_inc.
    ELSEIF ( n_type = 'add' ).
* additive:
      n     = n_init + n_inc * n_i.
      IF     ( n < 1 ).
        n = 1.
      ELSEIF ( n > '999999' ).
        n = 100.
      ENDIF.
    ELSE.
* predefined

      IF ( n_i > 13 ).
        n = 200000.
      ELSEIF ( n_i = 13 ).
        n = 100000.
      ELSEIF ( n_i = 12 ).
        n = 50000.
      ELSEIF ( n_i = 11 ).
        n = 20000.
      ELSEIF ( n_i = 10 ).
        n = 10000.
      ELSEIF ( n_i = 9 ).
        n = 5000.
      ELSEIF ( n_i = 8 ).
        n = 2000.
      ELSEIF ( n_i = 7 ).
        n = 1000.
      ELSEIF ( n_i = 6 ).
        n = 500.
      ELSEIF ( n_i = 5 ).
        n = 200.
      ELSEIF ( n_i = 4 ).
        n = 100.
      ELSEIF ( n_i = 3 ).
        n = 50.
      ELSEIF ( n_i = 2 ).
        n = 20.
      ELSEIF ( n_i = 1 ).
        n = 10.
      ENDIF.
    ENDIF.

    nn = n.
    CONCATENATE text206  nn ',  ' INTO text206.

* fill internal tables:

* 3. average: the statistical repeats:----------------------------------
    t1_min = '99999999.9'. t2_min = '99999999.9'. t3_min = '99999999.9'.
    t4_min = '99999999.9'. t5_min = '99999999.9'. t6_min = '99999999.9'.
    CLEAR  t1_sum.    CLEAR  t2_sum.     CLEAR  t3_sum.
    CLEAR  t4_sum.    CLEAR  t5_sum.     CLEAR  t6_sum.
    CLEAR  s.

    DO s_loops TIMES.
      s = s + 1.

* 2.average: the different locations:-----------------------------------
      CLEAR i.
      CLEAR n1.
      CLEAR sum1.        CLEAR sum2.        CLEAR sum3.
      CLEAR sum4.        CLEAR sum5.        CLEAR sum6.
      l_inc = n / ( l_loops + 1 ).

      WHILE ( i < l_loops ).
        i   = i + 1.
        l_i = l_inc * i.
        n1  = l_i * 10.

        IF NOT ( test1 IS INITIAL ).
          PERFORM (form1) IN PROGRAM (sy-repid).
          t1 = tt.  x1 = xx.
        ENDIF.
        IF NOT ( test2 IS INITIAL ).
          PERFORM (form2) IN PROGRAM (sy-repid).
          t2 = tt.  x2 = xx.
        ENDIF.
        IF NOT ( test3 IS INITIAL ).
          PERFORM (form3) IN PROGRAM (sy-repid).
          t3 = tt.  x3 = xx.
        ENDIF.
        IF NOT ( test4 IS INITIAL ).
          PERFORM (form4) IN PROGRAM (sy-repid).
          t4 = tt.  x4 = xx.
        ENDIF.
        IF NOT ( test5 IS INITIAL ).
          PERFORM (form5) IN PROGRAM (sy-repid).
          t5 = tt.  x5 = xx.
        ENDIF.
        IF NOT ( test6 IS INITIAL ).
          PERFORM (form6) IN PROGRAM (sy-repid).
          t6 = tt.  x6 = xx.
        ENDIF.

        IF ( detail_1 EQ 'X' ).
          IF ( s EQ s_loops ).
            FORMAT COLOR COL_TOTAL.
            WRITE: / n.
            WRITE AT  13 'L ='.
            WRITE AT  17 l_i.
            WRITE AT  30 x1.
            WRITE AT  32 x2.
            WRITE AT  34 x3.
            WRITE AT  36 x4.
            WRITE AT  38 x5.
            WRITE AT  40 x6.
            WRITE AT  44 t1.
            WRITE AT  60 t2.
            WRITE AT  76 t3.
            WRITE AT  92 t4.
            WRITE AT 108 t5.
            WRITE AT 124 t6.
            FORMAT RESET.
          ENDIF.
        ENDIF.

        sum1 = sum1 + t1.
        sum2 = sum2 + t2.
        sum3 = sum3 + t3.
        sum4 = sum4 + t4.
        sum5 = sum5 + t5.
        sum6 = sum6 + t6.
      ENDWHILE.

      IF ( detail_1 EQ 'X' AND s EQ s_loops ).
*        write: / cline.
      ENDIF.
      t1_m = sum1 / l_loops.
      t2_m = sum2 / l_loops.
      t3_m = sum3 / l_loops.
      t4_m = sum4 / l_loops.
      t5_m = sum5 / l_loops.
      t6_m = sum6 / l_loops.
* end of 2. average: ---------------------------------------------------

      IF ( detail_2 EQ 'X' AND s_loops GT 1 ).
        FORMAT COLOR COL_POSITIVE.
        WRITE: /       info1.
        WRITE AT  12   info2.
        WRITE AT  22   info3.
        WRITE AT  32   info4.

        WRITE AT  44 t1_m.
        WRITE AT  60 t2_m.
        WRITE AT  76 t3_m.
        WRITE AT  92 t4_m.
        WRITE AT 108 t5_m.
        WRITE AT 124 t6_m.

        FORMAT RESET.
        IF ( detail_1 EQ 'X'
             OR  ( detail_3 EQ 'X' AND s EQ s_loops ) ).
*          write: / cline.
        ENDIF.
      ENDIF.

      IF ( min_avg EQ 'avg' ).
        t1_sum = t1_sum + t1_m.
        t2_sum = t2_sum + t2_m.
        t3_sum = t3_sum + t3_m.
        t4_sum = t4_sum + t4_m.
        t5_sum = t5_sum + t5_m.
        t6_sum = t6_sum + t6_m.
      ELSE.
        IF ( t1_m < t1_min ). t1_min = t1_m. ENDIF.
        IF ( t2_m < t2_min ). t2_min = t2_m. ENDIF.
        IF ( t3_m < t3_min ). t3_min = t3_m. ENDIF.
        IF ( t4_m < t4_min ). t4_min = t4_m. ENDIF.
        IF ( t5_m < t5_min ). t5_min = t5_m. ENDIF.
        IF ( t6_m < t6_min ). t6_min = t6_m. ENDIF.
      ENDIF.
    ENDDO.

    IF ( min_avg EQ 'avg' ).
      t1_x = t1_sum / s_loops.
      t2_x = t2_sum / s_loops.
      t3_x = t3_sum / s_loops.
      t4_x = t4_sum / s_loops.
      t5_x = t5_sum / s_loops.
      t6_x = t6_sum / s_loops.
    ELSE.
      t1_x = t1_min.
      t2_x = t2_min.
      t3_x = t3_min.
      t4_x = t4_min.
      t5_x = t5_min.
      t6_x = t6_min.
    ENDIF.
* end of 3. average:----------------------------------------------------

    IF ( detail_3 EQ 'X' ).
      FORMAT COLOR COL_NEGATIVE.
      WRITE: /       info1.
      WRITE AT  12   info2.
      WRITE AT  22   info3.
      WRITE AT  32   info4.
*      if ( t1_x > 0 ). write at  44   t1_x.  endif.
*      if ( t2_x > 0 ). write at  60   t2_x.  endif.
*      if ( t3_x > 0 ). write at  76   t3_x.  endif.
*      if ( t4_x > 0 ). write at  92   t4_x.  endif.
*      if ( t5_x > 0 ). write at 108   t5_x.  endif.
*      if ( t6_x > 0 ). write at 124   t6_x.  endif.
*
      WRITE AT  44   t1_x.
      WRITE AT  60   t2_x.
      WRITE AT  76   t3_x.
      WRITE AT  92   t4_x.
      WRITE AT 108   t5_x.
      WRITE AT 124   t6_x.

      FORMAT RESET.

      IF ( detail_1 EQ 'X' OR detail_2 EQ 'X' ).
*        write: / cline.
      ENDIF.
    ENDIF.
  ENDDO.
*  write: / cline.

  PERFORM write_trailer.
*-----------------------------------------------------------------------
*  do_write:
*-----------------------------------------------------------------------
FORM write_header.

  DATA:
    nn(5)    TYPE c,

*--------------------
        text99      LIKE tx,
        text300     LIKE tx,
        text301     LIKE tx,
        text302     LIKE tx,
        text206(88) TYPE c.


  text300     = ' Ergebnisse:'.
  text300+55  = ' Laufzeiten (in Mikrosekunden)'.
  text301+139 = '.'.
  text302     = ' info1 info2 info3 info4'.
  text302+56  = test1.
  text302+72  = test2.
  text302+88  = test3.
  text302+104 = test4.
  text302+120 = test5.
  text302+136 = test6.

  CONCATENATE 'Selected Order: '
   test1 '  ' test2 '  ' test3 '  '
   test4 '  ' test5 '  ' test6 '  '
   INTO text99.

  FORMAT COLOR COL_KEY INTENSIFIED ON.
  WRITE: / cline.
  WRITE: / text300.
  WRITE: / text301.
  WRITE: / text302.
  WRITE: / text301.
  FORMAT RESET.

ENDFORM.                    "write_header
*-----------------------------------------------------------------------
*  do_write:
*-----------------------------------------------------------------------
FORM write_trailer.

  DATA:
        nn(5)     TYPE c,
        sel(20)   TYPE c,
        off       TYPE i,
        name(3)   TYPE c,
        wa        TYPE st_ex_names,
        text(100) TYPE c,
        text00    LIKE tx,
        text0     LIKE tx,

        ty(51) TYPE c,
        tz(76) TYPE c,
*--------------------
        text20      LIKE tx,
        text21      LIKE tx,
        text30      LIKE tx,
        text101     LIKE tx,
        text102     LIKE tx,
        text103     LIKE ty,
        text104     LIKE ty,
        text105     LIKE ty,
        text106     LIKE ty,
        text107     LIKE tx,
        text108     LIKE ty,
        text109     LIKE ty,
        text110     LIKE ty,
        text150     LIKE tx,
        text201     LIKE tx,
        text202     LIKE tx,
        text203(40) TYPE c,
        text204(40) TYPE c.

*------------------------------------------------------------------
  text20 = ' Parameter der Messung:'.
  text21 = ' Gemessene Routinen: '.

  SKIP 2.
  WRITE: / text20.
  WRITE: / text21.

* write selected routines:----------------------------------------------
  CONCATENATE test1 ' ' test2 ' ' test3 ' ' test4 ' ' test5 ' '
              test6 INTO sel.
  off = 0.
  DO 6 TIMES.
    name = sel+off(3).
    IF ( name <> ' ' ).
      READ TABLE gt_ex_names
           INTO wa
           WITH KEY a = name
           BINARY SEARCH.
      IF ( sy-subrc EQ 0 ).
        CONCATENATE wa-a ':' wa-b INTO text
                    SEPARATED BY space.
      ELSE.
        CONCATENATE name ':' 'not found!' INTO text
                    SEPARATED BY space.
      ENDIF.
      text30+6 = text.
      WRITE: / text30.
    ENDIF.
    off = off + 3.
  ENDDO.
*----------------------------------------------------------------------
* Selected Parameter

  text107 = ' Statistische Einstellungen:'.
  text110 = ' Wiederholungen s_loops ='.
  text105 = ' Variation der Groesse n_loops ='.
  text106 = ' => n ='.

  text201 = ' Dargestellte Details: '.
  text203 = ' D-level = 1 : Einzelmessung'.
  text204 = ' D-level = 2 : Durchschnitt'.

  SKIP 1.
  WRITE: / text101  .
  WRITE: / text107  .
  WRITE: / text110, s_loops, tz.
  WRITE: / text105, n_loops, tz.

  SKIP 1.
  WRITE: / text201  .
  WRITE: / text203  COLOR  COL_POSITIVE.
  WRITE: / text204  COLOR  COL_NEGATIVE.

*  WRITE: / text101  COLOR COL_KEY INTENSIFIED ON.
*  WRITE: / text107  COLOR  COL_KEY.
*  WRITE: / text110, s_loops, tz.
*  WRITE: / text105, n_loops, tz.
*  WRITE: / text106, text206.
*
*  WRITE: / text201  COLOR  COL_KEY INTENSIFIED OFF.
*  WRITE: / text203  COLOR  COL_POSITIVE.
*  WRITE: / text204  COLOR  COL_NEGATIVE.
*  WRITE: / text150  COLOR COL_KEY INTENSIFIED ON.
  FORMAT RESET.

ENDFORM.                    "write_trailer
************************************************************************
************************************************************************
*-----------------------------------------------------------------------
*    : Test the size of the tables
* 001: COUNT(*) dd01l
* 002: COUNT(*) dd02l
* 003: COUNT(*) dd03l
*-----------------------------------------------------------------------
FORM  test001.

  CLEAR     tt.
  xx        = 'N'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*)
         INTO  info1
         FROM  dd01l.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "test001
*-----------------------------------------------------------------------
FORM  test002.

  CLEAR     tt.
  xx        = 'N'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*)
         INTO  info2
         FROM  dd02l.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "test002
*-----------------------------------------------------------------------
FORM  test003.

  CLEAR     tt.
  xx        = 'N'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*)
         INTO  info3
         FROM  dd03l.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "test003
************************************************************************
* ABCD: Index Usage
************************************************************************
*-----------------------------------------------------------------------
*  AA : Different Selects on DD02L
** AA1: SELECT SINGLE        full primary key
** AA2: SELECT * INTO TABLE  RANGE
** AA3: SELECT * INTO TABLE  full table scan
** AA4: SELECT * INTO TABLE  as AB1 no SINGLE
*-----------------------------------------------------------------------
FORM  testaa1.

  DATA:
    ls_dd02l    TYPE wa_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE *
         INTO  ls_dd02l
         FROM  dd02l
         WHERE tabname   = 'DD02L'
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info1 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testAA1
*-----------------------------------------------------------------------
FORM  testaa2.

  DATA:
    lt_dd02l   TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   = 'DD02L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAA2
*-----------------------------------------------------------------------
FORM  testaa3.

  DATA:
    lt_dd02l    TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE applclass = 'SDIC'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAA3
*-----------------------------------------------------------------------
FORM  testaa4.

  DATA:
    lt_dd02l   TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   = 'DD02L'
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAA4
*-----------------------------------------------------------------------
*  AA : Different Selects on DD02L
** AB1: SELECT SINGLE        full primary key
** AB2: SELECT * INTO TABLE  RANGE
** AB3: SELECT * INTO TABLE  full table scan
** AB4: SELECT * INTO TABLE  as AB1 no SINGLE
*-----------------------------------------------------------------------
FORM  testab1.

  DATA:
    ls_dd03l    TYPE wa_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   fieldname = 'TABNAME'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info1 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testAB1
*-----------------------------------------------------------------------
FORM  testab2.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAB2
*-----------------------------------------------------------------------
FORM  testab3.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE fieldname = 'TABNAME'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testab3
*-----------------------------------------------------------------------
FORM  testab4.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   fieldname = 'TABNAME'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAB4
*-----------------------------------------------------------------------
*  AC : Secondary Keys on DD03L
** AC1: SELECT *      key1 (tabname position as4local as4vers)
** AC2: SELECT *      key2 (rollname)
** AC3: SELECT *      key3 (checktable)
** AC4: SELECT *      key7 (domname)
*-----------------------------------------------------------------------
FORM  testac1.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   position  = '1'
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAC1
*-----------------------------------------------------------------------
FORM  testac2.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE rollname = 'TABNAME'
         AND   as4local = 'A'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAC2
*-----------------------------------------------------------------------
FORM  testac3.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE checktable = 'DD02L'
         AND   as4local   = 'A'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAC3
*-----------------------------------------------------------------------
FORM  testac4.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE domname  = 'AS4TAB'
         AND   as4local = 'A'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testAC4
*-----------------------------------------------------------------------
*  AE : Different SELECT
*  AE1:  WHERE tabname =
*  AE2:  WHERE tabname =  fieldname =
*  AE3:  WHERE tabname =  as4local =   position  =
*  AE4:  WHERE tabname =  fieldname =  as4local =  as4vers =
*-----------------------------------------------------------------------
FORM  testae1.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testae1
*-----------------------------------------------------------------------
FORM  testae2.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   fieldname = 'TABNAME'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testae2
*-----------------------------------------------------------------------
FORM  testae3.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   as4local  = 'A'
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testae3
*-----------------------------------------------------------------------
FORM  testae4.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   fieldname = 'TABNAME'
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
*         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testae4
*-----------------------------------------------------------------------
*  AH :  Other secondary key
*  AH1:  WHERE reftable =
*  AH2:  WHERE reftable =  position =
*  AH3:  WHERE reftable =  tabname  =
*-----------------------------------------------------------------------
FORM  testah1.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE reftable  = 'DDLANGUAGE'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testah1
*-----------------------------------------------------------------------
FORM  testah2.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE reftable  = 'DDLANGUAGE'
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testah2
*-----------------------------------------------------------------------
FORM  testah3.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE reftable  = 'DDLANGUAGE'
         AND   fieldname = '.INCLUDE'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testah3
*-----------------------------------------------------------------------
*  AM :  fields of different indexes
*  AM1:  fieldname  =   rollname =
*  AM2:  fieldname  =   domname  =
*  AM3:  rollname   =   domname  =
*-----------------------------------------------------------------------
FORM  testam1.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE fieldname  = 'CONROUT'
         AND   rollname   = 'CONROUT'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testam1
*-----------------------------------------------------------------------
FORM  testam2.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE fieldname = 'CONROUT'
         AND   domname   = 'CHAR10'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testam2
*-----------------------------------------------------------------------
FORM  testam3.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE rollname = 'CONROUT'
         AND   domname  = 'CHAR10'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testam3
*-----------------------------------------------------------------------
*  BA :  SELECT FROM DD03L WHERE tabname LIKE ??
*  BA1:  WHERE tabname LIKE 'D%'
*  BA2:  as BA1  'T%'
*  BA3:  as BA1  'A%'
*  BA4:  as BA1  'Q%'
*-----------------------------------------------------------------------
FORM  testba0.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info5.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testba0
*-----------------------------------------------------------------------
FORM  testba1.

  tab_search = 'D%'.
  PERFORM testba0.
  info1 = info5.

ENDFORM.                                                    "testba1
*-----------------------------------------------------------------------
FORM  testba2.

  tab_search = 'T%'.
  PERFORM testba0.
  info2 = info5.

ENDFORM.                                                    "testba2
*-----------------------------------------------------------------------
FORM  testba3.

  tab_search = 'A%'.
  PERFORM testba0.
  info3 = info5.

ENDFORM.                                                    "testba3
*-----------------------------------------------------------------------
FORM  testba4.

  tab_search = 'Q%'.
  PERFORM testba0.
  info4 = info5.

ENDFORM.                                                    "testba4
*-----------------------------------------------------------------------
*  BB : SELECT * FRPM DD02L  WHERE tabname LIKE ??
*  BB1:  WHERE tabname LIKE 'D%'
*  BB2:  as BB1  'T%'
*  BB3:  as BB1  'A%'
*  BB1:  as BB1  'Q%'
*-----------------------------------------------------------------------
FORM  testbb0.

  DATA:
    lt_dd02l    TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE tab_search.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info5.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbb0
*-----------------------------------------------------------------------
FORM  testbb1.

  tab_search = 'D%'.
  PERFORM testbb0.
  info1 = info5.

ENDFORM.                                                    "testbb1
*-----------------------------------------------------------------------
FORM  testbb2.

  tab_search = 'T%'.
  PERFORM testbb0.
  info2 = info5.

ENDFORM.                                                    "testbb2
*-----------------------------------------------------------------------
FORM  testbb3.

  tab_search = 'A%'.
  PERFORM testbb0.
  info3 = info5.

ENDFORM.                                                    "testbb3
*-----------------------------------------------------------------------
FORM  testbb4.

  tab_search = 'Q%'.
  PERFORM testbb0.
  info4 = info5.

ENDFORM.                                                    "testbb4
*-----------------------------------------------------------------------
*  BC :
*  BC1: SELECT * WHERE tabname LIKE as4local
*  BC2: SELECT * WHERE tabname LIKE as4local =   as4vrs =
*  BC3: SELECT * WHERE tabname LIKE as4local
*  BC4: SELECT * WHERE tabname LIKE as4local =   as4vrs =
*-----------------------------------------------------------------------
FORM  testbc1.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  tab_search = 'D%'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbc1
*-----------------------------------------------------------------------
FORM  testbc2.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  tab_search = 'D%'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbc2
*-----------------------------------------------------------------------
FORM  testbc3.

  DATA:
    lt_dd02l    TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
  tab_search = 'D%'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbc3
*-----------------------------------------------------------------------
FORM  testbc4.

  DATA:
    lt_dd02l    TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
  tab_search = 'D%'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbc4
*-----------------------------------------------------------------------
*  BG :  SELECT FROM DD03L WHERE tabname LIKE ??
* BG1:  AND fieldname LIKE 'D%'
* BG2:  as BG1  'T%'
* BG3:  as BG1  'A%'
* BG4:  as BG1  'Q%'
*-----------------------------------------------------------------------
FORM  testbg0.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   fieldname LIKE field_search.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info5.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbg0
*-----------------------------------------------------------------------
FORM  testbg1.

  tab_search   = 'D%'.
  field_search = 'D%'.
  PERFORM testbg0.
  info1 = info5.

ENDFORM.                                                    "testbg1
*-----------------------------------------------------------------------
FORM  testbg2.

  tab_search   = 'D%'.
  field_search = 'T%'.
  PERFORM testbg0.
  info2 = info5.

ENDFORM.                                                    "testbg2
*-----------------------------------------------------------------------
FORM  testbg3.

  tab_search   = 'D%'.
  field_search = 'A%'.
  PERFORM testbg0.
  info3 = info5.

ENDFORM.                                                    "testbg3
*-----------------------------------------------------------------------
FORM  testbg4.

  tab_search   = 'D%'.
  field_search = 'Q%'.
  PERFORM testbg0.
  info4 = info5.

ENDFORM.                                                    "testbg4
*-----------------------------------------------------------------------
*  BH : ALL SELECT INTO TABLE  tabname LIKE 'A%' position > ?
*  BH1:  pos_search '1'
*  BH2:  pos_search '10'
*  BH3:  pos_search '50'
*  BH4:  pos_search '100'
*-----------------------------------------------------------------------
FORM  testbh0.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   position  >    pos_search.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info5.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbh0
*-----------------------------------------------------------------------
FORM  testbh1.

  tab_search = 'D%'.
  pos_search = '1'.
  PERFORM testbh0.
  info1 = info5.

ENDFORM.                                                    "testbh1
*-----------------------------------------------------------------------
FORM  testbh2.

  tab_search = 'D%'.
  pos_search = '10'.
  PERFORM testbh0.
  info2 = info5.

ENDFORM.                                                    "testbh2
*-----------------------------------------------------------------------
FORM  testbh3.

  tab_search = 'D%'.
  pos_search = '50'.
  PERFORM testbh0.
  info3 = info5.

ENDFORM.                                                    "testbh3
*-----------------------------------------------------------------------
FORM  testbh4.

  tab_search = 'D%'.
  pos_search = '100'.
  PERFORM testbh0.
  info4 = info5.

ENDFORM.                                                    "testbh4
*-----------------------------------------------------------------------
*  BJ :  GT, LT, GE, LE
*  BJ1:  position LT '11'
*  BJ2:  position LT '10'
*  BJ3:  position LE '10'
*  BJ4:  position GE '10'
*-----------------------------------------------------------------------
FORM  testbj1.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  =    'A'
         AND   position  LT   '11'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbj1
*-----------------------------------------------------------------------
FORM  testbj2.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  =    'A'
         AND   position  LT   '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbj2
*-----------------------------------------------------------------------
FORM  testbj3.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  =    'A'
         AND   position  LE   '10'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbj3
*-----------------------------------------------------------------------
FORM  testbj4.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  =    'A'
         AND   position  GE   '10'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbj4
*-----------------------------------------------------------------------
*  BL :  GT AND LT, GT AND LT, BETWEEN
*  BL1:  GT '0' AND LT '11'
*  BL2:  GE '1' AND LE '10'
*  BL3:  BETWEEN '1' AND '10'
*  BL4:  BETWEEN '0' AND '1'
*-----------------------------------------------------------------------
FORM  testbl1.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  = 'A'
         AND   position  GT '0'
         AND   position  LT '11'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbl1
*-----------------------------------------------------------------------
FORM  testbl2.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   position  GE   '1'
         AND   position  LE   '10'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbl2
*-----------------------------------------------------------------------
FORM  testbl3.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  = 'A'
         AND   position  BETWEEN '1' AND '10'.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbl3
*-----------------------------------------------------------------------
FORM  testbl4.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  = 'A'
         AND   position  BETWEEN '0' AND '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testbl4
*-----------------------------------------------------------------------
FORM  testbz2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   fieldname IN ('TABNAME' ,'FIELDNAME' ,'POSITION').

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testba2
*-----------------------------------------------------------------------
FORM  testbz3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   position  < '5'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testba3
*-----------------------------------------------------------------------
FORM  testbx4.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   position  BETWEEN '1' AND '4'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testba4
*-----------------------------------------------------------------------
* AT   Empfty Condtions
* AT1  5 equal conditions in IN
* AS2  5 equal conditions with OR
* AS3  5 equal conditions with LOOP itab
* AS4  5 equal conditions with FOR ALl ENTRIES
*-----------------------------------------------------------------------
FORM  testat1.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'
         AND   fieldname LIKE '%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testat1
*-----------------------------------------------------------------------
FORM  testat2.

  DATA:
        lv_fieldname LIKE dd03l-fieldname,
        lt_dd03l     TYPE tab_dd03l.

* ranges table
  RANGES fieldnames FOR dd03l-fieldname OCCURS 0.

  xx      = 'N'.
  CLEAR   tt.

  fieldnames-low    = 'TABNAME'.
  fieldnames-sign   = 'I'.
  fieldnames-option = 'EQ'.
  APPEND fieldnames.

  REFRESH fieldnames.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'
         AND   fieldname IN fieldnames.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testat2
*-----------------------------------------------------------------------
FORM  testat3.

  DATA:
        lv_fieldname LIKE dd03l-fieldname,
        lt_dd03l     TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.

*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE   tabname  = 'DD03L'
         AND   ( position LE '5' OR position GT '5' ).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testat3
************************************************************************
* C: Better Searches
************************************************************************
*-----------------------------------------------------------------------
*  CA : Conditions in Check OR WHERE
** CA1: WHERE A B C D E
** CA2: WHERE A B C D    CHECK E
** CA3: WHERE A B        CHECK C D E
** CA4: WHERE A          CHECK B C D E
*-----------------------------------------------------------------------
FORM  testca1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l TYPE wa_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   fieldname = 'SQLTAB'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '3'.
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testca1
*-----------------------------------------------------------------------
FORM  testca2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   fieldname = 'SQLTAB'
         AND   as4local  = 'A'
         AND   as4vers   = ' '.

    CHECK ( ls_dd03l-position = '3' ).
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testca2
*-----------------------------------------------------------------------
FORM  testca3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.


  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   fieldname = 'SQLTAB'.

    CHECK ( ls_dd03l-as4local = 'A' ).
    CHECK ( ls_dd03l-as4vers  = ' ' ).
    CHECK ( ls_dd03l-position = '3' ).
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testca3
*-----------------------------------------------------------------------
FORM  testca4.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'.

    CHECK ( ls_dd03l-fieldname = 'SQLTAB' ).
    CHECK ( ls_dd03l-as4local  = 'A' ).
    CHECK ( ls_dd03l-as4vers   = ' ' ).
    CHECK ( ls_dd03l-position  = '3' ).
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testca4
*-----------------------------------------------------------------------
*  CB :
** CB1: WHERE A B C D E
** CB2: WHERE A B C D    no CHECK
** CB3: WHERE A B        no CHECK
** CB4: WHERE A          no CHECK
*-----------------------------------------------------------------------
FORM  testcb1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   fieldname = 'SQLTAB'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '3'.
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcb1
*-----------------------------------------------------------------------
FORM  testcb2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   fieldname = 'SQLTAB'
         AND   as4local  = 'A'
         AND   as4vers   = ' '.

    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcb2
*-----------------------------------------------------------------------
FORM  testcb3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.


  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   fieldname = 'SQLTAB'.
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcb3
*-----------------------------------------------------------------------
FORM  testcb4.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'.
    APPEND ls_dd03l TO lt_dd03l.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcb4
*-----------------------------------------------------------------------
*  CE : Empty Conditions
** CE1: WHERE tabname LIKE '%'  AND ...
** CE2: WHERE tabname LIKE A    AND ...  - A = '%'
** CE3: WHERE tabname IN  range AND ...  - range = '*'
** CE4: WHERE                   AND ...
*-----------------------------------------------------------------------
FORM  testce1.

  DATA:
    lt_dd02l   TYPE  tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE '%'
         AND   tabclass  = 'CLUSTER'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testce1
*-----------------------------------------------------------------------
FORM  testce2.

  DATA:
    lt_dd02l   TYPE  tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
  tab_search = '%'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE tab_search
         AND   tabclass  = 'CLUSTER'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testce2
*-----------------------------------------------------------------------
FORM  testce3.

  DATA:
        lv_fieldname LIKE dd03l-fieldname,
        lt_dd02l     TYPE tab_dd02l,
        tabnames     TYPE RANGE OF dd02l-tabname,
        wa_names     LIKE LINE OF tabnames.

  xx      = 'N'.
  CLEAR   tt.

  wa_names-low    = '*'.
  wa_names-sign   = 'I'.
  wa_names-option = 'CP'.
  APPEND wa_names TO tabnames.

*  REFRESH tabnames.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname  IN tabnames
         AND   tabclass  = 'CLUSTER'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testce3
*-----------------------------------------------------------------------
FORM  testce4.

  DATA:
    lt_dd02l   TYPE  tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
  tab_search = '%'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabclass  = 'CLUSTER'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testce4
*-----------------------------------------------------------------------
*  CG : LIKE and Wildcards
** CG1: WHERE tabname LIKE 'D%'  fieldname =    'TABNAME'
** CG2: WHERE tabname LIKE 'D%'  fieldname LIKE 'TABNAME'
** CG3: WHERE tabname LIKE 'D%'  fieldname LIKE 'T%'
*-----------------------------------------------------------------------
FORM  testcg1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname    LIKE 'D%'
         AND   fieldname  = 'TABNAME'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcg1
*-----------------------------------------------------------------------
FORM  testcg2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname    LIKE 'D%'
         AND   fieldname  LIKE 'TABNAME'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcg2
*-----------------------------------------------------------------------
FORM  testcg3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname    LIKE 'D%'
         AND   fieldname  LIKE 'T%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcg3
*-----------------------------------------------------------------------
*  CH : LIKE and Wildcards
** CH1: SELECT *  ... fieldname =    'TABNAME'  position = '1'
** CH2: SELECT *  ... fieldname LIKE 'TABNAME'  position = '1'
*-----------------------------------------------------------------------
FORM  testch1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   = 'DD03L'
         AND   fieldname = 'TABNAME'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = 1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testch1
*-----------------------------------------------------------------------
FORM  testch2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD03L'
         AND   fieldname LIKE 'TABNAME'
         AND   position  <     100.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testch2
*-----------------------------------------------------------------------
*  CN : IN versus OR
** CN1: WHERE ... AND ( position = 11 or ... position = 20 )
** CN2: WHERE ... AND position IN (11,...,20)
** CN3: WHERE ... AND position BETWEEN 11 AND 20
*-----------------------------------------------------------------------
FORM  testcn1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   as4local  = 'A'
         AND ( position  = ' 11'
         OR    position  = ' 12'
         OR    position  = ' 13'
         OR    position  = ' 14'
         OR    position  = ' 15'
         OR    position  = ' 16'
         OR    position  = ' 17'
         OR    position  = ' 18'
         OR    position  = ' 19'
         OR    position  = ' 20' ).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcn1
*-----------------------------------------------------------------------
FORM  testcn2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   as4local  = 'A'
         AND   position  IN (11,12,13,14,15,16,17,18,19,20).

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcn2
*-----------------------------------------------------------------------
FORM  testcn3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD%'
         AND   as4local  = 'A'
         AND   position  BETWEEN '11' AND '20'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcn3
*-----------------------------------------------------------------------
*  CM : IN versus OR
** CM1: WHERE ( tabname = ... OR tabname =... ) AND ...
** CM2: WHERE tabname IN ( ..., ... )           AND ...
** CM3: WHERE tabname LIKE 'DD0__'
*-----------------------------------------------------------------------
FORM  testcm1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE
         (     tabname   = 'DD01D'
         OR    tabname   = 'DD01L'
         OR    tabname   = 'DD01T'
         OR    tabname   = 'DD01V'
         OR    tabname   = 'DD02D'
         OR    tabname   = 'DD02L'
         OR    tabname   = 'DD02T'
         OR    tabname   = 'DD02V'
         OR    tabname   = 'DD03D'
         OR    tabname   = 'DD03K'
         OR    tabname   = 'DD03L'
         OR    tabname   = 'DD03M'
         OR    tabname   = 'DD03N'
         OR    tabname   = 'DD03P'
         OR    tabname   = 'DD03T'
         OR    tabname   = 'DD03V'
         OR    tabname   = 'DD04D'
         OR    tabname   = 'DD04L'
         OR    tabname   = 'DD04T'
         OR    tabname   = 'DD04V'
         OR    tabname   = 'DD05M'
         OR    tabname   = 'DD05P'
         OR    tabname   = 'DD05Q'
         OR    tabname   = 'DD05S'
         OR    tabname   = 'DD05V'
         OR    tabname   = 'DD06D'
         OR    tabname   = 'DD06L'
         OR    tabname   = 'DD06T'
         OR    tabname   = 'DD06V'
         OR    tabname   = 'DD07D'
         OR    tabname   = 'DD07E'
         OR    tabname   = 'DD07L'
         OR    tabname   = 'DD07T'
         OR    tabname   = 'DD07V'
         OR    tabname   = 'DD08D'
         OR    tabname   = 'DD08L'
         OR    tabname   = 'DD08T'
         OR    tabname   = 'DD08V'
         OR    tabname   = 'DD09C'
         OR    tabname   = 'DD09L'
         OR    tabname   = 'DD09V' )
         AND   position  = 1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcm1
*-----------------------------------------------------------------------
FORM  testcm2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname IN
               ('DD01D', 'DD01L', 'DD01T', 'DD01V',
                'DD02D', 'DD02L', 'DD02T', 'DD02V',
                'DD03D', 'DD03K', 'DD03L', 'DD03M',
                'DD03N', 'DD03P', 'DD03T', 'DD03V',
                'DD04D', 'DD04L', 'DD04T', 'DD04V',
                'DD05M', 'DD05P', 'DD05Q', 'DD05S', 'DD05V',
                'DD06D', 'DD06L', 'DD06T', 'DD06V',
                'DD07D', 'DD07E', 'DD07L', 'DD07T', 'DD07V',
                'DD08D', 'DD08L', 'DD08T', 'DD08V',
                'DD09C', 'DD09L', 'DD09V')
         AND    position = 1.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcm2
*-----------------------------------------------------------------------
FORM  testcm3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'DD0__'
         AND   position  = 1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcm3
*-----------------------------------------------------------------------
*  CP : Index Gap
** CP1: SELECT tabname LIKE 'S%'                position > 20
** CP2: SELECT tabname LIKE 'S%' as4local = 'A' position > 20
** CP3: SELECT tabname LIKE 'S%' as4local IN ( 'A', ' ' ) position > 20
*-----------------------------------------------------------------------
FORM  testcp1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  =    'A'
         AND   position  =    20
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcp1
*-----------------------------------------------------------------------
FORM  testcp2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   position  =    20
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcp2
*-----------------------------------------------------------------------
FORM  testcp3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  IN   ('A','N')
         AND   position  =     20
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcp3
*-----------------------------------------------------------------------
*  CQ : Index Gap
** CQ1: SELECT * DD12T  ddlanguage =  'E'  sqltab LIKE 'S%'
** CQ2: SELECT * DD12T  ddlanguage <> 'D'  sqltab LIKE 'S%'
** CQ3: SELECT * DD12T  sqltab LIKE 'S%'   CHECK  ddlanguage =  'E'
** CQ4: SELECT * DD12T  ddlanguage LIKE '%' sqltab LIKE 'S%' CHECK
*-----------------------------------------------------------------------
FORM  testcq1.

  DATA:
        lt_dd12t TYPE tab_dd12t,
        ls_dd12t LIKE dd12t.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd12t
         FROM  dd12t
         WHERE ddlanguage = 'E'
         AND   sqltab     LIKE 'S%'
         AND   as4local   = 'A'
         AND   as4vers    = ' '.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd12t LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcq1
*-----------------------------------------------------------------------
FORM  testcq2.

  DATA:
        lt_dd12t TYPE tab_dd12t,
        ls_dd12t LIKE dd12t.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd12t
         FROM  dd12t
         WHERE ddlanguage <> 'D'
         AND   sqltab     LIKE 'S%'
         AND   as4local   = 'A'
         AND   as4vers    = ' '.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd12t LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcq2
*-----------------------------------------------------------------------
FORM  testcq3.

  DATA:
        lt_dd12t TYPE tab_dd12t,
        ls_dd12t LIKE dd12t.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd12t
         FROM  dd12t
         WHERE sqltab     LIKE 'S%'
         AND   as4local   = 'A'
         AND   as4vers    = ' '.
    CHECK ( ls_dd12t-ddlanguage = 'E' ).
    APPEND ls_dd12t TO lt_dd12t.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd12t LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcq3
*-----------------------------------------------------------------------
FORM  testcq4.

  DATA:
        lt_dd12t TYPE tab_dd12t,
        ls_dd12t LIKE dd12t.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd12t
         FROM  dd12t
         WHERE ddlanguage LIKE '%'
         AND   sqltab     LIKE 'S%'
         AND   as4local   = 'A'
         AND   as4vers    = ' '.
    CHECK ( ls_dd12t-ddlanguage = 'E' ).
    APPEND ls_dd12t TO lt_dd12t.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd12t LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcq4
*-----------------------------------------------------------------------
*  CU : Not Condition
** CU1: SELECT tabname LIKE 'D%' as4local <> ' '          pos = 20
** CU2: SELECT tabname LIKE 'D%' as4local =  'A'          pos = 20
** CU3: SELECT tabname LIKE 'D%' as4local IN ( 'A', ' ' ) pos = 20
*-----------------------------------------------------------------------
FORM  testcu1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  <>   ' '
         AND   position  =    20
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcu1
*-----------------------------------------------------------------------
FORM  testcu2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  =    'A'
         AND   position  =    20
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcu2
*-----------------------------------------------------------------------
FORM  testcu3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'D%'
         AND   as4local  LIKE '%'
         AND   position  =     20
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcu3
*-----------------------------------------------------------------------
*  CX : DNF Disjunktive Normalform
** CX1: SELECT *      ( key2 (rollname) ) OR ( key3 (checktable) )
* compared with AC2 and AC3
*-----------------------------------------------------------------------
FORM  testcx1.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE ( rollname   = 'TABNAME' AND as4local = 'A' )
         OR    ( checktable = 'DD02L'   AND as4local = 'A' ).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testcx1
************************************************************************
* EFGH: Number of selected Lines
************************************************************************
*-----------------------------------------------------------------------
*  EA : Identical Conditions
** EA1: 5 identical conditions in IN
** EA2: 5 identical conditions with OR
** EA3: 4 conditions IN(1;1) =1 < 2 between 0 1 (overlapping)
** EA4: 5 different positions
*-----------------------------------------------------------------------
FORM  testea1.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'
         AND   position IN (1,1,1,1,1).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testea1
*-----------------------------------------------------------------------
FORM  testea2.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'
         AND ( position = 1
            OR position = 1
            OR position = 1
            OR position = 1
            OR position = 1 ).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testea2
*-----------------------------------------------------------------------
FORM  testea3.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'
         AND ( position IN (1,1)
            OR position = 1
            OR position < 2
            OR position BETWEEN 0 AND 1 ).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testea3
*-----------------------------------------------------------------------
FORM  testea4.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'
         AND   position IN (1,2,3,4,5).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testea4
*-----------------------------------------------------------------------
*  EC : Identical Conditions
** EC1:   5 conditions     all identical in itab with LOOP itab
** EC2:   5 conditions     all identical in itab with FOR ALL ENTRIES
** EC3: 200 conditions 20 x 10 identical in itab with LOOP itab
** EC4: 200 conditions 20 x 10 identical in itab with FOR ALL ENTRIES
*-----------------------------------------------------------------------
FORM  testec1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        wa       TYPE st_key_dd03l,
        itab     TYPE tab_key_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  DO 5 TIMES.
    wa-position = 1.
    APPEND wa TO itab.
  ENDDO.

*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT itab INTO wa.
    SELECT *
           APPENDING TABLE lt_dd03l
           FROM  dd03l
           WHERE tabname  = 'DD03L'
           AND   position = wa-position.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testec1
*-----------------------------------------------------------------------
FORM  testec2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        wa       TYPE st_key_dd03l,
        itab     TYPE tab_key_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  DO 5 TIMES.
    wa-position = 1.
    APPEND wa TO itab.
  ENDDO.

*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         APPENDING TABLE lt_dd03l
         FROM  dd03l
         FOR ALL ENTRIES IN itab
         WHERE tabname  = 'DD03L'
         AND   position = itab-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testec2
*-----------------------------------------------------------------------
FORM  testec3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        wa       TYPE st_key_dd03l,
        itab     TYPE tab_key_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  DO 20 TIMES.
    wa-position = 1.
    APPEND wa TO itab.
    wa-position = 2.
    APPEND wa TO itab.
    wa-position = 3.
    APPEND wa TO itab.
    wa-position = 4.
    APPEND wa TO itab.
    wa-position = 5.
    APPEND wa TO itab.
    wa-position = 6.
    APPEND wa TO itab.
    wa-position = 7.
    APPEND wa TO itab.
    wa-position = 8.
    APPEND wa TO itab.
    wa-position = 9.
    APPEND wa TO itab.
    wa-position = 10.
    APPEND wa TO itab.
  ENDDO.

*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT itab INTO wa.
    SELECT *
           APPENDING TABLE lt_dd03l
           FROM  dd03l
           WHERE tabname  = 'DD03L'
           AND   position = wa-position.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testec3
*-----------------------------------------------------------------------
FORM  testec4.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        wa       TYPE st_key_dd03l,
        itab     TYPE tab_key_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  DO 20 TIMES.
    wa-position = 1.
    APPEND wa TO itab.
    wa-position = 2.
    APPEND wa TO itab.
    wa-position = 3.
    APPEND wa TO itab.
    wa-position = 4.
    APPEND wa TO itab.
    wa-position = 5.
    APPEND wa TO itab.
    wa-position = 6.
    APPEND wa TO itab.
    wa-position = 7.
    APPEND wa TO itab.
    wa-position = 8.
    APPEND wa TO itab.
    wa-position = 9.
    APPEND wa TO itab.
    wa-position = 10.
    APPEND wa TO itab.
  ENDDO.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         APPENDING TABLE lt_dd03l
         FROM  dd03l
         FOR ALL ENTRIES IN itab
         WHERE tabname  = 'DD03L'
         AND   position = itab-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testec4
*-----------------------------------------------------------------------
*  EN : DISTINCT
** EN1: SELECT tabname LIKE 'DD%'
** EN2: SELECT DISTINCT tabname LIKE 'DD%'
** EN3: SELECT tabname LIKE 'DD%'    SORT DELETE ADJACENT DUPLICATES
*-----------------------------------------------------------------------
FORM  testen1.

  DATA:
    lt_tabname  TYPE STANDARD TABLE OF dd03l-tabname.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname
         INTO  TABLE lt_tabname
         FROM  dd03l
         WHERE tabname  LIKE 'DD%'
         AND   position LE   pos_search.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_tabname LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testen1
*-----------------------------------------------------------------------
FORM  testen2.

  DATA:
    lt_tabname  TYPE STANDARD TABLE OF dd03l-tabname.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT DISTINCT tabname
         INTO  TABLE lt_tabname
         FROM  dd03l
         WHERE tabname  LIKE 'DD%'
         AND   position LE   pos_search.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_tabname LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testen2
*-----------------------------------------------------------------------
FORM  testen3.

  DATA:
        lt_tabname TYPE STANDARD TABLE OF dd03l-tabname,
        sysubrc    LIKE sy-subrc.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname
         INTO  TABLE lt_tabname
         FROM  dd03l
         WHERE tabname  LIKE 'DD%'
         AND   position LE   pos_search.
  sysubrc = sy-subrc.
  SORT lt_tabname.
  DELETE ADJACENT DUPLICATES FROM lt_tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sysubrc EQ 0 ).
    DESCRIBE TABLE lt_tabname LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testen3
*-----------------------------------------------------------------------
*  EX : UP TO 1 ROWS - SELECT SINGLE -
** EX1: UP TO 1 ROWS with fully specified key
** EX2: UP TO 1 ROWS with range select
** EX3: UP TO 1 ROWS with large selectfully specified key
*-----------------------------------------------------------------------
FORM  testex1.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname = 'DD03L'
         AND   fieldname = 'TABNAME'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testex1
*-----------------------------------------------------------------------
FORM  testex2.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname  = 'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testex2
*-----------------------------------------------------------------------
FORM  testex3.

  DATA:
    lt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname  LIKE 'D%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testex3
*-----------------------------------------------------------------------
*  EY : UP TO 1 ROWS - SELECT SINGLE -
** EY1: SELECT SINGLE  with fully specified key
** EY2: SELECT SINGLE  with range select
** EY3: SELECT SINGLE  with large selectfully specified key
*-----------------------------------------------------------------------
FORM  testey1.

  DATA:
    ls_dd03l    LIKE  dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname = 'DD03L'
         AND   fieldname = 'TABNAME'
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info1 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testey1
*-----------------------------------------------------------------------
FORM  testey2.

  DATA:
    ls_dd03l    LIKE  dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname  = 'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testey2
*-----------------------------------------------------------------------
FORM  testey3.

  DATA:
    ls_dd03l    LIKE  dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE *
         INTO  ls_dd03l
         FROM  dd03l
         WHERE tabname  LIKE 'D%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info3 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testey3
*-----------------------------------------------------------------------
*  FA :
** FA1: SELECT * No sort-order
** FA2: SELECT * ORDER        BY position fieldname tabname
** FA3: SELECT * SORT itab    BY position fieldname tabname
** FA4: SELECT * INTO SORTED KEY position fieldname tabname
*-----------------------------------------------------------------------
FORM  testfa1.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        t        TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfa1
*-----------------------------------------------------------------------
FORM  testfa2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        t        TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         ORDER BY position fieldname tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfa2
*-----------------------------------------------------------------------
FORM  testfa3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        t        TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  SORT lt_dd03l BY position fieldname tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfa3
*-----------------------------------------------------------------------
FORM  testfa4.

  DATA:
    sort_dd03l   TYPE SORTED TABLE OF dd03l
                 WITH NON-UNIQUE KEY position fieldname tabname,
    t            TYPE  i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE sort_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE sort_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfa4
*-----------------------------------------------------------------------
*  FB : Different ORDER BY   together with FA1 and FA2
** FB3: SELECT * ORDER BY position DESCENDING fieldname tabname
** FB4: SELECT * ORDER BY PRIMARY key
*-----------------------------------------------------------------------
FORM  testfb3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        t        TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         ORDER BY fieldname position DESCENDING fieldname tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfb3
*-----------------------------------------------------------------------
FORM  testfb4.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        t        TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         ORDER BY PRIMARY KEY.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfb4
*-----------------------------------------------------------------------
*  FD : ORDER BY and UP TO 10 ROWS
** FD1: SELECT ORDER BY position tabname fieldname DESCENDING
** FD2: SELECT UP TO 10 ROWS
** FD3: SELECT UP TO 10 ROWS  ORDER BY position tabname
** FD4: SELECT UP TO 10 ROWS  ORDER BY PRIMARY KEY
*-----------------------------------------------------------------------
FORM  testfd1.

  PERFORM testfb3.
  info1 = info3.

ENDFORM.                                                    "testfd2
*-----------------------------------------------------------------------
FORM  testfd2.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfd2
*-----------------------------------------------------------------------
FORM  testfd3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l,
        lv_max   LIKE dd03l-position.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         ORDER BY position DESCENDING fieldname tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  READ TABLE lt_dd03l
     INTO ls_dd03l
     INDEX 1.

  SELECT MAX( position )
         INTO  lv_max
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '.

  IF ( sy-subrc EQ 0 AND lv_max = ls_dd03l-position ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfd3
*-----------------------------------------------------------------------
FORM  testfd4.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l,
        lv_max   LIKE dd03l-tabname.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         ORDER BY PRIMARY KEY.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testfd4
*-----------------------------------------------------------------------
*  GA : Aggregates or own programm
** GA1: SELECT zaehl t006 ENDSELECT  IF ( max )          BYPASS BUFFER
** GA2: SELECT zaehl t006 INTO TABLE  LOOP max  ENDLOOP  BYPASS BUFFER
** GA3: SELECT MAX( zaehl ) t006
** GA4: SELECT zaehl t006 ENDSELECT  IF ( max )
** GA5: SELECT zaehl t006 INTO TABLE  LOOP max  ENDLOOP
*-----------------------------------------------------------------------
FORM  testga1.

  DATA:
        lv_zaehl  LIKE t006-zaehl,
        max_zaehl LIKE t006-zaehl.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  CLEAR   max_zaehl.

  SELECT zaehl
         INTO  lv_zaehl
         FROM  t006
         BYPASSING BUFFER.
    IF ( lv_zaehl > max_zaehl ).
      max_zaehl = lv_zaehl.
    ENDIF.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = max_zaehl / 1000.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testga1
*-----------------------------------------------------------------------
FORM  testga2.

  DATA:
        lt_zaehl  TYPE STANDARD TABLE OF t006-zaehl,
        lv_zaehl  LIKE t006-zaehl,
        max_zaehl LIKE t006-zaehl.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_zaehl.
  CLEAR   max_zaehl.

  SELECT zaehl
         INTO  TABLE lt_zaehl
         FROM  t006
         BYPASSING BUFFER.

  LOOP AT lt_zaehl INTO lv_zaehl.
    IF ( lv_zaehl > max_zaehl ).
      max_zaehl = lv_zaehl.
    ENDIF.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_zaehl LINES info1.
    info3 = max_zaehl / 1000.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testga2
*-----------------------------------------------------------------------
FORM  testga3.

  DATA:
    max_zaehl  TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  CLEAR   max_zaehl.
  SELECT MAX( zaehl )
         INTO  max_zaehl
         FROM  t006.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info4 = max_zaehl / 1000.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testga3
*-----------------------------------------------------------------------
FORM  testga4.

  DATA:
        lv_zaehl  LIKE t006-zaehl,
        max_zaehl LIKE t006-zaehl.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  CLEAR   max_zaehl.

  SELECT zaehl
         INTO  lv_zaehl
         FROM  t006.
    IF ( lv_zaehl > max_zaehl ).
      max_zaehl = lv_zaehl.
    ENDIF.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = max_zaehl / 1000.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testga4
*-----------------------------------------------------------------------
FORM  testga5.

  DATA:
        lt_zaehl  TYPE STANDARD TABLE OF t006-zaehl,
        lv_zaehl  LIKE t006-zaehl,
        max_zaehl LIKE t006-zaehl.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_zaehl.
  CLEAR   max_zaehl.

  SELECT zaehl
         INTO  TABLE lt_zaehl
         FROM  t006.

  LOOP AT lt_zaehl INTO lv_zaehl.
    IF ( lv_zaehl > max_zaehl ).
      max_zaehl = lv_zaehl.
    ENDIF.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_zaehl LINES info1.
    info3 = max_zaehl / 1000.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testga5
*-----------------------------------------------------------------------
*  GG : Existence Check
** GG1: SELECT *
** GG2: SELECT COUNT(*)
** GG2: SELECT COUNT(*)  UP TO 1 ROWS
** GG3: SELECT key1 UP TO 1 ROWS (Index-only poss)
*-----------------------------------------------------------------------
FORM  testgg1.

  PERFORM testab2.
  info1 = info2.

ENDFORM.                                                    "testgg1
*-----------------------------------------------------------------------
FORM  testgg2.

  DATA:
    lv_count   TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*)
         INTO  lv_count
         FROM  dd03l
         WHERE tabname  LIKE 'D%'.
  GET RUN TIME FIELD stop.

*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = lv_count.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testgg2
*-----------------------------------------------------------------------
FORM  testgg3.

  DATA:
    lv_count   TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*)
         INTO  lv_count
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname  LIKE 'D%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info3 = lv_count.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testgg3
*-----------------------------------------------------------------------
FORM  testgg4.

  DATA:
    ls_dd03l    LIKE  dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  ls_dd03l
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname  LIKE 'D%'.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info4 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testgg4
*-----------------------------------------------------------------------
*  GH : Existence Check
** GH1: SELECT *      UP TO 1 ROWS
** GH2: SELECT field  UP TO 1 ROWS
** GH3: SELECT key1   UP TO 1 ROWS (Index-only poss)
*-----------------------------------------------------------------------
FORM  testgh1.

  PERFORM testgg4.
  info1 = info4.
  CLEAR info4.

ENDFORM.                                                    "testgh1
*-----------------------------------------------------------------------
FORM  testgh2.

  DATA:
    lv_inttype   LIKE  dd03l-inttype.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT inttype
         INTO  lv_inttype
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname  LIKE 'D%'.
  ENDSELECT.
  GET RUN TIME FIELD stop.

*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testgh2
*-----------------------------------------------------------------------
FORM  testgh3.

  DATA:
    lv_as4local    LIKE  dd03l-as4local.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT as4local
         INTO  lv_as4local
         FROM  dd03l
         UP TO 1 ROWS
         WHERE tabname  LIKE 'D%'.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info3 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testgh3
*-----------------------------------------------------------------------
*  GP : Package Size / Open Cursor
** GP1: SELECT/ENDSELECT - PACKAGE SIZE
** GP2: OPEN CURSOR - FETCH NEXT CURSOR - PACKAGE SIZE
*-----------------------------------------------------------------------
FORM  testgp1.

  DATA:
        lt_dd03l TYPE STANDARD TABLE OF dd03l,
        kt_dd03l TYPE STANDARD TABLE OF dd03l,
        dbcur1   TYPE cursor.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  OPEN CURSOR dbcur1 FOR
    SELECT *
           FROM  dd03l
           WHERE tabname LIKE 'DD%'.

  DO 20 TIMES.
    FETCH NEXT CURSOR dbcur1
          INTO TABLE lt_dd03l
          PACKAGE SIZE 1000.
* blockwise processing:
    APPEND LINES OF lt_dd03l TO kt_dd03l.
* the following refresh is necesary otherwise the last records are
* processed twice
    REFRESH lt_dd03l.
  ENDDO.
  CLOSE CURSOR dbcur1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info1.
  DESCRIBE TABLE kt_dd03l LINES info2.
  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testgp1
*-----------------------------------------------------------------------
FORM  testgp2.

  DATA:
        lt_dd03l TYPE STANDARD TABLE OF dd03l,
        kt_dd03l TYPE STANDARD TABLE OF dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO TABLE lt_dd03l
         FROM  dd03l
         PACKAGE SIZE 1000
         WHERE tabname LIKE 'DD%'.
* blockwise processing:
    APPEND LINES OF lt_dd03l TO kt_dd03l.

  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info3.
  DESCRIBE TABLE kt_dd03l LINES info4.
  IF ( info4 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testgp2
************************************************************************
* IJKL: Number of Accesses
************************************************************************
*-----------------------------------------------------------------------
*  IA : SELECT / ENDSELECT versus SELECT INTO TABLE
** IA1: Array  select ... endselect
*  IA2: Array  select into table
*-----------------------------------------------------------------------
FORM  testia1.

  DATA:
        ls_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_dd03l.
  SELECT *
         INTO ls_dd03l
         FROM dd03l
         UP TO n ROWS
         WHERE tabname LIKE 'A%'.
    APPEND ls_dd03l TO lt_dd03l.

  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info1 = n.
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testIA1
*-----------------------------------------------------------------------
FORM  testia2.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        t        TYPE i.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_dd03l.
  SELECT *
         INTO TABLE lt_dd03l
         FROM dd03l
         UP TO n ROWS
         WHERE tabname LIKE 'A%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testIA2
*-----------------------------------------------------------------------
*  IB : SELECT / ENDSELECT versus SELECT INTO TABLE with processing
** IB1: SELECT / ENDSELECT Maximum of Positions
** IB2: SELECT INTO TABLE  Maximum of Positions
*-----------------------------------------------------------------------
FORM  testib1.

  DATA:
        ls_dd03l   LIKE dd03l,
        lt_dd03l   TYPE tab_dd03l,
        lv_pos_max LIKE dd03l-position.

  xx      = 'N'.
  CLEAR   tt.
  lv_pos_max = 0.
  REFRESH lt_dd03l.
*-------------------------------------
  GET RUN TIME FIELD start.
  lv_pos_max = 0.
  SELECT *
         INTO ls_dd03l
         FROM dd03l
         UP TO n ROWS
         WHERE tabname LIKE 'A%'.
    APPEND ls_dd03l TO lt_dd03l.

    IF ( ls_dd03l-position > lv_pos_max ).
      lv_pos_max = ls_dd03l-position.
    ENDIF.


  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    info2 = lv_pos_max.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testib1
*-----------------------------------------------------------------------
FORM  testib2.

  DATA:
        lt_dd03l   TYPE tab_dd03l,
        lv_pos_max LIKE dd03l-position,
        t          TYPE i.

  FIELD-SYMBOLS: <fs> TYPE dd03l.

  xx         = 'N'.
  lv_pos_max = 0.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO TABLE lt_dd03l
         FROM dd03l
         UP TO n ROWS
         WHERE tabname LIKE 'A%'.

  lv_pos_max = 0.
  LOOP AT lt_dd03l ASSIGNING <fs>.
    IF ( <fs>-position > lv_pos_max ).
      lv_pos_max = <fs>-position.
    ENDIF.
  ENDLOOP.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    info4 = lv_pos_max.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testib2
*-----------------------------------------------------------------------
*  IX : SELECT/ENDSELECT versus SELECT INTO TABLE (SE30 Tips & Tricks)
** IX1: Array  SELECT/ENDSELECT     BYPASSING BUFFER
** IX2: Array  SELECT INTO TABLE    BYPASSING BUFFER
** IX3: Array  SELECT/ENDSELECT
** IX4: Array  SELECT INTO TABLE
*-----------------------------------------------------------------------
FORM  testix1.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO ls_t006
         FROM t006
         BYPASSING BUFFER.
    APPEND ls_t006 TO lt_t006.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testix1
*-----------------------------------------------------------------------
FORM  testix2.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO TABLE lt_t006
         FROM t006
         BYPASSING BUFFER.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testix2
*-----------------------------------------------------------------------
FORM  testix3.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO ls_t006
         FROM t006.
*         BYPASSING BUFFER.
    APPEND ls_t006 TO lt_t006.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testix3
*-----------------------------------------------------------------------
FORM  testix4.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO TABLE lt_t006
         FROM t006.
*         BYPASSING BUFFER.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testix4
*-----------------------------------------------------------------------
*  IY : SELECT/ENDSELECT versus SELECT INTO TABLE (SE30 Tips & Tricks)
** IY1: Array  SELECT/ENDSELECT     BYPASSING BUFFER
** IY2: Array  SELECT INTO TABLE    BYPASSING BUFFER
** IY3: Array  SELECT/ENDSELECT
** IY4: Array  SELECT INTO TABLE
*-----------------------------------------------------------------------
FORM  testiy1.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO ls_t006
         FROM t006
         BYPASSING BUFFER.
*    APPEND ls_t006 TO lt_t006.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testiy1
*-----------------------------------------------------------------------
FORM  testiy2.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  FIELD-SYMBOLS: <fs>.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO TABLE lt_t006
         FROM t006
         BYPASSING BUFFER.

  LOOP AT lt_t006
          ASSIGNING <fs>.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testiy2
*-----------------------------------------------------------------------
FORM  testiy3.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO ls_t006
         FROM t006.
*         BYPASSING BUFFER.
*    APPEND ls_t006 TO lt_t006.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testiy3
*-----------------------------------------------------------------------
FORM  testiy4.

  DATA:
        ls_t006 LIKE t006,
        lt_t006 TYPE tab_t006.

  FIELD-SYMBOLS: <fs>.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_t006.
  SELECT *
         INTO TABLE lt_t006
         FROM t006.
*         BYPASSING BUFFER.
  LOOP AT lt_t006
          ASSIGNING <fs>.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t006 LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testiy4
*-----------------------------------------------------------------------
*  JA : RANGES TABLE
** JA1: SELECT SINGLE in LOOP
** JA2: SELECT RANGES table
** JA3: Test: Identical records by range SELECT
*-----------------------------------------------------------------------
FORM  testja1.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        kt_dd02l TYPE tab_dd02l,
        ls_dd02l LIKE dd02l,
        ks_dd02l LIKE dd02l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname as4local as4vers
         INTO  TABLE lt_dd02l
         FROM  dd02l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local = 'A'
         AND   as4vers  = ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT lt_dd02l INTO ls_dd02l.
    SELECT SINGLE *
           INTO  ks_dd02l
           FROM  dd02l
           WHERE tabname  = ls_dd02l-tabname
           AND   as4local = 'A'
           AND   as4vers  = ' '.
    APPEND ks_dd02l TO kt_dd02l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE kt_dd02l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testja1

*-----------------------------------------------------------------------
FORM  testja2.

  DATA:
        lv_tabname LIKE dd02l-tabname,
        kt_dd02l   TYPE tab_dd02l.

* ranges table
  RANGES tabnames FOR dd02l-tabname OCCURS 0.

*  DATA:  rangetab     TYPE RANGE OF dbtab-field1,
*        wa_rangetab  LIKE LINE OF rangetab.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  lv_tabname
         FROM  dd02l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local = 'A'
         AND   as4vers  = ' '.

    tabnames-low    = lv_tabname.
    tabnames-sign   = 'I'.
    tabnames-option = 'EQ'.
    APPEND tabnames.

  ENDSELECT.

*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd02l
         FROM  dd02l
         WHERE tabname  IN tabnames
         AND   as4local = 'A'
         AND   as4vers  = ' ' .
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE kt_dd02l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testja2
*-----------------------------------------------------------------------
FORM  testja3.

  DATA:
    lt_dd02l  TYPE  tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_dd02l
         FROM  dd02l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local = 'A'
         AND   as4vers  = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testja3
*-----------------------------------------------------------------------
*  KA : FOR ALL ENTRIES - like JA
** KA1: SELECT  FAE  3 fields
** KA2: SELECT  FAE  only 1 field
*-----------------------------------------------------------------------
FORM  testka1.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        kt_dd02l TYPE tab_dd02l.

* ranges table
*  RANGES tabnames FOR dd02l-tabname OCCURS 0.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname as4local as4vers
         INTO  TABLE lt_dd02l
         FROM  dd02l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local = 'A'
         AND   as4vers  = ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd02l
         FROM  dd02l
         FOR ALL ENTRIES IN lt_dd02l
         WHERE tabname  = lt_dd02l-tabname
         AND   as4local = lt_dd02l-as4local
         AND   as4vers  = lt_dd02l-as4vers.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE kt_dd02l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testka1
*-----------------------------------------------------------------------
FORM  testka2.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        kt_dd02l TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  TABLE lt_dd02l
         FROM  dd02l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local = 'A'
         AND   as4vers  = ' '.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd02l
         FROM  dd02l
         FOR ALL ENTRIES IN lt_dd02l
         WHERE tabname  = lt_dd02l-tabname
         AND   as4local = 'A'
         AND   as4vers  = ' ' .
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE kt_dd02l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testka2
*-----------------------------------------------------------------------
*  KC : Empty FOR ALL ENTRIES
** KC1: FAE 100 lines in itab
** KC2: FAE   0 lines in itab
** KC3: FAE 100 lines in itab 2.condition
** KC4: FAE   0 lines in itab 2.condition
*-----------------------------------------------------------------------
FORM  testkc1.

  DATA:
        lt_dd35l TYPE tab_dd35l,
        kt_dd35l TYPE tab_dd35l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  TABLE lt_dd35l
         FROM  dd35l
         UP TO 100 ROWS
         WHERE tabname  LIKE 'S%'.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd35l
         FROM  dd35l
         FOR ALL ENTRIES IN lt_dd35l
         WHERE tabname  = lt_dd35l-tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  DESCRIBE TABLE lt_dd35l LINES info0.

  IF ( sy-subrc EQ 0 AND info0 = '100' ).
    DESCRIBE TABLE kt_dd35l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkc1
*-----------------------------------------------------------------------
FORM  testkc2.

  DATA:
        lt_dd35l TYPE tab_dd35l,
        kt_dd35l TYPE tab_dd35l.

  xx      = 'N'.
  CLEAR   tt.

  REFRESH lt_dd35l.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd35l
         FROM  dd35l
         FOR ALL ENTRIES IN lt_dd35l
         WHERE tabname  = lt_dd35l-tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  DESCRIBE TABLE lt_dd35l LINES info0.

  IF ( sy-subrc EQ 0 AND info0 = '0' ).
    DESCRIBE TABLE kt_dd35l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkc2
*-----------------------------------------------------------------------
FORM  testkc3.

  DATA:
        lt_dd35l TYPE tab_dd35l,
        kt_dd35l TYPE tab_dd35l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  TABLE lt_dd35l
         FROM  dd35l
         UP TO 100 ROWS
         WHERE tabname  LIKE 'S%'.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd35l
         FROM  dd35l
         FOR ALL ENTRIES IN lt_dd35l
         WHERE tabname  = lt_dd35l-tabname
         AND   fieldname LIKE 'CL%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  DESCRIBE TABLE lt_dd35l LINES info0.

  IF ( sy-subrc EQ 0 AND info0 = '100' ).
    DESCRIBE TABLE kt_dd35l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkc3
*-----------------------------------------------------------------------
FORM  testkc4.

  DATA:
        lt_dd35l TYPE tab_dd35l,
        kt_dd35l TYPE tab_dd35l.

  xx      = 'N'.
  CLEAR   tt.

  REFRESH lt_dd35l.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd35l
         FROM  dd35l
         FOR ALL ENTRIES IN lt_dd35l
         WHERE tabname  = lt_dd35l-tabname
         AND   fieldname LIKE 'CL%'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  DESCRIBE TABLE lt_dd35l LINES info0.

  IF ( sy-subrc EQ 0 AND info0 = '0' ).
    DESCRIBE TABLE kt_dd35l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkc4
*-----------------------------------------------------------------------
*  KG : FAE with duplicate lines in itab
** KG2: FAE COUNT(*)  info1  and info2
** KG3: FAE reduces duplicates
** KG4: FAE with DELETE ADJACENT DUPLICATES before SELECT
*-----------------------------------------------------------------------
FORM  testkg2.

  DATA:
        nt_dd03l TYPE tab_dd03l,
        lt_dd03l TYPE tab_dd03l,
        numb     TYPE i.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  TABLE nt_dd03l
         FROM  dd03l
         UP TO 100 ROWS
         WHERE tabname  LIKE 'D%'
         AND   as4local = 'A'
         AND   as4vers  = ' '
         AND   position = 1.

  DO gv_n1 TIMES.
    APPEND LINES OF nt_dd03l TO lt_dd03l.
  ENDDO.

  IF ( gv_l1 > 1 ).
    APPEND LINES OF nt_dd03l
           FROM 1 TO gv_l1
           TO lt_dd03l.
  ENDIF.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*)
         INTO  numb
         FROM  dd03l
         FOR ALL ENTRIES IN lt_dd03l
         WHERE tabname  = lt_dd03l-tabname
         AND   as4local = 'A'
         AND   as4vers  = ' '
         AND   position = 1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    info2 = numb.

    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkg2
*-----------------------------------------------------------------------
FORM  testkg3.

  DATA:
        nt_dd03l TYPE tab_dd03l,
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  TABLE nt_dd03l
         FROM  dd03l
         UP TO 100 ROWS
         WHERE tabname  LIKE 'D%'
         AND   as4local = 'A'
         AND   as4vers  = ' '
         AND   position = 1.

  DO gv_n1 TIMES.
    APPEND LINES OF nt_dd03l TO lt_dd03l.
  ENDDO.

  IF ( gv_l1 > 1 ).
    APPEND LINES OF nt_dd03l
           FROM 1 TO gv_l1
           TO lt_dd03l.
  ENDIF.
*---------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE kt_dd03l
         FROM  dd03l
         FOR ALL ENTRIES IN lt_dd03l
         WHERE tabname  = lt_dd03l-tabname
         AND   as4local = 'A'
         AND   as4vers  = ' '
         AND   position = 1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  DESCRIBE TABLE lt_dd03l LINES info2.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE kt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkg3
*-----------------------------------------------------------------------
FORM  testkg4.

  DATA:
        nt_dd03l TYPE tab_dd03l,
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname
         INTO  TABLE nt_dd03l
         FROM  dd03l
         UP TO 100 ROWS
         WHERE tabname  LIKE 'D%'
         AND   as4local = 'A'
         AND   as4vers  = ' '
         AND   position = 1.

  DO gv_n1 TIMES.
    APPEND LINES OF nt_dd03l TO lt_dd03l.
  ENDDO.

  IF ( gv_l1 > 1 ).
    APPEND LINES OF nt_dd03l
           FROM 1 TO gv_l1
           TO lt_dd03l.
  ENDIF.
*---------------------------------------
  GET RUN TIME FIELD start.
  SORT lt_dd03l BY tabname.
  DELETE ADJACENT DUPLICATES FROM lt_dd03l
         COMPARING tabname.

  SELECT *
         INTO  TABLE kt_dd03l
         FROM  dd03l
         FOR ALL ENTRIES IN lt_dd03l
         WHERE tabname  = lt_dd03l-tabname
         AND   as4local = 'A'
         AND   as4vers  = ' '
         AND   position = 1.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE kt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkg4
*-----------------------------------------------------------------------
*  KK : FAE with 1:1 Relation Combination of Results
** KK1: FAE 1:1 Relation  SELECT
** KK2: FAE 1:1 Relation  LOOP/READ Standard Table
** KK3: FAE 1:1 Relation  LOOP/READ Standard Table RB
** KK4: FAE 1:1 Relation  LOOP/READ Sorted Table
** KK5: FAE 1:1 Relation  LOOP/READ Hashed Table
*-----------------------------------------------------------------------
FORM  testkk1.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_key_dd03l,
        kt_dd03l TYPE HASHED TABLE OF dd03l
                WITH  UNIQUE KEY tabname,
    mt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '1'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  =  '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkk1
*-----------------------------------------------------------------------
FORM  testkk2.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_key_dd03l,
        kt_dd03l TYPE tab_dd03l,
        mt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '1'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  =  '1'.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT lt_dd03l INTO ls_dd03l.
    READ TABLE kt_dd03l
         INTO ks_dd03l
         WITH KEY tabname  = ls_dd03l-tabname.

    IF ( sy-subrc EQ 0 ).
      APPEND ks_dd03l TO mt_dd03l.
    ENDIF.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE mt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testkk2
*-----------------------------------------------------------------------
FORM  testkk3.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_key_dd03l,
        kt_dd03l TYPE tab_dd03l,
        mt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '1'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  =  '1'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SORT kt_dd03l BY tabname.
  LOOP AT lt_dd03l INTO ls_dd03l.
    READ TABLE kt_dd03l
         INTO ks_dd03l
         WITH KEY tabname  = ls_dd03l-tabname
         BINARY SEARCH.

    IF ( sy-subrc EQ 0 ).
      APPEND ks_dd03l TO mt_dd03l.
    ENDIF.
  ENDLOOP.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE mt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testkk3

*-----------------------------------------------------------------------
FORM  testkk4.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_key_dd03l,
        kt_dd03l TYPE SORTED TABLE OF dd03l
                WITH  UNIQUE KEY tabname,
    mt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '1'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  =  '1'.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT lt_dd03l INTO ls_dd03l.
    READ TABLE kt_dd03l
         INTO ks_dd03l
         WITH TABLE KEY tabname  = ls_dd03l-tabname.

    IF ( sy-subrc EQ 0 ).
      APPEND ks_dd03l TO mt_dd03l.
    ENDIF.
  ENDLOOP.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE mt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testkk4
*-----------------------------------------------------------------------
FORM  testkk5.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_key_dd03l,
        kt_dd03l TYPE HASHED TABLE OF dd03l
                WITH  UNIQUE KEY tabname,
    mt_dd03l    TYPE  tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '1'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  =  '1'.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT lt_dd03l INTO ls_dd03l.
    READ TABLE kt_dd03l
         INTO ks_dd03l
         WITH TABLE KEY tabname  = ls_dd03l-tabname.

    IF ( sy-subrc EQ 0 ).
      APPEND ks_dd03l TO mt_dd03l.
    ENDIF.
  ENDLOOP.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkk5
*-----------------------------------------------------------------------
*  KL : FAE with 1:C Relation ( C = 10)
** KL1: FAE 1:C Relation  SELECT
** KL2: FAE 1:C Relation  LOOP/LOOP WHERE  Standard Table
** KL3: FAE 1:C Relation  LOOP/LOOP WHERE  Standard Table BINARY SEARCH
** KL4: FAE 1:C Relation  LOOP/LOOP WHERE  Sorted Table
*-----------------------------------------------------------------------
FORM  testkl1.

  DATA:
        tabix    LIKE sy-tabix,
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        ms_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE SORTED TABLE OF dd03l
                     WITH NON-UNIQUE KEY tabname,
    mt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '10'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  LE  '10'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testkl1
*-----------------------------------------------------------------------
FORM  testkl2.

  DATA:
        tabix    LIKE sy-tabix,
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        ms_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_key_dd03l,
        kt_dd03l TYPE tab_dd03l,
        mt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '10'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  LE  '10'.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT lt_dd03l INTO ls_dd03l.
    LOOP AT kt_dd03l INTO ks_dd03l
            WHERE  tabname = ls_dd03l-tabname.

      APPEND ks_dd03l TO mt_dd03l.
    ENDLOOP.
  ENDLOOP.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE mt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testkl2
*-----------------------------------------------------------------------
FORM  testkl3.

  DATA:
        tabix    LIKE sy-tabix,
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        ms_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE tab_dd03l,
        mt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '10'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  LE  '10'.
*-------------------------------------
  GET RUN TIME FIELD start.

  SORT lt_dd03l BY tabname.
  LOOP AT lt_dd03l INTO ls_dd03l.
    READ TABLE kt_dd03l
         WITH KEY tabname = ls_dd03l-tabname
         TRANSPORTING NO FIELDS
         BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      tabix = sy-tabix.
      LOOP AT kt_dd03l INTO ks_dd03l
           FROM tabix.
        IF ( ks_dd03l-tabname NE ls_dd03l-tabname ).
          EXIT.
        ENDIF.
        APPEND ks_dd03l TO mt_dd03l.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE mt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testkl3
*-----------------------------------------------------------------------
FORM  testkl4.

  DATA:
        tabix    LIKE sy-tabix,
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        ms_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE SORTED TABLE OF dd03l
                     WITH NON-UNIQUE KEY tabname,
    mt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
* Read tabname with more than 10 fields => 1:10 FAE possible:
  SELECT tabname
         INTO  TABLE lt_dd03l
         FROM  dd03l
         UP TO n ROWS
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   position =    '10'.

  SELECT  *
          INTO  TABLE kt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN lt_dd03l
          WHERE tabname   =  lt_dd03l-tabname
          AND   as4local  =  'A'
          AND   as4vers   =  ' '
          AND   position  LE  '10'.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT lt_dd03l INTO ls_dd03l.
    LOOP AT kt_dd03l INTO ks_dd03l
         WHERE tabname = ls_dd03l-tabname.

      APPEND ks_dd03l TO mt_dd03l.
    ENDLOOP.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE mt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testkl4
************************************************************************
* MNOP: Fieldlist - Into corresponding
************************************************************************
*-----------------------------------------------------------------------
*  MA : SELECT range
** MA1: SELECT range  *  INTO FIELDS OF st
** MA2: SELECT range 293 INTO FIELDS OF st_293
** MA3: SELECT range 244 INTO FIELDS OF st_244
** MA4: SELECT range 139 INTO FIELDS OF st_139
** MA5: SELECT range  70 INTO FIELDS OF st_70
*-----------------------------------------------------------------------
FORM  testma1.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  *
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testma1
*-----------------------------------------------------------------------
FORM  testma2.

  DATA:
    lt_dd03l_293    TYPE tab_293_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen     reftable  precfield  reffield
          conrout notnull    datatype  leng       decimals
          domname shlporigin tabletype depth      comptype
          reftype languflag
          INTO  TABLE lt_dd03l_293
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l_293 LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testma2
*-----------------------------------------------------------------------
FORM  testma3.

  DATA:
    lt_dd03l_244    TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen     reftable  precfield  reffield
          conrout notnull    datatype
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l_244
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l_244 LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testma3
*-----------------------------------------------------------------------
FORM  testma4.

  DATA:
    lt_dd03l_139    TYPE tab_139_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen
*          REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l_139
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l_139 LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testma4
*-----------------------------------------------------------------------
FORM  testma5.

  DATA:
    lt_dd03l    TYPE tab_70_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag
*          MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info0.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testma5
*-----------------------------------------------------------------------
*  MB : SELECT FOR ALL ENTRIES (FAE)
** MB1: SELECT FAE  *  INTO FIELDS OF st
** MB2: SELECT FAE 293 INTO FIELDS OF st_293
** MB3: SELECT FAE 244 INTO FIELDS OF st_244
** MB4: SELECT FAE 139 INTO FIELDS OF st_139
** MB5: SELECT FAE  70 INTO FIELDS OF st_70
*-----------------------------------------------------------------------
FORM  testmb1.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  *
          INTO  TABLE lt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN kt_dd03l
          WHERE tabname   = kt_dd03l-tabname
          AND   fieldname = kt_dd03l-fieldname
          AND   as4local  = kt_dd03l-as4local
          AND   as4vers   = kt_dd03l-as4vers
          AND   position  = kt_dd03l-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmb1
*-----------------------------------------------------------------------
FORM  testmb2.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_293_dd03l,
        lt_dd03l TYPE tab_293_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen     reftable  precfield  reffield
          conrout notnull    datatype  leng       decimals
          domname shlporigin tabletype depth      comptype
          reftype languflag
          INTO  TABLE lt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN kt_dd03l
          WHERE tabname   = kt_dd03l-tabname
          AND   fieldname = kt_dd03l-fieldname
          AND   as4local  = kt_dd03l-as4local
          AND   as4vers   = kt_dd03l-as4vers
          AND   position  = kt_dd03l-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmb2
*-----------------------------------------------------------------------
FORM  testmb3.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_244_dd03l,
        lt_dd03l TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen     reftable  precfield  reffield
          conrout notnull    datatype
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN kt_dd03l
          WHERE tabname   = kt_dd03l-tabname
          AND   fieldname = kt_dd03l-fieldname
          AND   as4local  = kt_dd03l-as4local
          AND   as4vers   = kt_dd03l-as4vers
          AND   position  = kt_dd03l-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmb3
*-----------------------------------------------------------------------
FORM  testmb4.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_139_dd03l,
        lt_dd03l TYPE tab_139_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen
*          REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN kt_dd03l
          WHERE tabname   = kt_dd03l-tabname
          AND   fieldname = kt_dd03l-fieldname
          AND   as4local  = kt_dd03l-as4local
          AND   as4vers   = kt_dd03l-as4vers
          AND   position  = kt_dd03l-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmb4
*-----------------------------------------------------------------------
FORM  testmb5.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_70_dd03l,
        lt_dd03l TYPE tab_70_dd03l.


  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag
*          MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN kt_dd03l
          WHERE tabname   = kt_dd03l-tabname
          AND   fieldname = kt_dd03l-fieldname
          AND   as4local  = kt_dd03l-as4local
          AND   as4vers   = kt_dd03l-as4vers
          AND   position  = kt_dd03l-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info0.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmb5
*-----------------------------------------------------------------------
*  MC : SELECT SINGLE
** MC1: SELECT SINGLE  *  INTO FIELDS OF st
** MC2: SELECT SINGLE 293 INTO FIELDS OF st_293
** MC3: SELECT SINGLE 244 INTO FIELDS OF st_244
** MC4: SELECT SINGLE 139 INTO FIELDS OF st_139
** MC5: SELECT SINGLE  70 INTO FIELDS OF st_70
*-----------------------------------------------------------------------
FORM  testmc1.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE *
            INTO  ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmc1
*-----------------------------------------------------------------------
FORM  testmc2.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_293_dd03l,
        lt_dd03l TYPE tab_293_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            tabname fieldname  as4local  as4vers    position
            keyflag mandatory  rollname  checktable adminfield
            inttype intlen     reftable  precfield  reffield
            conrout notnull    datatype  leng       decimals
            domname shlporigin tabletype depth      comptype
            reftype languflag
            INTO  ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmc2
*-----------------------------------------------------------------------
FORM  testmc3.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_244_dd03l,
        lt_dd03l TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            tabname fieldname  as4local  as4vers    position
            keyflag mandatory  rollname  checktable adminfield
            inttype intlen     reftable  precfield  reffield
            conrout notnull    datatype
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
            INTO  ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmc3
*-----------------------------------------------------------------------
FORM  testmc4.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_139_dd03l,
        lt_dd03l TYPE tab_139_dd03l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            tabname fieldname  as4local  as4vers    position
            keyflag mandatory  rollname  checktable adminfield
            inttype intlen
*          REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
            INTO  ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmc4
*-----------------------------------------------------------------------
FORM  testmc5.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_70_dd03l,
        lt_dd03l TYPE tab_70_dd03l.


  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            tabname fieldname  as4local  as4vers    position
            keyflag
*          MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
            INTO  ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info0.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmc5
*-----------------------------------------------------------------------
*  MD : SELECT range  - Full Table Scan
** MD1: SELECT range   * INTO FIELDS OF st      (full table scan)
** MD2: SELECT range 293 INTO FIELDS OF st_293  (full table scan)
** MD3: SELECT range 244 INTO FIELDS OF st_244  (full table scan)
** MD4: SELECT range 139 INTO FIELDS OF st_139  (full table scan)
** MD5: SELECT range  70 INTO FIELDS OF st_70   (full table scan)
*-----------------------------------------------------------------------
FORM  testmd1.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  *
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE fieldname = 'TABNAME'
          AND   as4local  = 'A'
          AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmd1
*-----------------------------------------------------------------------
FORM  testmd2.

  DATA:
    lt_dd03l    TYPE tab_293_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen     reftable  precfield  reffield
          conrout notnull    datatype  leng       decimals
          domname shlporigin tabletype depth      comptype
          reftype languflag
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE fieldname = 'TABNAME'
          AND   as4local  = 'A'
          AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmd2
*-----------------------------------------------------------------------
FORM  testmd3.

  DATA:
    lt_dd03l    TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen     reftable  precfield  reffield
          conrout notnull    datatype
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE fieldname = 'TABNAME'
          AND   as4local  = 'A'
          AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmd3
*-----------------------------------------------------------------------
FORM  testmd4.

  DATA:
    lt_dd03l    TYPE tab_139_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
          keyflag mandatory  rollname  checktable adminfield
          inttype intlen
*          REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE fieldname = 'TABNAME'
          AND   as4local  = 'A'
          AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmd4
*-----------------------------------------------------------------------
FORM  testmd5.

  DATA:
    lt_dd03l    TYPE tab_70_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname    fieldname  as4local  as4vers    position
          keyflag
*          MANDATORY ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE    INTLEN   REFTABLE  PRECFIELD  REFFIELD
*          CONROUT    NOTNULL  DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE fieldname = 'TABNAME'
          AND   as4local  = 'A'
          AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info0.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmd5
*-----------------------------------------------------------------------
*  MI : SELECT range INTO CORRESPONDING FIELDS  (244)
** MI1: ... alphabetic
** MI2: ... random order
** MI3: ... INTO TABLE  (MA3)
** MI4: ... more fields, random
*-----------------------------------------------------------------------
FORM  testmi1.

  DATA:
    lt_dd03l    TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  adminfield as4local  as4vers  checktable conrout
          datatype   fieldname intlen   inttype    keyflag
          mandatory  notnull   position precfield  reffield
          reftable   rollname  tabname
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmi1
*-----------------------------------------------------------------------
FORM  testmi2.

  DATA:
    lt_dd03l    TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname   fieldname rollname   checktable reftable
          precfield reffield  conrout    datatype   as4local
          keyflag   mandatory adminfield inttype    notnull
          intlen    as4vers   position
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmi2
*-----------------------------------------------------------------------
FORM  testmi4.

  DATA:
    lt_dd03l    TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  depth     as4vers   position   intlen    leng
          decimals  as4local  keyflag    mandatory adminfield
          inttype   notnull   shlporigin tabletype comptype
          reftype   languflag datatype   conrout   tabname
          fieldname rollname  checktable reftable  precfield
          reffield  domname
          INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmi4
*-----------------------------------------------------------------------
*  MJ : SELECT SINGLE INTO CORRESPONDING FIELDS  (244)
** MJ1: ... alphabetic
** MJ2: ... random order
** MC3: ... INTO TABLE
** MJ4: ... more fields, random
*-----------------------------------------------------------------------
FORM  testmj1.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_244_dd03l,
        lt_dd03l TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            adminfield as4local  as4vers  checktable conrout
            datatype   fieldname intlen   inttype    keyflag
            mandatory  notnull   position precfield  reffield
            reftable   rollname  tabname
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
            INTO  CORRESPONDING FIELDS OF ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmj1
*-----------------------------------------------------------------------
FORM  testmj2.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_244_dd03l,
        lt_dd03l TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            tabname   fieldname rollname   checktable reftable
            precfield reffield  conrout    datatype   as4local
            keyflag   mandatory adminfield inttype    notnull
            intlen    as4vers   position
*          LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
            INTO  CORRESPONDING FIELDS OF ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmj2
*-----------------------------------------------------------------------
FORM  testmj4.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_244_dd03l,
        lt_dd03l TYPE tab_244_dd03l.

  xx      = 'N'.
  CLEAR   tt.
  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            depth     as4vers   position   intlen    leng
            decimals  as4local  keyflag    mandatory adminfield
            inttype   notnull   shlporigin tabletype comptype
            reftype   languflag datatype   conrout   tabname
            fieldname rollname  checktable reftable  precfield
            reffield  domname
            INTO  CORRESPONDING FIELDS OF ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testmj4

*-----------------------------------------------------------------------
*  NA : SELECT range
** NA1: SELECT range key+1 INTO FIELDS OF st_70
** NA2: SELECT range key   INTO FIELDS OF st_key
*-----------------------------------------------------------------------
FORM  testna1.

  PERFORM testma5.
  info1 = info0.

ENDFORM.                                                    "testna1
*-----------------------------------------------------------------------
FORM  testna2.

  DATA:
    lt_dd03l    TYPE tab_key_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
*          KEYFLAG MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testna2
*-----------------------------------------------------------------------
*  NB : SELECT FOR ALL ENTRIES (FAE)
** NB1: SELECT FAE key+1 INTO FIELDS OF st_70
** NB2: SELECT FAE key   INTO FIELDS OF st_key
*-----------------------------------------------------------------------
FORM  testnb1.

  PERFORM testmb5.
  info1 = info0.

ENDFORM.                                                    "testnb1
*-----------------------------------------------------------------------
FORM  testnb2.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_key_dd03l,
        lt_dd03l TYPE tab_key_dd03l.


  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
*          KEYFLAG MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          FOR ALL ENTRIES IN kt_dd03l
          WHERE tabname   = kt_dd03l-tabname
          AND   fieldname = kt_dd03l-fieldname
          AND   as4local  = kt_dd03l-as4local
          AND   as4vers   = kt_dd03l-as4vers
          AND   position  = kt_dd03l-position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testnb2
*-----------------------------------------------------------------------
*  NC : SELECT SINGLE
** NC1: SELECT SINGLE key+1 INTO FIELDS OF st_70
** NC2: SELECT SINGLE key   INTO FIELDS OF st_key
*-----------------------------------------------------------------------
FORM  testnc1.

  PERFORM testmc5.
  info1 = info0.

ENDFORM.                                                    "testnc1
*-----------------------------------------------------------------------
FORM  testnc2.

  DATA:
        ks_dd03l TYPE st_key_dd03l,
        kt_dd03l TYPE tab_key_dd03l,
        ls_dd03l TYPE st_key_dd03l,
        lt_dd03l TYPE tab_key_dd03l.


  xx      = 'N'.
  CLEAR   tt.

  SELECT  tabname fieldname as4local as4vers position
          INTO  TABLE kt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE tabname  LIKE 'DD0%'
          AND   as4local =    'A'
          AND   as4vers  =    ' '.
*-------------------------------------
  GET RUN TIME FIELD start.
  LOOP AT kt_dd03l INTO ks_dd03l.
    SELECT  SINGLE
            tabname fieldname  as4local  as4vers    position
*          KEYFLAG MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
            INTO  ls_dd03l
            FROM  dd03l
            WHERE tabname   = ks_dd03l-tabname
            AND   fieldname = ks_dd03l-fieldname
            AND   as4local  = ks_dd03l-as4local
            AND   as4vers   = ks_dd03l-as4vers
            AND   position  = ks_dd03l-position.
    APPEND ls_dd03l TO lt_dd03l.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testnc2
*-----------------------------------------------------------------------
*  ND : SELECT range - full table scan/ index full scan
** ND1: SELECT range key +1 INTO FIELDS OF st_70  (full table scan)
** ND2: SELECT range key    INTO FIELDS OF st_key (index full scan)
*-----------------------------------------------------------------------
FORM  testnd1.

  PERFORM testmd5.
  info1 = info0.

ENDFORM.                                                    "testmd1
*-----------------------------------------------------------------------
FORM  testnd2.

  DATA:
    lt_dd03l    TYPE tab_key_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT  tabname fieldname  as4local  as4vers    position
*          KEYFLAG MANDATORY  ROLLNAME  CHECKTABLE ADMINFIELD
*          INTTYPE INTLEN     REFTABLE  PRECFIELD  REFFIELD
*          CONROUT NOTNULL    DATATYPE  LENG       DECIMALS
*          DOMNAME SHLPORIGIN TABLETYPE DEPTH      COMPTYPE
*          REFTYPE LANGUFLAG
          INTO  TABLE lt_dd03l
          FROM  dd03l
          UP TO n ROWS
          WHERE fieldname = 'TABNAME'
          AND   as4local  = 'A'
          AND   as4vers   = ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testnd2
************************************************************************
* QRST: Special
************************************************************************
*-----------------------------------------------------------------------
*   QA : CLIENT SPECIFIED
**  QA1: SELECT no mandt - automatic handling
**  QA2: SELECT CLIENT SPECIFIED  sy-mandt
**  QA3: SELECT CLIENT SPECIFIED  '000'
**  QA3: SELECT CLIENT SPECIFIED  all mandt from T000
*-----------------------------------------------------------------------
FORM  testqa1.

  DATA:
    lt_t005f   TYPE STANDARD TABLE OF t005f.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_t005f
         FROM  t005f
         WHERE spras   =  'D'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t005f LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqa1
*-----------------------------------------------------------------------
FORM  testqa2.

  DATA:
    lt_t005f   TYPE STANDARD TABLE OF t005f.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_t005f
         FROM  t005f
         CLIENT SPECIFIED
         WHERE mandt = sy-mandt
         AND   spras =  'D'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t005f LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqa2
*-----------------------------------------------------------------------
FORM  testqa3.

  DATA:
    lt_t005f   TYPE STANDARD TABLE OF t005f.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_t005f
         FROM  t005f
         CLIENT SPECIFIED
         WHERE mandt = '000'
         AND   spras =  'D'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t005f LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqa3
*-----------------------------------------------------------------------
FORM  testqa4.

  DATA:
    lt_t005f   TYPE STANDARD TABLE OF t005f.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO  TABLE lt_t005f
         FROM  t005f
         CLIENT SPECIFIED
         WHERE mandt IN ( SELECT mandt FROM t000 )
         AND   spras =  'D'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_t005f LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqa4


*-----------------------------------------------------------------------
*   QD : Dynamic Coding
**  QD1: SELECT  range on dynamic for reference
**  QD2: dynamic FROM-clause
**  QD3: dynamic WHERE-clause
**  QD4: dynamic fieldlist
*-----------------------------------------------------------------------
FORM  testqd1.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
        INTO  TABLE lt_dd03l
        FROM  dd03l
        WHERE tabname   =  'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqd1
*-----------------------------------------------------------------------
FORM  testqd2.

  DATA:
        lt_dd03l     TYPE tab_dd03l,
        dyn_from(10) TYPE c.

  CLEAR   tt.
  xx       = 'N'.
  dyn_from = 'DD03L'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO  TABLE lt_dd03l
         FROM  (dyn_from)
         WHERE tabname   =  'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqd2
*-----------------------------------------------------------------------
FORM  testqd3.

  DATA:
        lt_dd03l      TYPE tab_dd03l,
        a_tab         LIKE dd03l-tabname,
        dyn_where(20) TYPE c.


  CLEAR     tt.
  xx        = 'N'.
  a_tab     = 'DD03L'.
  dyn_where = 'tabname = a_tab'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE (dyn_where).
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqd3
*-----------------------------------------------------------------------
FORM  testqd4.

  DATA:
        lt_dd03l        TYPE tab_dd03l,
        dyn_fields(100) TYPE c.


  CLEAR      tt.
  xx         = 'N'.
  dyn_fields = 'tabname fieldname as4local as4vers position'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT (dyn_fields)
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   =  'DD03L'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testqd4
*-----------------------------------------------------------------------
*   QM : Native SQL
**  QM1: SELECT SINGLE           DD03L
**  QM2: EXEC SQL SELECT SINGLE  DD03L
**  QM3: SELECT SINGLE           DD30L  single-record buffered
**  QM4: EXEC SQL SELECT SINGLE  DD30L  single-record buffered
*-----------------------------------------------------------------------
FORM  testqm1.

  DATA:
        ls_dd03l     TYPE dd03l,
        lv_tabname   LIKE dd03l-tabname,
        lv_fieldname LIKE dd03l-fieldname,
        lv_as4local  LIKE dd03l-as4local,
        lv_as4vers   LIKE dd03l-as4vers,
        lv_position  LIKE dd03l-position,
        lv_tabname_2 LIKE dd03l-tabname.

  CLEAR   tt.
  xx           = 'N'.
  lv_tabname   = 'DD03L'.
  lv_fieldname = 'FIELDNAME'.
  lv_as4local  = 'A'.
  lv_as4vers   = ' '.
  lv_position  = '2'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE tabname
        INTO  lv_tabname_2
        FROM  dd03l
        WHERE tabname   = lv_tabname
        AND   fieldname = lv_fieldname
        AND   as4local  = lv_as4local
        AND   as4vers   = lv_as4vers
        AND   position  = lv_position.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info1 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqm1
*-----------------------------------------------------------------------
FORM  testqm2.

  DATA:
        ls_dd03l     TYPE dd03l,
        lv_tabname   LIKE dd03l-tabname,
        lv_fieldname LIKE dd03l-fieldname,
        lv_as4local  LIKE dd03l-as4local,
        lv_as4vers   LIKE dd03l-as4vers,
        lv_position  LIKE dd03l-position,
        lv_tabname_2 LIKE dd03l-tabname.

  CLEAR   tt.
  xx           = 'N'.
  lv_tabname   = 'DD03L'.
  lv_fieldname = 'FIELDNAME'.
  lv_as4local  = 'A'.
  lv_as4vers   = ' '.
  lv_position  = '2'.
*-------------------------------------
  GET RUN TIME FIELD start.
  EXEC SQL.
    SELECT tabname
           INTO :lv_tabname_2
           FROM  dd03l
           WHERE tabname   = :lv_tabname
           AND   fieldname = :lv_fieldname
           AND   as4local  = :lv_as4local
           AND   as4vers   = :lv_as4vers
           AND   position  = :lv_position
  ENDEXEC.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqm2
*-----------------------------------------------------------------------
FORM  testqm3.

  DATA:
        ls_dd03l      TYPE dd30l,
        lv_shlpname   LIKE dd30l-shlpname,
        lv_shlpname_2 LIKE dd30l-shlpname,
        lv_as4local   LIKE dd30l-as4local.

  CLEAR   tt.
  xx          = 'N'.
  lv_shlpname = 'DD_SHLP'.
  lv_as4local = 'A'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE shlpname
        INTO  lv_shlpname_2
        FROM  dd30l
        WHERE shlpname = lv_shlpname
        AND   as4local = lv_as4local.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info3 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqm3
*-----------------------------------------------------------------------
FORM  testqm4.

  DATA:
        ls_dd03l      TYPE dd30l,
        lv_shlpname   LIKE dd30l-shlpname,
        lv_shlpname_2 LIKE dd30l-shlpname,
        lv_as4local   LIKE dd30l-as4local.

  CLEAR   tt.
  xx          = 'N'.
  lv_shlpname = 'DD_SHLP'.
  lv_as4local = 'A'.
*-------------------------------------
  GET RUN TIME FIELD start.
  EXEC SQL.
    SELECT shlpname
           INTO :lv_shlpname_2
           FROM  dd30l
           WHERE shlpname  = :lv_shlpname
           AND   as4local  = :lv_as4local
  ENDEXEC.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info4 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqm4
*-----------------------------------------------------------------------
*   QN :   Native SQL
**  QN1:   SELECT SINGLE           T005U         single-record buffered
**  QN2:   EXEC SQL SELECT SINGLE  T005U         single-record buffered
**  QN3:   EXEC SQL SELECT SINGLE  T005U  mandt  single-record buffered
*-----------------------------------------------------------------------
FORM  testqn1.

  DATA:
        ls_t005u TYPE t005u,
        lv_spras LIKE t005u-spras,
        lv_land1 LIKE t005u-land1,
        lv_bland LIKE t005u-bland,
        lv_bezei LIKE t005u-bezei.

  CLEAR   tt.
  xx       = 'N'.
  lv_spras = 'D'.
  lv_land1 = 'DE'.
  lv_bland = 'BW'.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT SINGLE bezei
        INTO  lv_bezei
        FROM  t005u
        WHERE spras = lv_spras
        AND   land1 = lv_land1
        AND   bland = lv_bland.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info1 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqn1
*-----------------------------------------------------------------------
FORM  testqn2.

  DATA:
        ls_t005u TYPE t005u,
        lv_spras LIKE t005u-spras,
        lv_land1 LIKE t005u-land1,
        lv_bland LIKE t005u-bland,
        lv_bezei LIKE t005u-bezei.

  CLEAR   tt.
  xx       = 'N'.
  lv_spras = 'D'.
  lv_land1 = 'DE'.
  lv_bland = 'BW'.
*-------------------------------------
  GET RUN TIME FIELD start.
  EXEC SQL.
    SELECT bezei
           INTO :lv_bezei
           FROM  t005u
           WHERE spras  = :lv_spras
           AND   land1  = :lv_land1
           AND   bland  = :lv_bland
  ENDEXEC.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info2 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqn2
*-----------------------------------------------------------------------
FORM  testqn3.

  DATA:
        ls_t005u TYPE t005u,
        lv_mandt LIKE t005u-mandt,
        lv_spras LIKE t005u-spras,
        lv_land1 LIKE t005u-land1,
        lv_bland LIKE t005u-bland,
        lv_bezei LIKE t005u-bezei.

  CLEAR   tt.
  xx       = 'N'.
  lv_mandt = '000'.
  lv_spras = 'D'.
  lv_land1 = 'DE'.
  lv_bland = 'BW'.
*-------------------------------------
  GET RUN TIME FIELD start.
  EXEC SQL.
    SELECT bezei
           INTO :lv_bezei
           FROM  t005u
           WHERE mandt  = :lv_mandt
           AND   spras  = :lv_spras
           AND   land1  = :lv_land1
           AND   bland  = :lv_bland
  ENDEXEC.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    info3 = 1.
    xx    = 'Y'.
    tt    = t.
  ENDIF.
ENDFORM.                                                    "testqn3
************************************************************************
*  S  : Nested SELECTs: JOINs, FAE, Subquery
************************************************************************
*-----------------------------------------------------------------------
*  SA : Nested SELECT 1
** SA0: Improve Nested Selects - Status quo
** SA1: Improve Nested Selects - FOR ALL ENTRIES
** SA2: Improve Nested Selects - Subquery
** SA3: Improve Nested Selects - Join
** SA4: Improve Nested Selects - View
** SA7: Improve Nested Selects - Independent SELECTs
*-----------------------------------------------------------------------
FORM  testsa0.

  DATA:
        ls_dd02l LIKE dd02l,
        lt_dd02l TYPE tab_dd02l,
        ls_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname as4local as4vers
         INTO  ls_dd02l
         FROM  dd02l
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   tabclass =    'TRANSP'.

    SELECT tabname fieldname position
           INTO CORRESPONDING FIELDS OF ls_dd03l
           FROM dd03l
           WHERE tabname  =  ls_dd02l-tabname
           AND   as4local =  ls_dd02l-as4local
           AND   as4vers  =  ls_dd02l-as4vers
           AND   position = '1'.

      IF ( sy-subrc EQ 0 ).
        APPEND ls_dd02l TO lt_dd02l.
        APPEND ls_dd03l TO lt_dd03l.
      ENDIF.
    ENDSELECT.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsa0
*-----------------------------------------------------------------------
FORM  testsa1.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname as4local as4vers
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
         AND   as4vers  =    ' '
         AND   tabclass =    'TRANSP'.

  IF NOT ( lt_dd02l IS INITIAL ).
    SELECT tabname fieldname position keyflag
           INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
           FROM dd03l
           FOR ALL ENTRIES IN lt_dd02l
           WHERE tabname  =   lt_dd02l-tabname
           AND   as4local =   lt_dd02l-as4local
           AND   as4vers  =   lt_dd02l-as4vers
           AND   position =   '1'.
  ENDIF.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsa1
*-----------------------------------------------------------------------
FORM  testsa2.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname position
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM dd03l
         WHERE tabname   IN ( SELECT tabname
                                     FROM  dd02l
                                     WHERE tabname  LIKE tab_search
                                     AND   as4local =    'A'
                                     AND   as4vers  =    ' '
                                     AND   tabclass =    'TRANSP' )
         AND as4local = 'A'
         AND as4vers  = ' '
         AND position = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsa2
*-----------------------------------------------------------------------
FORM  testsa3.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT b~tabname  b~fieldname  b~position  b~keyflag
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM       dd02l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname  = a~tabname
         AND   b~as4local = a~as4local
         AND   b~as4vers  = a~as4vers
         WHERE a~tabname  LIKE tab_search
         AND   a~as4local =    'A'
         AND   a~as4vers  =    ' '
         AND   a~tabclass =    'TRANSP'
         AND   b~position =    '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsa3
*-----------------------------------------------------------------------
FORM  testsa4.

  DATA:
    lt_dd03vv    TYPE STANDARD TABLE OF dd03vv.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname  fieldname  position  keyflag
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03vv
         FROM   dd03vv
         WHERE tabname  LIKE tab_search
         AND   as4local =    'A'
*         AND   as4vers  =    ' '
         AND   tabclass =    'TRANSP'
         AND   position =    '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03vv LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsa4
*-----------------------------------------------------------------------
FORM  testsa7.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        ls_dd02l LIKE dd02l,
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l,
        kt_dd03l TYPE tab_dd03l,
        kt_dd02l TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname as4local as4vers
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname  LIKE tab_search
         AND   as4local =   'A'
         AND   as4vers  =   ' '
         AND   tabclass =    'TRANSP'.

  LOOP AT lt_dd02l INTO ls_dd02l.
    SELECT tabname fieldname position keyflag
           APPENDING CORRESPONDING FIELDS OF TABLE lt_dd03l
           FROM dd03l
           WHERE tabname  = ls_dd02l-tabname
           AND   as4local = ls_dd02l-as4local
           AND   as4vers  = ls_dd02l-as4vers
           AND   position = '1'.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    SORT lt_dd02l BY tabname.
    LOOP AT lt_dd03l INTO ls_dd03l.
      READ TABLE lt_dd02l
           TRANSPORTING NO FIELDS
           WITH KEY tabname = ls_dd03l
           BINARY SEARCH.
      IF ( sy-subrc NE 0 ).
        APPEND ls_dd03l TO kt_dd03l.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE lt_dd02l LINES info2.
    DESCRIBE TABLE lt_dd03l LINES info3.
    DESCRIBE TABLE kt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsa7
*-----------------------------------------------------------------------
*  SD : All client-dependent tables, client field, position, keyflag
** SD0: Improve Nested Selects - Independent SELECTs
** SD1: Improve Nested Selects - Status quo
** SD2: Improve Nested Selects - Join
** SD3: Improve Nested Selects - FOR ALL ENTRIES
** SD4: Improve Nested Selects - Subquery
*-----------------------------------------------------------------------
FORM  testsd0.

  DATA:
        ls_dd02l LIKE dd02l,
        lt_dd02l TYPE tab_dd02l,
        ls_dd03l LIKE dd03l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_dd03l.
  REFRESH lt_dd03l.

  SELECT tabname
         INTO  ls_dd02l
         FROM  dd02l
         WHERE tabname  LIKE 'A%'
         AND   tabclass =    'TRANSP'
         AND   clidep   =    'X'.

    SELECT tabname fieldname position keyflag
           INTO CORRESPONDING FIELDS OF ls_dd03l
           FROM dd03l
           WHERE tabname   =  ls_dd02l-tabname
           AND   fieldname IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT').

      IF ( sy-subrc EQ 0 ).
        APPEND ls_dd02l TO lt_dd02l.
        APPEND ls_dd03l TO lt_dd03l.
      ENDIF.
    ENDSELECT.
  ENDSELECT.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsd0
*-----------------------------------------------------------------------
FORM  testsd1.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname  LIKE 'A%'
         AND   tabclass =    'TRANSP'
         AND   clidep   =    'X'.

  IF NOT ( lt_dd02l IS INITIAL ).
    SELECT tabname fieldname position keyflag
           INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
           FROM dd03l
           FOR ALL ENTRIES IN lt_dd02l
           WHERE tabname   =  lt_dd02l-tabname
           AND   fieldname IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT').
  ENDIF.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsd1
*-----------------------------------------------------------------------
FORM  testsd2.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname position keyflag
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM dd03l
         WHERE tabname   IN ( SELECT tabname
                                     FROM  dd02l
                                     WHERE tabname  LIKE 'A%'
                                     AND   tabclass =    'TRANSP'
                                     AND   clidep   =    'X' )
         AND   fieldname IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT').

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsd2
*-----------------------------------------------------------------------
FORM  testsd3.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT b~tabname  b~fieldname  b~position  b~keyflag
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM       dd02l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname   = a~tabname
         WHERE a~tabname   LIKE 'A%'
         AND   a~tabclass =    'TRANSP'
         AND   a~clidep    =    'X'
         AND   b~fieldname IN   ('MANDANT', 'MANDT', 'CLIENT', 'CLNT').
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsd3
*-----------------------------------------------------------------------
FORM  testsd5.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_dd03l.

  SELECT b~tabname  b~fieldname  b~position  b~keyflag
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM       dd02l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname   = a~tabname
         WHERE a~tabname   LIKE 'A%'
         AND   a~tabclass =    'TRANSP'
         AND   a~clidep    =    'X'
         AND   b~fieldname IN   ('MANDANT', 'MANDT', 'CLIENT', 'CLNT')
*         %_HINTS ADABAS 'ORDERED'.
          %_HINTS ORACLE 'USE_NL(T_01) ORDERED'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsd5
*-----------------------------------------------------------------------
FORM  testsd7.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        ls_dd02l LIKE dd02l,
        lt_dd03l TYPE tab_dd03l,
        ls_dd03l LIKE dd03l,
        kt_dd03l TYPE tab_dd03l,
        kt_dd02l TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  REFRESH lt_dd03l.
  REFRESH lt_dd03l.

  SELECT tabname
         INTO  TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname  LIKE 'A%'
         AND   tabclass =    'TRANSP'
         AND   clidep   =    'X'.

  SELECT tabname fieldname position keyflag
         INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM dd03l
         WHERE tabname   LIKE 'A%'
         AND   fieldname IN   ('MANDANT', 'MANDT', 'CLIENT', 'CLNT').
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

* get the one which are in lt_dd03l but not in lt_dd03l
  SORT lt_dd02l BY tabname.
  LOOP AT lt_dd03l INTO ls_dd03l.
    READ TABLE lt_dd02l
         TRANSPORTING NO FIELDS
         WITH KEY tabname = ls_dd03l-tabname
         BINARY SEARCH.
    IF ( sy-subrc NE 0 ).
      APPEND ls_dd03l TO kt_dd03l.
    ENDIF.
  ENDLOOP.

  SELECT tabname tabclass clidep
           INTO CORRESPONDING FIELDS OF TABLE kt_dd02l
           FROM dd02l
           FOR ALL ENTRIES IN kt_dd03l
           WHERE tabname   =  kt_dd03l-tabname.

  SORT kt_dd02l BY tabclass clidep.
  SORT lt_dd03l BY position keyflag.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info1.
    DESCRIBE TABLE lt_dd03l LINES info2.
    DESCRIBE TABLE kt_dd03l LINES info3.
    DESCRIBE TABLE kt_dd02l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testsd7
************************************************************************
* UV: JOINs, FAE,
************************************************************************
*-----------------------------------------------------------------------
*  UF : Flexibility
** UF1: JOIN with 2 tables (A,B) and 2 ranges
** UF2: FOR ALL ENTRIES A then B
** UF3: FOR ALL ENTRIES B then A
*----------------------------------------------------------------------
FORM  testuf1.

  DATA:
    lt_dd02_03l   TYPE tab_dd02_03l.

* ranges table
  RANGES r_clidep    FOR dd02l-clidep OCCURS 0.
  RANGES fieldnames  FOR dd03l-fieldname OCCURS 0.

  r_clidep-sign   = 'I'.
  r_clidep-option = 'EQ'.
  r_clidep-low    = 'X'.
  APPEND r_clidep.

  fieldnames-sign   = 'I'.
  fieldnames-option = 'EQ'.

  fieldnames-low     = 'MANDT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLIENT'.
  APPEND fieldnames.
  fieldnames-low     = 'MANDANT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLNT'.
  APPEND fieldnames.

  IF     ( search = '0' ).
*  refresh r_clidep.
*  refresh fieldnames.
  ELSEIF ( search = '1' ).
    REFRESH r_clidep.
*  refresh fieldnames.
  ELSEIF ( search = '2' ).
*  refresh r_clidep.
    REFRESH fieldnames.
  ENDIF.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname  a~as4local a~as4vers b~fieldname b~position a~clidep
         INTO TABLE lt_dd02_03l
         FROM       dd02l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname   = a~tabname
         AND   b~as4local  = a~as4local
         AND   b~as4vers   = a~as4vers
         WHERE a~tabname   LIKE tab_search
         AND   a~as4local  =    'A'
         AND   a~as4vers   =    ' '
         AND   a~clidep    IN   r_clidep
         AND   b~position  =    '1'
         AND   b~fieldname IN   fieldnames.

*  SORT lt_dd02_03l
*       BY tabname as4local as4vers fieldname position clidep.
*  DELETE ADJACENT DUPLICATES FROM lt_dd02_03l
*       COMPARING tabname as4local as4vers fieldname position clidep.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd02_03l LINES info1.
  IF ( info1 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf1
*----------------------------------------------------------------------
FORM  testuf2.

  DATA:
        wa02_03  TYPE st_dd02_03l,
        wa02     TYPE dd02l,
        wa03     LIKE dd03l,
        lt_dd02l TYPE HASHED TABLE OF dd02l
                  WITH UNIQUE KEY tabname as4local as4vers,
        lt_dd03l    TYPE tab_dd03l,
        lt_dd02_03l TYPE tab_dd02_03l.

* ranges table
  RANGES r_clidep    FOR dd02l-clidep OCCURS 0.
  RANGES fieldnames  FOR dd03l-fieldname OCCURS 0.

  r_clidep-sign   = 'I'.
  r_clidep-option = 'EQ'.
  r_clidep-low    = 'X'.
  APPEND r_clidep.

  fieldnames-sign   = 'I'.
  fieldnames-option = 'EQ'.

  fieldnames-low     = 'MANDT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLIENT'.
  APPEND fieldnames.
  fieldnames-low     = 'MANDANT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLNT'.
  APPEND fieldnames.

  IF     ( search = '0' ).
*  refresh r_clidep.
*  refresh fieldnames.
  ELSEIF ( search = '1' ).
    REFRESH r_clidep.
*  refresh fieldnames.
  ELSEIF ( search = '2' ).
*  refresh r_clidep.
    REFRESH fieldnames.
  ENDIF.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname  as4local as4vers tabclass
     INTO TABLE lt_dd02l
     FROM       dd02l
     WHERE tabname  LIKE tab_search
     AND   as4local =    'A'
     AND   as4vers  =    ' '
     AND   clidep  IN    r_clidep.

  IF ( sy-subrc EQ 0 ).
    SELECT tabname fieldname as4local as4vers position
           INTO TABLE lt_dd03l
           FROM dd03l
           FOR ALL ENTRIES IN lt_dd02l
           WHERE tabname   =  lt_dd02l-tabname
           AND   as4local  =  lt_dd02l-as4local
           AND   as4vers   =  lt_dd02l-as4vers
           AND   position  =  '1'
           AND   fieldname IN fieldnames.

    LOOP AT lt_dd03l INTO wa03.
      READ TABLE lt_dd02l INTO wa02
           WITH TABLE KEY tabname  = wa03-tabname
                          as4local = wa03-as4local
                          as4vers  = wa03-as4vers.
      IF ( sy-subrc EQ 0 ).
        wa02_03-tabname   = wa02-tabname.
        wa02_03-as4local  = wa02-as4local.
        wa02_03-as4vers   = wa02-as4vers.
        wa02_03-fieldname = wa03-fieldname.
        wa02_03-position  = wa03-position.
        wa02_03-clidep    = wa02-clidep.
        APPEND wa02_03 TO lt_dd02_03l.
      ENDIF.
    ENDLOOP.

*    SORT lt_dd02_03l
*         BY tabname as4local as4vers fieldname position clidep.
*    DELETE ADJACENT DUPLICATES FROM lt_dd02_03l
*         COMPARING tabname as4local as4vers fieldname position clidep.
  ENDIF.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd02_03l LINES info2.
  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf2
*-----------------------------------------------------------------------
FORM  testuf3.

  DATA:
        wa02_03     TYPE st_dd02_03l,
        wa02        TYPE dd02l,
        wa03        LIKE dd03l,
        ks_dd03l    LIKE dd03l,
        lt_dd02_03l TYPE tab_dd02_03l,
        lt_dd02l    TYPE tab_dd02l,
        lt_dd03l    TYPE HASHED TABLE OF st_key_dd03l
                  WITH UNIQUE KEY tabname position as4local as4vers.


* ranges table
  RANGES r_clidep    FOR dd02l-clidep OCCURS 0.
  RANGES fieldnames  FOR dd03l-fieldname OCCURS 0.

  r_clidep-sign   = 'I'.
  r_clidep-option = 'EQ'.
  r_clidep-low    = 'X'.
  APPEND r_clidep.

  fieldnames-sign   = 'I'.
  fieldnames-option = 'EQ'.

  fieldnames-low     = 'MANDT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLIENT'.
  APPEND fieldnames.
  fieldnames-low     = 'MANDANT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLNT'.
  APPEND fieldnames.

  IF     ( search = '0' ).
*  refresh r_clidep.
*  refresh fieldnames.
  ELSEIF ( search = '1' ).
    REFRESH r_clidep.
*  refresh fieldnames.
  ELSEIF ( search = '2' ).
*  refresh r_clidep.
    REFRESH fieldnames.
  ENDIF.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO TABLE lt_dd03l
         FROM dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  =    'A'
         AND   as4vers   =    ' '
         AND   position  =    '1'
         AND   fieldname IN   fieldnames.

  IF ( sy-subrc EQ 0 ).
    SELECT tabname  as4local as4vers clidep
           INTO TABLE lt_dd02l
           FROM dd02l
           FOR ALL ENTRIES IN lt_dd03l
           WHERE tabname   =  lt_dd03l-tabname
           AND   as4local  =  lt_dd03l-as4local
           AND   as4vers   =  lt_dd03l-as4vers
           AND   clidep    IN r_clidep.

    LOOP AT lt_dd02l INTO wa02.
      READ TABLE lt_dd03l INTO wa03
           WITH TABLE KEY tabname  = wa02-tabname
                          position = 1
                          as4local = wa02-as4local
                          as4vers  = wa02-as4vers.
      IF ( sy-subrc EQ 0 ).
        wa02_03-tabname   = wa02-tabname.
        wa02_03-as4local  = wa02-as4local.
        wa02_03-as4vers   = wa02-as4vers.
        wa02_03-fieldname = wa03-fieldname.
        wa02_03-position  = wa03-position.
        wa02_03-clidep    = wa02-clidep.
        APPEND wa02_03 TO lt_dd02_03l.
      ENDIF.
    ENDLOOP.

*    SORT lt_dd02_03l
*         BY tabname as4local as4vers fieldname position clidep.
*    DELETE ADJACENT DUPLICATES FROM lt_dd02_03l
*         COMPARING tabname as4local as4vers fieldname position clidep.
  ENDIF.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd02_03l LINES info3.
  IF ( info3 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf3
*-----------------------------------------------------------------------
*  UF : Flexibility
** UF5: same SELECT only on A - range not empty
** UF6: same SELECT only on A - range empty
** UF7: same SELECT only on B - range not empty
** UF8: same SELECT only on B - range empty
*-----------------------------------------------------------------------
FORM  testuf5.

  DATA:
    lt_dd02l      TYPE tab_dd02l.

* ranges table
  RANGES r_clidep FOR dd02l-clidep OCCURS 0.

  r_clidep-sign   = 'I'.
  r_clidep-option = 'EQ'.
  r_clidep-low    = 'X'.
  APPEND r_clidep.

*  refresh r_clidep.
  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname  as4local as4vers
         INTO TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE tab_search
         AND   as4local  =    'A'
         AND   as4vers   =    ' '
         AND   clidep    IN   r_clidep.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf5
*-----------------------------------------------------------------------
FORM  testuf6.

  DATA:
    lt_dd02l      TYPE tab_dd02l.

* ranges table
  RANGES r_clidep FOR dd02l-clidep OCCURS 0.

  r_clidep-sign   = 'I'.
  r_clidep-option = 'EQ'.
  r_clidep-low    = 'X'.
  APPEND r_clidep.

  REFRESH r_clidep.
  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname  as4local as4vers
         INTO TABLE lt_dd02l
         FROM  dd02l
         WHERE tabname   LIKE tab_search
         AND   as4local  =    'A'
         AND   as4vers   =    ' '
         AND   clidep    IN   r_clidep.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf6
*-----------------------------------------------------------------------
FORM  testuf7.

  DATA:
    lt_dd03l      TYPE tab_dd03l.

* ranges table
  RANGES fieldnames  FOR dd03l-fieldname OCCURS 0.

  fieldnames-sign   = 'I'.
  fieldnames-option = 'EQ'.

  fieldnames-low     = 'MANDT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLIENT'.
  APPEND fieldnames.
  fieldnames-low     = 'MANDANT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLNT'.
  APPEND fieldnames.

*  refresh fieldnames.
  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO TABLE lt_dd03l
         FROM dd03l
         WHERE tabname   LIKE tab_search
         AND   position  =    '1'
         AND   fieldname IN   fieldnames
         AND   as4local  =    'A'
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf7
*-----------------------------------------------------------------------
FORM  testuf8.

  DATA:
    lt_dd03l      TYPE tab_dd03l.

* ranges table
  RANGES fieldnames  FOR dd03l-fieldname OCCURS 0.

  fieldnames-sign   = 'I'.
  fieldnames-option = 'EQ'.

  fieldnames-low     = 'MANDT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLIENT'.
  APPEND fieldnames.
  fieldnames-low     = 'MANDANT'.
  APPEND fieldnames.
  fieldnames-low     = 'CLNT'.
  APPEND fieldnames.

  REFRESH fieldnames.
  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO TABLE lt_dd03l
         FROM dd03l
         WHERE tabname   LIKE tab_search
         AND   position  =    '1'
         AND   fieldname IN   fieldnames
         AND   as4local  =    'A'
         AND   as4vers   =    ' '.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuf8
*-----------------------------------------------------------------------
*  UG : Large JOIN
** UG1: Join on 5 tables: tabname, fieldname, ddlanguage,
** UG2: as XL2 with FOR ALL ENTRIES
*-----------------------------------------------------------------------
FORM  testug1.

  TYPES:
    BEGIN OF st_all,
           tabname    LIKE dd08l-tabname,
           fieldname  LIKE dd08l-fieldname,
           as4local   LIKE dd08l-as4local,
           as4vers    LIKE dd08l-as4vers,
           position   LIKE dd03l-position,
           ddlanguage LIKE dd08t-ddlanguage,
           frkart     LIKE dd08l-frkart,
           tabclass   LIKE dd02l-tabclass,
           ddtext_2   LIKE dd02t-ddtext,
           ddtext_8   LIKE dd08t-ddtext,
    END OF st_all.

  DATA:
    lt_all       TYPE STANDARD TABLE OF st_all.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname  a~fieldname  a~as4local a~as4vers
         c~position b~ddlanguage a~frkart   d~tabclass
         e~ddtext AS ddtext_2
         b~ddtext AS ddtext_8
         INTO  CORRESPONDING FIELDS OF TABLE lt_all
         FROM       dd08l AS a
         INNER JOIN dd08t AS b
         ON    b~tabname    = a~tabname
         AND   b~fieldname  = a~fieldname
         AND   b~as4local   = a~as4local
         AND   b~as4vers    = a~as4vers
         INNER JOIN dd03l AS c
         ON    c~tabname    = a~tabname
         AND   c~fieldname  = a~fieldname
         AND   c~as4local   = a~as4local
         AND   c~as4vers    = a~as4vers
         INNER JOIN dd02l AS d
         ON    d~tabname    = a~tabname
         AND   d~as4local   = a~as4local
         AND   d~as4vers    = a~as4vers
         INNER JOIN dd02t AS e
         ON    e~tabname    = a~tabname
         AND   e~ddlanguage = b~ddlanguage
         AND   e~as4local   = a~as4local
         AND   e~as4vers    = a~as4vers
         WHERE a~tabname    LIKE tab_search
         AND   a~fieldname  = field_search
         AND   a~as4local   = 'A'
         AND   a~as4vers    = ' '
         AND   b~ddlanguage = 'E'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_all LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testug1
*-----------------------------------------------------------------------
FORM  testug2.

  TYPES:
    BEGIN OF st_all,
           tabname    LIKE dd08l-tabname,
           fieldname  LIKE dd08l-fieldname,
           as4local   LIKE dd08l-as4local,
           as4vers    LIKE dd08l-as4vers,
           position   LIKE dd03l-position,
           ddlanguage LIKE dd08t-ddlanguage,
           frkart     LIKE dd08l-frkart,
           tabclass   LIKE dd02l-tabclass,
           ddtext_2   LIKE dd02t-ddtext,
           ddtext_8   LIKE dd08t-ddtext,
    END OF st_all.

  DATA:
        lt_all  TYPE STANDARD TABLE OF st_all,
        lt_all2 TYPE STANDARD TABLE OF st_all,
        lt_all3 TYPE STANDARD TABLE OF st_all,
        lt_all4 TYPE STANDARD TABLE OF st_all,
        lt_all5 TYPE STANDARD TABLE OF st_all,
        lt_all9 TYPE STANDARD TABLE OF st_all,
        wa      TYPE st_all,
        wa2     TYPE st_all,
        wa3     TYPE st_all,
        wa4     TYPE st_all,
        wa5     TYPE st_all.

  FIELD-SYMBOLS: <fs>  TYPE st_all.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname  fieldname  as4local as4vers frkart
         INTO  CORRESPONDING FIELDS OF TABLE lt_all
         FROM  dd08l
         WHERE tabname    LIKE tab_search
         AND   fieldname  = field_search
         AND   as4local   = 'A'
         AND   as4vers    = ' '.

  SELECT tabname fieldname as4local as4vers ddlanguage
         ddtext AS ddtext_8
         INTO  CORRESPONDING FIELDS OF TABLE lt_all2
         FROM  dd08t
         FOR ALL ENTRIES IN lt_all
         WHERE tabname    = lt_all-tabname
         AND   fieldname  = lt_all-fieldname
         AND   as4local   = lt_all-as4local
         AND   as4vers    = lt_all-as4vers
         AND   ddlanguage = 'E'.

  SELECT tabname fieldname as4local as4vers position
         INTO  CORRESPONDING FIELDS OF TABLE lt_all3
         FROM  dd03l
         FOR ALL ENTRIES IN lt_all
         WHERE tabname    = lt_all-tabname
         AND   fieldname  = lt_all-fieldname
         AND   as4local   = lt_all-as4local
         AND   as4vers    = lt_all-as4vers.

  SELECT tabname as4local as4vers tabclass
         INTO  CORRESPONDING FIELDS OF TABLE lt_all4
         FROM   dd02l
         FOR ALL ENTRIES IN lt_all
         WHERE tabname    = lt_all-tabname
         AND   as4local   = lt_all-as4local
         AND   as4vers    = lt_all-as4vers.

  SELECT tabname as4local as4vers   ddtext AS ddtext_2
         INTO  CORRESPONDING FIELDS OF TABLE lt_all5
         FROM  dd02t
         FOR ALL ENTRIES IN lt_all
         WHERE tabname    = lt_all-tabname
         AND   as4local   = lt_all-as4local
         AND   as4vers    = lt_all-as4vers
         AND   ddlanguage = 'E'.

  SORT lt_all2 BY tabname fieldname as4local as4vers.
  SORT lt_all3 BY tabname fieldname as4local as4vers.
  SORT lt_all4 BY tabname as4local as4vers.
  SORT lt_all5 BY tabname as4local as4vers.
  LOOP AT lt_all ASSIGNING <fs>.

    READ TABLE lt_all2 INTO wa
         WITH KEY tabname   = <fs>-tabname
                  fieldname = <fs>-fieldname
                  as4local  = <fs>-as4local
                  as4vers   = <fs>-as4vers
         BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      <fs>-ddlanguage = wa-ddlanguage.
      <fs>-ddtext_8   = wa-ddtext_8.
    ELSE.
      CLEAR <fs>-tabname.
    ENDIF.

    READ TABLE lt_all3 INTO wa
         WITH KEY tabname   = <fs>-tabname
                  fieldname = <fs>-fieldname
                  as4local  = <fs>-as4local
                  as4vers   = <fs>-as4vers
         BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      <fs>-position = wa-position.
    ELSE.
      CLEAR <fs>-tabname.
    ENDIF.

    READ TABLE lt_all4 INTO wa
         WITH KEY tabname   = <fs>-tabname
                  as4local  = <fs>-as4local
                  as4vers   = <fs>-as4vers
         BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      <fs>-tabclass  = wa-tabclass.
    ELSE.
      CLEAR <fs>-tabname.
    ENDIF.

    READ TABLE lt_all5 INTO wa
         WITH KEY tabname   = <fs>-tabname
                  as4local  = <fs>-as4local
                  as4vers   = <fs>-as4vers
         BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).
      <fs>-ddtext_2   = wa-ddtext_2.
    ELSE.
      CLEAR <fs>-tabname.
    ENDIF.

  ENDLOOP.
  LOOP AT lt_all INTO wa.
    IF NOT ( wa-tabname IS INITIAL ).
      APPEND wa TO lt_all9.
    ENDIF.
  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_all9 LINES info2.
  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testug2
*-----------------------------------------------------------------------
*  UJ : Self-Join: all tables also with tabname and fieldname
** UJ1: Self-JOIN
** UJ2: Subquery
** UJ3: FAE
** UJ4: Aggregate GROUP BY HAVING
*-----------------------------------------------------------------------
FORM  testuj1.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname
*  a~fieldname a~as4local a~as4vers a~position
         INTO TABLE lt_dd03l
         FROM       dd03l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname   = a~tabname
         AND   b~as4local  = a~as4local
         AND   b~as4vers   = a~as4vers
         WHERE a~tabname   LIKE tab_search
         AND   a~as4local  = 'A'
         AND   a~as4vers   = ' '
         AND   a~fieldname = field_search
         AND   b~fieldname = field_search2.

  SORT lt_dd03l BY tabname.
  DELETE ADJACENT DUPLICATES FROM lt_dd03l
         COMPARING tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info1.
  IF ( info1 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuj1
*-----------------------------------------------------------------------
FORM  testuj2.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname
*   a~fieldname a~as4local a~as4vers a~position
         INTO TABLE lt_dd03l
         FROM       dd03l AS a
         WHERE a~tabname   IN
           ( SELECT b~tabname
                    FROM dd03l AS b
                    WHERE b~tabname   LIKE tab_search
                    AND   b~fieldname = field_search2
                    AND   b~as4local  =    'A'
                    AND   b~as4vers   =    ' '   )
         AND   a~fieldname = field_search
         AND   a~as4local  = 'A'
         AND   a~as4vers   = ' '.

  SORT lt_dd03l BY tabname.
  DELETE ADJACENT DUPLICATES FROM lt_dd03l
         COMPARING tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info2.
  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuj2
*-----------------------------------------------------------------------
FORM  testuj3.

  DATA:
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   fieldname = field_search.

  SELECT tabname fieldname as4local as4vers position
     INTO TABLE kt_dd03l
     FROM dd03l
     FOR ALL ENTRIES IN lt_dd03l
     WHERE tabname   = lt_dd03l-tabname
     AND   as4local  = lt_dd03l-as4local
     AND   as4vers   = lt_dd03l-as4vers
     AND   fieldname = field_search2.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE kt_dd03l LINES info3.
  IF ( info3 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuj3
*-----------------------------------------------------------------------
FORM  testuj4.

  TYPES:
    BEGIN OF st_agg,
           tabname LIKE dd03l-tabname,
           count   TYPE i,
    END OF st_agg.

  DATA:
        wa     TYPE st_agg,
        lt_agg TYPE STANDARD TABLE OF st_agg,
        kt_agg TYPE STANDARD TABLE OF st_agg.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname COUNT( DISTINCT fieldname )
         INTO TABLE lt_agg
         FROM       dd03l
         WHERE tabname   LIKE tab_search
         AND ( fieldname = field_search
            OR fieldname = field_search2 )
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         GROUP BY tabname
         HAVING COUNT( DISTINCT fieldname ) = 2.

*  LOOP AT lt_agg INTO wa.
*    IF ( wa-count = 2 ).
*      APPEND wa TO kt_agg.
*    ENDIF.
*  ENDLOOP.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_agg LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuj4
*-----------------------------------------------------------------------
FORM  testuj5.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        kt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position
         INTO ls_dd03l
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   fieldname = field_search.
    IF ( sy-subrc = 0 ).
      SELECT tabname fieldname as4local as4vers position
         INTO ks_dd03l
         FROM dd03l
         WHERE tabname   = ls_dd03l-tabname
         AND   as4local  = ls_dd03l-as4local
         AND   as4vers   = ls_dd03l-as4vers
         AND   fieldname = field_search2.
        IF ( sy-subrc = 0 ).
          APPEND ks_dd03l TO kt_dd03l.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ENDSELECT.

  SORT kt_dd03l
       BY tabname fieldname as4local as4vers.
  DELETE ADJACENT DUPLICATES FROM kt_dd03l
       COMPARING tabname fieldname as4local as4vers.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE kt_dd03l LINES info1.
  IF ( info1 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuj5
*-----------------------------------------------------------------------
FORM  testuj6.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT COUNT(*) INTO info2
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   fieldname = field_search.

  SELECT COUNT(*) INTO info3
         FROM  dd03l
         WHERE tabname   LIKE tab_search
         AND   fieldname = field_search2.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testuj6
*-----------------------------------------------------------------------
FORM  testuj7.

  DATA:
    lt_dd03l   TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname
*  a~fieldname a~as4local a~as4vers a~position
         INTO TABLE lt_dd03l
         FROM       dd03l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname   = a~tabname
         AND   b~fieldname = a~fieldname
         AND   b~as4local  = a~as4local
         AND   b~as4vers   = a~as4vers
         AND   b~position <> a~position
         WHERE a~tabname   LIKE tab_search
         AND   a~as4local  = 'A'
         AND   a~as4vers   = ' '
         AND   a~fieldname = field_search.

  SORT lt_dd03l BY tabname.
  DELETE ADJACENT DUPLICATES FROM lt_dd03l
         COMPARING tabname.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info2.
  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.

ENDFORM.                                                    "testuj7
*-----------------------------------------------------------------------
FORM  testuj8.

  field_search  = field_search2.
  PERFORM testuj7.
  info3 = info2.

ENDFORM.                                                    "testuj8
*-----------------------------------------------------------------------
*  UM :Large Selection on First table, reduction in second
** UM1: SELECT * DD02L client-dependent tables
** UM2: SELECT * DD03L with fields CLIENT ...
** UM3: SELECT JOIN  first  DD02L then DD03L
** UM4: SELECT JOIN  first  DD03L then DD02L
** UN1: FAE   first DD02L then DD03L
** UN2: FAE   first DD03L then DD02L
*-----------------------------------------------------------------------
FORM  testum1.

  DATA:
    lt_dd02l    TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname as4local as4vers clidep
         INTO  TABLE lt_dd02l
         FROM       dd02l AS a
         WHERE tabname   LIKE tab_search
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   clidep    = 'X'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd02l LINES info1.
  IF ( info1 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testum1
*-----------------------------------------------------------------------
FORM  testum2.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position keyflag
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE  tab_search
         AND   fieldname NOT IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT')
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info2.
  IF ( info2 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testum2
*-----------------------------------------------------------------------
FORM  testum3.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT b~tabname b~fieldname b~position b~keyflag
         INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM       dd02l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname = a~tabname
         AND   b~as4local = a~as4local
         AND   b~as4vers  = a~as4vers
         WHERE a~tabname   BETWEEN tab_search AND tab_search2
         AND   a~as4local  = 'A'
         AND   a~as4vers   = ' '
         AND   a~clidep    = 'X'
         AND   b~position  = '1'
         AND   b~fieldname NOT IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT')
.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info3.
  IF ( info3 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testum3
*-----------------------------------------------------------------------
FORM  testum4.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname a~fieldname a~position a~keyflag
         INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM       dd03l AS a
         INNER JOIN dd02l AS b
         ON    b~tabname = a~tabname
         AND   b~as4local = a~as4local
         AND   b~as4vers  = a~as4vers
         WHERE a~tabname   BETWEEN tab_search AND tab_search2
         AND   a~as4local  = 'A'
         AND   a~as4vers   = ' '
         AND   a~position  = '1'
         AND   a~fieldname NOT IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT')
         AND   b~clidep    =    'X'.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info4.
  IF ( info4 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testum4
*-----------------------------------------------------------------------
FORM  testum5.

  DATA:
        lt_dd02l TYPE tab_dd02l,
        lt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname as4local as4vers clidep
         INTO  TABLE lt_dd02l
         FROM       dd02l AS a
         WHERE tabname   BETWEEN tab_search AND tab_search2
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   clidep    = 'X'.

  SELECT tabname fieldname as4local as4vers position keyflag
         INTO  TABLE lt_dd03l
         FROM  dd03l
         FOR ALL ENTRIES IN lt_dd02l
         WHERE tabname   = lt_dd02l-tabname
         AND   as4local  = lt_dd02l-as4local
         AND   as4vers   = lt_dd02l-as4vers
         AND   fieldname NOT IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT')
         AND   position  = '1'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd02l LINES info1.
  DESCRIBE TABLE lt_dd03l LINES info2.
  IF ( info1 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testum5
*-----------------------------------------------------------------------
FORM  testum6.

  DATA:
        ls_dd03l LIKE dd03l,
        ks_dd03l LIKE dd03l,
        lt_dd02l TYPE tab_dd02l,
        lt_dd03l TYPE tab_dd03l,
        kt_dd03l TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT tabname fieldname as4local as4vers position keyflag
         INTO  TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   BETWEEN tab_search AND tab_search2
         AND   fieldname NOT IN ('MANDANT', 'MANDT', 'CLIENT', 'CLNT')
         AND   as4local  = 'A'
         AND   as4vers   = ' '
         AND   position  = '1'.

  LOOP AT lt_dd03l INTO ls_dd03l.
    ks_dd03l-tabname  = ls_dd03l-tabname.
    ks_dd03l-as4local = ls_dd03l-as4local.
    ks_dd03l-as4vers  = ls_dd03l-as4vers.
    COLLECT ls_dd03l INTO kt_dd03l.
  ENDLOOP.

  SELECT tabname as4local as4vers clidep
         INTO  TABLE lt_dd02l
         FROM       dd02l AS a
         FOR ALL ENTRIES IN kt_dd03l
         WHERE tabname   = kt_dd03l-tabname
         AND   as4local  = kt_dd03l-as4local
         AND   as4vers   = kt_dd03l-as4vers
         AND   clidep    = 'X'.


  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  DESCRIBE TABLE lt_dd03l LINES info3.
  DESCRIBE TABLE lt_dd02l LINES info4.
  IF ( info3 > 0 ).
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testum6


*-----------------------------------------------------------------------
*  UR : Redundant Information FAE faster ?
** UR1: Prepare info1 = lines to select, info2 = lines in first table
** UR2: JOIN            - result lines
** UR3: FOR ALL ENTRIES - INTO SORTED TABLE and nested loop
*-----------------------------------------------------------------------

FORM  testur1.

  TYPES:
    st_dd02l   LIKE dd02l.

  TYPES:
    BEGIN OF st_dd02_3l,
           include   TYPE st_dd02l,
           fieldname LIKE dd03l-fieldname,
           position  LIKE dd03l-position,
    END OF st_dd02_3l.

  TYPES:
    BEGIN OF st_tabname,
           tabname  LIKE dd03l-tabname,
           as4local LIKE dd03l-as4local,
           as4vers  LIKE dd03l-as4vers,
    END OF st_tabname.

  DATA:
        lt_tabname TYPE STANDARD TABLE OF st_tabname,
        lt_dd02l   TYPE tab_dd02l,
        lt_dd03l   TYPE tab_dd03l,
*               WITH NON-UNIQUE KEY tabname fieldname as4local as4vers ,
        lt_dd02_3l TYPE STANDARD TABLE OF st_dd02_3l,
        wa02       LIKE dd02l,
        wa03       LIKE dd03l,
        wa02_03    TYPE st_dd02_3l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname as4local as4vers
         INTO TABLE lt_tabname
         FROM dd03l
         WHERE tabname LIKE 'D%'
         AND   position = pos_search.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO TABLE lt_dd02l
         FROM  dd02l
         FOR ALL ENTRIES IN lt_tabname
         WHERE tabname  = lt_tabname-tabname
         AND   as4local = lt_tabname-as4local
         AND   as4vers  = lt_tabname-as4vers.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_tabname LINES info1.
    DESCRIBE TABLE lt_dd02l   LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testur1
*-----------------------------------------------------------------------
FORM  testur2.

  TYPES:
    st_dd02l   LIKE dd02l.

  TYPES:
    BEGIN OF st_dd02_3l,
           include   TYPE st_dd02l,
           fieldname LIKE dd03l-fieldname,
           position  LIKE dd03l-position,
    END OF st_dd02_3l.

  TYPES:
    BEGIN OF st_tabname,
           tabname  LIKE dd03l-tabname,
           as4local LIKE dd03l-as4local,
           as4vers  LIKE dd03l-as4vers,
    END OF st_tabname.


  DATA:
        lt_tabname TYPE STANDARD TABLE OF st_tabname,
        lt_dd02_3l TYPE STANDARD TABLE OF st_dd02_3l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname as4local as4vers
         INTO TABLE lt_tabname
         FROM dd03l
         WHERE tabname LIKE 'D%'
         AND   position = pos_search.

*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT a~tabname   a~as4local   a~as4vers    a~tabclass
         a~sqltab    a~datmin     a~datmax     a~datavg
         a~clidep    a~buffered   a~comprflag  a~langdep
         a~actflag   a~applclass  a~authclass  a~as4user
         a~as4date   a~as4time    a~masterlang a~mainflag
         a~contflag  a~reservetab a~globalflag a~prozpuff
         a~viewclass a~viewgrant  a~multiplex  a~shlpexi
         a~proxytype a~exclass    a~wrongcl
         b~fieldname b~position
         INTO CORRESPONDING FIELDS OF TABLE lt_dd02_3l
         FROM       dd02l AS a
         INNER JOIN dd03l AS b
         ON    b~tabname  = a~tabname
         AND   b~as4local = a~as4local
         AND   b~as4vers  = a~as4vers
         FOR   ALL ENTRIES IN lt_tabname
         WHERE a~tabname  = lt_tabname-tabname
         AND   a~as4local = lt_tabname-as4local
         AND   a~as4vers  = lt_tabname-as4vers
         AND   b~position <= pos_search.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02_3l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testur2
*-----------------------------------------------------------------------
FORM  testur3.

  TYPES:
    st_dd02l   LIKE dd02l.

  TYPES:
    BEGIN OF st_dd02_3l,
           include   TYPE st_dd02l,
           fieldname LIKE dd03l-fieldname,
           position  LIKE dd03l-position,
    END OF st_dd02_3l.

  TYPES:
    BEGIN OF st_tabname,
           tabname  LIKE dd03l-tabname,
           as4local LIKE dd03l-as4local,
           as4vers  LIKE dd03l-as4vers,
    END OF st_tabname.

  DATA:
        lt_tabname TYPE STANDARD TABLE OF st_tabname,
        lt_dd02l   TYPE tab_dd02l,
        lt_dd03l   TYPE SORTED TABLE OF dd03l
                 WITH NON-UNIQUE KEY tabname fieldname as4local as4vers
,
        lt_dd02_3l TYPE STANDARD TABLE OF st_dd02_3l,
        wa02       LIKE dd02l,
        wa03       LIKE dd03l,
        wa02_03    TYPE st_dd02_3l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname as4local as4vers
         INTO TABLE lt_tabname
         FROM dd03l
         WHERE tabname LIKE 'D%'
         AND   position = pos_search.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO TABLE lt_dd02l
         FROM  dd02l
         FOR ALL ENTRIES IN lt_tabname
         WHERE tabname  = lt_tabname-tabname
         AND   as4local = lt_tabname-as4local
         AND   as4vers  = lt_tabname-as4vers.

  IF  NOT ( lt_dd02l IS INITIAL ).
    SELECT tabname fieldname as4local as4vers position
           INTO TABLE lt_dd03l
           FROM dd03l
           FOR ALL ENTRIES IN lt_dd02l
           WHERE tabname  =  lt_dd02l-tabname
           AND   as4local =  lt_dd02l-as4local
           AND   as4vers  =  lt_dd02l-as4vers
           AND   position <= pos_search.

    LOOP AT lt_dd02l INTO wa02.
      LOOP AT  lt_dd03l INTO wa03
           WHERE  tabname  = wa02-tabname
             AND  as4local = wa02-as4local
             AND  as4vers  = wa02-as4vers.
        IF ( sy-subrc EQ 0 ).
          wa02_03           = wa02.
          wa02_03-fieldname = wa03-fieldname.
          wa02_03-position  = wa03-position.
          APPEND wa02_03 TO lt_dd02_3l.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02_3l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testur3
*-----------------------------------------------------------------------
FORM  testur4.

  TYPES:
    st_dd02l   LIKE dd02l.

  TYPES:
    BEGIN OF st_dd02_3l,
           include   TYPE st_dd02l,
           fieldname LIKE dd03l-fieldname,
           position  LIKE dd03l-position,
    END OF st_dd02_3l.

  TYPES:
    BEGIN OF st_tabname,
           tabname  LIKE dd03l-tabname,
           as4local LIKE dd03l-as4local,
           as4vers  LIKE dd03l-as4vers,
    END OF st_tabname.

  DATA:
        lt_tabname TYPE STANDARD TABLE OF st_tabname,
        lt_dd02l   TYPE tab_dd02l,
        lt_dd03l   TYPE tab_dd03l,
*               WITH NON-UNIQUE KEY tabname fieldname as4local as4vers ,
        lt_dd02_3l TYPE STANDARD TABLE OF st_dd02_3l,
        wa02       LIKE dd02l,
        wa03       LIKE dd03l,
        wa02_03    TYPE st_dd02_3l.

  xx      = 'N'.
  CLEAR   tt.

  SELECT tabname as4local as4vers
         INTO TABLE lt_tabname
         FROM dd03l
         WHERE tabname LIKE 'D%'
         AND   position = pos_search.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT *
         INTO TABLE lt_dd02l
         FROM  dd02l
         FOR ALL ENTRIES IN lt_tabname
         WHERE tabname  = lt_tabname-tabname
         AND   as4local = lt_tabname-as4local
         AND   as4vers  = lt_tabname-as4vers.

  IF  NOT ( lt_dd02l IS INITIAL ).
    SELECT tabname fieldname as4local as4vers position
           INTO TABLE lt_dd03l
           FROM dd03l
           FOR ALL ENTRIES IN lt_dd02l
           WHERE tabname  = lt_dd02l-tabname
           AND   as4local = lt_dd02l-as4local
           AND   as4vers  = lt_dd02l-as4vers.
    GET RUN TIME FIELD stop.
*-------------------------------------

    LOOP AT lt_dd02l INTO wa02.
      LOOP AT  lt_dd03l INTO wa03
           WHERE  tabname  = wa02-tabname
             AND  as4local = wa02-as4local
             AND  as4vers  = wa02-as4vers.
        IF ( sy-subrc EQ 0 ).
          wa02_03           = wa02.
          wa02_03-fieldname = wa03-fieldname.
          wa02_03-position  = wa03-position.
          APPEND wa02_03 TO lt_dd02_3l.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  t = stop - start.
  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02_3l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testur4
*-----------------------------------------------------------------------
*   UX : LEFT OUTER JOINs
*       Same example as SD but only 'CLIENT'
**  UX1: info1 all tables 'A%', transp and client dependent from DD02L
**  UX2. info2 all tables 'A%', fieldname = 'CLIENT'        from DD03L
**  UX3: LEFT OUTER JOIN   DD02L -> DD03L
**  UX4: LEFT OUTER JOIN   DD03L -> DD02L
*-----------------------------------------------------------------------
FORM  testux1.

  DATA:
    lt_dd02l    TYPE tab_dd02l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.

  SELECT DISTINCT tabname
*  SELECT tabname fieldname position keyflag
         INTO  CORRESPONDING FIELDS OF TABLE lt_dd02l
         FROM  dd02l AS a
         WHERE tabname  LIKE 'A%'
         AND   tabclass = 'TRANSP'
         AND   clidep   = 'X'.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd02l LINES info1.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testux1
*-----------------------------------------------------------------------
FORM  testux2.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.

  SELECT DISTINCT tabname
*  SELECT tabname fieldname position keyflag
         INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM  dd03l
         WHERE tabname   LIKE 'A%'
         AND   fieldname =  'CLIENT'.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info2.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testux2
*-----------------------------------------------------------------------
FORM  testux3.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT DISTINCT a~tabname
*  SELECT a~tabname a~fieldname a~position a~keyflag
         INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM            dd02l AS a
         LEFT OUTER JOIN dd03l AS b
         ON    b~tabname   = a~tabname
         AND   b~fieldname = 'CLIENT'
         WHERE a~tabname   LIKE 'A%'
         AND   a~tabclass  = 'TRANSP'
         AND   a~clidep    = 'X'.

  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info3.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testux3
*-----------------------------------------------------------------------
FORM  testux4.

  DATA:
    lt_dd03l    TYPE tab_dd03l.

  xx      = 'N'.
  CLEAR   tt.
*-------------------------------------
  GET RUN TIME FIELD start.
  SELECT DISTINCT a~tabname
*  SELECT a~tabname a~fieldname a~position a~keyflag
         INTO  CORRESPONDING FIELDS OF TABLE lt_dd03l
         FROM            dd03l AS a
         LEFT OUTER JOIN dd02l AS b
         ON    b~tabname  = a~tabname
         AND   b~tabclass = 'TRANSP'
         AND   b~clidep   = 'X'
         WHERE a~tabname   LIKE 'A%'
         AND   a~fieldname = 'CLIENT'.
  GET RUN TIME FIELD stop.
*-------------------------------------
  t = stop - start.

  IF ( sy-subrc EQ 0 ).
    DESCRIBE TABLE lt_dd03l LINES info4.
    xx = 'Y'.
    tt = t.
  ENDIF.
ENDFORM.                                                    "testux4
************************************************************************
* Z: Other tests
************************************************************************






*-----------------------------------------------------------------------
*  define_testcases:
*-----------------------------------------------------------------------
FORM  define_testcases
      USING error TYPE c.

* defaults:
  min_avg  = 'min'.
  test1    = ''.
  test2    = ''.
  test3    = ''.
  test4    = ''.
  test5    = ''.
  test6    = ''.
  detail_1 = ''.
  detail_2 = 'X'.
  detail_3 = 'X'.
* i_loops  = '10'.
  s_loops  = '10'.

  n_type  = 'add'.
  n_init  = '1000'.
  n_inc   = '0'.
  n_loops = '1'.

***************************************************
* Size of the tables
***************************************************
  IF     ( testcase EQ '001' ).
    test1   = '001'.
    test2   = '002'.
    test3   = '003'.
    s_loops = '1'.
    n_init  = '1'.


***************************************************
* Explain: Different access types
***************************************************
* Basic Accesses: Single, Range, Full Table---
  ELSEIF ( testcase EQ '011' ).
    test1   = 'AA1'.
    test2   = 'AA2'.
    test3   = 'AA3'.
    s_loops = '1'.
    n_init  = '1'.
* Index-Only, Index Full Scan
  ELSEIF ( testcase EQ '014' ).
    test1   = 'NA2'.
    test2   = 'ND2'.
    test3   = ''.
    test4   = ''.
    s_loops = '1'.

** IN versus OR
*  ELSEIF ( testcase EQ '017' ).
*    test1    = 'CN1'.
*    test2    = 'CN2'.
*    s_loops    = '1'.
*
** UP TO 1 ROWS / MAX
*  ELSEIF ( testcase EQ '021' ).
*    test1    = 'EX2'.
*    test2    = 'GA3'.
*    test3    = 'FB4'.
*    test4    = ''.
*    tab_search = 'DD%'.
*    s_loops    = '1'.
*    n_init     = '10'.


* FOR ALL ENTRIES: 3 conditions, 1 condition
  ELSEIF ( testcase EQ '027' ).
    test1      = 'JA2'.
    test2      = 'KA1'.
    test3      = 'KA2'.
    test4      = ''.
    s_loops    = '1'.
    tab_search = 'S%'.
    n_inc      = '100'.

* Subquery, INNER JOIN, OUTER JOIN
  ELSEIF ( testcase EQ '031' ).
    test1      = ''.
    test2      = 'SA2'.
    test3      = 'SA3'.
    test4      = 'UX3'.
    tab_search = 'D%'.
    s_loops    = '1'.




***************************************************
* ABCD: Searches, WHERE-conditions etc. (4.2)
***************************************************
* different Access Types (4.2.1):-----------------
  ELSEIF ( testcase EQ '101' ).
    test1   = 'AA1'.
    test2   = 'AA2'.
    test3   = 'AA3'.
    test4   = 'AA4'.
    s_loops = '5'.
    n_init  = '1'.
  ELSEIF ( testcase EQ '102' ).
    test1   = 'AB1'.
    test2   = 'AB2'.
    test3   = 'AB3'.
    test4   = 'AB4'.
    s_loops = '5'.

* repeat the fastest for better statictics:
  ELSEIF ( testcase EQ '105' ).
    test1   = 'AA1'.
    test2   = 'AA4'.
    test3   = 'AB1'.
    test4   = 'AB4'.
    test5   = 'AA1'.
    s_loops = '50'.
    n_init  = '1'.

* Secondary Key:
  ELSEIF ( testcase EQ '111' ).
    test1   = 'AC1'.
    test2   = 'AC2'.
    test3   = 'AC3'.
    test4   = 'AC4'.
    s_loops = '5'.

* other Selects with =
  ELSEIF ( testcase EQ '121' ).
    test1   = 'AE1'.
    test2   = 'AE2'.
    test3   = 'AE3'.
    test4   = 'AE4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '122' ).
    test1   = 'AH1'.
    test2   = 'AH2'.
    test3   = 'AH3'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '123' ).
    test1   = 'AM1'.
    test2   = 'AM2'.
    test3   = 'AM3'.
    test4   = ''.
    s_loops = '1'.

* different operators (4.2.2):-------------------
  ELSEIF ( testcase EQ '151' ).
    test1   = 'BA1'.
    test2   = 'BA2'.
    test3   = 'BA3'.
    test4   = 'BA4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '152' ).
    test1   = 'BB1'.
    test2   = 'BB2'.
    test3   = 'BB3'.
    test4   = 'BB4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '153' ).
    test1   = 'BC1'.
    test2   = 'BC2'.
    test3   = 'BC3'.
    test4   = 'BC4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '154' ).
    test1   = 'BG1'.
    test2   = 'BG2'.
    test3   = 'BG3'.
    test4   = 'BG4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '156' ).
    test1   = 'BH1'.
    test2   = 'BH2'.
    test3   = 'BH3'.
    test4   = 'BH4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '158' ).
    test1   = 'BJ1'.
    test2   = 'BJ2'.
    test3   = 'BJ3'.
    test4   = 'BJ4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '159' ).
    test1   = 'BL1'.
    test2   = 'BL2'.
    test3   = 'BL3'.
    test4   = 'BL4'.
    s_loops = '1'.
  ELSEIF ( testcase EQ '041' ).
    test1   = 'AS1'.
    test2   = 'AS2'.
    test3   = 'AS3'.
    test4   = 'AS4'.
    s_loops = '5'.
  ELSEIF ( testcase EQ '042' ).
    test1   = 'AT1'.
    test2   = 'AT2'.
    test3   = 'AT3'.
    test4   = ''.
    s_loops = '20'.


***************************************************
* C: WHERE-Clauses (4.2.3)
***************************************************
* Form of the WHERE-Clause
  ELSEIF ( testcase EQ '211' ).
    test1   = 'CA1'.
    test2   = 'CA2'.
    test3   = 'CA3'.
    test4   = 'CA4'.
    s_loops = '5'.
  ELSEIF ( testcase EQ '212' ).
    test1   = 'CB1'.
    test2   = 'CB2'.
    test3   = 'CB3'.
    test4   = 'CB4'.
    s_loops = '5'.
* Empty Condition
  ELSEIF ( testcase EQ '221' ).
    test1   = 'CE1'.
    test2   = 'CE2'.
    test3   = 'CE3'.
    test4   = 'CE4'.
    s_loops = '5'.
* LIKE
  ELSEIF ( testcase EQ '231' ).
    test1 = 'CG1'.
    test2 = 'CG2'.
    test3 = 'CG3'.
    test4 = ''.
  ELSEIF ( testcase EQ '232' ).
    test1 = 'CH1'.
    test2 = 'CH2'.
    test3 = ''.
    test4 = ''.
* IN versus OR
  ELSEIF ( testcase EQ '241' ).
    test1 = 'CN1'.
    test2 = 'CN2'.
    test3 = 'CN3'.
  ELSEIF ( testcase EQ '242' ).
    test1 = 'CM1'.
    test2 = 'CM2'.
    test3 = 'CM3'.


* Index Gap
  ELSEIF ( testcase EQ '251' ).
    test1    = 'CP1'.
    test2    = 'CP2'.
    test3    = 'CP3'.
    detail_2 = 'X'.
    detail_3 = 'X'.
    s_loops  = '3'.
  ELSEIF ( testcase EQ '252' ).
    test1    = 'CQ1'.
    test2    = 'CQ2'.
    test3    = 'CQ3'.
    test4    = 'CQ4'.
    detail_2 = 'X'.
    s_loops  = '5'.
* Not Equal
  ELSEIF ( testcase EQ '261' ).
    test1 = 'CU1'.
    test2 = 'CU2'.
    test3 = 'CU3'.
* DNF
  ELSEIF ( testcase EQ '291' ).
    test1 = 'CX1'.
    test2 = 'AC2'.
    test3 = 'AC3'.
***************************************************
* EFGH:  Number of Records
***************************************************
* Duplicates:--------------------------
  ELSEIF ( testcase EQ '321' ).
    test1 = 'EA1'.
    test2 = 'EA2'.
    test3 = 'EA3'.
    test4 = 'EA4'.
  ELSEIF ( testcase EQ '322' ).
    test1 = 'EC1'.
    test2 = 'EC2'.
    test3 = 'EC3'.
    test4 = 'EC4'.
* DISTINCT:--------------------------
  ELSEIF ( testcase EQ '331' ).
    test1      = 'EN1'.
    test2      = 'EN2'.
    test3      = 'EN3'.
    pos_search = '1'.
  ELSEIF ( testcase EQ '332' ).
    test1      = 'EN1'.
    test2      = 'EN2'.
    test3      = 'EN3'.
    pos_search = '2'.
  ELSEIF ( testcase EQ '333' ).
    test1      = 'EN1'.
    test2      = 'EN2'.
    test3      = 'EN3'.
    pos_search = '3'.
  ELSEIF ( testcase EQ '334' ).
    test1      = 'EN1'.
    test2      = 'EN2'.
    test3      = 'EN3'.
    pos_search = '5'.
* UP TO 1 ROWS / SELECT SINGLE-------------
  ELSEIF ( testcase EQ '351' ).
    test1   = 'EX1'.
    test2   = 'EX2'.
    test3   = 'EX3'.
    s_loops = '20'.
  ELSEIF ( testcase EQ '352' ).
    test1   = 'EY1'.
    test2   = 'EY2'.
    test3   = 'EY3'.
    s_loops = '20'.
* ORDER BY----------------------------------
  ELSEIF ( testcase EQ '361' ).
    test1      = 'FA1'.
    test2      = 'FA2'.
    test3      = 'FA3'.
    test4      = 'FA4'.
    tab_search = 'DD%'.
    s_loops    = '5'.
    n_init     = '10'.
  ELSEIF ( testcase EQ '362' ).
    test1      = 'FA1'.
    test2      = 'FA2'.
    test3      = 'FB3'.
    test4      = 'FB4'.
    tab_search = 'DD%'.
    s_loops    = '5'.
    n_init     = '10'.
  ELSEIF ( testcase EQ '363' ).
    test1      = 'FD1'.
    test2      = 'FD2'.
    test3      = 'FD3'.
    test4      = 'FD4'.
    tab_search = 'DD%'.
    s_loops    = '5'.
    n_init     = '10'.
  ELSEIF ( testcase EQ '364' ).
    test1      = 'FD1'.
    test2      = 'FD2'.
    test3      = 'FD3'.
    test4      = 'FD4'.
    tab_search = 'D%'.
    s_loops    = '3'.
    n_init     = '10'.
* Aggregates - bypassing buffer and not bypassing buffer !
  ELSEIF ( testcase EQ '371' ).
    test1 = 'GA1'.
    test2 = 'GA2'.
    test3 = 'GA3'.
  ELSEIF ( testcase EQ '372' ).
    test1 = 'GA3'.
    test2 = 'GA4'.
    test3 = 'GA5'.
* COUNT(*) / UP TO 1 ROWS
  ELSEIF ( testcase EQ '380' ).
    test1  = 'GG1'.
    n_init = '2'.
  ELSEIF ( testcase EQ '381' ).
    test1 = ''.
    test2 = 'GG2'.
    test3 = 'GG3'.
    test4 = 'GG4'.
  ELSEIF ( testcase EQ '382' ).
    test1   = 'GH1'.
    test2   = 'GH2'.
    test3   = 'GH3'.
    s_loops = '50'.
* Package Size:-----------------------------------
  ELSEIF ( testcase EQ '391' ).
    test1 = 'GP1'.
    test3 = 'GP2'.
***************************************************
* IJKL: Number of Accesses (4.4)
***************************************************
  ELSEIF ( testcase EQ '401' ).
    test1 = 'IA1'.
    test2 = 'IA2'.
    CLEAR n_type.
    n_loops = 7.
  ELSEIF ( testcase EQ '402' ).
    test1 = 'IB1'.
    test2 = 'IB2'.
    CLEAR n_type.
    n_loops = 7.
  ELSEIF ( testcase EQ '408' ).
    test1   = 'IX1'.
    test2   = 'IX2'.
    test3   = 'IX3'.
    test4   = 'IX4'.
    s_loops = '20'.
  ELSEIF ( testcase EQ '409' ).
    test1   = 'IY1'.
    test2   = 'IY2'.
    test3   = 'IY3'.
    test4   = 'IY4'.
    s_loops = '20'.
* Ranges:-------------------------
  ELSEIF ( testcase EQ '431' ).
    test1      = 'JA1'.
    test2      = 'JA2'.
    test3      = 'JA3'.
    tab_search = 'S%'.
    detail_2   = ''.
    s_loops    = '10'.
    n_type     = 'add'.
    n_init     = '0'.
    n_inc      = '100'.
    n_loops    = '10'.
  ELSEIF ( testcase EQ '432' ).
    test1      = 'JA1'.
    test2      = ''.
    test3      = 'JA3'.
    tab_search = 'S%'.
    detail_2   = ''.
    s_loops    = '1'.
    n_type     = 'add'.
    n_init     = '0'.
    n_inc      = '1000'.
    n_loops    = '10'.
  ELSEIF ( testcase EQ '433' ).
    test1      = ''.
    test2      = 'JA2'.
    tab_search = 'S%'.
    detail_2   = ''.
    s_loops    = '1'.
    n_type     = 'add'.
    n_init     = '0'.
    n_inc      = '1000'.
    n_loops    = '10'.

* FOR ALL ENTRIES (4.4.5):--------------------------
  ELSEIF ( testcase EQ '451' ).
    test1      = 'KA1'.
    test2      = 'KA2'.
    tab_search = 'S%'.
    detail_2   = ''.
    s_loops    = '10'.
    n_type     = 'add'.
    n_init     = '0'.
    n_inc      = '100'.
    n_loops    = '10'.
  ELSEIF ( testcase EQ '452' ).
    test1      = 'KA1'.
    test2      = 'KA2'.
    tab_search = 'S%'.
    detail_2   = ''.
    s_loops    = '5'.
    n_type     = 'add'.
    n_init     = '0'.
    n_inc      = '1000'.
    n_loops    = '10'.
  ELSEIF ( testcase EQ '461' ).
    test1   = 'KB1'.
    test2   = 'KB2'.
    test3   = 'KB3'.
    test4   = 'KB4'.
    s_loops = '10'.
  ELSEIF ( testcase EQ '465' ).
    test1    = 'KC1'.
    test2    = 'KC2'.
    test3    = 'KC3'.
    test4    = ''.
    detail_2 = ''.
    s_loops  = '5'.
    n_loops  = '1'.
* duplicates
  ELSEIF ( testcase EQ '471' ).
    test3   = 'KG3'.
    test4   = 'KG4'.
    gv_n1   = '1'.
    gv_l1   = '1'.
    s_loops = '10'.
  ELSEIF ( testcase EQ '472' ).
    test3   = 'KG3'.
    test4   = 'KG4'.
    gv_n1   = '2'.
    gv_l1   = '1'.
    s_loops = '10'.
  ELSEIF ( testcase EQ '473' ).
    test3   = 'KG3'.
    test4   = 'KG4'.
    gv_n1   = '3'.
    gv_l1   = '1'.
    s_loops = '10'.
  ELSEIF ( testcase EQ '474' ).
    test3   = 'KG3'.
    test4   = 'KG4'.
    gv_n1   = '5'.
    gv_l1   = '1'.
    s_loops = '10'.
  ELSEIF ( testcase EQ '475' ).
    test3   = 'KG3'.
    test4   = 'KG4'.
    gv_n1   = '1'.
    gv_l1   = '5'.
    s_loops = '10'.

* combination of itab and lt_result
  ELSEIF ( testcase EQ '481' ).
    test1      = 'KK1'.
    test2      = 'KK2'.
    test3      = 'KK3'.
    test4      = 'KK4'.
    test5      = 'KK5'.
    tab_search = 'D%'.
    s_loops    = '5'.
    n_init     = '1000'.
  ELSEIF ( testcase EQ '482' ).
    test1      = 'KK1'.
    test2      = 'KK2'.
    test3      = 'KK3'.
    test4      = 'KK4'.
    test5      = 'KK5'.
    tab_search = 'S%'.
    s_loops    = '5'.
    n_init     = '10000'.
  ELSEIF ( testcase EQ '483' ).
    test1      = 'KL1'.
    test2      = 'KL2'.
    test3      = 'KL3'.
    test4      = 'KL4'.
    tab_search = 'D%'.
    s_loops    = '10'.
    n_init     = '100'.
  ELSEIF ( testcase EQ '484' ).
    test1      = 'KL1'.
    test2      = 'KL2'.
    test3      = 'KL3'.
    test4      = 'KL4'.
    tab_search = 'S%'.
    s_loops    = '5'.
    n_init     = '1000'.
***************************************************
* MNOP: Fieldlists etc.
***************************************************
* fieldlists  with SELECT SINGLE and SELECT range
* SELECT range
  ELSEIF ( testcase EQ '501' ).
    test1   = 'MA1'.
    test2   = 'MA2'.
    test3   = 'MA3'.
    test4   = 'MA4'.
    test5   = 'MA5'.
    s_loops = '50'.
    n_init  = '10'.
* SELECT FAE
  ELSEIF ( testcase EQ '502' ).
    test1   = 'MB1'.
    test2   = 'MB2'.
    test3   = 'MB3'.
    test4   = 'MB4'.
    test5   = 'MB5'.
    s_loops = '50'.
    n_init  = '10'.
* SELECT SINGLE
  ELSEIF ( testcase EQ '503' ).
    test1   = 'MC1'.
    test2   = 'MC2'.
    test3   = 'MC3'.
    test4   = 'MC4'.
    test5   = 'MC5'.
    s_loops = '50'.
    n_init  = '10'.
* SELECT range - full table scan
  ELSEIF ( testcase EQ '504' ).
    test1   = 'MD1'.
    test2   = 'MD2'.
    test3   = 'MD3'.
    test4   = 'MD4'.
    test5   = 'MD5'.
    s_loops = '30'.
    n_init  = '10'.

* INTO CORRESPONDING FIELDS
* with SELECT SINGLE and SELECT range
  ELSEIF ( testcase EQ '511' ).
    test1   = 'MI1'.
    test2   = 'MI2'.
    test3   = 'MA3'.
    test4   = 'MI4'.
    s_loops = '50'.
    n_init  = '10'.
  ELSEIF ( testcase EQ '512' ).
    test1   = 'MJ1'.
    test2   = 'MJ2'.
    test3   = 'MC3'.
    test4   = 'MJ4'.
    s_loops = '50'.
    n_init  = '10'.
* Index-only - corresponding to 501 - 504
  ELSEIF ( testcase EQ '521' ).
    test1   = 'NA1'.
    test2   = 'NA2'.
    s_loops = '50'.
    n_init  = '10'.
  ELSEIF ( testcase EQ '522' ).
    test1   = 'NB1'.
    test2   = 'NB2'.
    s_loops = '50'.
    n_init  = '10'.
  ELSEIF ( testcase EQ '523' ).
    test1   = 'NC1'.
    test2   = 'NC2'.
    s_loops = '50'.
    n_init  = '10'.
  ELSEIF ( testcase EQ '524' ).
    test1   = 'ND1'.
    test2   = 'ND2'.
    s_loops = '50'.
    n_init  = '10'.
***************************************************
* QR: Special Commands (4.6)
***************************************************
* Client-specified
  ELSEIF ( testcase EQ '611' ).
    test1 = 'QA1'.
    test2 = 'QA2'.
    test3 = 'QA3'.
    test4 = 'QA4'.
* Dynamic Coding
  ELSEIF ( testcase EQ '631' ).
    test1 = 'QD1'.
    test2 = 'QD2'.
    test3 = 'QD3'.
    test4 = 'QD4'.
* Native SQL
  ELSEIF ( testcase EQ '651' ).
    test1 = 'QM1'.
    test2 = 'QM2'.
    test3 = 'QM3'.
    test4 = 'QM4'.
  ELSEIF ( testcase EQ '652' ).
    test1 = 'QN1'.
    test2 = 'QN2'.
    test3 = 'QN3'.
***************************************************
* S: Nested SELECTs: JOINs, FAE, Subquery
***************************************************
  ELSEIF ( testcase EQ '711' ).
* 1. variant
    tab_search = 'D%'.
    test1      = 'SA0'.
    s_loops    = '2'.
  ELSEIF ( testcase EQ '712' ).
* 1. variant
    tab_search = 'D%'.
    test1      = 'SA1'.
    test2      = 'SA2'.
    test3      = 'SA3'.
    test4      = 'SA4'.
    detail_2   = 'X'.
  ELSEIF ( testcase EQ '715' ).
* 1. variant    tab_search = 'S%'.
    tab_search = 'S%'.
    test1      = 'SA0'.
    s_loops    = '2'.
  ELSEIF ( testcase EQ '716' ).
    tab_search = 'S%'.
    test1      = 'SA1'.
    test2      = 'SA2'.
    test3      = 'SA3'.
    test4      = 'SA4'.
    detail_2   = 'X'.
  ELSEIF ( testcase EQ '721' ).
    test1   = 'SD0'.
    s_loops = '2'.
  ELSEIF ( testcase EQ '722' ).
    test1   = 'SD1'.
    test2   = 'SD2'.
    test3   = 'SD3'.
    test4   = 'SD5'.
    s_loops = '10'.
***************************************************
* UVW: JOINS
***************************************************
* Flexibility:-------------------------------------
  ELSEIF ( testcase EQ '820' ).
    test1      = 'UF5'.
    test2      = 'UF6'.
    test3      = 'UF7'.
    test4      = 'UF8'.
    tab_search = 'T0%'.
    s_loops    = '2'.
  ELSEIF ( testcase EQ '821' ).
    test1      = 'UF1'.
    test2      = 'UF2'.
    test3      = 'UF3'.
    tab_search = 'T0%'.
    search     = '0'.
  ELSEIF ( testcase EQ '822' ).
    test1      = 'UF1'.
    test2      = 'UF2'.
    test3      = 'UF3'.
    tab_search = 'T0%'.
    search     = '1'.
  ELSEIF ( testcase EQ '823' ).
    test1      = 'UF1'.
    test2      = 'UF2'.
    test3      = 'UF3'.
    tab_search = 'T0%'.
    search     = '2'.
* Large Join---------------------------
  ELSEIF ( testcase EQ '831' ).
    test1        = 'UG1'.
    test2        = 'UG2'.
    test3        = ''.
    tab_search   = 'D%'.
    field_search = 'TABNAME'.
    s_loops      = '10'.
  ELSEIF ( testcase EQ '832' ).
    test1        = 'UG1'.
    test2        = 'UG2'.
    test3        = ''.
    tab_search   = 'S%'.
    field_search = 'SPRAS'.
    s_loops      = '5'.
* Self-Join:--------------------------------------
  ELSEIF ( testcase EQ '841' ).
    test1         = 'UJ1'.
    test2         = 'UJ2'.
    test3         = 'UJ3'.
    test4         = 'UJ4'.
    tab_search    = 'DD0%'.
    field_search  = 'TABNAME'.
    field_search2 = 'FIELDNAME'.
    s_loops       = '5'.
  ELSEIF ( testcase EQ '842' ).
    test1         = 'UJ5'.
    test2         = 'UJ7'.
    test3         = 'UJ8'.
    tab_search    = 'DD0%'.
    field_search  = 'TABNAME'.
    field_search2 = 'FIELDNAME'.
    s_loops       = '5'.
  ELSEIF ( testcase EQ '843' ).
    test1         = 'UJ1'.
    test2         = 'UJ2'.
    test3         = 'UJ3'.
    test4         = 'UJ4'.
    tab_search    = '%'.
    field_search  = 'TABNAME'.
    field_search2 = 'FIELDNAME'.
    s_loops       = '5'.
  ELSEIF ( testcase EQ '844' ).
    test1         = 'UJ5'.
    test2         = 'UJ7'.
    test3         = 'UJ8'.
    tab_search    = '%'.
    field_search  = 'TABNAME'.
    field_search2 = 'FIELDNAME'.
    s_loops       = '5'.

* Distributed Selectivity---------------------
  ELSEIF ( testcase EQ '851' ).
    test1       = 'UM1'.
    test2       = 'UM2'.
    test3       = 'UM3'.
    test4       = 'UM4'.
    tab_search  = 'A%'.
    tab_search2 = 'Q%'.
    s_loops     = '5'.
  ELSEIF ( testcase EQ '852' ).
    test1       = 'UM5'.
    test2       = 'UM6'.
    tab_search  = 'A%'.
    tab_search2 = 'Q%'.
    s_loops     = '5'.
* Redundant Information (Relation 1:pos_search-----
  ELSEIF ( testcase EQ '861' ).
* 1. variant
    pos_search = '1'.
    test1      = 'UR1'.
    test2      = 'UR2'.
    test3      = 'UR3'.
    s_loops    = '5'.
* 2. variant
  ELSEIF ( testcase EQ '862' ).
    pos_search = '10'.
    test1      = 'UR1'.
    test2      = 'UR2'.
    test3      = 'UR3'.
    s_loops    = '5'.
* 3. variant
  ELSEIF ( testcase EQ '863' ).
    pos_search = '20'.
    test1      = 'UR1'.
    test2      = 'UR2'.
    test3      = 'UR3'.
    s_loops    = '5'.
* 4. variant
  ELSEIF ( testcase EQ '864' ).
    pos_search = '35'.
    test1      = 'UR1'.
    test2      = 'UR2'.
    test3      = 'UR3'.
    s_loops    = '5'.
* 5. variant
  ELSEIF ( testcase EQ '865' ).
    pos_search = '100'.
    test1      = 'UR1'.
    test2      = 'UR2'.
    test3      = 'UR3'.
    s_loops    = '5'.

* Left Outer Join
* nearly same problem as 721 and 722 now LEFT OUTER JOIN
  ELSEIF ( testcase EQ '871' ).
    test1   = 'UX1'.
    test2   = 'UX2'.
    test3   = 'UX3'.
    test4   = 'UX4'.
    s_loops = '5'.
***************************************************
* XYZ: Others
***************************************************











*--------------------other------------
  ELSE.
    WRITE:/ 'Please choose a valid test case!'.
    WRITE:/ '(see documentation)'.
    error = 'X'.
  ENDIF.

* set defaults:------------------------------------------
  l_loops = '1'.
  IF ( min_avg IS INITIAL ).
    min_avg = 'min'.
  ENDIF.

  IF ( test_run EQ 'X' ).
*    i_loops  = '1'.
    l_loops = '1'.
    s_loops = '1'.
    n_loops = '1'.
  ENDIF.
  c3 = s_loops.

  CONCATENATE  'TEST' test1 INTO form1.
  CONCATENATE  'TEST' test2 INTO form2.
  CONCATENATE  'TEST' test3 INTO form3.
  CONCATENATE  'TEST' test4 INTO form4.
  CONCATENATE  'TEST' test5 INTO form5.
  CONCATENATE  'TEST' test6 INTO form6.

ENDFORM.                    "define_testcases
*-----------------------------------------------------------------------
* fill_texts: the test routines  !!
*-----------------------------------------------------------------------
FORM fill_texts.

  DATA:
   wa   TYPE st_ex_names.

  wa-a = 'AE1'.
  wa-b = 'WHERE tabname ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AE2'.
  wa-b = 'WHERE tabname = fieldname ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AE3'.
  wa-b = 'WHERE tabname = as4local = position ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AE4'.
  wa-b = 'WHERE tabname = fieldname = as4local = as4vers ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AH1'.
  wa-b = 'WHERE reftable ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AH2'.
  wa-b = 'WHERE reftable = position ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AH3'.
  wa-b = 'WHERE reftable = tabname ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AM1'.
  wa-b = 'fieldname = rollname ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AM2'.
  wa-b = 'fieldname = domname ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'AM3'.
  wa-b = 'rollname = domname ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'BA1'.
  wa-b = 'WHERE tabname LIKE D%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BA2'.
  wa-b = 'as BA1 T%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BA3'.
  wa-b = 'as BA1 A%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BA4'.
  wa-b = 'as BA1 Q%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BB1'.
  wa-b = 'as BB1 Q%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BB1'.
  wa-b = 'WHERE tabname LIKE D%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BB2'.
  wa-b = 'as BB1 T%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BB3'.
  wa-b = 'as BB1 A%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BC1'.
  wa-b = 'SELECT * WHERE tabname LIKE as4local'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BC2'.
  wa-b = 'SELECT * WHERE tabname LIKE as4local = as4vrs ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'BC3'.
  wa-b = 'SELECT * WHERE tabname LIKE as4local'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BC4'.
  wa-b = 'SELECT * WHERE tabname LIKE as4local = as4vrs ='.
  APPEND wa TO gt_ex_names.
  wa-a = 'BH1'.
  wa-b = 'pos_search 1'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BH2'.
  wa-b = 'pos_search 10'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BH3'.
  wa-b = 'pos_search 50'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BH4'.
  wa-b = 'pos_search 100'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BJ1'.
  wa-b = 'position LT 11'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BJ2'.
  wa-b = 'position LT 10'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BJ3'.
  wa-b = 'position LE 10'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BJ4'.
  wa-b = 'position GE 10'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BL1'.
  wa-b = 'GT 0 AND LT 11'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BL2'.
  wa-b = 'GE 1 AND LE 10'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BL3'.
  wa-b = 'BETWEEN 1 AND 10'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BL4'.
  wa-b = 'BETWEEN 0 AND 1'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IA2'.
  wa-b = 'Array select into table'.
  APPEND wa TO gt_ex_names.
  wa-a = '001'.
  wa-b = 'COUNT(*) dd01l'.
  APPEND wa TO gt_ex_names.
  wa-a = '002'.
  wa-b = 'COUNT(*) dd02l'.
  APPEND wa TO gt_ex_names.
  wa-a = '003'.
  wa-b = 'COUNT(*) dd03l'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BG1'.
  wa-b = 'AND fieldname LIKE D%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BG2'.
  wa-b = 'as BG1 T%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BG3'.
  wa-b = 'as BG1 A%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'BG4'.
  wa-b = 'as BG1 Q%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QA1'.
  wa-b = 'SELECT no mandt - automatic handling'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QA2'.
  wa-b = 'SELECT CLIENT SPECIFIED sy-mandt'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QA3'.
  wa-b = 'SELECT CLIENT SPECIFIED 000'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QA3'.
  wa-b = 'SELECT CLIENT SPECIFIED all mandt from T000'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QD1'.
  wa-b = 'SELECT range on dynamic for reference'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QD2'.
  wa-b = 'dynamic FROM-clause'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QD3'.
  wa-b = 'dynamic WHERE-clause'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QD4'.
  wa-b = 'dynamic fieldlist'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QM1'.
  wa-b = 'SELECT SINGLE DD03L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QM2'.
  wa-b = 'EXEC SQL SELECT SINGLE DD03L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QM3'.
  wa-b = 'SELECT SINGLE DD30L single-record buffered'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QM4'.
  wa-b = 'EXEC SQL SELECT SINGLE DD30L single-record buffered'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QN1'.
  wa-b = 'SELECT SINGLE T005U single-record buffe'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QN2'.
  wa-b = 'EXEC SQL SELECT SINGLE T005U single-record buffe'.
  APPEND wa TO gt_ex_names.
  wa-a = 'QN3'.
  wa-b = 'EXEC SQL SELECT SINGLE T005U mandt single-record buffe'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SA0'.
  wa-b = 'Improve Nested Selects - Status quo'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SA1'.
  wa-b = 'Improve Nested Selects - FOR ALL ENTRIES'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SA2'.
  wa-b = 'Improve Nested Selects - Subquery'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SA3'.
  wa-b = 'Improve Nested Selects - Join'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SA4'.
  wa-b = 'Improve Nested Selects - View'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SA7'.
  wa-b = 'Improve Nested Selects - Independent SELECTs'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SD0'.
  wa-b = 'Improve Nested Selects - Independent SELECTs'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SD1'.
  wa-b = 'Improve Nested Selects - Status quo'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SD2'.
  wa-b = 'Improve Nested Selects - Join'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SD3'.
  wa-b = 'Improve Nested Selects - FOR ALL ENTRIES'.
  APPEND wa TO gt_ex_names.
  wa-a = 'SD4'.
  wa-b = 'Improve Nested Selects - Subquery'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UX1'.
  wa-b = 'info1 all tables A%, transp and client dependent from DD0'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UX3'.
  wa-b = 'LEFT OUTER JOIN DD02L -> DD03L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UX4'.
  wa-b = 'LEFT OUTER JOIN DD03L -> DD02L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AA1'.
  wa-b = 'SELECT SINGLE full primary key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AA2'.
  wa-b = 'SELECT * INTO TABLE RANGE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AA3'.
  wa-b = 'SELECT * INTO TABLE full table scan'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AA4'.
  wa-b = 'SELECT * INTO TABLE as AB1 no SINGLE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AB1'.
  wa-b = 'SELECT SINGLE full primary key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AB2'.
  wa-b = 'SELECT * INTO TABLE RANGE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AB3'.
  wa-b = 'SELECT * INTO TABLE full table scan'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AB4'.
  wa-b = 'SELECT * INTO TABLE as AB1 no SINGLE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AC1'.
  wa-b = 'SELECT * key1 (tabname position as4local as4vers)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AC2'.
  wa-b = 'SELECT * key2 (rollname)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AC3'.
  wa-b = 'SELECT * key3 (checktable)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'AC4'.
  wa-b = 'SELECT * key7 (domname)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CA1'.
  wa-b = 'WHERE A B C D E'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CA2'.
  wa-b = 'WHERE A B C D CHECK E'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CA3'.
  wa-b = 'WHERE A B CHECK C D E'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CA4'.
  wa-b = 'WHERE A CHECK B C D E'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CB1'.
  wa-b = 'WHERE A B C D E'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CB2'.
  wa-b = 'WHERE A B C D no CHECK'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CB3'.
  wa-b = 'WHERE A B no CHECK'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CB4'.
  wa-b = 'WHERE A no CHECK'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CE1'.
  wa-b = 'WHERE tabname LIKE % AND ...'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CE2'.
  wa-b = 'WHERE tabname LIKE A AND ... - A = %'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CE3'.
  wa-b = 'WHERE tabname IN range AND ... - range = *'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CE4'.
  wa-b = 'WHERE AND ...'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CG1'.
  wa-b = 'WHERE tabname LIKE D% fieldname = TABNAME'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CG2'.
  wa-b = 'WHERE tabname LIKE D% fieldname LIKE TABNAME'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CG3'.
  wa-b = 'WHERE tabname LIKE D% fieldname LIKE T%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CH1'.
  wa-b = 'SELECT * ... fieldname = TABNAME position = 1'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CH2'.
  wa-b = 'SELECT * ... fieldname LIKE TABNAME position = 1'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CM1'.
  wa-b = 'WHERE ( tabname = ... OR tabname =... ) AND ...'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CM2'.
  wa-b = 'WHERE tabname IN ( ..., ... ) AND ...'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CM3'.
  wa-b = 'WHERE tabname LIKE DD0__'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CN1'.
  wa-b = 'WHERE ... AND ( position = 11 or ... position = 20 )'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CN2'.
  wa-b = 'WHERE ... AND position IN (11,...,20)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CN3'.
  wa-b = 'WHERE ... AND position BETWEEN 11 AND 20'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CP1'.
  wa-b = 'SELECT tabname LIKE S% position > 20'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CP2'.
  wa-b = 'SELECT tabname LIKE S% as4local = A position > 20'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CP3'.
  wa-b = 'SELECT tabname LIKE S% as4local IN ( A, ) position'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CQ1'.
  wa-b = 'SELECT * DD12T ddlanguage = E sqltab LIKE S%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CQ2'.
  wa-b = 'SELECT * DD12T ddlanguage <> D sqltab LIKE S%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CQ3'.
  wa-b = 'SELECT * DD12T sqltab LIKE S% CHECK ddlanguage = E'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CQ4'.
  wa-b = 'SELECT * DD12T ddlanguage LIKE % sqltab LIKE S% CHECK'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CU1'.
  wa-b = 'SELECT tabname LIKE D% as4local <> pos = 20'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CU2'.
  wa-b = 'SELECT tabname LIKE D% as4local = A pos = 20'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CU3'.
  wa-b = 'SELECT tabname LIKE D% as4local IN ( A, ) pos = 20'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CX1'.
  wa-b = 'SELECT * ( key2 (rollname) ) OR ( key3 (checktable) )'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CX2'.
  wa-b = 'as AB2'.
  APPEND wa TO gt_ex_names.
  wa-a = 'CX3'.
  wa-b = 'as AB3'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EA1'.
  wa-b = '5 identical conditions in IN'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EA2'.
  wa-b = '5 identical conditions with OR'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EA3'.
  wa-b = '4 conditions IN(1;1) =1 < 2 between 0 1 (overlapping)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EA4'.
  wa-b = '5 different positions'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EC1'.
  wa-b = '5 conditions all identical in itab with LOOP itab'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EC2'.
  wa-b = '5 conditions all identical in itab with FOR ALL ENTRI'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EC3'.
  wa-b = '200 conditions 20 x 10 identical in itab with LOOP itab'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EC4'.
  wa-b = '200 conditions 20 x 10 identical in itab with FOR ALL ENTRI'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EN1'.
  wa-b = 'SELECT tabname LIKE DD%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EN2'.
  wa-b = 'SELECT DISTINCT tabname LIKE DD%'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EN3'.
  wa-b = 'SELECT tabname LIKE DD% SORT DELETE ADJACENT DUPLICATE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EX1'.
  wa-b = 'UP TO 1 ROWS with fully specified key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EX2'.
  wa-b = 'UP TO 1 ROWS with range select'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EX3'.
  wa-b = 'UP TO 1 ROWS with large selectfully specified key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EY1'.
  wa-b = 'SELECT SINGLE with fully specified key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EY2'.
  wa-b = 'SELECT SINGLE with range select'.
  APPEND wa TO gt_ex_names.
  wa-a = 'EY3'.
  wa-b = 'SELECT SINGLE with large selectfully specified key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FA1'.
  wa-b = 'SELECT * No sort-order'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FA2'.
  wa-b = 'SELECT * ORDER BY position fieldname tabname'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FA3'.
  wa-b = 'SELECT * SORT itab BY position fieldname tabname'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FA4'.
  wa-b = 'SELECT * INTO SORTED KEY position fieldname tabname'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FB3'.
  wa-b = 'SELECT * ORDER BY position DESCENDING fieldname tabname'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FB4'.
  wa-b = 'SELECT * ORDER BY PRIMARY key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FD1'.
  wa-b = 'SELECT ORDER BY position tabname fieldname DESCENDING'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FD2'.
  wa-b = 'SELECT UP TO 10 ROWS'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FD3'.
  wa-b = 'SELECT UP TO 10 ROWS ORDER BY position tabname'.
  APPEND wa TO gt_ex_names.
  wa-a = 'FD4'.
  wa-b = 'SELECT UP TO 10 ROWS ORDER BY PRIMARY KEY'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GA1'.
  wa-b = 'SELECT zaehl t006 ENDSELECT IF ( max ) BYPASS BUF'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GA2'.
  wa-b = 'SELECT zaehl t006 INTO TABLE LOOP max ENDLOOP BYPASS BUF'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GA3'.
  wa-b = 'SELECT MAX( zaehl ) t006'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GA4'.
  wa-b = 'SELECT zaehl t006 ENDSELECT IF ( max )'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GA5'.
  wa-b = 'SELECT zaehl t006 INTO TABLE LOOP max ENDLOOP'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GG1'.
  wa-b = 'SELECT *'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GG2'.
  wa-b = 'SELECT COUNT(*)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GG2'.
  wa-b = 'SELECT COUNT(*) UP TO 1 ROWS'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GG3'.
  wa-b = 'SELECT key1 UP TO 1 ROWS (Index-only poss)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GH1'.
  wa-b = 'SELECT * UP TO 1 ROWS'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GH2'.
  wa-b = 'SELECT field UP TO 1 ROWS'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GH3'.
  wa-b = 'SELECT key1 UP TO 1 ROWS (Index-only poss)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GI1'.
  wa-b = 'SELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GI2'.
  wa-b = 'SELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GP1'.
  wa-b = 'SELECT/ENDSELECT - PACKAGE SIZE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'GP2'.
  wa-b = 'OPEN CURSOR - FETCH NEXT CURSOR - PACKAGE SIZE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IA1'.
  wa-b = 'Array select ... endselect'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IB1'.
  wa-b = 'SELECT / ENDSELECT Maximum of Positions'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IB2'.
  wa-b = 'SELECT INTO TABLE Maximum of Positions'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IX1'.
  wa-b = 'Array SELECT/ENDSELECT BYPASSING BUFFER'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IX2'.
  wa-b = 'Array SELECT INTO TABLE BYPASSING BUFFER'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IX3'.
  wa-b = 'Array SELECT/ENDSELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IX4'.
  wa-b = 'Array SELECT INTO TABLE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IY1'.
  wa-b = 'Array SELECT/ENDSELECT BYPASSING BUFFER'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IY2'.
  wa-b = 'Array SELECT INTO TABLE BYPASSING BUFFER'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IY3'.
  wa-b = 'Array SELECT/ENDSELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'IY4'.
  wa-b = 'Array SELECT INTO TABLE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'JA1'.
  wa-b = 'SELECT SINGLE in LOOP'.
  APPEND wa TO gt_ex_names.
  wa-a = 'JA2'.
  wa-b = 'SELECT RANGES table'.
  APPEND wa TO gt_ex_names.
  wa-a = 'JA3'.
  wa-b = 'Test: Identical records by range SELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KA1'.
  wa-b = 'SELECT FAE 3 fields'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KA2'.
  wa-b = 'SELECT FAE only 1 field'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KC1'.
  wa-b = 'FAE 100 lines in itab'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KC2'.
  wa-b = 'FAE 0 lines in itab'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KC3'.
  wa-b = 'FAE 100 lines in itab 2.condition'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KC4'.
  wa-b = 'FAE 0 lines in itab 2.condition'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KG2'.
  wa-b = 'FAE COUNT(*) info1 and info2'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KG3'.
  wa-b = 'FAE reduces duplicates'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KG4'.
  wa-b = 'FAE with DELETE ADJACENT DUPLICATES before SELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KK1'.
  wa-b = 'FAE 1:1 Relation SELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KK2'.
  wa-b = 'FAE 1:1 Relation LOOP/READ Standard Table'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KK3'.
  wa-b = 'FAE 1:1 Relation LOOP/READ Standard Table RB'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KK4'.
  wa-b = 'FAE 1:1 Relation LOOP/READ Sorted Table'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KK5'.
  wa-b = 'FAE 1:1 Relation LOOP/READ Hashed Table'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KL1'.
  wa-b = 'FAE 1:C Relation SELECT'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KL2'.
  wa-b = 'FAE 1:C Relation LOOP/LOOP WHERE Standard Table'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KL3'.
  wa-b = 'FAE 1:C Relation LOOP/LOOP WHERE Standard Table BINARY SE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'KL4'.
  wa-b = 'FAE 1:C Relation LOOP/LOOP WHERE Sorted Table'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MA1'.
  wa-b = 'SELECT range * INTO FIELDS OF st'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MA2'.
  wa-b = 'SELECT range 293 INTO FIELDS OF st_293'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MA3'.
  wa-b = 'SELECT range 244 INTO FIELDS OF st_244'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MA4'.
  wa-b = 'SELECT range 139 INTO FIELDS OF st_139'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MA5'.
  wa-b = 'SELECT range 70 INTO FIELDS OF st_70'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MB1'.
  wa-b = 'SELECT FAE * INTO FIELDS OF st'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MB2'.
  wa-b = 'SELECT FAE 293 INTO FIELDS OF st_293'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MB3'.
  wa-b = 'SELECT FAE 244 INTO FIELDS OF st_244'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MB4'.
  wa-b = 'SELECT FAE 139 INTO FIELDS OF st_139'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MB5'.
  wa-b = 'SELECT FAE 70 INTO FIELDS OF st_70'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MC1'.
  wa-b = 'SELECT SINGLE * INTO FIELDS OF st'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MC2'.
  wa-b = 'SELECT SINGLE 293 INTO FIELDS OF st_293'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MC3'.
  wa-b = '... INTO TABLE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MC3'.
  wa-b = 'SELECT SINGLE 244 INTO FIELDS OF st_244'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MC4'.
  wa-b = 'SELECT SINGLE 139 INTO FIELDS OF st_139'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MC5'.
  wa-b = 'SELECT SINGLE 70 INTO FIELDS OF st_70'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MD1'.
  wa-b = 'SELECT range * INTO FIELDS OF st (full table scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MD2'.
  wa-b = 'SELECT range 293 INTO FIELDS OF st_293 (full table scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MD3'.
  wa-b = 'SELECT range 244 INTO FIELDS OF st_244 (full table scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MD4'.
  wa-b = 'SELECT range 139 INTO FIELDS OF st_139 (full table scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MD5'.
  wa-b = 'SELECT range 70 INTO FIELDS OF st_70 (full table scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MI1'.
  wa-b = '... alphabetic'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MI2'.
  wa-b = '... random order'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MI3'.
  wa-b = '... INTO TABLE (MA3)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MI4'.
  wa-b = '... more fields, random'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MJ1'.
  wa-b = '... alphabetic'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MJ2'.
  wa-b = '... random order'.
  APPEND wa TO gt_ex_names.
  wa-a = 'MJ4'.
  wa-b = '... more fields, random'.
  APPEND wa TO gt_ex_names.
  wa-a = 'NA1'.
  wa-b = 'SELECT range key+1 INTO FIELDS OF st_70'.
  APPEND wa TO gt_ex_names.
  wa-a = 'NA2'.
  wa-b = 'SELECT range key INTO FIELDS OF st_key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'NB1'.
  wa-b = 'SELECT FAE key+1 INTO FIELDS OF st_70'.
  APPEND wa TO gt_ex_names.
  wa-a = 'NB2'.
  wa-b = 'SELECT FAE key INTO FIELDS OF st_key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'NC1'.
  wa-b = 'SELECT SINGLE key+1 INTO FIELDS OF st_70'.
  APPEND wa TO gt_ex_names.
  wa-a = 'NC2'.
  wa-b = 'SELECT SINGLE key INTO FIELDS OF st_key'.
  APPEND wa TO gt_ex_names.
  wa-a = 'ND1'.
  wa-b = 'SELECT range key +1 INTO FIELDS OF st_70 (full table scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'ND2'.
  wa-b = 'SELECT range key INTO FIELDS OF st_key (index full scan)'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF1'.
  wa-b = 'JOIN with 2 tables (A,B) and 2 ranges'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF2'.
  wa-b = 'FOR ALL ENTRIES A then B'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF3'.
  wa-b = 'FOR ALL ENTRIES B then A'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF5'.
  wa-b = 'same SELECT only on A - range not empty'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF6'.
  wa-b = 'same SELECT only on A - range empty'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF7'.
  wa-b = 'same SELECT only on B - range not empty'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UF8'.
  wa-b = 'same SELECT only on B - range empty'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UG1'.
  wa-b = 'Join on 5 tables: tabname, fieldname, ddlanguage,'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UG2'.
  wa-b = 'as XL2 with FOR ALL ENTRIES'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UJ1'.
  wa-b = 'Self-JOIN'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UJ2'.
  wa-b = 'Subquery'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UJ3'.
  wa-b = 'FAE'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UJ4'.
  wa-b = 'Aggregate GROUP BY HAVING'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UM1'.
  wa-b = 'SELECT * DD02L client-dependent tables'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UM2'.
  wa-b = 'SELECT * DD03L with fields CLIENT ...'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UM3'.
  wa-b = 'SELECT JOIN first DD02L then DD03L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UM4'.
  wa-b = 'SELECT JOIN first DD03L then DD02L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UN1'.
  wa-b = 'FAE first DD02L then DD03L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UN2'.
  wa-b = 'FAE first DD03L then DD02L'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UR1'.
  wa-b = 'Prepare info1 = lines to select, info2 = lines in first tab'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UR2'.
  wa-b = 'JOIN - result lines'.
  APPEND wa TO gt_ex_names.
  wa-a = 'UR3'.
  wa-b = 'FOR ALL ENTRIES - INTO SORTED TABLE and nested loop'.
  APPEND wa TO gt_ex_names.

  SORT gt_ex_names BY a.

ENDFORM.                    "fill_texts
*-----------------------------------------------------------------------
*---------------------------------------------------------
