REPORT znote1813133.

PARAMETERS testrun  RADIOBUTTON GROUP mode.
PARAMETERS update   RADIOBUTTON GROUP mode.
PARAMETERS genview  RADIOBUTTON GROUP mode.
PARAMETERS showlogs RADIOBUTTON GROUP mode.
DATA: gv_copy_translation.

  TYPE-POOLS: seox, seoc, seok, seex, bcwbn, ststc, trmtd, trexe.

  TYPES: t_switch_id TYPE char30,   "sfw_switch_id does not exist in SAP_BASIS 640
         t_bfunction TYPE char30,  "sfw_bfunction does not exist in SAP_BASIS 640
         BEGIN OF t_bf_sw, "sfw_bf_sw
            switch_id TYPE t_switch_id,
            version,
            bfunction TYPE t_bfunction,
            no_check,
         END OF t_bf_sw,
         BEGIN OF t_permission, client_pak(30), intf_name(30), err_sever(4), END OF t_permission,  "has to be in sync with structure permission
         BEGIN OF dd43v,typename(30),seckeyname(30),ddlanguage,seckeyunique,accessmode,kind,keydescription(80),END OF dd43v.

  TYPES: tt_dd05m  TYPE TABLE OF dd05m,
         tt_dd17v  TYPE TABLE OF dd17v,
         tt_dd26v  TYPE TABLE OF dd26v,
         tt_dd27p  TYPE TABLE OF dd27p,
         tt_dd28v  TYPE TABLE OF dd28v,
         tt_dd30v  TYPE TABLE OF dd30v,
         tt_dd30tv TYPE TABLE OF dd30tv,
         tt_dd31v  TYPE TABLE OF dd31v,
         tt_dd32v  TYPE TABLE OF dd32v,
         tt_dd32p  TYPE TABLE OF dd32p,
         tt_dd33v  TYPE TABLE OF dd33v,
         tt_dd36m  TYPE TABLE OF dd36m,
         tt_dd42v  TYPE TABLE OF dd42v,
         tt_dd43v  TYPE TABLE OF dd43v,
         tt_e071k  TYPE TABLE OF e071k,
         tt_permission TYPE TABLE OF t_permission,
         tt_tstca  TYPE TABLE OF tstca,
         tt_langu  TYPE TABLE OF sylangu,
         tt_sta    TYPE TABLE OF rsmpe_stat,
         tt_fun    TYPE TABLE OF rsmpe_funt,
         tt_men    TYPE TABLE OF rsmpe_men,
         tt_mtx    TYPE TABLE OF rsmpe_mnlt,
         tt_act    TYPE TABLE OF rsmpe_act,
         tt_but    TYPE TABLE OF rsmpe_but,
         tt_pfk    TYPE TABLE OF rsmpe_pfk,
         tt_set    TYPE TABLE OF rsmpe_staf,
         tt_doc    TYPE TABLE OF rsmpe_atrt,
         tt_tit    TYPE TABLE OF rsmpe_titt,
         tt_biv    TYPE TABLE OF rsmpe_buts.

  DATA: gr_devclass     TYPE RANGE OF devclass,
        gr_domname      TYPE RANGE OF domname,
        gr_rollname     TYPE RANGE OF rollname,
        gr_tabname      TYPE RANGE OF tabname,  "structures and database tables
        gr_dbtabname    TYPE RANGE OF tabname,  "only database tables
        gt_tabname      TYPE TABLE OF tabname,
        gr_indexes      TYPE RANGE OF trobj_name,
        gr_indxtab      TYPE RANGE OF tabname,
        gr_indxname     TYPE RANGE OF indexid,
        gr_shlpname     TYPE RANGE OF shlpname,
        gr_enqname      TYPE RANGE OF enqname,
        gr_viewname     TYPE RANGE OF viewname,
        gr_ttypname     TYPE RANGE OF ttypename,
        gt_ttypname     TYPE TABLE OF ttypename,
        gr_guistatus    TYPE RANGE OF progname,
        gr_switchname   TYPE RANGE OF t_switch_id,
        gr_msg_class    TYPE RANGE OF msgid,
        gv_trkorr       TYPE trkorr,
        gt_trkey        TYPE TABLE OF trkey,
        gv_unit_test,
        gv_translation,
        gv_repository_changed,
        gv_errors_occured,
        gv_uname        TYPE syuname,
        gv_log_handle   TYPE balloghndl.


  CONSTANTS: c_bal_object  TYPE balobj_d  VALUE 'SNOTE',
             c_bal_subobj  TYPE balsubobj VALUE space,
             c_bal_context TYPE tabname   VALUE 'ADIR_KEY',
             c_include6    TYPE fieldname VALUE '.INCLU',  ".INCLUDE or .INCLU--AP or .INCLU-_BY
             c_memory_id(40) VALUE 'SAP_LOCAL_DOWNPORT_ASSISTANT',
             c_logical_object VALUE 'L'.

  CONSTANTS: gc_report   TYPE text15    VALUE 'Report',     "#EC NOTEXT
             gc_local    LIKE gc_report VALUE 'Local',      "#EC NOTEXT
             gc_snote    LIKE gc_report VALUE 'SNote',      "#EC NOTEXT
             gc_manual   LIKE gc_report VALUE 'Manual Instr. ', "#EC NOTEXT
             gc_bcset    LIKE gc_report VALUE 'Manual/BC-Set', "#EC NOTEXT
             gc_ignore   LIKE gc_report VALUE '-',          "#EC NOTEXT
             gc_generate LIKE gc_report VALUE 'Generation', "#EC NOTEXT
             gc_suppack  LIKE gc_report VALUE 'Supp.Pack.', "#EC NOTEXT
             gc_dest_init TYPE rfcdest VALUE 'MISSING INITIALIZATION'. "#EC NOTEXT

  DATA: akb_get_tadir TYPE funcname VALUE 'AKB_GET_TADIR'.

*&---------------------------------------------------------------------*
*&      Form  bal_callback_ucomm
*&---------------------------------------------------------------------*
*       called from ballog, e.g. during double click
*----------------------------------------------------------------------*
FORM bal_callback_ucomm CHANGING c_state TYPE bal_s_cbuc.   "#EC CALLED
  DATA: ls_msg TYPE bal_s_msg,
        l_date(8),
        ls_tadir TYPE adir_key.
  CASE c_state-ucomm.
    WHEN '%LONGTEXT' OR '&IC1'.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = c_state-list_msgh
        IMPORTING
          e_s_msg        = ls_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF ls_msg-msgid EQ 'DO' AND ls_msg-msgno EQ '626'.
        "MESSAGE i626(do) WITH 'Click this message to view' 'detailed activation logs with name' l_logname INTO sy-lisel. "#EC *
        SPLIT ls_msg-msgv3 AT ':' INTO sy-lisel l_date.
        SUBMIT radprotb WITH protname EQ ls_msg-msgv3
                        WITH date     EQ l_date
*                       with TIME
*                       with USER
                        AND RETURN.
        c_state-ucomm_exec = 'X'.
      ELSEIF c_state-ucomm EQ '&IC1'.
        ls_tadir = ls_msg-context-value.
        CHECK ls_tadir-object IS NOT INITIAL.
        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
            operation   = 'SHOW'
            object_name = ls_tadir-obj_name
            object_type = ls_tadir-object
          EXCEPTIONS
            OTHERS      = 1.
        CHECK sy-subrc EQ 0.
        c_state-ucomm_exec = 'X'.
      ENDIF.
  ENDCASE.
ENDFORM.                    "bal_callback_ucomm
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
DEFINE bdc_field.
  clear: ls_bdc.
  ls_bdc-fnam = &1.
  ls_bdc-fval = &2.
  append ls_bdc to lt_bdc.
END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
DEFINE bdc_screen.
  clear ls_bdc.
  ls_bdc-program  = &1.
  ls_bdc-dynpro   = &2.
  ls_bdc-dynbegin = 'X'.
  append ls_bdc to lt_bdc.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
*       CLASS lcl_wb DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_wb DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS init
       IMPORTING
         value(i_note)  TYPE clike OPTIONAL      "note which indicates that report is not needed anymore
         value(i_cinst) TYPE cwbcialeid OPTIONAL "correction instruction which describes the validity of the report
       EXCEPTIONS
         stop_processing.

    CLASS-METHODS get_object_text
      IMPORTING
        i_object   TYPE e071-object    "e.g. TABD
        i_obj_name TYPE c OPTIONAL     "for DOCU
      RETURNING value(r_text) TYPE string.

    CLASS-METHODS get_function_pool_master
      IMPORTING
        i_area  TYPE c
      RETURNING value(r_master) TYPE progdir-name.

    CLASS-METHODS create_doma
      IMPORTING
        i_domname    TYPE domname
        i_datatype   TYPE dd01v-datatype OPTIONAL
        i_leng       TYPE dd01v-leng OPTIONAL
        i_outputlen  TYPE dd01v-outputlen OPTIONAL
        i_convexit   TYPE dd01v-convexit  OPTIONAL
        i_decimals   TYPE dd01v-decimals  OPTIONAL
        i_signflag   TYPE dd01v-signflag  OPTIONAL
        i_lowercase  TYPE dd01v-lowercase OPTIONAL
        i_entitytab  TYPE dd01v-entitytab OPTIONAL
        i_appendname TYPE domname OPTIONAL  "appendname not known in all releases
        i_devclass   TYPE devclass
        i_langu      TYPE sylangu OPTIONAL
        i_ddtext     TYPE ddtext OPTIONAL
        i_domvalue   TYPE dd07v-domvalue_l OPTIONAL
        i_valpos     TYPE dd07v-valpos OPTIONAL
        i_valtext    TYPE dd07v-ddtext OPTIONAL.

    CLASS-METHODS create_dtel
      IMPORTING
        i_rollname  TYPE rollname
        i_domname   TYPE domname
        i_datatype  TYPE dd04v-domname   OPTIONAL  "only needed if domain is initial
        i_decimals  TYPE dd04v-decimals  OPTIONAL  "only needed if rollname is initial
        i_leng      TYPE dd04v-leng      OPTIONAL  "only needed if domain is initial
        i_devclass  TYPE devclass
        i_logflag   TYPE logflag DEFAULT 'X'
        i_memoryid  TYPE dd04v-memoryid OPTIONAL
        i_headlen   TYPE dd04v-headlen OPTIONAL  "defaulted with fieldlen
        i_scrlen1   TYPE dd04v-scrlen1 DEFAULT 10
        i_scrlen2   TYPE dd04v-scrlen2 DEFAULT 15
        i_scrlen3   TYPE dd04v-scrlen3 DEFAULT 20
        i_langu     TYPE sylangu
        i_ddtext    TYPE ddtext
        i_reptext   TYPE reptext OPTIONAL
        i_scrtext_m TYPE scrtext_m
        i_scrtext_l TYPE scrtext_l
        i_scrtext_s TYPE scrtext_s
        i_shlpname  TYPE shlpname OPTIONAL
        i_shlpfield TYPE shlpfield OPTIONAL
        i_deffdname TYPE deffdname OPTIONAL
        i_proxytype TYPE ddproxyty OPTIONAL.

    CLASS-METHODS create_search_help
       IMPORTING
         i_shlpname       TYPE ddobjname
         i_langu          TYPE sylangu          DEFAULT 'E' "Default EN
         i_ddtext         TYPE ddtext           OPTIONAL
         i_devclass       TYPE devclass
         i_issimple       TYPE dd30v-issimple   DEFAULT 'X'
         i_elemexi        TYPE dd30v-elemexi    OPTIONAL
         i_nofields       TYPE dd30v-nofields   OPTIONAL
         i_attachexi      TYPE dd30v-attachexi  OPTIONAL
         i_selmethod      TYPE dd30v-selmethod
         i_selmtype       TYPE dd30v-selmtype   DEFAULT 'T'
         i_texttab        TYPE dd30v-texttab
         i_selmexit       TYPE dd30v-selmexit   OPTIONAL "name of function module
         i_function_group TYPE tlibg-area       OPTIONAL "name of function group for new selmexit
         i_hotkey         TYPE dd30v-hotkey     OPTIONAL
         i_dialogtype     TYPE dd30v-dialogtype DEFAULT 'D'
         it_dd31v         TYPE tt_dd31v
         it_dd33v         TYPE tt_dd33v
         it_dd32p         TYPE tt_dd32p.

    CLASS-METHODS create_table    "or structure or append or database table
      IMPORTING
        i_tabname    TYPE tabname
        i_devclass   TYPE devclass
        i_exclass    TYPE dd02v-exclass  OPTIONAL
        i_langu      TYPE sylangu   DEFAULT 'E'   "Default EN
        i_ddtext     TYPE ddtext    OPTIONAL      "can be taken from tabname
        i_authclass  TYPE dd02v-authclass DEFAULT '00'
        i_mainflag   TYPE dd02v-mainflag OPTIONAL
        i_tabclass   TYPE tabclass  DEFAULT 'INTTAB'
        i_sqltab     TYPE sqlappdtab OPTIONAL
        i_proxytype  TYPE ddproxyty         OPTIONAL
        i_contflag   TYPE contflag         DEFAULT 'C'     "only needed for db tables
        i_tabkat     TYPE dd09v-tabkat     DEFAULT '0'     "only needed for db tables
        i_tabart     TYPE dd09v-tabart     DEFAULT 'APPL2' "only needed for db tables
        i_bufallow   TYPE dd09v-bufallow   DEFAULT 'N'     "only needed for db tables
        i_pufferung  TYPE dd09v-pufferung  OPTIONAL        "only needed for db tables
        i_schfeldanz TYPE dd09v-schfeldanz OPTIONAL        "only needed for db tables
        i_speichpuff TYPE dd09v-speichpuff OPTIONAL        "only needed for db tables
        i_javaonly   TYPE xfeld OPTIONAL  "dd09v-javaonly  "only needed for db tables   ">=SAPBASIS700
        i_protokoll  TYPE dd09v-protokoll  OPTIONAL.       "only needed for db tables

    CLASS-METHODS add_field_to_table
      IMPORTING
        i_tabname   TYPE tabname
        i_keyflag   TYPE keyflag DEFAULT space
        i_anonymous TYPE xfeld OPTIONAL "dd03p-anonymous
        i_fieldname TYPE fieldname
        i_rollname  TYPE rollname       OPTIONAL  "data element, structure, table_type, include/append name
        i_datatype  TYPE dd03p-datatype OPTIONAL  "only needed if rollname is initial
        i_decimals  TYPE dd03p-decimals OPTIONAL  "only needed if rollname is initial
        i_leng      TYPE dd03p-leng     OPTIONAL  "only needed if rollname is initial
        i_langu     TYPE dd03p-ddlanguage OPTIONAL  "only needed if rollname is initial
        i_ddtext    TYPE dd03p-ddtext   OPTIONAL  "only needed if rollname is initial
        i_reftable  TYPE dd03p-reftable OPTIONAL
        i_reffield  TYPE dd03p-reffield OPTIONAL
        i_notnull   TYPE dd03p-notnull  OPTIONAL
        i_languflag TYPE dd03p-languflag OPTIONAL
        i_groupname TYPE ddgroup        OPTIONAL
        is_dd08v    TYPE dd08v          OPTIONAL   "foreign key relationship
        it_dd05m    TYPE tt_dd05m       OPTIONAL   "foreign key relationship fields
        i_context   TYPE fieldname      OPTIONAL.  "name of previous field or include, blank->append

    CLASS-METHODS create_view
      IMPORTING
        i_viewname   TYPE viewname
        i_devclass   TYPE devclass
        i_langu      TYPE sylangu DEFAULT 'E'   "Default EN
        i_ddtext     TYPE ddtext OPTIONAL      "can be taken from tabname
        i_contflag   TYPE dd25v-customauth DEFAULT 'C'
        i_aggtype    TYPE dd25v-aggtype
        i_roottab    TYPE dd25v-roottab
        i_viewclass  TYPE dd25v-viewclass
        i_viewgrant  TYPE dd25v-viewgrant
        i_readonly   TYPE dd25v-readonly   OPTIONAL
        i_globalflag TYPE dd25v-globalflag OPTIONAL
        it_dd26v     TYPE tt_dd26v    "basis tables
        "it_dd28j    TYPE tt_dd28j    "join conditions
        it_dd28v    TYPE tt_dd28v.   "view conditions

    CLASS-METHODS add_field_to_view
      IMPORTING
        i_viewname   TYPE viewname
        i_viewfield  TYPE viewfield OPTIONAL  "omit if same like fieldname
        i_tabname    TYPE tabname
        i_fieldname  TYPE fieldname
        i_keyflag    TYPE dd27v-keyflag    OPTIONAL
        i_rollchange TYPE dd27v-rollchange OPTIONAL
        i_rollname   TYPE dd27v-rollname   OPTIONAL  "can be taken from tabfield
        i_rdonly     TYPE dd27v-rdonly     OPTIONAL
        i_context    TYPE fieldname        OPTIONAL. "name of previous field or include, blank->append

    CLASS-METHODS generate_maint_view                     "SE54
      IMPORTING
          i_devclass    TYPE devclass
          i_objectname  TYPE objh-objectname
          i_objecttype  TYPE objh-objecttype DEFAULT 'V'
          i_masterlang  TYPE masterlang      DEFAULT 'E'
          i_auth_group  TYPE tddat-cclass    DEFAULT '&NC&'
          i_func_group  TYPE tvdir-area
          i_dynp_list   TYPE tvdir-liste
          i_dynp_detail TYPE tvdir-liste OPTIONAL.

    CLASS-METHODS create_message_class
      IMPORTING
         i_devclass TYPE devclass
         i_msgid    TYPE msgid
         i_langu    TYPE sylangu DEFAULT 'E'   "Default EN
         i_text     TYPE natxt.

    CLASS-METHODS create_message
      IMPORTING
         i_msgid TYPE msgid
         i_msgno TYPE msgno
         i_langu TYPE sylangu DEFAULT 'E'   "Default EN
         i_text  TYPE natxt
         i_selfdef TYPE doku_selfd DEFAULT 'X'.

    CLASS-METHODS create_transaction
      IMPORTING
        i_tcode     TYPE tstc-tcode
        i_devclass  TYPE devclass OPTIONAL
        i_langu     TYPE sylangu DEFAULT 'E'
        i_text      TYPE tstct-ttext    OPTIONAL
        i_type      LIKE ststc_c_type_dialog DEFAULT 'D'
        i_param     TYPE tstcp-param    OPTIONAL
        it_auth_obj TYPE tt_tstca       OPTIONAL
        i_program   TYPE tstc-pgmna     OPTIONAL "only for report transaction
        i_dynpro    TYPE sydynnr        OPTIONAL "tstc-dypno
        i_javagui   TYPE tstcc-s_platin DEFAULT 'X'
        i_htmlgui   TYPE tstcc-s_webgui DEFAULT 'X'
        i_wingui    TYPE tstcc-s_win32  DEFAULT 'X'.

    CLASS-METHODS create_dtel_docu
      IMPORTING
         i_rollname  TYPE rollname
         i_langu     TYPE sylangu DEFAULT 'E'   "Default EN
         i_line      TYPE string OPTIONAL "all lines get collected until i_line is omitted
       EXPORTING
         et_lines    TYPE tline_tab.

    CLASS-METHODS create_mess_docu
      IMPORTING
         i_msgid     TYPE msgid
         i_msgno     TYPE msgno
         i_langu     TYPE sylangu DEFAULT 'E'   "Default EN
         i_line      TYPE string OPTIONAL  "all lines get collected until i_line is omitted
       EXPORTING
         et_lines    TYPE tline_tab.

    CLASS-METHODS create_other_docu
      IMPORTING
         i_devclass  TYPE devclass OPTIONAL   "only needed for new objects with own TADIR
         i_docname   TYPE thead-tdname        "e.g. TXCDESK_PARAM, where TX represents general text
         i_typ       TYPE dokil-typ DEFAULT 'E'    "E or T
         i_langu     TYPE sylangu DEFAULT 'E'   "Default EN
         i_line      TYPE string OPTIONAL  "all lines get collected until i_line is omitted
       EXPORTING
         et_lines    TYPE tline_tab.

    CLASS-METHODS create_function_group
       IMPORTING
          i_area     TYPE tlibg-area
          i_devclass TYPE devclass
          i_langu    TYPE sylangu DEFAULT 'E'
          i_text     TYPE tlibt-areat OPTIONAL
          i_appl     TYPE trdir-appl    OPTIONAL
          i_fixpt    TYPE trdir-fixpt   DEFAULT 'X'
          i_uccheck  TYPE trdir-uccheck DEFAULT 'X'.

    CLASS-METHODS create_logical_object
      IMPORTING
        i_object   TYPE e071-object  "VCLS, AUTH, OSOD, SFRN
        i_obj_name TYPE c     "flexible number of characters
        i_devclass TYPE devclass OPTIONAL
        i_langu    TYPE sylangu DEFAULT 'E'
      EXCEPTIONS
        error.

    CLASS-METHODS execute_method
      IMPORTING
        i_method   TYPE c.    "AFTER_IMP or BEFORE_EXP

    CLASS-METHODS create_table_entries
      IMPORTING
         i_tabname        TYPE tabname
         it_entries       TYPE STANDARD TABLE OPTIONAL
         i_logical_object TYPE xfeld OPTIONAL
       CHANGING
         c_deliver        TYPE c OPTIONAL.

    CLASS-METHODS add_to_transport
      IMPORTING
        i_object   TYPE e071-object    "TABD
        i_obj_name TYPE c     "flexible number of characters
        i_devclass TYPE devclass OPTIONAL
        i_langu    TYPE sylangu OPTIONAL
      EXCEPTIONS
        error
        simulation.

    CLASS-METHODS register_inactive_object
        IMPORTING
           i_objtype  TYPE e071-object
           i_treetype TYPE e071-object OPTIONAL  "space = do not update tree, optionally different type in tree, e.g. TABL vs. CDS
           i_objname TYPE c.

    CLASS-METHODS log_message.

    CLASS-METHODS display_log.

    CLASS-METHODS set_context
      IMPORTING i_object    TYPE e071-object
                i_obj_name  TYPE c
                i_obj_name2 TYPE c OPTIONAL
                i_text      TYPE c OPTIONAL
                i_langu     TYPE sylangu.

    CLASS-METHODS activate
      IMPORTING i_result_auth_check TYPE sysubrc OPTIONAL.

    CLASS-METHODS mass_activation
      IMPORTING
        i_step     TYPE i      DEFAULT 1
        i_inactive TYPE c      OPTIONAL
        i_ddmode   TYPE ddmode DEFAULT 'O'
        i_logname  TYPE c
       EXCEPTIONS
         error.
ENDCLASS.                    "lcl_wb DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_dcm_to_po_bridge IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_wb IMPLEMENTATION.

  METHOD init.


    CLEAR gv_errors_occured.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SNOTE'
      EXCEPTIONS
        ok     = 0
        OTHERS = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gv_errors_occured = 'X'.
      RAISE stop_processing.
    ENDIF.

    IF showlogs IS NOT INITIAL.
      CALL METHOD display_log.
      LEAVE PROGRAM.
    ENDIF.

    DATA: ls_log TYPE bal_s_log.
    "prepare application log
    ls_log-object    = c_bal_object.
    ls_log-subobject = c_bal_subobj.
    ls_log-extnumber = sy-cprog.
    ls_log-aldate    = sy-datum.
    ls_log-altime    = sy-uzeit.
    ls_log-aluser    = sy-uname.
    ls_log-alprog    = sy-repid.
    ls_log-altcode   = sy-tcode.
    "create standard application log handle
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = gv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    EXPORT gv_log_handle FROM gv_log_handle TO MEMORY ID c_memory_id.

    CALL METHOD set_context
      EXPORTING
        i_langu    = space
        i_object   = space
        i_obj_name = space.


    "Main check: Last changed by SAP -> Report came via SP / Upgrade
    "==> no execution anymore.
    IF sy-cprog NE 'SAP_LOCAL_DOWNPORT_ASSISTANT'.
      SELECT SINGLE prog FROM d010sinf INTO sy-lisel
                        WHERE prog   EQ sy-repid
                          AND ( unam EQ 'SAP' OR cnam EQ 'SAP' ).
      IF sy-subrc EQ 0.
        MESSAGE w666(01) WITH 'The neccessary changes already came' 'via support package.' 'Report execution not required anymore.' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        CALL METHOD display_log.
        RAISE stop_processing.
      ENDIF.
    ENDIF.

    IF i_note IS NOT INITIAL.
*     that that note is not already implemented
*     this must not be the note containing the generated report for DDIC but a follow-up note with the coding using that new DDIC
      DATA l_status TYPE c.
      CALL FUNCTION 'SCWB_NOTE_IMPL_STATUS_SIMPLE'
        EXPORTING
          iv_numm   = i_note
        IMPORTING
          ev_status = l_status
        EXCEPTIONS
          OTHERS    = 1.
      CASE l_status.
        WHEN 'E'.
          MESSAGE w666(01) WITH 'SAP Note' i_note 'completely implemented;' 'Report execution not required anymore.' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          CALL METHOD display_log.
          RAISE stop_processing.
        WHEN 'O'.
          MESSAGE w666(01) WITH 'SAP Note' i_note 'is obsolete;' 'Report execution not required anymore.' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          CALL METHOD display_log.
          RAISE stop_processing.
        WHEN OTHERS. "okay
          DATA: ls_note TYPE  bcwbn_note.
          ls_note-key-numm = i_note.
          CALL FUNCTION 'SCWB_NOTE_READ'
            EXPORTING
              iv_read_corr_instructions  = 'X'
            CHANGING
              cs_note                    = ls_note
            EXCEPTIONS
              note_not_found             = 1
              language_not_found         = 2
              unreadable_text_format     = 3
              corr_instruction_not_found = 4
              OTHERS                     = 5.
          IF sy-subrc <> 0.
            MESSAGE e666(01) WITH 'SAP Note' i_note 'cannot be read;' 'Ensure correct download of note first.' INTO sy-lisel. "#EC *
            CALL METHOD log_message.
            CALL METHOD display_log.
            RAISE stop_processing.
          ENDIF.
*------
          FIELD-SYMBOLS: <ls_corr> TYPE bcwbn_corr_instruction.
          DATA: l_valid TYPE bcwbn_bool.
          LOOP AT ls_note-corr_instructions ASSIGNING <ls_corr>.
            IF i_cinst IS NOT INITIAL.
              CHECK <ls_corr>-key-aleid = i_cinst.
            ENDIF.
            CALL FUNCTION 'SCWB_CINST_CHECK_VALID'
              IMPORTING
                ev_valid                   = l_valid
              CHANGING
                cs_corr_instruction        = <ls_corr>
              EXCEPTIONS
                corr_inst_not_found        = 1
                inconsistent_delivery_data = 2
                undefined                  = 3
                OTHERS                     = 4.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            IF l_valid IS NOT INITIAL.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_valid IS INITIAL.
            MESSAGE e666(01) WITH 'Manual changes cannot be applied;' 'Correction instruction invalid' 'for current patch level' INTO sy-lisel. "#EC *
            CALL METHOD log_message.
            CALL METHOD display_log.
            RAISE stop_processing.
          ENDIF.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'SCWG_TOOLFLAG_SET'. "Reset will happen at leave of program

    IF testrun IS INITIAL.
      MESSAGE i666(01) WITH 'Running in Update Mode' INTO sy-lisel. "#EC *
    ELSE.
      MESSAGE w666(01) WITH 'Running in Test Mode' INTO sy-lisel. "#EC *
    ENDIF.
    CALL METHOD log_message.

  ENDMETHOD.                    "init


  METHOD get_object_text.

    STATICS: lt_object_text TYPE TABLE OF ko100,
             lt_doktypes TYPE TABLE OF dd07v.

    DATA: ls_object_text TYPE ko100,
          ls_doktype TYPE dd07v.

    IF lt_object_text IS INITIAL.
      CALL FUNCTION 'TR_OBJECT_TABLE'
        TABLES
          wt_object_text = lt_object_text.
      SORT lt_object_text BY object.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name      = 'DOK_ID'
          langu     = sy-langu
        TABLES
          dd07v_tab = lt_doktypes
        EXCEPTIONS
          OTHERS    = 0.
      SORT lt_doktypes BY domvalue_l.
    ENDIF.

    IF i_object EQ 'DOCU'.
      READ TABLE lt_doktypes INTO ls_doktype BINARY SEARCH
        WITH KEY domvalue_l = i_obj_name(2).
      IF sy-subrc EQ 0.
        CONCATENATE 'Documentation' ls_doktype-ddtext INTO r_text SEPARATED BY space. "#EC NOTEXT
      ELSE.
        r_text = 'Documentation'.                           "#EC NOTEXT
      ENDIF.
    ELSEIF i_object IS NOT INITIAL.
      READ TABLE lt_object_text INTO ls_object_text BINARY SEARCH
        WITH KEY object = i_object.  "ignore PGMID!
      IF sy-subrc EQ 0.
        r_text = ls_object_text-text.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_object_text


  METHOD get_function_pool_master.
    "/SAPAPO/ABC  => /SAPAPO/SAPLABC
    "XYZ1         => SAPLXYZ1
    r_master = i_area.
    REPLACE '/' WITH '/SAPL' INTO r_master+1.
    IF sy-subrc NE 0.
      CONCATENATE 'SAPL' r_master INTO r_master.
    ENDIF.

  ENDMETHOD.                    "get_docu_object

  METHOD create_doma.
    DATA: ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v,
          ls_dd07v TYPE dd07v,
          l_update,
          ls_domname LIKE LINE OF gr_domname VALUE 'IEQ'.

    FIELD-SYMBOLS: <l_appendname> TYPE c,
                   <ls_dd07v> LIKE LINE OF lt_dd07v.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'DOMA'
        i_obj_name = i_domname.

    ls_domname-low = i_domname.
    COLLECT ls_domname INTO gr_domname.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name      = i_domname
        langu     = i_langu      "reads all values, returns value without language/text if text does not exist in i_langu
        state     = 'M'  "newest version (e.g. inactive)
      IMPORTING
        dd01v_wa  = ls_dd01v
      TABLES
        dd07v_tab = lt_dd07v
      EXCEPTIONS
        OTHERS    = 0.

    IF ls_dd01v IS INITIAL OR
       ls_dd01v-ddtext    NE i_ddtext    OR
       ls_dd01v-domname   NE i_domname   OR
       ls_dd01v-datatype  NE i_datatype  OR
       ls_dd01v-leng      NE i_leng      OR
       i_outputlen NE 0 AND i_outputlen NE ls_dd01v-outputlen OR
       i_outputlen EQ 0 AND i_leng      NE ls_dd01v-outputlen OR
       ls_dd01v-convexit  NE i_convexit  OR
       ls_dd01v-decimals  NE i_decimals  OR
       ls_dd01v-lowercase NE i_lowercase OR
       ls_dd01v-signflag  NE i_signflag  OR
       ls_dd01v-entitytab NE i_entitytab.
      l_update = 'X'.
    ENDIF.

    IF gv_copy_translation IS INITIAL.
      ls_dd01v-domname     = i_domname.
      ls_dd01v-datatype    = i_datatype.
      ls_dd01v-leng        = i_leng.
      IF i_outputlen IS INITIAL.
        ls_dd01v-outputlen = i_leng.
      ELSE.
        ls_dd01v-outputlen = i_outputlen.
      ENDIF.
      ls_dd01v-convexit    = i_convexit.
      ASSIGN ('LS_DD01V-APPENDNAME') TO <l_appendname>.
      IF sy-subrc EQ 0.
        <l_appendname>  = i_appendname.
      ENDIF.
    ENDIF.

    ls_dd01v-ddlanguage = i_langu.
    ls_dd01v-ddtext     = i_ddtext.
    ls_dd01v-domname    = i_domname.
    ls_dd01v-datatype   = i_datatype.
    ls_dd01v-leng       = i_leng.
    ls_dd01v-outputlen  = i_outputlen.
    ls_dd01v-convexit   = i_convexit.
    ls_dd01v-decimals   = i_decimals.
    ls_dd01v-lowercase  = i_lowercase.
    ls_dd01v-signflag   = i_signflag.
    ls_dd01v-entitytab  = i_entitytab.

    IF i_domvalue IS SUPPLIED.   "INITIAL value shall be possible fix value
      READ TABLE lt_dd07v ASSIGNING <ls_dd07v> WITH KEY domvalue_l = i_domvalue.
      IF sy-subrc EQ 0.
        "value exists -> compare text
        IF <ls_dd07v>-ddtext NE i_valtext.
          <ls_dd07v>-ddlanguage = i_langu.
          <ls_dd07v>-ddtext     = i_valtext.
          l_update = 'X'.
        ENDIF.
      ELSEIF gv_copy_translation IS INITIAL.
        "value does not exist -> insert
        READ TABLE lt_dd07v TRANSPORTING NO FIELDS WITH KEY valpos = i_valpos.
        IF sy-subrc EQ 0.
          "insert new value and increase valpos of exiting entries
          LOOP AT lt_dd07v ASSIGNING <ls_dd07v> WHERE valpos GE i_valpos.
            ADD 1 TO <ls_dd07v>-valpos.
          ENDLOOP.
        ENDIF.
        CLEAR ls_dd07v.
        ls_dd07v-domname    = i_domname.
        ls_dd07v-domvalue_l = i_domvalue.
        IF i_valpos IS INITIAL.
          DESCRIBE TABLE lt_dd07v.
          ls_dd07v-valpos = sy-tfill + 1.
        ELSE.
          ls_dd07v-valpos     = i_valpos.
        ENDIF.
        ls_dd07v-ddlanguage = i_langu.
        ls_dd07v-ddtext     = i_valtext.
        APPEND ls_dd07v TO lt_dd07v.
        SORT lt_dd07v BY valpos.
        l_update = 'X'.
      ENDIF.
      IF lt_dd07v[] IS NOT INITIAL.
        ls_dd01v-valexi = 'X'.
      ENDIF.
    ENDIF.

    IF l_update IS INITIAL.
      IF i_domvalue IS SUPPLIED.
        MESSAGE i666(01) WITH 'No update necessary for fix value' i_domvalue INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ELSE.
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      RETURN.
    ENDIF.

    CHECK testrun IS INITIAL.

    IF gv_copy_translation IS INITIAL.
      CALL METHOD add_to_transport
        EXPORTING
          i_object   = 'DOMA'
          i_obj_name = i_domname
          i_devclass = i_devclass
          i_langu    = i_langu
        EXCEPTIONS
          OTHERS     = 1.
      CHECK sy-subrc EQ 0.
    ENDIF.

    "align redundant field DOMMASTER with TADIR-MASTERLANG
    SELECT SINGLE masterlang FROM tadir INTO ls_dd01v-dommaster
                            WHERE pgmid    EQ 'R3TR'
                              AND object   EQ 'DOMA'
                              AND obj_name EQ i_domname.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = i_domname
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      MESSAGE e108(do) WITH i_domname INTO sy-lisel.        "#EC *
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

    CALL METHOD register_inactive_object
      EXPORTING
        i_objtype = 'DOMA'
        i_objname = i_domname.

  ENDMETHOD.                    "create_doma

  METHOD create_dtel.
    DATA: ls_dd04v TYPE dd04v,
          l_gotstate,
          ls_rollname LIKE LINE OF gr_domname VALUE 'IEQ'.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'DTEL'
        i_obj_name = i_rollname.

    ls_rollname-low = i_rollname.
    COLLECT ls_rollname INTO gr_rollname.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name     = i_rollname
        langu    = i_langu
        state    = 'M'  "newest version (e.g. inactive)
      IMPORTING
        dd04v_wa = ls_dd04v
        gotstate = l_gotstate
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc EQ 0 AND
      ls_dd04v-rollname    = i_rollname   AND
      ls_dd04v-logflag     = i_logflag    AND
      ls_dd04v-memoryid    = i_memoryid   AND
      ls_dd04v-ddlanguage  = i_langu      AND
      ls_dd04v-ddtext      = i_ddtext     AND
      ls_dd04v-scrtext_m   = i_scrtext_m  AND
      ls_dd04v-scrtext_l   = i_scrtext_l  AND
      ls_dd04v-scrtext_s   = i_scrtext_s  AND
      ls_dd04v-reptext     = i_reptext    AND
      ls_dd04v-shlpname    = i_shlpname   AND
      ls_dd04v-shlpfield   = i_shlpfield  AND
      ls_dd04v-deffdname   = i_deffdname  AND
      ls_dd04v-proxytype   = i_proxytype  AND
    ( ls_dd04v-domname     = i_domname OR i_domname IS INITIAL AND
                                          ls_dd04v-datatype = i_datatype AND
                                          ls_dd04v-decimals = i_decimals AND
                                          ls_dd04v-leng     = i_leng ).
      "data element already exists with that properties
      MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    "check whether domain exists
    IF i_domname IS NOT INITIAL.
      READ TABLE gr_domname TRANSPORTING NO FIELDS WITH KEY low = i_domname.
      IF sy-subrc NE 0.
        SELECT COUNT(*) FROM dd01l WHERE domname EQ i_domname.
      ENDIF.
      IF sy-subrc NE 0.
        MESSAGE e666(01) WITH 'Domain' i_domname 'does not exist' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ENDIF.

    CHECK testrun IS INITIAL.

    ls_dd04v-ddlanguage  = i_langu.
    ls_dd04v-ddtext      = i_ddtext.
    ls_dd04v-scrtext_s   = i_scrtext_s.
    ls_dd04v-scrlen1     = i_scrlen1.
    ls_dd04v-scrtext_m   = i_scrtext_m.
    ls_dd04v-scrlen2     = i_scrlen2.
    ls_dd04v-scrtext_l   = i_scrtext_l.
    ls_dd04v-scrlen3     = i_scrlen3.
    IF i_reptext IS NOT INITIAL.
      ls_dd04v-reptext = i_reptext.
      IF i_headlen IS INITIAL.
        ls_dd04v-headlen = strlen( i_reptext ).
      ELSE.
        ls_dd04v-headlen = i_headlen.
      ENDIF.
    ENDIF.

    IF gv_copy_translation IS INITIAL.
      ls_dd04v-rollname    = i_rollname.
      ls_dd04v-domname     = i_domname.
      IF ls_dd04v-domname IS INITIAL.
        ls_dd04v-datatype    = i_datatype.
        ls_dd04v-decimals    = i_decimals.
        ls_dd04v-leng        = i_leng.
      ENDIF.
      ls_dd04v-shlpname    = i_shlpname.
      ls_dd04v-shlpfield   = i_shlpfield.
      ls_dd04v-deffdname   = i_deffdname.
      ls_dd04v-logflag     = i_logflag.
      ls_dd04v-memoryid    = i_memoryid.
      ls_dd04v-proxytype   = i_proxytype.

      CALL METHOD add_to_transport
        EXPORTING
          i_object   = 'DTEL'
          i_obj_name = i_rollname
          i_devclass = i_devclass
          i_langu    = i_langu  "masterlanguage (only relevant during first call)
        EXCEPTIONS
          OTHERS     = 1.
      CHECK sy-subrc EQ 0.
    ENDIF.

    "align redundant field DTELMASTER with TADIR-MASTERLANG
    SELECT SINGLE masterlang FROM tadir INTO ls_dd04v-dtelmaster
                            WHERE pgmid    EQ 'R3TR'
                              AND object   EQ 'DTEL'
                              AND obj_name EQ i_rollname.

    IF i_langu EQ ls_dd04v-dtelmaster OR gv_copy_translation IS INITIAL.
      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING
          name              = i_rollname
          dd04v_wa          = ls_dd04v
        EXCEPTIONS
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      IF sy-subrc NE 0.
        CALL METHOD log_message.
        RETURN.
      ELSE.
        MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.

      CALL METHOD register_inactive_object
        EXPORTING
          i_objtype = 'DTEL'
          i_objname = i_rollname.

    ELSE.
      DATA: ls_dd04l TYPE dd04l,
            ls_dd04t TYPE dd04t,
            lt_dd04t TYPE TABLE OF dd04t.
      MOVE-CORRESPONDING ls_dd04v TO: ls_dd04l, ls_dd04t.
      APPEND ls_dd04t TO lt_dd04t.
      TRANSLATE l_gotstate USING 'MN'.
      CALL FUNCTION 'DD_DTEL_PUT'
        EXPORTING
          dd04l_wa            = ls_dd04l
          prid                = -2  "no_log
          put_state           = l_gotstate
          rollname            = ls_dd04v-rollname
        TABLES
          dd04t_tab           = lt_dd04t
        EXCEPTIONS
          object_inconsistent = 1
          OTHERS              = 2.
    ENDIF.

  ENDMETHOD.                    "create_dtel



  METHOD create_search_help.

    DATA: ls_shlpname   LIKE LINE OF gr_shlpname VALUE 'IEQ',
          ls_dd30v      TYPE dd30v,
          ls_dd30tv     TYPE dd30tv,
          ls_dd32v      TYPE dd32v,
          lt_dd30v      TYPE tt_dd30v,
          lt_dd30tv     TYPE tt_dd30tv,
          lt_dd31v      TYPE tt_dd31v,
          lt_dd32v      TYPE tt_dd32v,
          lt_dd33v      TYPE tt_dd33v,
          lt_dd32p      TYPE tt_dd32p,
          ls_dd32p      TYPE dd32p,
          l_text        TYPE tftit-stext,
          l_namespace   TYPE rs38l-namespace,
          l_area        TYPE tlibg-area,
          l_object_name TYPE vrsd-objname.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'SHLP'
        i_obj_name = i_shlpname.

    ls_shlpname-low = i_shlpname.
    COLLECT ls_shlpname INTO gr_shlpname.

    "CALL FUNCTION 'DDIF_SHLP_GET' has different result
    l_object_name = i_shlpname.
    CALL FUNCTION 'SVRS_GET_VERSION_SHLD_40'
      EXPORTING
        object_name           = l_object_name
        versno                = 0
      TABLES
        dd30tv_tab            = lt_dd30tv
        dd30v_tab             = lt_dd30v
        dd31v_tab             = lt_dd31v
        dd32v_tab             = lt_dd32v
        dd33v_tab             = lt_dd33v
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        OTHERS                = 4.
    IF sy-subrc EQ 0 AND lt_dd30v[] IS NOT INITIAL. " search help exist
      LOOP AT lt_dd32v INTO ls_dd32v.
        MOVE-CORRESPONDING ls_dd32v TO ls_dd32p.
        APPEND ls_dd32p TO lt_dd32p.
      ENDLOOP.
      READ TABLE lt_dd30v  INTO ls_dd30v  INDEX 1.
      READ TABLE lt_dd30tv INTO ls_dd30tv WITH KEY ddlanguage = i_langu.
      IF lt_dd31v            EQ it_dd31v     AND
         lt_dd32p            EQ it_dd32p     AND
         lt_dd33v            EQ it_dd33v     AND
         ls_dd30v-elemexi    EQ i_elemexi    AND
         ls_dd30v-nofields   EQ i_nofields   AND
         ls_dd30v-attachexi  EQ i_attachexi  AND
         ls_dd30v-selmethod  EQ i_selmethod  AND
         ls_dd30v-selmtype   EQ i_selmtype   AND
         ls_dd30v-texttab    EQ i_texttab    AND
         ls_dd30v-selmexit   EQ i_selmexit   AND
         ls_dd30v-hotkey     EQ i_hotkey     AND
         ls_dd30v-issimple   EQ i_issimple   AND
         ls_dd30v-dialogtype EQ i_dialogtype AND
       ( ls_dd30tv-ddtext    EQ i_ddtext OR i_ddtext IS INITIAL ).
        "exist
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ENDIF.

    CHECK testrun IS INITIAL.

    ls_dd30v-shlpname     = i_shlpname.
    ls_dd30v-ddlanguage   = i_langu.
    ls_dd30v-elemexi      = i_elemexi.
    ls_dd30v-nofields     = i_nofields.
    ls_dd30v-attachexi    = i_attachexi.
    ls_dd30v-selmethod    = i_selmethod.
    ls_dd30v-selmtype     = i_selmtype.
    ls_dd30v-texttab      = i_texttab.
    ls_dd30v-selmexit     = i_selmexit.
    ls_dd30v-hotkey       = i_hotkey.
    ls_dd30v-issimple     = i_issimple.
    ls_dd30v-dialogtype   = i_dialogtype.

    IF i_ddtext IS INITIAL.
      ls_dd30v-ddtext   = i_shlpname.
    ELSE.
      ls_dd30v-ddtext   = i_ddtext.
    ENDIF.

    IF i_selmexit IS NOT INITIAL.
      "check if function module exists.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = i_selmexit
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 0.
      IF sy-subrc EQ 1.
        "create empty function module on the fly
        CALL METHOD add_to_transport
          EXPORTING
            i_object   = 'FUNC'
            i_obj_name = i_selmexit
          EXCEPTIONS
            OTHERS     = 1.
        CHECK sy-subrc EQ 0.

        l_text = i_selmexit.
        IF i_function_group+1 CA '/'.
          ADD 2 TO sy-fdpos.
          l_namespace = i_function_group(sy-fdpos).
          l_area      = i_function_group+sy-fdpos.
        ELSE.
          l_area      = i_function_group.
        ENDIF.

        DATA: lt_tables   TYPE TABLE OF rstbl,
              ls_tables   TYPE rstbl,
              lt_changing TYPE TABLE OF rscha,
              ls_changing TYPE rscha.

        CLEAR ls_tables.
        ls_tables-parameter = 'SHLP_TAB'.
        ls_tables-typ       = 'SHLP_DESCT'.
        APPEND ls_tables TO lt_tables.
        CLEAR ls_tables.
        ls_tables-parameter = 'RECORD_TAB'.
        ls_tables-dbstruct  = 'SEAHLPRES'.
        APPEND ls_tables TO lt_tables.

        CLEAR ls_changing.
        ls_changing-parameter = 'SHLP'.
        ls_changing-typ       = 'SHLP_DESCR'.
        ls_changing-types     = 'X'.
        ls_changing-reference = 'X'.
        APPEND ls_changing TO lt_changing.
        CLEAR ls_changing.
        ls_changing-parameter = 'CALLCONTROL'.
        ls_changing-typ       = 'DDSHF4CTRL'.
        ls_changing-types     = 'X'.
        ls_changing-reference = 'X'.
        APPEND ls_changing TO lt_changing.

        CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
          EXPORTING
            funcname                = i_selmexit
            function_pool           = l_area
            namespace               = l_namespace
            short_text              = l_text
            suppress_corr_check     = ' '
            suppress_language_check = 'X'
            authority_check         = 'X'
            suppress_upgrade_check  = 'X'
          TABLES
            tables_parameter        = lt_tables
            changing_parameter      = lt_changing
          EXCEPTIONS
            OTHERS                  = 1.
        IF sy-subrc NE 0.
          CALL METHOD log_message.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL METHOD add_to_transport
      EXPORTING
        i_object   = 'SHLP'
        i_langu    = i_langu
        i_obj_name = i_shlpname
        i_devclass = i_devclass
      EXCEPTIONS
        OTHERS     = 1.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'DDIF_SHLP_PUT'
      EXPORTING
        name              = i_shlpname
        dd30v_wa          = ls_dd30v
      TABLES
        dd31v_tab         = it_dd31v
        dd32p_tab         = it_dd32p
        dd33v_tab         = it_dd33v
      EXCEPTIONS
        shlp_not_found    = 1
        name_inconsistent = 2
        shlp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

    CALL METHOD register_inactive_object
      EXPORTING
        i_objtype = 'SHLP'
        i_objname = i_shlpname.

  ENDMETHOD.                    "create_search_help

  METHOD create_table.
    DATA: ls_dd02v TYPE dd02v,
          ls_dd09l TYPE dd09l,
          lt_dd03p TYPE TABLE OF dd03p,
          lt_dd05m TYPE TABLE OF dd05m,
          lt_dd08v TYPE TABLE OF dd08v,
          lt_dd12v TYPE TABLE OF dd12v,
          lt_dd17v TYPE TABLE OF dd17v,
          lt_dd35v TYPE TABLE OF dd35v,
          lt_dd36m TYPE TABLE OF dd36m,
          l_treetype TYPE trobjtype,
          ls_tabname LIKE LINE OF gr_tabname VALUE 'IEQ'.

    FIELD-SYMBOLS: <ls_dd09l_javaonly> TYPE char1.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'TABD'
        i_obj_name = i_tabname.

    ls_tabname-low = i_tabname.
    COLLECT ls_tabname INTO gr_tabname.

    ASSIGN ('LS_DD09L-JAVAONLY') TO <ls_dd09l_javaonly>.                ">=SAPBASIS700

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = i_tabname
        state         = 'M'  "newest version (e.g. inactive)
        langu         = i_langu
      IMPORTING
*       GOTSTATE      = GOTSTATE
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc           EQ 0           AND
       ls_dd02v-tabname   IS NOT INITIAL AND "existing.
       ls_dd02v-ddtext    EQ i_ddtext    AND
       ls_dd02v-exclass   EQ i_exclass   AND
       ls_dd02v-tabclass  EQ i_tabclass  AND
       ls_dd02v-authclass EQ i_authclass AND
       ls_dd02v-mainflag  EQ i_mainflag  AND
       ls_dd02v-proxytype EQ i_proxytype AND
       ls_dd02v-sqltab    EQ i_sqltab.

      IF ( ls_dd02v-tabclass   EQ 'INTTAB' ) OR
         ( ls_dd02v-contflag   EQ i_contflag   AND
           ls_dd09l-tabkat     EQ i_tabkat     AND
           ls_dd09l-tabart     EQ i_tabart     AND
           ls_dd09l-pufferung  EQ i_pufferung  AND
           ls_dd09l-bufallow   EQ i_bufallow   AND
           ls_dd09l-schfeldanz EQ i_schfeldanz AND
           ls_dd09l-protokoll  EQ i_protokoll  AND
           "ls_dd09l-javaonly   EQ i_javaonly   AND     ">=SAPBASIS700
           ls_dd09l-speichpuff EQ i_speichpuff ).   "do not check javaonly field
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.

    ENDIF.

    CHECK testrun IS INITIAL.

    "new table
    ls_dd02v-tabname    = i_tabname.
    ls_dd02v-ddlanguage = i_langu.
    ls_dd02v-exclass    = i_exclass.
    ls_dd02v-tabclass   = i_tabclass.
    ls_dd02v-authclass  = i_authclass.
    ls_dd02v-mainflag   = i_mainflag.
    ls_dd02v-proxytype  = i_proxytype.
    ls_dd02v-sqltab     = i_sqltab.

    IF i_ddtext IS INITIAL.
      ls_dd02v-ddtext   = i_tabname.
    ELSE.
      ls_dd02v-ddtext   = i_ddtext.
    ENDIF.

    IF ls_dd02v-tabclass EQ 'CLUSTER' OR  "only for real database tables
       ls_dd02v-tabclass EQ 'POOL'    OR
       ls_dd02v-tabclass EQ 'TRANSP'.
      ls_dd02v-contflag   = i_contflag.
      ls_dd09l-tabname    = i_tabname.
      ls_dd09l-tabkat     = i_tabkat.
      ls_dd09l-tabart     = i_tabart.
      ls_dd09l-pufferung  = i_pufferung.
      ls_dd09l-bufallow   = i_bufallow.
      ls_dd09l-schfeldanz = i_schfeldanz.
      ls_dd09l-protokoll  = i_protokoll.
      ls_dd09l-speichpuff = i_speichpuff.
      IF <ls_dd09l_javaonly> IS ASSIGNED.         ">=SAPBASIS700
        <ls_dd09l_javaonly> = i_javaonly.         ">=SAPBASIS700
      ENDIF.                                      ">=SAPBASIS700
      l_treetype = 'TABL'.
    ELSE.
      l_treetype = 'CDS'.
    ENDIF.

    CALL METHOD add_to_transport
      EXPORTING
        i_object   = 'TABL'
        i_langu    = i_langu
        i_obj_name = i_tabname
        i_devclass = i_devclass
      EXCEPTIONS
        OTHERS     = 1.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = i_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
        dd05m_tab         = lt_dd05m
        dd08v_tab         = lt_dd08v
        dd35v_tab         = lt_dd35v
        dd36m_tab         = lt_dd36m
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

    CALL METHOD register_inactive_object
      EXPORTING
        i_objtype  = 'TABL'
        i_treetype = l_treetype
        i_objname  = i_tabname.


  ENDMETHOD.                    "create_table

  METHOD add_field_to_table.

    DATA: ls_dd02v TYPE dd02v,
          ls_dd09l TYPE dd09l,
          lt_dd03p TYPE TABLE OF dd03p,
          ls_dd05m TYPE dd05m,
          lt_dd05m TYPE TABLE OF dd05m,
          ls_dd08v TYPE dd08v,
          lt_dd08v TYPE TABLE OF dd08v,
          lt_dd12v TYPE TABLE OF dd12v,
          lt_dd17v TYPE TABLE OF dd17v,
          lt_dd35v TYPE TABLE OF dd35v,
          lt_dd36m TYPE TABLE OF dd36m,
          l_tabix  TYPE sytabix,
          ls_tabname LIKE LINE OF gr_tabname VALUE 'IEQ'.

    FIELD-SYMBOLS: <ls_dd03p> TYPE dd03p.

    CALL METHOD set_context
      EXPORTING
        i_langu    = space
        i_text     = 'Add field(s) to table'                "#EC NOTEXT
        i_object   = 'TABD'
        i_obj_name = i_tabname.

    ls_tabname-low = i_tabname.
    COLLECT ls_tabname INTO gr_tabname.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = i_tabname
        state         = 'M'  "newest version (e.g. inactive)
      IMPORTING
*       GOTSTATE      = GOTSTATE
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc NE 0 OR ls_dd02v-tabname IS INITIAL.  "not existing.
      IF testrun IS INITIAL OR i_tabname NOT IN gr_tabname.
        MESSAGE e666(01) WITH 'Error while reading table' i_tabname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      RETURN.
    ENDIF.

    CASE ls_dd02v-tabclass.
      WHEN 'CLUSTER' OR 'POOL' OR 'TRANSP'.
        COLLECT ls_tabname INTO gr_dbtabname.
    ENDCASE.

    "check whether data element respectivly included/appended/referenced structure exists
    IF i_rollname IS NOT INITIAL.
      READ TABLE gr_rollname TRANSPORTING NO FIELDS WITH KEY low = i_rollname.  "new data element
      IF sy-subrc NE 0.
        READ TABLE gt_tabname TRANSPORTING NO FIELDS WITH KEY table_line = i_rollname.  "new structure
      ENDIF.
      IF sy-subrc NE 0.
        READ TABLE gt_ttypname TRANSPORTING NO FIELDS WITH KEY table_line = i_rollname.  "new table type
      ENDIF.
      IF sy-subrc NE 0.
        SELECT SINGLE rollname FROM dd04l INTO sy-lisel WHERE rollname EQ i_rollname.  "existing data element
      ENDIF.
      IF sy-subrc NE 0.
        SELECT SINGLE tabname FROM dd03l INTO sy-lisel WHERE tabname EQ i_rollname.  "existing structure
      ENDIF.
      IF sy-subrc NE 0.
        SELECT SINGLE typename FROM dd40l INTO sy-lisel WHERE typename EQ i_rollname.  "existing table_type
      ENDIF.
      IF sy-subrc NE 0.
        SELECT SINGLE clsname FROM seoclass INTO sy-lisel WHERE clsname EQ i_rollname.  "existing class
      ENDIF.
      IF sy-subrc NE 0.
        IF i_rollname(3) EQ 'CL_'. "probably a class
          MESSAGE w666(01) WITH 'Class' i_rollname 'does not exist (yet)' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ELSEIF i_rollname(3) EQ 'CI_' AND i_fieldname(6) EQ c_include6.
          "Customer include
        ELSE.
          MESSAGE e666(01) WITH 'Data Element or Structure' i_rollname 'does not exist' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    "Find field to insert/change
    IF i_fieldname(6) EQ c_include6.
      READ TABLE lt_dd03p ASSIGNING <ls_dd03p>
           WITH KEY fieldname = i_fieldname
                    precfield = i_rollname
                    groupname = i_groupname.
      IF sy-subrc EQ 0.
        "INCLUDE already exists
        MESSAGE i666(01) WITH 'Include ' i_rollname 'already part of table' i_tabname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ELSE.
      READ TABLE lt_dd03p ASSIGNING <ls_dd03p>
           WITH KEY fieldname = i_fieldname.
      IF sy-subrc EQ 0.
        "Check if fields already has data element
        IF i_rollname NE <ls_dd03p>-rollname.
          "change data element
          <ls_dd03p>-rollname  = i_rollname.
          MESSAGE i666(01) WITH 'Data element of field' i_fieldname 'will be changed to' i_rollname INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ELSEIF it_dd05m IS SUPPLIED OR is_dd08v IS NOT INITIAL.
          "update foreign key relation ship
          DATA: lt_dd05m_h TYPE tt_dd05m,
                it_dd05m_h TYPE tt_dd05m.

          lt_dd05m_h[] = lt_dd05m[].
          it_dd05m_h[] = it_dd05m[].
          DELETE lt_dd05m_h WHERE fieldname NE i_fieldname.
          CLEAR ls_dd05m.
          MODIFY lt_dd05m_h FROM ls_dd05m TRANSPORTING domname datatype leng WHERE fieldname IS NOT INITIAL.
          MODIFY it_dd05m_h FROM ls_dd05m TRANSPORTING domname datatype leng WHERE fieldname IS NOT INITIAL.
          CLEAR ls_dd08v.
          READ TABLE lt_dd08v INTO ls_dd08v WITH KEY fieldname = i_fieldname.
          IF lt_dd05m_h[] EQ it_dd05m[] AND ls_dd08v EQ is_dd08v.
            "field already exists with correct data element
            MESSAGE i666(01) WITH 'Field' i_fieldname 'already exists with SHLP' INTO sy-lisel. "#EC *
            CALL METHOD log_message.
            RETURN.
          ENDIF.
        ELSEIF i_languflag NE <ls_dd03p>-languflag.
          "field already exists with correct data element
          MESSAGE i666(01) WITH 'Properties of field' i_fieldname 'will be changed' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ELSE.
          "field already exists with correct data element, langu and foreign key
          MESSAGE i666(01) WITH 'Field' i_fieldname 'already exists' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
    IF <ls_dd03p> IS NOT ASSIGNED. "new field
      "Find context (one field is enough since fieldname is unique)
      IF i_context IS NOT INITIAL.
        READ TABLE lt_dd03p TRANSPORTING NO FIELDS
             WITH KEY fieldname = i_context.
        IF sy-subrc NE 0.
          READ TABLE lt_dd03p TRANSPORTING NO FIELDS  "include name as context
                WITH KEY precfield = i_context.
        ENDIF.
        IF sy-subrc NE 0 AND sy-tfill NE 0.
          IF testrun IS INITIAL OR i_tabname NOT IN gr_tabname.
            MESSAGE w666(01) WITH 'Context' i_context 'not found in TABD' i_tabname INTO sy-lisel. "#EC *
            CALL METHOD log_message.
          ELSE.
            MESSAGE i666(01) WITH 'Field' i_fieldname 'will be added' INTO sy-lisel. "#EC *
            CALL METHOD log_message.
          ENDIF.
          RETURN.
        ENDIF.

        "insert new field after context
        l_tabix = sy-tabix + 1.
      ELSE.
        "no context -> insert field at end
        l_tabix = lines( lt_dd03p ) + 1.
      ENDIF.

      INSERT INITIAL LINE INTO lt_dd03p INDEX l_tabix ASSIGNING <ls_dd03p>.
    ENDIF.

    <ls_dd03p>-tabname   = i_tabname.
    <ls_dd03p>-fieldname = i_fieldname.
    <ls_dd03p>-groupname = i_groupname.
    <ls_dd03p>-languflag = i_languflag.
    <ls_dd03p>-notnull   = i_notnull.
    <ls_dd03p>-keyflag   = i_keyflag.
    FIELD-SYMBOLS: <ls_dd01v_anonymous> TYPE c.
    ASSIGN ('LS_DD01V-ANONYMOUS') TO <ls_dd01v_anonymous>.
    IF sy-subrc EQ 0.
      <ls_dd01v_anonymous> = i_anonymous.
    ENDIF.
    IF i_fieldname(6) EQ c_include6.
      <ls_dd03p>-precfield = i_rollname.
      <ls_dd03p>-comptype  = 'S'.
    ELSEIF i_rollname IS INITIAL.
      <ls_dd03p>-datatype  = i_datatype.
      <ls_dd03p>-decimals  = i_decimals.
      <ls_dd03p>-leng      = i_leng.
      IF i_ddtext IS NOT INITIAL.
        <ls_dd03p>-ddtext     = i_ddtext.
        <ls_dd03p>-ddlanguage = i_langu.
      ENDIF.
    ELSE.
      <ls_dd03p>-rollname  = i_rollname.
    ENDIF.
    <ls_dd03p>-reftable = i_reftable.
    <ls_dd03p>-reffield = i_reffield.

    LOOP AT lt_dd03p ASSIGNING <ls_dd03p> FROM l_tabix.
      <ls_dd03p>-position = sy-tabix.
    ENDLOOP.
    "field will be added
    MESSAGE i666(01) WITH 'Field' i_fieldname 'will be added' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

    IF is_dd08v IS NOT INITIAL OR it_dd05m IS SUPPLIED.
      DELETE lt_dd08v WHERE fieldname EQ i_fieldname.
      DELETE lt_dd05m WHERE fieldname EQ i_fieldname.

      LOOP AT it_dd05m INTO ls_dd05m WHERE checktable IS NOT INITIAL
                                       AND checkfield IS NOT INITIAL.
        APPEND ls_dd05m TO lt_dd05m.
      ENDLOOP.
      IF is_dd08v IS NOT INITIAL.
        APPEND is_dd08v TO lt_dd08v.
      ENDIF.
    ENDIF.

    CHECK testrun IS INITIAL.

    CALL METHOD add_to_transport
      EXPORTING
        i_object   = 'TABL'
        i_obj_name = i_tabname
      EXCEPTIONS
        OTHERS     = 1.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = i_tabname
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
        dd05m_tab         = lt_dd05m
        dd08v_tab         = lt_dd08v
        dd35v_tab         = lt_dd35v
        dd36m_tab         = lt_dd36m
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    CALL METHOD register_inactive_object
      EXPORTING
        i_objtype = 'TABL'
        i_objname = i_tabname.

  ENDMETHOD.                    "add_field_to_table



  METHOD create_view.

    DATA: ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28v TYPE TABLE OF dd28v.

    DATA: ls_viewname LIKE LINE OF gr_viewname VALUE 'IEQ'.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'VIEW'
        i_obj_name = i_viewname.

    ls_viewname-low = i_viewname.
    COLLECT ls_viewname INTO gr_viewname.

    CALL FUNCTION 'DD_VIEW_GET'
      EXPORTING
        view_name      = i_viewname
        langu          = i_langu
        withtext       = 'X'
      IMPORTING
        dd25v_wa_a     = ls_dd25v
        dd09l_wa_a     = ls_dd09l
      TABLES
        dd26v_tab_a    = lt_dd26v
        dd27p_tab_a    = lt_dd27p
        dd28v_tab_a    = lt_dd28v
      EXCEPTIONS
        access_failure = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      IF testrun IS INITIAL OR i_viewname NOT IN gr_viewname.
        MESSAGE e666(01) WITH 'Error while reading view' i_viewname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      RETURN.
    ENDIF.

    IF sy-subrc EQ 0 AND ls_dd25v-viewname IS NOT INITIAL. "existing.
      IF ls_dd25v-ddtext     EQ i_ddtext     AND
         ls_dd25v-viewname   EQ i_viewname   AND
         ls_dd25v-ddlanguage EQ i_langu      AND
         ls_dd25v-customauth EQ i_contflag   AND
         ls_dd25v-aggtype    EQ i_aggtype    AND
         ls_dd25v-roottab    EQ i_roottab    AND
         ls_dd25v-viewclass  EQ i_viewclass  AND
         ls_dd25v-viewgrant  EQ i_viewgrant  AND
         ls_dd25v-globalflag EQ i_globalflag AND
         ls_dd25v-readonly   EQ i_readonly   AND
         lt_dd26v[]          EQ it_dd26v[]   AND
         lt_dd28v[]          EQ it_dd28v[].
        "exist
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ENDIF.

    CHECK testrun IS INITIAL.

    "new view
    ls_dd25v-viewname   = i_viewname.
    ls_dd25v-ddlanguage = i_langu.
    ls_dd25v-customauth = i_contflag.
    ls_dd25v-aggtype    = i_aggtype.
    ls_dd25v-roottab    = i_roottab.
    ls_dd25v-viewclass  = i_viewclass.
    ls_dd25v-viewgrant  = i_viewgrant.
    ls_dd25v-globalflag = i_globalflag.
    ls_dd25v-readonly   = i_readonly.

    IF i_ddtext IS INITIAL.
      ls_dd25v-ddtext   = i_viewname.
    ELSE.
      ls_dd25v-ddtext   = i_ddtext.
    ENDIF.

    CALL METHOD add_to_transport
      EXPORTING
        i_object   = 'VIEW'
        i_langu    = i_langu
        i_obj_name = i_viewname
        i_devclass = i_devclass
      EXCEPTIONS
        OTHERS     = 1.
    CHECK sy-subrc EQ 0.

    "clear redundant field MASTERLANG, relevant is only TADIR-MASTERLANG
    CLEAR ls_dd25v-masterlang.

    CALL FUNCTION 'DD_VIEW_PUT'
      EXPORTING
        view_name         = i_viewname
        put_state         = 'N'    "new
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = it_dd26v
        dd27p_tab         = lt_dd27p   "fields: emtpy
        dd28v_tab         = it_dd28v
      EXCEPTIONS
        db_access_failure = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    CALL METHOD register_inactive_object
      EXPORTING
        i_objtype = 'VIEW'
        i_objname = i_viewname.

  ENDMETHOD.                    "create_view

  METHOD add_field_to_view.
    DATA: ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    DATA: l_viewfield LIKE i_viewfield,
          l_rollname  LIKE i_rollname,
          l_tabix     TYPE sytabix,
          ls_viewname LIKE LINE OF gr_viewname VALUE 'IEQ'.

    CALL METHOD set_context
      EXPORTING
        i_langu    = space
        i_text     = 'Add Field(s) to View'                 "#EC NOTEXT
        i_object   = 'VIEW'
        i_obj_name = i_viewname.

    ls_viewname-low = i_viewname.
    COLLECT ls_viewname INTO gr_viewname.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = i_viewname
        state         = 'M'     "most recent, e.g. inactive
      IMPORTING
        dd25v_wa      = ls_dd25v
        dd09l_wa      = ls_dd09l
      TABLES
        dd26v_tab     = lt_dd26v
        dd27p_tab     = lt_dd27p
        dd28j_tab     = lt_dd28j
        dd28v_tab     = lt_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      IF testrun IS INITIAL OR i_viewname NOT IN gr_viewname.
        MESSAGE e666(01) WITH 'Error while reading view' i_viewname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      RETURN.
    ENDIF.

    IF i_viewfield IS INITIAL.
      l_viewfield = i_fieldname.
    ELSE.
      l_viewfield = i_viewfield.
    ENDIF.

    IF i_rollname IS INITIAL.
      SELECT SINGLE rollname FROM dd03l INTO l_rollname
                            WHERE tabname   EQ i_tabname
                              AND fieldname EQ i_fieldname.
      IF sy-subrc NE 0.
        MESSAGE i666(01) WITH 'Field' i_fieldname 'not found in ' i_tabname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ELSE.
      l_rollname = i_rollname.
    ENDIF.

    "Check if fields already exists
    READ TABLE lt_dd27p ASSIGNING <ls_dd27p> WITH KEY viewfield  = l_viewfield.
    IF sy-subrc EQ 0.
      IF <ls_dd27p>-rollname EQ i_rollname AND
         <ls_dd27p>-keyflag  EQ i_keyflag.
        "field already exists
        MESSAGE i666(01) WITH 'Field' i_fieldname 'already exists in view' i_viewname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ELSE.
      IF i_context IS INITIAL.
        "append at the end
        APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
      ELSE.
        "Find context (one field is enough since fieldname is unique)
        READ TABLE lt_dd27p TRANSPORTING NO FIELDS
             WITH KEY viewfield = i_context.
        IF sy-subrc EQ 0.
          "insert new field after context
          l_tabix = sy-tabix + 1.
          INSERT INITIAL LINE INTO lt_dd27p INDEX l_tabix ASSIGNING <ls_dd27p>.
        ELSE.
          IF testrun IS INITIAL OR i_viewname NOT IN gr_viewname.
            MESSAGE i666(01) WITH 'Context' i_context 'not found in VIED' i_viewname INTO sy-lisel. "#EC *
            CALL METHOD log_message.
          ENDIF.
          APPEND INITIAL LINE TO lt_dd27p ASSIGNING <ls_dd27p>.
        ENDIF.
      ENDIF.
    ENDIF.

    "check whether data element exists
    IF l_rollname IS NOT INITIAL.
      READ TABLE gr_rollname TRANSPORTING NO FIELDS WITH KEY low = l_rollname.
      IF sy-subrc NE 0.
        SELECT COUNT(*) FROM dd04l WHERE rollname EQ l_rollname.
      ENDIF.
      IF sy-subrc NE 0.
        MESSAGE e666(01) WITH 'Data Element' l_rollname 'does not exist' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ENDIF.

    IF testrun IS NOT INITIAL.
      MESSAGE i666(01) WITH 'Field' i_fieldname 'will be added to view' i_viewname INTO sy-lisel. "#EC *
      CALL METHOD log_message.
    ELSE.

      <ls_dd27p>-viewname   = i_viewname.
      <ls_dd27p>-viewfield  = l_viewfield.
      <ls_dd27p>-tabname    = i_tabname.
      <ls_dd27p>-fieldname  = i_fieldname.
      <ls_dd27p>-rdonly     = i_rdonly.    "S/H/R
      <ls_dd27p>-keyflag    = i_keyflag.
      <ls_dd27p>-rollchange = i_rollchange.
      <ls_dd27p>-rollnamevi = l_rollname.

      LOOP AT lt_dd27p ASSIGNING <ls_dd27p> FROM l_tabix.
        <ls_dd27p>-objpos = sy-tabix.
      ENDLOOP.

      CALL METHOD add_to_transport
        EXPORTING
          i_object   = 'VIEW'
          i_obj_name = i_viewname
        EXCEPTIONS
          OTHERS     = 1.
      CHECK sy-subrc EQ 0.

      CALL FUNCTION 'DDIF_VIEW_PUT'
        EXPORTING
          name              = i_viewname
          dd25v_wa          = ls_dd25v
          dd09l_wa          = ls_dd09l
        TABLES
          dd26v_tab         = lt_dd26v
          dd27p_tab         = lt_dd27p
          dd28j_tab         = lt_dd28j
          dd28v_tab         = lt_dd28v
        EXCEPTIONS
          view_not_found    = 1
          name_inconsistent = 2
          view_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      IF sy-subrc <> 0.
        CALL METHOD log_message.
        RETURN.
      ENDIF.

      MESSAGE i666(01) WITH 'Field' i_fieldname 'has been added to view' i_viewname INTO sy-lisel. "#EC *
      CALL METHOD log_message.

      CALL METHOD register_inactive_object
        EXPORTING
          i_objtype = 'VIEW'
          i_objname = i_viewname.

    ENDIF.

  ENDMETHOD.                    "add_field_to_view

  METHOD generate_maint_view.   "call SE54, leave screens untouched

    "generate or regenerate maintaince view via batch input of SE54
    "assumptions:
    "- maint view dialog is based on view (not table)
    "- new fields have been added
    "- no screen changes needed or done via separate corr instruction

    DATA: lt_bdc   TYPE TABLE OF bdcdata,
          ls_bdc   TYPE bdcdata,
          ls_bdcp  TYPE bdcdata,
          ls_radio LIKE ls_bdc-fnam,
          ls_opt   TYPE ctu_params.
    DATA: ls_tvdir TYPE tvdir.

    CASE i_objecttype.
      WHEN 'V' OR 'S'.   "view or table
        CALL METHOD set_context
          EXPORTING
            i_langu    = space
            i_text     = 'Generate Maintainance View'       "#EC NOTEXT
            i_object   = 'VIEW'
            i_obj_name = i_objectname.

        "lock function group in advance to avoid transport popup during batch input
        IF testrun IS INITIAL.
          CALL METHOD add_to_transport
            EXPORTING
              i_object   = 'FUGR'
              i_obj_name = i_func_group
            EXCEPTIONS
              OTHERS     = 1.
          IF sy-subrc NE 0.
            MESSAGE e666(01) WITH 'Could not add to transport request' INTO sy-lisel. "#EC *
            CALL METHOD log_message.
            RETURN.
          ENDIF.
        ENDIF.

        "-------------------------------------------
        bdc_screen 'SAPMSVIM' '0050'.
        bdc_field  'VIMDYNFLDS-VIEWNAME' i_objectname.
        bdc_field  'VIMDYNFLDS-ELEM_GEN' 'X'.
        bdc_field  'BDC_OKCODE' '=CRMO'.

        SELECT SINGLE * FROM tvdir INTO ls_tvdir WHERE tabname EQ i_objectname.
        IF sy-subrc EQ 0.
          MESSAGE i666(01) WITH 'Maintainance View' i_objectname 'will be regenerated.' INTO sy-lisel. "#EC *
          CALL METHOD log_message.

          "-------------------------------------------
          bdc_screen 'SAPMSVIM'   '0120'.
          bdc_field  'BDC_OKCODE' '=RESE'.
          "-------------------------------------------
          bdc_screen 'SAPMSVIM' '1240'.
          bdc_field  'VIMDYNFLDS-NEW_FIELD'  'X'.
          bdc_field  'VIMDYNFLDS-DEL_FIELD'  'X'.
          bdc_field  'VIMDYNFLDS-TEXT_ASS'   'X'.
          IF i_objecttype EQ 'V'. "dynpro field visible only for View
            bdc_field  'VIMDYNFLDS-CHANGE_SEL' 'X'.
            bdc_field  'VIMDYNFLDS-CHANGE_SUB' 'X'.
            bdc_field  'VIMDYNFLDS-READ_ONLY'  'X'.
            bdc_field  'VIMDYNFLDS-CHANGE_HID' 'X'.
          ENDIF.
          bdc_field  'BDC_OKCODE' '=O.K.'.
          "-------------------------------------------
          bdc_screen 'SAPMSVIM'   '1243'.
          bdc_field  'VIMDYNFLDS-FIELD_NORM' 'X'.
          bdc_field  'BDC_OKCODE' '=O.K.'.
          "-------------------------------------------
          bdc_screen 'SAPMSVIM'   '0120'.
          bdc_field  'BDC_OKCODE' '=BACK'.
          "-------------------------------------------
          bdc_screen 'SAPMSVIM' '0050'.
          bdc_field  'BDC_OKCODE' '=BACK'.
        ELSE.
          "-------------------------------------------
          bdc_screen 'SAPLSPO1' '0300'.   "Popup Create Modules?
          bdc_field  'BDC_OKCODE' '=YES'.
          bdc_screen 'SAPMSVIM' '0120'.
          bdc_field  'TDDAT-CCLASS' i_auth_group.
          bdc_field  'TVDIR-LISTE'  i_dynp_list.
          IF i_dynp_detail IS INITIAL.
            bdc_field  'VIMDYNFLDS-MTYPE1' 'X'.  "einstufig
          ELSE.
            bdc_field  'VIMDYNFLDS-MTYPE2' 'X'.  "zweistufig
            bdc_field  'TVDIR-DETAIL' i_dynp_detail.
          ENDIF.
          bdc_field  'TVDIR-AREA' i_func_group.
          bdc_field  'BDC_OKCODE' '/00'.  "Warnung -> Enter
          "-------------------------------------------
          IF i_objecttype EQ 'S'.
            "Obsolete Popup "screen already assigned" triggered in SAPMSVIM D0120_CHECK_DYNNRS2
            bdc_screen 'SAPLSPO1'   '0300'.
            bdc_field  'BDC_OKCODE' '=YES'.
            IF i_dynp_detail IS NOT INITIAL.
              "Obsolete Popup "screen already assigned" triggered in SAPMSVIM D0120_CHECK_DYNNRS3
              bdc_screen 'SAPLSPO1'   '0300'.
              bdc_field  'BDC_OKCODE' '=YES'.
            ENDIF.
          ENDIF.
          bdc_screen 'SAPMSVIM'   '0120'.
          bdc_field  'BDC_OKCODE' '=GENE'.
          bdc_screen 'SAPMSVIM'   '0120'.
          bdc_field  'BDC_OKCODE' '=BACK'.

          bdc_screen 'SAPMSVIM' '0050'.
          bdc_field  'BDC_OKCODE' '=BACK'.

          MESSAGE i666(01) WITH 'Maintainance View' i_objectname 'will be created.'     INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ENDIF.

      WHEN 'C'.
        bdc_screen  'SAPMSVIM'   '0050' .
        bdc_field   'BDC_OKCODE' '=CLUS'.
        bdc_screen  'SAPMSVIM'   '0052'.
        bdc_field   'VIMDYNFLDS-VCLNAME' i_objectname.
        bdc_field   'BDC_OKCODE' '=CLMN'.   "Maintain
        bdc_screen  'SAPL0SVM'   '0101'.
        bdc_field   'BDC_OKCODE' '=1AKT'.   "Aktivieren
        bdc_screen  'SAPLSPO1'   '0100'.    "Sollen die Pflegedialoge...?
        bdc_field   'BDC_OKCODE' '=YES'.
        bdc_screen  'SAPL0SVM'   '0101'.
        bdc_field   'BDC_OKCODE' '=UEBE'.   "Übernehmen
        bdc_screen  'SAPMSVIM'   '0052'.
        bdc_field   'BDC_OKCODE' '=BACK'.
        bdc_screen  'SAPMSVIM'   '0050'.
        bdc_field   'BDC_OKCODE' '=BACK'.

        MESSAGE i666(01) WITH 'View cluster' i_objectname 'will be generated.' INTO sy-lisel. "#EC NOTEXT
        CALL METHOD log_message.

      WHEN OTHERS.
        MESSAGE e666(01) WITH 'Object type' i_objecttype 'not supported' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.

    ENDCASE.

    "-------------------------------------------
    CHECK testrun IS INITIAL.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SE54'
      EXCEPTIONS
        ok     = 0
        OTHERS = 4.
    IF sy-subrc NE 0.
      sy-msgty = 'E'. CALL METHOD log_message.
      RETURN.
    ENDIF.

    DATA: ls_tadir TYPE tadir.
    "CTO_TADIR_GET_OBJECT
    CALL FUNCTION 'CTO_OBJECT_GET_TADIR_KEY'
      EXPORTING
        iv_objectname = i_objectname
        iv_objecttype = i_objecttype
      IMPORTING
        ev_pgmid      = ls_tadir-pgmid
        ev_object     = ls_tadir-object
        ev_obj_name   = ls_tadir-obj_name.

    CALL METHOD add_to_transport    "and create TADIR
      EXPORTING
        i_object   = ls_tadir-object
        i_obj_name = ls_tadir-obj_name
        i_devclass = i_devclass
        i_langu    = i_masterlang
      EXCEPTIONS
        OTHERS     = 1.

    ls_opt-dismode = 'E'. " Dynpros nicht Anzeigen
*    ls_opt-DISMODE = 'A'. " Dynpros Anzeigen
*    ls_opt-UPDMODE = 'A'. " Asynchrone Verbuchung
*    ls_opt-CATTMODE = ' '. " Kein CATT aktiv
*    ls_opt-DEFSIZE = ' '. " Keine Standard Fenster Größe
*    ls_opt-NOBINPT = 'X'. " Batchinputmodus
*    ls_opt-NOBIEND = 'X'. " Batchinputmodus nach Ende der BDC-Daten
    ls_opt-racommit = 'X'. " Kein Stop nach Commit Work
    CALL TRANSACTION 'SE54' USING lt_bdc OPTIONS FROM ls_opt.

    CALL METHOD log_message.

  ENDMETHOD.                    "generate_maint_view

  METHOD activate.

    DATA: lt_dwinactiv TYPE TABLE OF dwinactiv,
          lt_dwinactiv_ddic TYPE TABLE OF dwinactiv,
          lv_answer,
          l_logname TYPE ddprh-protname,
          ls_job TYPE tbtcjob,
          lv_rc TYPE sysubrc,
          l_dref TYPE REF TO data,
          l_message   TYPE sy-lisel,
          ls_tabname  LIKE LINE OF gr_tabname.

    FIELD-SYMBOLS: <ls_dwinactiv> LIKE LINE OF lt_dwinactiv.

    CALL METHOD set_context
      EXPORTING
        i_langu    = space
        i_object   = space
        i_obj_name = 'Activation'.                          "#EC *

    IF testrun IS INITIAL.

      "activate switches "online"
      IF gr_switchname IS NOT INITIAL.
        SELECT * FROM dwinactiv INTO TABLE lt_dwinactiv
                                     WHERE obj_name IN gr_switchname.
        IF lt_dwinactiv[] IS  NOT INITIAL.
          LOOP AT lt_dwinactiv ASSIGNING <ls_dwinactiv>
                                WHERE uname NE sy-uname
                                  AND obj_name IN gr_switchname.
            <ls_dwinactiv>-uname = sy-uname.
          ENDLOOP.
          INSERT dwinactiv FROM TABLE lt_dwinactiv ACCEPTING DUPLICATE KEYS.
        ENDIF.

        MESSAGE s357(scwn).
        CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
          EXPORTING
            dictionary_only = 'X'
          TABLES
            objects         = lt_dwinactiv
          EXCEPTIONS
            OTHERS          = 5.
      ENDIF.

      "make sure that ranges are not empty
      APPEND 'IEQ' TO: "gr_switchname,
                       gr_domname,
                       gr_rollname,
                       gr_tabname,
                       gr_shlpname,
                       gr_viewname,
                       gr_enqname,
                       gr_ttypname,
                       gr_guistatus,
                       gr_indexes,
                       gr_indxtab,
                       gr_indxname.

      SELECT * FROM dwinactiv INTO TABLE lt_dwinactiv
              WHERE ( object NE space )   "dummy
                AND ( "obj_name IN gr_switchname OR
                      obj_name IN gr_domname
                   OR obj_name IN gr_rollname
                   OR obj_name IN gr_tabname
                   OR obj_name IN gr_shlpname
                   OR obj_name IN gr_viewname
                   OR obj_name IN gr_enqname
                   OR obj_name IN gr_ttypname
                   OR obj_name IN gr_guistatus
                   OR obj_name IN gr_indexes ).
    ENDIF.

    "add all relevant inactive objects also to current user's worklist
    "gets relevant if a different user is doing the activation.
    "entries for all users will be deleted if any user activates the object
    LOOP AT lt_dwinactiv ASSIGNING <ls_dwinactiv> WHERE uname NE sy-uname.
      <ls_dwinactiv>-uname = sy-uname.
    ENDLOOP.
    INSERT dwinactiv FROM TABLE lt_dwinactiv ACCEPTING DUPLICATE KEYS.

    LOOP AT lt_dwinactiv ASSIGNING <ls_dwinactiv>
                             WHERE object EQ 'DOMA' OR
                                   object EQ 'DTEL' OR
                                   object EQ 'TABL' OR
                                   object EQ 'INDX' OR
                                   object EQ 'VIEW' OR
                                   object EQ 'ENQU' OR
                                   object EQ 'TTYP' OR
                                   object EQ 'SHLP'.
      CALL FUNCTION 'RS_DD_CHECK_ACTIVATE'
        EXPORTING
          objtype = <ls_dwinactiv>-object
          objname = <ls_dwinactiv>-obj_name
*         i_no_ui = 'X' "not in all sap_basis releases
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc EQ 0.
        "DDIC always via RADMASG0
        "move DDIC objects to separate worklist
        APPEND <ls_dwinactiv> TO lt_dwinactiv_ddic.
        DELETE lt_dwinactiv.
      ELSE.
        MESSAGE e666(01) WITH 'No authorization to activate' <ls_dwinactiv>-object <ls_dwinactiv>-obj_name '. Call transaction SU53 for details' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        CASE <ls_dwinactiv>-object.
          WHEN 'DOMA'. DELETE gr_domname  WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'DTEL'. DELETE gr_rollname WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'TABL'.
            DELETE gr_tabname  WHERE low EQ <ls_dwinactiv>-obj_name.
            DELETE gr_indxtab  WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'INDX'. DELETE gr_indxname WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'VIEW'. DELETE gr_viewname WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'ENQU'. DELETE gr_enqname  WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'TTYP'. DELETE gr_ttypname WHERE low EQ <ls_dwinactiv>-obj_name.
          WHEN 'SHLP'. DELETE gr_shlpname WHERE low EQ <ls_dwinactiv>-obj_name.
        ENDCASE.
        DELETE lt_dwinactiv.
      ENDIF.
    ENDLOOP.

    IF lt_dwinactiv[] IS INITIAL AND lt_dwinactiv_ddic[] IS INITIAL AND gv_repository_changed IS INITIAL.
      IF testrun IS NOT INITIAL.
        MESSAGE i666(01) WITH 'No activation neccessary' '' '' '' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        IF gv_translation EQ 'X' OR gv_copy_translation EQ 'X'.
          MESSAGE i666(01) WITH 'Translation has been updated' '' '' '' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ENDIF.
      ENDIF.
    ELSEIF gv_errors_occured IS NOT INITIAL.
      MESSAGE w666(01) WITH 'Activation was skipped because of errors' 'in previous steps. Please try again.' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
    ELSEIF i_result_auth_check IS NOT INITIAL.
      MESSAGE e666(01) WITH 'No authorization to activate the DDIC changes.' 'Call transaction SU53 for details' '' '' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
    ELSE.  "do the activation

      IF lt_dwinactiv[] IS NOT INITIAL.
        "1) activate non DDIC objects (e.g. CUAD) online
        MESSAGE s357(scwn).
        CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
          EXPORTING
            dictionary_only = ' '
*           cwb_mode        = 'X'
          TABLES
            objects         = lt_dwinactiv
          EXCEPTIONS
            OTHERS          = 5.
        IF sy-subrc NE 0.
          lv_answer = 'A'.
          CALL METHOD log_message.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      IF lt_dwinactiv_ddic[] IS NOT INITIAL AND lv_answer NE 'A'.
        IF gv_unit_test IS NOT INITIAL OR genview IS NOT INITIAL.
          lv_answer = '1'.  "force online activation
        ELSE.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Activation'          "#EC NOTEXT
              text_question         = 'How do you want to activate DDIC?' "#EC NOTEXT
              text_button_1         = 'Online'              "#EC NOTEXT
              text_button_2         = 'Batch'               "#EC NOTEXT
              display_cancel_button = 'X'
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              OTHERS                = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            CALL METHOD log_message.
          ENDIF.
        ENDIF.
      ENDIF.

      IF sy-repid(2) EQ '%_'. "Temp. Subroutine Pool
        l_logname = 'UDO'.
      ELSEIF sy-repid(1) EQ '/'.
        SPLIT sy-repid AT '/' INTO l_logname l_logname l_logname. "remove namespace
      ELSE.
        l_logname = sy-repid.
      ENDIF.
      CONCATENATE l_logname sy-datum sy-uzeit INTO l_logname SEPARATED BY '_'.

      IF lv_answer EQ '1'.  "online
        "2a) activate DDIC online
        IF sy-repid(2) EQ '%_'. "Temp. Subroutine Pool
          l_logname = 'UDO'.
        ELSEIF sy-repid(1) EQ '/'.
          SPLIT sy-repid AT '/' INTO l_logname l_logname l_logname. "remove namespace
        ELSE.
          l_logname = sy-repid.
        ENDIF.

        CONCATENATE l_logname ':' sy-datum ':' sy-uzeit INTO l_logname.

        CALL METHOD mass_activation   "Step 1
          EXPORTING
            i_step     = 1
            i_logname  = space "no log, any error messages will appear in step 2 also
            i_ddmode   = 'T'
            i_inactive = 'X'
          EXCEPTIONS
            error      = 1.
        IF sy-subrc EQ 0.
          CALL METHOD mass_activation  "Step 2
            EXPORTING
              i_step     = 2
              i_logname  = l_logname
              i_ddmode   = 'O'
              i_inactive = ' '
            EXCEPTIONS
              error      = 1.
        ENDIF.

        CLEAR l_message.
        IF sy-subrc EQ 0.
          LOOP AT gr_dbtabname INTO ls_tabname WHERE low IS NOT INITIAL.
            "make a test SELECT * FROM table UP TP 1 ROWS to see whether it is really consistent
            "go via RFC to have the chance to catch short dumps
            "or GET_TABLE_KEYLIST_RFC
            CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'NONE' "#EC FB_PAR_MIS
              EXPORTING
                query_table         = ls_tabname-low
                rowcount            = 1
              EXCEPTIONS
                system_failure      = 1  MESSAGE l_message
                table_not_available = 0
                OTHERS              = 0.
            IF sy-subrc <> 0.
              IF l_message CS 'ASSIGN ... CASTING'.   "RFC_READ_TABLE does not like SSTRING fields
                MESSAGE i666(01) WITH 'Skipping additional check' 'for database table' ls_tabname-low INTO sy-lisel. "#EC *
                CLEAR l_message.  "we do not treat the as an error
              ELSE.
                MESSAGE e666(01) WITH l_message INTO sy-lisel. "#EC *
              ENDIF.
              CALL METHOD log_message.
            ENDIF.
          ENDLOOP.

          APPEND LINES OF gr_ttypname TO gr_tabname.
          APPEND LINES OF gr_rollname TO gr_tabname.

          LOOP AT gr_tabname INTO ls_tabname WHERE low IS NOT INITIAL.
            TRY.
                CREATE DATA l_dref TYPE (ls_tabname-low).
              CATCH cx_sy_create_data_error.
                MESSAGE e666(01) WITH 'Data Type' ls_tabname-low 'is inconsistent' INTO l_message. "#EC *
                CALL METHOD log_message.
            ENDTRY.
          ENDLOOP.
          IF l_message IS INITIAL.
            CLEAR sy-subrc.
          ELSE.
            MESSAGE e666(01) WITH 'DDIC activation step 3 ended with errors.' INTO sy-lisel. "#EC *
            CALL METHOD log_message.
            sy-subrc = 4.
          ENDIF.
        ENDIF.

        IF sy-subrc EQ 0.
          MESSAGE s666(01) WITH 'DDIC activation successful' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          MESSAGE i626(do) WITH 'Click this message to view' 'detailed activation logs with name' l_logname INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ELSE.
          "e.g. no authorization for activation or conversion of dependent database tables
          "make sure that the objects appear as inactive; corresponding are removed by DDIC activation phase 1 although phase 2 or 3 may fail.
          MODIFY dwinactiv FROM TABLE lt_dwinactiv_ddic.
          MESSAGE e626(do) WITH 'Click this message to view' 'detailed activation logs with name' l_logname INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ENDIF.

      ELSEIF lv_answer EQ '2' .
        "2b) activate DDIC objects via batch job
        ls_job-jobname = sy-repid.

        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = ls_job-jobname
          IMPORTING
            jobcount         = ls_job-jobcount
          CHANGING
            ret              = lv_rc
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.
        IF sy-subrc <> 0 OR lv_rc NE 0.
          MESSAGE e026(bt) WITH ls_job-jobname INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ELSE.
          SUBMIT radmasg0 WITH domname  IN gr_domname
                          WITH rollname IN gr_rollname
                          WITH tabname  IN gr_tabname
                          WITH indxtab  IN gr_indxtab
                          WITH indxname IN gr_indxname
                          WITH viewname IN gr_viewname
                          WITH ttypname IN gr_ttypname
                          WITH shlpname IN gr_shlpname
                          WITH logname  EQ l_logname
                          WITH logshow  EQ 'X'      "show log in SPOOL

                          WITH ddmode   EQ 'T'
                          WITH inactive EQ 'X'

                          VIA JOB ls_job-jobname
                          NUMBER ls_job-jobcount
                          AND RETURN.

          SUBMIT radmasg0 WITH domname  IN gr_domname
                          WITH rollname IN gr_rollname
                          WITH tabname  IN gr_tabname
                          WITH indxtab  IN gr_indxtab
                          WITH indxname IN gr_indxname
                          WITH viewname IN gr_viewname
                          WITH ttypname IN gr_ttypname
                          WITH shlpname IN gr_shlpname
                          WITH logname  EQ l_logname
                          WITH logshow  EQ 'X'      "show log in SPOOL

                          WITH ddmode   EQ 'O'
                          WITH inactive EQ ' '

                          VIA JOB ls_job-jobname
                          NUMBER ls_job-jobcount
                          AND RETURN.

          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount  = ls_job-jobcount
              jobname   = ls_job-jobname
              strtimmed = 'X'
            EXCEPTIONS
              OTHERS    = 1.
          IF sy-subrc EQ 0.
            MESSAGE s305(ut) WITH ls_job-jobname INTO sy-lisel. "#EC *
            CALL METHOD log_message.
            MESSAGE i666(01) WITH 'Use Report RADPROTB to view' 'activation log' l_logname 'after job has finished in transaction SMX.'. "#EC *
            CALL METHOD log_message.
          ELSE.
            MESSAGE e026(bt) WITH ls_job-jobname INTO sy-lisel. "#EC *
            CALL METHOD log_message.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL METHOD lcl_wb=>execute_method   "e.g. OSOD, SCP2
        EXPORTING
           i_method = trmtd_after_imp.

    ENDIF.

    SET PARAMETER ID 'EUK' FIELD space.
    EXPORT current_devclass FROM space TO MEMORY ID 'EUK'.

  ENDMETHOD.                    "activate

  METHOD mass_activation.

    DATA: lt_log TYPE TABLE OF trlog,
          ls_log TYPE trlog.

    SUBMIT radmasg0 WITH domname  IN gr_domname
                    WITH rollname IN gr_rollname
                    WITH tabname  IN gr_tabname
                    WITH indxtab  IN gr_indxtab
                    WITH indxname IN gr_indxname
                    WITH viewname IN gr_viewname
                    WITH enquname IN gr_enqname
                    WITH ttypname IN gr_ttypname
                    WITH shlpname IN gr_shlpname
                    WITH logname  EQ i_logname
                    WITH logshow  EQ space

                    WITH inactive EQ i_inactive
                    WITH ddmode   EQ i_ddmode

                    AND RETURN.

    IMPORT act_rc TO sy-subrc FROM MEMORY ID 'ACRC'.

    CHECK i_logname IS NOT INITIAL.

    IF sy-subrc > 4.  "4 is only warning
      CHECK i_step <> 1.  "messages of step 1 have to be ignored
      MESSAGE e666(01) WITH 'DDIC activation step' i_step 'ended with errors:' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
      CALL FUNCTION 'TR_READ_LOG'
        EXPORTING
          iv_log_type   = 'DB'
          iv_logname_db = i_logname
        TABLES
          et_lines      = lt_log
        EXCEPTIONS
          invalid_input = 1
          access_error  = 2
          OTHERS        = 3.
      "append errors from activation log
      LOOP AT lt_log INTO ls_log WHERE severity EQ 'E'.
        MESSAGE e666(01) WITH ls_log-line INTO sy-lisel.    "#EC *
        CALL METHOD log_message.
      ENDLOOP.
      RAISE error.
    ENDIF.

  ENDMETHOD.                    "mass_activation

  METHOD create_logical_object.

    CALL METHOD set_context
      EXPORTING
        i_langu    = space
        i_object   = i_object
        i_obj_name = i_obj_name.

    CHECK testrun IS INITIAL.

    CALL METHOD lcl_wb=>add_to_transport
      EXPORTING
        i_object   = i_object
        i_obj_name = i_obj_name
        i_devclass = i_devclass
        i_langu    = i_langu
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

    gv_repository_changed = 'X'.

  ENDMETHOD.                    "create_logical_object

  METHOD execute_method.

    DATA: ls_e070                   TYPE e070,
          ls_e071                   TYPE e071,
          lt_e071                   TYPE TABLE OF e071,
          lt_e071k                  TYPE TABLE OF e071k,
          ls_trkey                  TYPE trkey.
    DATA: lt_result TYPE scts_cl_results.                  ">=SAPBASIS700
    DATA: lt_client TYPE trexe_t_client.

    DATA: lt_trlog                  TYPE TABLE OF trlog,
          ls_trlog                  TYPE trlog.

    FIELD-SYMBOLS: <ls_method_call> TYPE trmtd_method_call.

    CHECK i_method IS NOT INITIAL.

    "get AFTER_IMP method
    SORT gt_trkey.
    DELETE ADJACENT DUPLICATES FROM gt_trkey COMPARING obj_type obj_name.
    LOOP AT gt_trkey INTO ls_trkey.
      ls_e071-pgmid    = 'R3TR'.
      ls_e071-object   = ls_trkey-obj_type.
      ls_e071-obj_name = ls_trkey-obj_name.
      APPEND ls_e071 TO lt_e071.
    ENDLOOP.

    CALL FUNCTION 'TRINT_FREE_MEMORY'.
*    CALL FUNCTION 'TR_INITIALIZE_LOG'  "to much action, do the main stuff only:
    DATA: gv_mem_id(9)               VALUE 'prot_file'.
    EXPORT gv_mem_file FROM space TO MEMORY ID gv_mem_id.

    IF sy-saprl >= 700.
      APPEND sy-mandt TO lt_result.                                       ">=SAPBASIS700
      CALL FUNCTION 'TRINT_CALL_AFTER_IMP_METHOD'
        EXPORTING
          iv_trkorr          = space
          is_e070            = ls_e070
          it_e071            = lt_e071[]
          it_e071k           = lt_e071k[]
          iv_update_lockflag = ''  "'X'
        CHANGING                                                         ">=SAPBASIS700
          it_client          = lt_result.                                ">=SAPBASIS700
      .                "keep for 640!
    ELSE.
      APPEND sy-mandt TO lt_client.
      CALL FUNCTION 'TRINT_CALL_AFTER_IMP_METHOD'
        EXPORTING
          iv_trkorr          = space
          is_e070            = ls_e070
          it_e071            = lt_e071[]
          it_e071k           = lt_e071k[]
          iv_update_lockflag = ''  "'X'
          it_client          = lt_client.
    ENDIF.
    CALL FUNCTION 'TR_READ_LOG'
      EXPORTING
        iv_log_type       = 'MEMORY'
*       IV_LOGNAME_FILE   =
*       IV_LOGNAME_DB     =
        iv_logname_memory = 'APPEND_LOG'
*       IV_TIMESTAMP      = '00000000000000'
*       IV_CLIENT         = '   '
*       IV_LANGUAGE       = SYST-LANGU
      TABLES
        et_lines          = lt_trlog
      EXCEPTIONS
        invalid_input     = 1
        access_error      = 2
        OTHERS            = 3.

    "append messages from ,ethod execution log
    LOOP AT lt_trlog INTO ls_trlog WHERE NOT ( class EQ 'PU' AND number EQ '131' ).  "WHERE severity EQ 'E'.
      MESSAGE i666(sodq) WITH ls_trlog-line(50) ls_trlog-line+50(50) ls_trlog-line+100(31) INTO sy-lisel. "#EC *
      sy-msgty = ls_trlog-severity.
      CALL METHOD log_message.
    ENDLOOP.

  ENDMETHOD.                    "execute_method


  METHOD create_table_entries.
*
    DATA: lt_ko200 TYPE TABLE OF ko200,
          ls_ko200 TYPE ko200,
          lt_e071k TYPE TABLE OF e071k,
          ls_e071k TYPE e071k.

    DATA: l_keylen TYPE sy-fleng.
    FIELD-SYMBOLS: <ls_entry>     TYPE any.

    IF i_logical_object IS INITIAL AND it_entries[] IS NOT INITIAL.
      CALL METHOD set_context
        EXPORTING
          i_langu    = space
          i_object   = 'TABU'
          i_obj_name = i_tabname.
    ENDIF.

    "Currently only a bunch of tables is supported:
    " - client independent
    " - language independent
    " - system tables
    " - key fields of data type char/numc

    CASE i_tabname.
      WHEN 'TVIMF' OR               "view events
           'TRESC' OR               "reserved namespace
           'T100S' OR 'T100SA' OR   "variable messages
           'TRGTS_SOURCE' OR 'TRGTS_SOURCE_T' OR 'TKEDR' OR   "TARO project
           'ROMDDELTA' OR 'ROOHIECAT' OR 'ROOHIECOM' OR 'ROOSESRMAP' OR 'ROOSFIELD' OR 'ROOSOURCE' OR 'ROOSOURCET' OR 'ROOSSEG' OR  "OSOD Datasource
           'BALOBJ' OR 'BALOBJT' OR 'BALSUB' OR 'BALSUBT' OR  "CDAT APPL_LOG  Application log defintion (SLG0)
           'SFRELN' OR 'SFREIM' OR 'SFREAC' OR 'SFRECOU' OR   "SFRN Release Note Attributes
           '/PLMB/FRW_TABPRO' OR                              "VDAT /PLMB/V_TABPRO
           'TCLO'.                                            "VDAT V_CLO  Class System
        "transported via R3TR TABU with keys in E071K
        DATA: ls_x030l TYPE x030l.
        CALL FUNCTION 'DDIF_NAMETAB_GET'
          EXPORTING
            tabname   = i_tabname
          IMPORTING
            x030l_wa  = ls_x030l
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        l_keylen = ls_x030l-keylen / ls_x030l-unicodelg.

        CLEAR ls_ko200.
        ls_ko200-pgmid    = 'R3TR'.
        ls_ko200-object   = 'TABU'.
        ls_ko200-objfunc  = 'K'.
        ls_ko200-obj_name = i_tabname.
        APPEND ls_ko200 TO lt_ko200.

        CLEAR ls_e071k.
        ls_e071k-pgmid      = ls_ko200-pgmid.
        ls_e071k-objname    = ls_ko200-obj_name.
        ls_e071k-object     = ls_ko200-object.
        ls_e071k-mastertype = ls_ko200-object.
        ls_e071k-mastername = ls_ko200-obj_name.

        LOOP AT it_entries ASSIGNING <ls_entry>.
          ls_e071k-tabkey = <ls_entry>(l_keylen).
          APPEND ls_e071k TO lt_e071k.
*       FIELD-SYMBOLS: <ls_key_entry> TYPE any,
*                      <ls_key_order> TYPE any.
*      ASSIGN <ls_entry>(l_keylen) TO <ls_key_entry> CASTING TYPE x.
*      ASSIGN ls_e071k-tabkey  TO <ls_key_order> CASTING TYPE x.
*      <ls_key_order> = <ls_key_entry>.
        ENDLOOP.
      WHEN 'VCLDIR' OR 'VCLDIRT' OR 'VCLMF' OR 'VCLSTRUC' OR 'VCLSTRUCT' OR 'VCLSTRUDEP'.
        "transported via R3TR VCLS -> Table entries will be inserted without transport request
      WHEN 'SCPRSATTR' OR 'SCPRSKEYS' OR 'SCPRSRECA' OR 'SCPRSTEXT' OR 'SCPRSVALS' OR 'SCPRSVALL'.             ">=SAPBASIS700
        "R3TR SCP2 Switched BC Set                                                                             ">=SAPBASIS700
        CHECK it_entries[] IS NOT INITIAL.   "entire deletion not supported                                    ">=SAPBASIS700
        READ TABLE it_entries[] INDEX 1 ASSIGNING <ls_entry>.                                                  ">=SAPBASIS700
        DATA: ls_scprsattr TYPE scprsattr. "All tables shall have (at least) same key fields like main table   ">=SAPBASIS700
        MOVE-CORRESPONDING <ls_entry> TO ls_scprsattr.                                                         ">=SAPBASIS700
        ASSERT ls_scprsattr-id IS NOT INITIAL.                                                                 ">=SAPBASIS700
        ">=SAPBASIS700
        FIELD-SYMBOLS: <lt_entries> TYPE STANDARD TABLE.                                                       ">=SAPBASIS700
        DATA: dref TYPE REF TO data.                                                                           ">=SAPBASIS700
        CREATE DATA dref LIKE it_entries.                                                                      ">=SAPBASIS700
        ASSIGN dref->* TO <lt_entries>.                                                                        ">=SAPBASIS700
        ">=SAPBASIS700
        "dynamic table with fix fieldnames! Dump will show if this is not the case                             ">=SAPBASIS700
        SELECT * FROM (i_tabname) INTO TABLE <lt_entries>                                                      ">=SAPBASIS700
                                  WHERE id      EQ ls_scprsattr-id                                             ">=SAPBASIS700
                                    AND version EQ ls_scprsattr-version                                        ">=SAPBASIS700
                                  ORDER BY PRIMARY KEY.                                                        ">=SAPBASIS700
        IF it_entries[] EQ <lt_entries>.                                                                       ">=SAPBASIS700
          MESSAGE s666(01) WITH 'No update neccessary' INTO sy-lisel. "#EC *                                   ">=SAPBASIS700
          CALL METHOD log_message.                                                                             ">=SAPBASIS700
          RETURN.                                                                                              ">=SAPBASIS700
        ENDIF.                                                                                                 ">=SAPBASIS700
        "remove all current entries before inserting the new ones                                              ">=SAPBASIS700
        DELETE   FROM (i_tabname) WHERE id      EQ ls_scprsattr-id                                             ">=SAPBASIS700
                                    AND version EQ ls_scprsattr-version.                                       ">=SAPBASIS700
      WHEN 'TDDAT' OR 'TVDIR'.
        "generated via execution of SE54 -> no need to insert and no need to transport
        c_deliver = gc_generate.
        RETURN.
      WHEN OTHERS.
        "objects which might have to be described as manual step or BCSet
        CONCATENATE gc_bcset '?' INTO c_deliver.
        MESSAGE w666(01) WITH 'Delivering entries for table' i_tabname 'is not approved yet' INTO sy-lisel. "#EC *
        IF it_entries[] IS NOT INITIAL.
          CALL METHOD log_message.
        ENDIF.
        RETURN.
    ENDCASE.

    c_deliver = gc_report.

    CHECK it_entries[] IS NOT INITIAL.

    IF testrun IS INITIAL.
      IF i_logical_object IS INITIAL.
        CALL FUNCTION 'TR_OBJECTS_CHECK'
          TABLES
            wt_ko200                = lt_ko200
*           WT_E071K                =
          EXCEPTIONS
            cancel_edit_other_error = 1
            show_only_other_error   = 2
            OTHERS                  = 3.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'TR_OBJECTS_INSERT'
            EXPORTING
              wi_order                = gv_trkorr
            IMPORTING
              we_order                = gv_trkorr
            TABLES
              wt_ko200                = lt_ko200
              wt_e071k                = lt_e071k
*             TT_TADIR                =
            EXCEPTIONS
              cancel_edit_other_error = 1
              show_only_other_error   = 2
              OTHERS                  = 3.
        ENDIF.
        IF sy-subrc NE 0.
          MESSAGE e666(01) WITH 'Could not be added to transport request.' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          RETURN.
        ENDIF.
      ENDIF.

*   FIELD-SYMBOLS: <lt_entries> type STANDARD TABLE.
*   data: dref type ref to data.
*   create data dref LIKE it_entries.
*   assign dref->* to <lt_entries>.
*
*CALL FUNCTION 'GET_TABLE_KEYLIST_RFC'
*  EXPORTING
*    tabname                    = i_tabname
*    get_systab                 = 'X'
*    rfc_dest                   = space
**   MTYPE                      =
**   MTABNAME                   =
** IMPORTING
**   NR_OF_ROWS                 =
**   RFC_MSG_TEXT               =
**   ET_KEYCONVERT_ERRORS       =
**   ET_CONVERT_ERRORS          =
*  TABLES
**   NAME_TAB                   =
*    inttab                     = <lt_entries>
*    tabkey                     = lt_e071k
* EXCEPTIONS
*   CLIENT_NOT_FOUND           = 1
*   PROTECTED                  = 2
*   READ_ERROR                 = 3
*   TABLE_NOT_FOUND            = 4
*   TOO_SMALL                  = 5
*   CONVERT_ERROR              = 6
*   WRONG_TYPE                 = 7
*   FIELDTAB_ERROR             = 8
*   NO_RIGHTS                  = 9
*   RFC_ERROR                  = 10
*   LENGTH_ERROR               = 11
*   STRING_ERROR               = 12
*   OTHERS                     = 13
*          .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.
*
*      if it_entries[] ne <lt_entries>[].
*      endif.
      MODIFY (i_tabname) FROM TABLE it_entries.

      DESCRIBE TABLE it_entries.
      MESSAGE i666(01) WITH sy-tfill 'entries were inserted into table' i_tabname INTO sy-lisel. "#EC *
      CALL METHOD log_message.
    ELSE.
      DESCRIBE TABLE it_entries.
      MESSAGE i666(01) WITH sy-tfill 'entries will be inserted into table' i_tabname INTO sy-lisel. "#EC *
      CALL METHOD log_message.
    ENDIF.

  ENDMETHOD.                    "create_table_entries


  METHOD add_to_transport.

    STATICS: s_object   LIKE i_object,
             s_obj_name TYPE string,
             s_subrc    TYPE sysubrc.

    DATA: l_tadir_name   TYPE tadir-obj_name,
          l_object_name  TYPE string,
          l_object_class TYPE string,
          l_extend,
          l_global_lock,
          ls_trkey TYPE trkey,
          ls_t000 TYPE t000,
          l_bcset_clients TYPE string.

    CLEAR sy-subrc.
    CHECK gv_copy_translation IS INITIAL.
    CHECK testrun IS INITIAL.

    IF i_object   NE s_object OR
       i_obj_name NE s_obj_name.

      s_object   = i_object.
      s_obj_name = i_obj_name.

      CASE i_object.
        WHEN 'SCP1'.
        WHEN 'SCP2'.
          "check if there is any BCSet client.
          SELECT mandt FROM t000 INTO ls_t000 WHERE ccorigcont EQ 'X'. "AND cccategory EQ 'C'    <- sometimes not customized as C but T, e.g. E27
            CONCATENATE l_bcset_clients ls_t000-mandt INTO l_bcset_clients SEPARATED BY space.
          ENDSELECT.
          IF l_bcset_clients IS NOT INITIAL.
            "check if current client is a BCSet client
            IF l_bcset_clients NS sy-mandt.
              MESSAGE e666(01) WITH 'BCSets can only be changed in one' 'of the following clients:' l_bcset_clients INTO sy-lisel. "#EC *
              CALL METHOD log_message.
              RAISE error.
            ENDIF.
          ENDIF.
      ENDCASE.

      CASE i_object.
        WHEN 'REPT' OR 'FUNC' OR 'CUAD' OR 'DOCU'.
          l_object_class = i_object.
          l_object_name  = i_obj_name.
        WHEN 'DOCV' OR 'DOCT' OR 'DSYS' OR 'STCS' OR 'MSAG'
          OR 'SFRN' OR 'OSOD' OR 'SCP1' OR 'SCP2'. "logical transport object
          l_object_class = i_object.
          l_object_name  = i_obj_name.
          l_global_lock  = 'X'.
          l_extend       = 'X'.
          l_tadir_name   = i_obj_name.
        WHEN 'MESS'.
          l_object_class = 'T100'.
          l_object_name  = i_obj_name.
        WHEN 'CLAS' OR 'FUGR' OR 'PARA' OR 'VCLS' OR 'AUTH'.
          l_object_class = i_object.
          l_object_name  = i_obj_name.
          l_tadir_name   = i_obj_name.
          l_global_lock  = 'X'.
        WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'VIEW' OR 'INDX' OR 'TTYP' OR 'SHLP' OR 'ENQU'.
          l_object_class = 'DICT'.
          CONCATENATE i_object i_obj_name INTO l_object_name.
          l_tadir_name   = i_obj_name.
          IF i_object EQ 'VIEW'.
            l_global_lock  = 'X'.   "always write R3TR VIEW
          ENDIF.
        WHEN OTHERS.
          l_object_class = i_object.
          l_object_name  = i_obj_name.
      ENDCASE.

      "create TADIR entry in advance avoid popup
      IF l_tadir_name IS NOT INITIAL.
        DATA: ls_tadir_old TYPE tadir.
        SELECT SINGLE * FROM tadir INTO ls_tadir_old
                       WHERE pgmid      EQ 'R3TR'
                         AND object     EQ i_object
                         AND obj_name   EQ l_tadir_name.
        IF i_langu CA 'ED'.
          "set correct language and package on first call per object
          "skip this for other languages than EN/DE.
          CALL FUNCTION 'TR_TADIR_INTERFACE'
            EXPORTING
*             WI_DELETE_TADIR_ENTRY = ' '
              wi_test_modus         = space
              wi_tadir_pgmid        = 'R3TR'
              wi_tadir_object       = i_object
              wi_tadir_obj_name     = l_tadir_name
              wi_tadir_devclass     = i_devclass
              wi_tadir_masterlang   = i_langu
            EXCEPTIONS
              OTHERS                = 25.
          IF sy-subrc <> 0.
          ENDIF.
        ENDIF.
        IF i_devclass IS NOT INITIAL.
          "set correct package in memory
          EXPORT current_devclass FROM i_devclass TO MEMORY ID 'EUK'.
          SET PARAMETER ID 'EUK' FIELD i_devclass.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'RS_CORR_INSERT'    "and create TADIR-entry
        EXPORTING
          object              = l_object_name
          object_class        = l_object_class
          devclass            = i_devclass
          korrnum             = gv_trkorr
          global_lock         = l_global_lock
*         AUTHOR              = ' '
          master_language     = i_langu
*         GENFLAG             = ' '
*         PROGRAM             = ' '
*         OBJECT_CLASS_SUPPORTS_MA = ' '
          extend              = l_extend
*         SUPPRESS_DIALOG     = ' '
*         MOD_LANGU           = ' '
*         ACTIVATION_CALL     = ' '
        IMPORTING
          korrnum             = gv_trkorr
*         ORDERNUM            =
*         NEW_CORR_ENTRY      =
*         AUTHOR              =
          transport_key       = ls_trkey
*         NEW_EXTEND          =
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.
      s_subrc = sy-subrc.

      IF sy-subrc EQ 0.
        "collect objects with update
        COLLECT ls_trkey INTO gt_trkey.
        "correct master language if neccessary
        IF l_tadir_name IS NOT INITIAL AND ls_tadir_old IS INITIAL.
          IF i_langu CA 'ED'.
            UPDATE tadir SET masterlang = i_langu
                       WHERE pgmid      EQ 'R3TR'
                         AND object     EQ i_object
                         AND obj_name   EQ l_tadir_name
                         AND masterlang NE i_langu.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF s_subrc NE 0.
      MESSAGE e666(01) WITH 'Could not add to transport request' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
      RAISE error.
    ENDIF.

  ENDMETHOD.                    "add_to_transport

  METHOD register_inactive_object.

    DATA: l_obj_name TYPE e071-obj_name.

    l_obj_name = i_objname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object            = i_objtype
        obj_name          = l_obj_name
      EXCEPTIONS
        wrong_object_name = 1
        OTHERS            = 2.

    IF i_treetype IS SUPPLIED.
      CHECK i_treetype NE space.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          type   = i_treetype
          object = l_obj_name.
    ELSE.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          type   = i_objtype
          object = l_obj_name.
    ENDIF.

  ENDMETHOD.                    "register_inactive_object

  METHOD create_message_class.    "will be activated immediately

    DATA: ls_t100a     TYPE t100a,
          ls_t100t     TYPE t100t.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'MSAG'
        i_obj_name = i_msgid.

    "check if message class exists
    SELECT SINGLE * FROM t100a INTO ls_t100a WHERE arbgb = i_msgid.
    IF sy-subrc EQ 0.
      MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    CHECK testrun IS INITIAL.

    CALL METHOD add_to_transport    "and create TADIR
      EXPORTING
        i_object   = 'MSAG'
        i_obj_name = i_msgid
        i_devclass = i_devclass
        i_langu    = i_langu
      EXCEPTIONS
        OTHERS     = 1.

    CHECK sy-subrc EQ 0.

    ls_t100a-lastuser   = sy-uname.
    ls_t100a-ldate      = sy-datum.
    ls_t100a-ltime      = sy-uzeit.
    ls_t100a-masterlang = i_langu.
    ls_t100a-arbgb      = i_msgid.
    MODIFY t100a FROM ls_t100a. "no activation needed

    ls_t100t-sprsl = ls_t100a-masterlang.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = i_msgid.  "take any text
    MODIFY t100t FROM ls_t100t.

    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        type   = 'MSAG'
        object = i_msgid.

  ENDMETHOD.                    "create_message_class

  METHOD create_message.

    DATA: lt_bdc   TYPE TABLE OF bdcdata,
          ls_bdc   TYPE bdcdata,
          ls_bdcp  TYPE bdcdata,
          ls_opt     TYPE ctu_params.
    DATA: ls_t100    TYPE t100,
          ls_t100a   TYPE t100a,
          ls_t100u   TYPE t100u,
          l_obj_name TYPE dokil-object,
          ls_msg_class LIKE LINE OF gr_msg_class VALUE 'IEQ'.

    ls_msg_class-low = i_msgid.
    COLLECT ls_msg_class INTO gr_msg_class.

    CALL METHOD set_context
      EXPORTING
        i_langu     = i_langu
        i_object    = 'MESS'
        i_obj_name  = i_msgid
        i_obj_name2 = i_msgno.

    SELECT SINGLE * FROM t100a INTO ls_t100a
                   WHERE arbgb EQ i_msgid.
    IF sy-subrc NE 0.
      IF testrun IS INITIAL OR i_msgid NOT IN gr_msg_class.
        MESSAGE e509(eu) WITH i_msgid INTO sy-lisel.        "#EC *
        CALL METHOD log_message.
      ENDIF.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM t100 INTO ls_t100
                   WHERE sprsl EQ i_langu
                     AND arbgb EQ i_msgid
                     AND msgnr EQ i_msgno.

    SELECT SINGLE * FROM t100u INTO ls_t100u
                   WHERE arbgb EQ i_msgid
                     AND msgnr EQ i_msgno.
    IF i_text EQ ls_t100-text.
      IF i_selfdef IS INITIAL     AND ls_t100u-selfdef IS INITIAL OR
         i_selfdef IS NOT INITIAL AND ls_t100u-selfdef IS NOT INITIAL.
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.
    ENDIF.

    CHECK testrun IS INITIAL.

    CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
      EXPORTING
        docu_id  = 'NA'
        element  = i_msgid
        addition = i_msgno
      IMPORTING
        object   = l_obj_name
      EXCEPTIONS
        OTHERS   = 0.

    CALL METHOD add_to_transport
      EXPORTING
        i_object   = 'MESS'
        i_obj_name = l_obj_name
      EXCEPTIONS
        OTHERS     = 1.
    CHECK sy-subrc EQ 0.

    IF i_langu EQ ls_t100a-masterlang AND gv_copy_translation IS INITIAL.
      "create or change message and write into transport request
      "if sy-langu is different from master language SE91 will switch to master lang

      "-------------------------------------------
      ls_bdcp-program  = 'SAPLWBMESSAGES'.
      ls_bdcp-dynpro   = '0100'.
      ls_bdcp-dynbegin = 'X'.
      APPEND ls_bdcp TO lt_bdc.

*    ls_bdc-fnam = 'BDC_CURSOR'.
*    ls_bdc-fval = 'MSG_NUMMER'.
*    APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'RSDAG-ARBGB'.
      ls_bdc-fval = i_msgid.
      APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'MSG_NUMMER'.
      ls_bdc-fval = i_msgno.
      APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'RSDAG-MSGFLAG'.
      ls_bdc-fval = 'X'.
      APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'BDC_OKCODE'.
      ls_bdc-fval = '=WB_EDIT'.
      APPEND ls_bdc TO lt_bdc.

      "-------------------------------------------
      ls_bdcp-dynpro   = '1000'.
      ls_bdcp-dynbegin = 'X'.
      APPEND ls_bdcp TO lt_bdc.

      ls_bdc-fnam = 'BDC_SUBSCR'.
      ls_bdc-fval = 'SAPLWBMESSAGES                          0101SUB'.
      APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'T100-TEXT(01)'.
      ls_bdc-fval = i_text.
      APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'LISTTAB-DOKU_FLAG(01)'.
      IF i_selfdef IS INITIAL.
        ls_bdc-fval = ' '.
      ELSE.
        ls_bdc-fval = 'X'.
      ENDIF.
      APPEND ls_bdc TO lt_bdc.

      ls_bdc-fnam = 'BDC_OKCODE'.
      ls_bdc-fval = '=WB_SAVE'.
      APPEND ls_bdc TO lt_bdc.

      "-------------------------------------------
      ls_bdcp-dynpro   = '1000'.
      ls_bdcp-dynbegin = 'X'.
      APPEND ls_bdcp TO lt_bdc.

      ls_bdc-fnam = 'BDC_OKCODE'.
      ls_bdc-fval = '=WB_BACK'.
      APPEND ls_bdc TO lt_bdc.

      "-------------------------------------------
      ls_bdcp-dynpro   = '0100'.
      ls_bdcp-dynbegin = 'X'.
      APPEND ls_bdcp TO lt_bdc.

      ls_bdc-fnam = 'BDC_OKCODE'.
      ls_bdc-fval = '=WB_BACK'.
      APPEND ls_bdc TO lt_bdc.

      ls_opt-dismode = 'E'.
      ls_opt-racommit = 'X'.
      ls_opt-nobinpt = 'X'.
      ls_opt-nobiend = 'X'.

      "-------------------------------------------
      CALL TRANSACTION 'SE91' USING lt_bdc OPTIONS FROM ls_opt.
    ELSE.
      "update translation only
      ls_t100-arbgb = i_msgid.
      ls_t100-msgnr = i_msgno.
      ls_t100-sprsl = i_langu.
      ls_t100-text  = i_text.
      MODIFY t100 FROM ls_t100.  "no activation needed
    ENDIF.
    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.
    gv_translation = 'X'.
    gv_repository_changed = 'X'.

  ENDMETHOD.                    "create_message

  METHOD create_transaction.

    CONSTANTS: hex_rep   TYPE x VALUE '80', " Report              R
               hex_rep84 TYPE x VALUE '84'. " Report              R   with Authority-Check object

    DATA: l_text TYPE tstct-ttext.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'TRAN'
        i_obj_name = i_tcode.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = i_tcode
*      TABLES
*       TCODES           =
*       GUI_ATTRIBUTES   =
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc EQ 0.
      "evtl vergleichen und bei Ungleichheit zuerst löschen
      "First appraoch: No update
      MESSAGE i666(01) WITH 'Transaction exists. No update necessary' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    CHECK testrun IS INITIAL.

    IF i_text IS INITIAL.
      l_text = i_tcode.
    ELSE.
      l_text = i_text.
    ENDIF.
    CALL FUNCTION 'RPY_TRANSACTION_INSERT'          "performs rs_corr_insert
      EXPORTING
        transaction         = i_tcode
        program             = i_program
        dynpro              = i_dynpro
        language            = i_langu
        development_class   = i_devclass
        transaction_type    = i_type
        shorttext           = i_text
*       CALLED_TRANSACTION  =
*       CALLED_TRANSACTION_SKIP             =
*       VARIANT             = i_param
*       CL_INDEPENDEND      =
*       EASY_WEB_TRANSACTION                =
*       PROFESSIONEL_USER_TRANSACTION       =
        html_enabled        = i_htmlgui
        java_enabled        = i_javagui
        wingui_enabled      = i_wingui
*       SERVICEFILE         =
*       GENFLAG             = ' '
*    TABLES
*       DOCU_TABLE_USER     =
*       DOCU_TABLE_TECH     =
*       PARAM_VALUES        =
      EXCEPTIONS
        cancelled           = 1
        already_exist       = 2
        permission_error    = 3
        name_not_allowed    = 4
        name_conflict       = 5
        illegal_type        = 6
        object_inconsistent = 7
        db_access_error     = 8
        OTHERS              = 9.
    IF sy-subrc <> 0.
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    "directly write some table entries which are not part of the "API"
    "will be transported along with the R3TR TRAN
    IF i_param IS NOT INITIAL.
      DATA: ls_tstcp TYPE tstcp.
      ls_tstcp-tcode = i_tcode.
      ls_tstcp-param = i_param.
      MODIFY tstcp FROM ls_tstcp.
    ENDIF.

    IF it_auth_obj IS NOT INITIAL.
      MODIFY tstca FROM TABLE it_auth_obj.
      UPDATE tstc SET cinfo = hex_rep84
                WHERE tcode EQ i_tcode
                  AND cinfo EQ hex_rep.
    ENDIF.

    "update TADIR master language if necessary
    UPDATE tadir SET masterlang = i_langu
               WHERE pgmid      EQ 'R3TR'
                 AND object     EQ 'TRAN'
                 AND obj_name   EQ i_tcode
                 AND masterlang NE i_langu.

    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

    gv_repository_changed = 'X'.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        type   = 'TRAN'
        object = i_tcode.

  ENDMETHOD.                    "create_transaction


  METHOD create_mess_docu.

    STATICS: lt_line TYPE TABLE OF tline.

    DATA: ls_head TYPE thead,
          lt_line_old LIKE lt_line,
          ls_line TYPE tline,
          ls_tadir TYPE tadir,
          lv_no_masterlang.

    IF i_line IS NOT INITIAL.
      ls_line = i_line.
      SHIFT ls_line-tdline LEFT.
      APPEND ls_line TO lt_line.
    ELSE.
      CALL METHOD set_context
        EXPORTING
          i_langu     = i_langu
          i_object    = 'MESS'
          i_obj_name  = i_msgid
          i_obj_name2 = i_msgno
          i_text      = 'Message Longtext'.                 "#EC NOTEXT

      ls_head-tdobject = 'DOKU'.
      CONCATENATE i_msgid i_msgno INTO ls_head-tdname.
      ls_head-tdid     = 'NA'.    "Nachricht/Message
      ls_head-tdspras  = i_langu.
      ls_head-tdform   = 'S_DOCU_SHOW'.
      ls_head-tdstyle  = 'S_DOCUS1'.

      CALL FUNCTION 'DOCU_GET'
        EXPORTING
          id     = ls_head-tdid(2)
          langu  = ls_head-tdspras
          object = ls_head-tdname(60)
        TABLES
          line   = lt_line_old
        EXCEPTIONS
          OTHERS = 0.

      IF et_lines IS SUPPLIED.
        et_lines[] = lt_line[] = lt_line_old[].
        RETURN.
      ENDIF.

      IF lt_line_old[] EQ lt_line.
        REFRESH lt_line.
        "no update necessary
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.

      IF testrun IS INITIAL.

        CONCATENATE ls_head-tdid ls_head-tdname INTO ls_tadir-obj_name.

        CALL METHOD add_to_transport
          EXPORTING
            i_object   = 'DOCU'
            i_obj_name = ls_tadir-obj_name
          EXCEPTIONS
            OTHERS     = 0.

        SELECT SINGLE * FROM tadir INTO ls_tadir
                       WHERE pgmid    EQ 'R3TR'
                         AND object   EQ 'MSAG'
                         AND obj_name EQ i_msgid.
        IF ls_head-tdspras NE ls_tadir-masterlang.
          lv_no_masterlang = 'X'.
        ENDIF.

        "create new version
        DATA l_dokversion TYPE dokhl-dokversion.
        SELECT MAX( dokversion ) FROM dokhl INTO l_dokversion "#EC *
                          WHERE id     = ls_head-tdid(2)
                            AND object = ls_head-tdname
                            AND langu  = ls_head-tdspras.
        ADD 1 TO l_dokversion.

        ls_head-tdfdate = sy-datum.
        ls_head-tdftime = sy-uzeit.
        ls_head-tdfuser = sy-uname.
        ls_head-tdldate = sy-datum.
        ls_head-tdltime = sy-uzeit.
        ls_head-tdluser = sy-uname.

        CALL FUNCTION 'DOCU_UPDATE'
          EXPORTING
*           ACTCLASS      = ' '
            head          = ls_head
            no_masterlang = lv_no_masterlang
            state         = 'A'
            typ           = 'E'
            version       = l_dokversion
          TABLES
            line          = lt_line.

        MESSAGE i666(01) WITH 'Update successful' 'for longtext of message' i_msgid i_msgno INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      gv_translation = 'X'.
      REFRESH lt_line.
    ENDIF.

  ENDMETHOD.                    "create_mess_docu

  METHOD create_dtel_docu.

    STATICS: lt_line TYPE TABLE OF tline.

    DATA: ls_head TYPE thead,
          lt_line_old LIKE lt_line,
          ls_line TYPE tline,
          ls_tadir TYPE tadir,
          lv_no_masterlang.

    IF i_line IS NOT INITIAL.
      ls_line = i_line.
      SHIFT ls_line-tdline LEFT.
      APPEND ls_line TO lt_line.
    ELSE.
      CALL METHOD set_context
        EXPORTING
          i_langu    = i_langu
          i_object   = 'DTEL'
          i_obj_name = i_rollname
          i_text     = 'Data Element Longtext'.             "#EC NOTEXT

      ls_head-tdobject = 'DOKU'.
      ls_head-tdname   = i_rollname.
      ls_head-tdid     = 'DE'.      "Data element
      ls_head-tdspras  = i_langu.
      ls_head-tdform   = 'S_DOCU_SHOW'.
      ls_head-tdstyle  = 'S_DOCUS1'.

      CALL FUNCTION 'DOCU_GET'
        EXPORTING
          id     = ls_head-tdid(2)
          langu  = ls_head-tdspras
          object = ls_head-tdname(60)
        TABLES
          line   = lt_line_old
        EXCEPTIONS
          OTHERS = 0.

      IF et_lines IS SUPPLIED.
        et_lines[] = lt_line[] = lt_line_old[].
        RETURN.
      ENDIF.

      IF lt_line_old[] EQ lt_line.
        REFRESH lt_line.
        "no update necessary
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
        RETURN.
      ENDIF.

      IF testrun IS INITIAL.

        CONCATENATE ls_head-tdid ls_head-tdname INTO ls_tadir-obj_name.

        CALL METHOD add_to_transport
          EXPORTING
            i_object   = 'DOCU'
            i_obj_name = ls_tadir-obj_name
          EXCEPTIONS
            OTHERS     = 1.
        CHECK sy-subrc EQ 0.

        SELECT SINGLE * FROM tadir INTO ls_tadir
                       WHERE pgmid    EQ 'R3TR'
                         AND object   EQ 'DTEL'
                         AND obj_name EQ i_rollname.
        IF ls_head-tdspras NE ls_tadir-masterlang.
          lv_no_masterlang = 'X'.
        ENDIF.

        "create new version
        DATA l_dokversion TYPE dokhl-dokversion.
        SELECT MAX( dokversion ) FROM dokhl INTO l_dokversion "#EC *
                          WHERE id     = ls_head-tdid(2)
                            AND object = ls_head-tdname
                            AND langu  = ls_head-tdspras.
        ADD 1 TO l_dokversion.

        ls_head-tdfdate = sy-datum.
        ls_head-tdftime = sy-uzeit.
        ls_head-tdfuser = sy-uname.
        ls_head-tdldate = sy-datum.
        ls_head-tdltime = sy-uzeit.
        ls_head-tdluser = sy-uname.

        CALL FUNCTION 'DOCU_UPDATE'
          EXPORTING
*           ACTCLASS      = ' '
            head          = ls_head
            no_masterlang = lv_no_masterlang
            state         = 'A'
            typ           = 'E'
            version       = l_dokversion
          TABLES
            line          = lt_line.

        MESSAGE i666(01) WITH 'Update successful' 'for longtext of data element' i_rollname INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      gv_translation = 'X'.
      REFRESH lt_line.
    ENDIF.

  ENDMETHOD.                    "create_dtel_docu

  METHOD create_other_docu.

    STATICS: lt_line TYPE TABLE OF tline.

    DATA: ls_head TYPE thead,
          lt_line_old LIKE lt_line,
          ls_line TYPE tline,
          ls_tadir TYPE tadir,
          lt_e071k TYPE TABLE OF e071k,
          ls_ko200 TYPE ko200,
          lv_no_masterlang.

    DATA: lt_sfreln  TYPE TABLE OF sfreln,
          lt_sfreim  TYPE TABLE OF sfreim,
          lt_sfreac  TYPE TABLE OF sfreac,
          lt_sfrecou TYPE TABLE OF sfrecou.

    IF i_line IS NOT INITIAL.
      ls_line = i_line.
      SHIFT ls_line-tdline LEFT.
      APPEND ls_line TO lt_line.
    ELSE.
      CALL METHOD set_context
        EXPORTING
          i_langu     = i_langu
          i_object    = 'DOCU'
          i_obj_name  = i_docname(2)
          i_obj_name2 = i_docname+2.

      ls_head-tdobject = 'DOKU'.
      ls_head-tdname   = i_docname+2.
      ls_head-tdid     = i_docname(2).
      ls_head-tdspras  = i_langu.
      ls_head-tdform   = 'S_DOCU_SHOW'.
      ls_head-tdstyle  = 'S_DOCUS1'.

      CALL FUNCTION 'DOCU_GET'
        EXPORTING
          id     = ls_head-tdid(2)
          langu  = ls_head-tdspras
          object = ls_head-tdname(60)
        TABLES
          line   = lt_line_old
        EXCEPTIONS
          OTHERS = 0.

      IF et_lines IS SUPPLIED.
        et_lines[] = lt_line[] = lt_line_old[].
        RETURN.
      ENDIF.

      IF lt_line_old[] EQ lt_line.
        "no update necessary
        MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
        CALL METHOD log_message.

        REFRESH lt_line.
        RETURN.
      ENDIF.

      IF testrun IS INITIAL.

*DT TEST_UDO            R3TR DOCV DTUDO_TEST
*DT /ASU/UDO_TEST       R3TR DOCV /ASU/DTUDO_TEST
*UO /AIN/BW             R3TR DOCV UO/AIN/BW
*UO A_S_ANLKL           R3TR DOCV UOA_S_ANLKL
*MO AAIP0001            R3TR DOCV MOAAIP0001
*MO /ASU/UDO_TEST	      R3TR DOCV MO/ASU/UDO_TEST
*HY BOOK/NFM/IMG_ALL    R3TR DSYS /NFM/BOOKIMG_ALL
*HY BOOKAUDIT_FI        R3TR DSYS BOOKAUDIT_FI
*HY SIMGTEST_UDO        R3TR DSYS SIMGTEST_UDO
*HY SIMG/ASU/TEST_UDO	  R3TR DSYS /ASU/SIMGTEST_UDO
*TX /ASU/UDO_TEST       R3TR DOCT /ASU/UDO_TEST
*TX UDO_TEST            R3TR DOCT UDO_TEST

        CALL FUNCTION 'DOCU_TR_OBJECT_CHECK'
          EXPORTING
            object_id   = ls_head-tdid(2)
            object_name = ls_head-tdname(60)
          TABLES
            wt_e071k    = lt_e071k
          CHANGING
            corr_entry  = ls_ko200.

* DOCU_FROM_TR_OBJECT_RECEIVE  (not in 640)

        CALL METHOD add_to_transport   "and create TADIR entry
          EXPORTING
            i_langu    = i_langu
            i_devclass = i_devclass
            i_object   = ls_ko200-object
            i_obj_name = ls_ko200-obj_name
          EXCEPTIONS
            OTHERS     = 1.
        CHECK sy-subrc EQ 0.

        MOVE-CORRESPONDING ls_ko200 TO ls_tadir.
        CALL FUNCTION akb_get_tadir
          EXPORTING
            obj_type = ls_tadir-object
            obj_name = ls_tadir-obj_name
          IMPORTING
            tadir    = ls_tadir
          EXCEPTIONS
            OTHERS   = 1.
        IF sy-subrc <> 0.
          MESSAGE i666(01) WITH 'Error reading' ls_ko200-object ls_ko200-obj_name INTO sy-lisel. "#EC *
          CALL METHOD log_message.
          RETURN.
        ENDIF.

        IF ls_head-tdspras NE ls_tadir-masterlang.
          lv_no_masterlang = 'X'.
        ENDIF.

        "create new version
        DATA l_dokversion TYPE dokhl-dokversion.
        SELECT MAX( dokversion ) FROM dokhl INTO l_dokversion "#EC *
                          WHERE id     = ls_head-tdid(2)
                            AND object = ls_head-tdname
                            AND langu  = ls_head-tdspras.
        ADD 1 TO l_dokversion.

        ls_head-tdfdate = sy-datum.
        ls_head-tdftime = sy-uzeit.
        ls_head-tdfuser = sy-uname.
        ls_head-tdldate = sy-datum.
        ls_head-tdltime = sy-uzeit.
        ls_head-tdluser = sy-uname.

        IF ls_head-tdid EQ 'IN'. "Release note  / oder DOCV
          IF 1  = 0.
            "Nachlesen der Releasenote-Attribute
            "und umformen in zusätzliche Zeilen in lt_line
            "vergleiche Aufruf von DOCU_UPDATE, wenn man in der SE61 ein IN RELN pflegt.
            PERFORM index_descriptors IN PROGRAM saplsdoc.  "#EC *
          ENDIF.
          "read current attributes
          SELECT * FROM sfreln  INTO TABLE lt_sfreln  WHERE txtkey = i_docname+6. "#EC CI_GENBUFF
          SELECT * FROM sfreim  INTO TABLE lt_sfreim  WHERE rel_cla EQ 'RELN' AND rel_obj EQ i_docname+6. "#EC CI_GENBUFF
          SELECT * FROM sfreac  INTO TABLE lt_sfreac  WHERE rel_cla EQ 'RELN' AND rel_obj EQ i_docname+6. "#EC CI_GENBUFF
          SELECT * FROM sfrecou INTO TABLE lt_sfrecou WHERE rel_cla EQ 'RELN' AND rel_obj EQ i_docname+6. "#EC CI_GENBUFF
        ENDIF.

        CALL FUNCTION 'DOCU_UPDATE'   "this initializes attributes
          EXPORTING
*         ACTCLASS      = ' '
            head          = ls_head
            no_masterlang = lv_no_masterlang
            state         = 'A'
            typ           = i_typ
            version       = l_dokversion
          TABLES
            line          = lt_line.

        "restore attributes
        MODIFY sfreln  FROM TABLE lt_sfreln.
        MODIFY sfreim  FROM TABLE lt_sfreim.
        MODIFY sfreac  FROM TABLE lt_sfreac.
        MODIFY sfrecou FROM TABLE lt_sfrecou.

        MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
        CALL METHOD log_message.
      ENDIF.
      gv_translation = 'X'.
      REFRESH lt_line.
    ENDIF.

  ENDMETHOD.                    "create_other_docu

  METHOD create_function_group.

    DATA: l_text       LIKE i_text,
          l_tadir_name TYPE tadir-obj_name,
          l_master TYPE progname.

    CALL METHOD set_context
      EXPORTING
        i_langu    = i_langu
        i_object   = 'FUGR'
        i_obj_name = i_area.

    CALL FUNCTION 'RPY_EXISTENCE_CHECK_FUGR'
      EXPORTING
        name      = i_area
*       LIMU_KEY  =
      EXCEPTIONS
        not_exist = 1
        OTHERS    = 2.
    IF sy-subrc EQ 0.
      "no update necessary
      MESSAGE i666(01) WITH 'No update necessary' INTO sy-lisel. "#EC *
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    CHECK testrun IS INITIAL.

    IF i_text IS INITIAL.
      l_text = i_area.
    ELSE.
      l_text = i_text.
    ENDIF.

*    CALL METHOD add_to_transport
*      EXPORTING
*        i_object   = 'FUGR'
*        i_obj_name = i_area
*        i_devclass = i_devclass
*        i_langu    = i_langu  "masterlanguage (only relevant during first call)
*      EXCEPTIONS
*        OTHERS     = 1.
*    CHECK sy-subrc EQ 0.

    "create TADIR entry in advance avoid popup
    IF i_langu CA 'ED'.
      "set correct language and package on first call per object
      "skip this for other languages than EN/DE.
      l_tadir_name = i_area.
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
*         WI_DELETE_TADIR_ENTRY = ' '
          wi_test_modus         = space
          wi_tadir_pgmid        = 'R3TR'
          wi_tadir_object       = 'FUGR'
          wi_tadir_obj_name     = l_tadir_name
          wi_tadir_devclass     = i_devclass
          wi_tadir_masterlang   = i_langu
        EXCEPTIONS
          OTHERS                = 25.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
    IF i_devclass IS NOT INITIAL.
      "set correct package in memory
      FREE MEMORY ID 'EUK'.
      SET PARAMETER ID 'EUK' FIELD i_devclass.
    ENDIF.

    "SELECT SINGLE FROM tlibg.
    "SELECT SINGLE FROM tlibt.
    CALL FUNCTION 'RS_FUNCTION_POOL_ADD'
      EXPORTING
        aktion                 = 'INSR'  "UPDA
        area                   = i_area
        corrnum                = gv_trkorr
        text                   = l_text
        uname                  = sy-uname
*       WITH_MONITOR           = ' '
*       with_korr              = ' '     "pass ' ' if add_to_transport has happend!
        save_active            = 'X'
*       WB_FB_MANAGER          =
*     IMPORTING
*       NEW_NAME               =
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_pool_exist    = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        OTHERS                 = 8.
    IF sy-subrc NE 0.
      CALL METHOD log_message.
      RETURN.
    ENDIF.

    "update TRDIR/REPOSRC/PROGRDIR
    l_master = lcl_wb=>get_function_pool_master( i_area ).

    UPDATE progdir SET appl    = i_appl
                       fixpt   = i_fixpt
                       uccheck = i_uccheck
                 WHERE name EQ l_master.

    "update TADIR master language if necessary
    UPDATE tadir SET masterlang = i_langu
               WHERE pgmid      EQ 'R3TR'
                 AND object     EQ 'FUGR'
                 AND obj_name   EQ i_area
                 AND masterlang NE i_langu.

    MESSAGE i666(01) WITH 'Update successful' INTO sy-lisel. "#EC *
    CALL METHOD log_message.

  ENDMETHOD.                    "create_function_group





  METHOD log_message.

    DATA: ls_msg     TYPE bal_s_msg.

    IF sy-msgno IS INITIAL OR sy-msgid IS INITIAL.
      MESSAGE i666(01) WITH 'unknown error' INTO sy-lisel.  "#EC *
    ENDIF.
    MOVE-CORRESPONDING syst TO ls_msg.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = gv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 0.

    IF ls_msg-msgty CA 'EA'.
      gv_errors_occured = 'X'.
    ENDIF.

  ENDMETHOD.                    "log_message

  METHOD display_log.

    DATA: lt_log_handle     TYPE bal_t_logh,
          ls_profile        TYPE bal_s_prof,
          l_s_fcat          TYPE bal_s_fcat,
          lt_dfies          TYPE TABLE OF dfies,
          ls_dfies          TYPE dfies.
    DATA: lr_filter_client  TYPE RANGE OF mandt,
          ls_filter_client  LIKE LINE OF lr_filter_client,
          ls_filter         TYPE bal_s_lfil,
          lr_filter         TYPE bal_s_extn,    "range table
          lr_extnumber      TYPE bal_s_extn,
          lt_log_header     TYPE balhdr_t.

    IF gv_log_handle IS INITIAL.

      CLEAR: ls_filter, lr_extnumber.

*- Search only log file of this application
      lr_filter-sign   = 'I'.
      lr_filter-option = 'EQ'.
      lr_filter-low    = c_bal_object.
      APPEND lr_filter TO ls_filter-object.

      lr_filter-sign   = 'I'.
      lr_filter-option = 'EQ'.
      lr_filter-low    = c_bal_subobj.
      APPEND lr_filter TO ls_filter-subobject.

*- Search only log file of this change number
      IF sy-repid NE 'SAP_LOCAL_DOWNPORT_ASSISTANT'.
        lr_extnumber-low    = sy-repid.
        lr_extnumber-sign   = 'I'.
        lr_extnumber-option = 'EQ'.
        APPEND lr_extnumber TO ls_filter-extnumber.
        ls_filter_client-sign   = 'I'.
        ls_filter_client-option = 'EQ'.
        ls_filter_client-low    = sy-mandt.
        APPEND ls_filter_client TO lr_filter_client.
      ENDIF.

*-- Search for log files on the database
*      CALL FUNCTION 'BAL_DB_SEARCH'
*        EXPORTING
*          i_client           = sy-mandt
*          i_s_log_filter     = ls_filter
*        IMPORTING
*          e_t_log_header     = lt_log_header
*        EXCEPTIONS
*          log_not_found      = 1
*          no_filter_criteria = 2
*          OTHERS             = 3.

      SELECT * FROM balhdr CLIENT SPECIFIED
             INTO TABLE lt_log_header
             WHERE mandant   IN lr_filter_client
               AND object    IN ls_filter-object
               AND subobject IN ls_filter-subobject
               AND extnumber IN ls_filter-extnumber.
      IF sy-subrc EQ 0.
*-- Load log files from database into memory
        CALL FUNCTION 'BAL_DB_LOAD'
          EXPORTING
            i_t_log_header     = lt_log_header
          IMPORTING
            e_t_log_handle     = lt_log_handle
          EXCEPTIONS
            no_logs_specified  = 1
            log_not_found      = 2
            log_already_loaded = 3
            OTHERS             = 4.
      ENDIF.
    ELSE.
      APPEND gv_log_handle TO lt_log_handle.
    ENDIF.

    IF 1 = 1.
      "grid
      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
        IMPORTING
          e_s_display_profile = ls_profile
        EXCEPTIONS
          OTHERS              = 0.
      "Set profile
      ls_profile-show_all   = abap_on.
      ls_profile-use_grid   = abap_on.
      ls_profile-tree_ontop = abap_on.
      ls_profile-exp_level  = 1.
      ls_profile-mess_mark  = abap_on.
    ELSE.
      "tree
      CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
        IMPORTING
          e_s_display_profile = ls_profile
        EXCEPTIONS
          OTHERS              = 0.
    ENDIF.

    "add own fields to field cat
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = c_bal_context
      TABLES
        dfies_tab      = lt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    ASSERT sy-subrc EQ 0.

    LOOP AT lt_dfies INTO ls_dfies WHERE fieldname EQ 'OBJECT'
                                      OR fieldname EQ 'OBJ_NAME'.
      l_s_fcat-ref_table = c_bal_context.
      l_s_fcat-ref_field = ls_dfies-fieldname.
      l_s_fcat-outputlen = ls_dfies-outputlen + 1.
*      l_s_fcat-col_pos   = 100 + sy-tabix.   "most rigth columns
      APPEND l_s_fcat TO ls_profile-mess_fcat.
    ENDLOOP.


    "set parameters for saving layout.
    ls_profile-disvariant-report = sy-repid.
    ls_profile-disvariant-handle = c_bal_subobj.

    ls_profile-clbk_ucbf-userexitp = sy-repid.
    ls_profile-clbk_ucbf-userexitf = 'BAL_CALLBACK_UCOMM'.
*    ls_profile-clbk_ucom-userexitp = sy-repid.
*    ls_profile-clbk_ucom-userexitf = 'BAL_CALLBACK_UCOMM'.
*    ls_profile-ext_push1-active        = 'X'.
*    ls_profile-ext_push1-def-text      = 'DDIC'.
*    ls_profile-ext_push1-def-icon_id   = icon_history.
*    ls_profile-ext_push1-def-icon_text = 'DDIC'.
*    ls_profile-ext_push1-def-quickinfo = 'DDIC Activation log'.

    IF testrun IS NOT INITIAL.
      ls_profile-title = 'UDO - Simulation log'.            "#EC *
    ELSEIF update IS NOT INITIAL.
      ls_profile-title = 'UDO - Change Log'.                "#EC *
    ELSEIF genview IS NOT INITIAL.
      ls_profile-title = 'UDO - View Generation Log'.       "#EC *
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle      = lt_log_handle
        i_s_display_profile = ls_profile
        i_amodal            = space
      EXCEPTIONS
        OTHERS              = 0.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = lt_log_handle
      EXCEPTIONS
        OTHERS         = 0.

  ENDMETHOD.                    "display_log

  METHOD set_context.

    STATICS: BEGIN OF lss_key, "save last key
             i_object    TYPE e071-object,
             i_obj_name  TYPE adir_key-obj_name,
             i_obj_name2 TYPE adir_key-obj_name,
             i_text      TYPE string,
             i_langu     TYPE sylangu,
            END OF lss_key.

    DATA: ls_msg_defaults TYPE bal_s_mdef,
          ls_context TYPE adir_key,
          l_text(70),
          l_mode TYPE string,
          ls_key LIKE lss_key.

    DATA: ls_e071      TYPE e071,
          ls_tadir     TYPE tadir,
          ls_tadir_key TYPE tadir.

    ls_key-i_object    = i_object.
    ls_key-i_obj_name  = i_obj_name.
    ls_key-i_obj_name2 = i_obj_name2.
    ls_key-i_text      = i_text.
    ls_key-i_langu     = i_langu.
    IF ls_key EQ lss_key. "check last key
      "avoid double message like "add field to table"
      RETURN.
    ELSE.
      lss_key = ls_key.
    ENDIF.

    ls_context-object   = i_object.
    CONCATENATE i_obj_name i_obj_name2 INTO ls_context-obj_name.
    ls_msg_defaults-log_handle      = gv_log_handle.
    ls_msg_defaults-context-value   = ls_context.
    ls_msg_defaults-context-tabname = c_bal_context.

    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = ls_msg_defaults
      EXCEPTIONS
        OTHERS           = 0.

    "try to check if object exists
    IF i_object IS NOT INITIAL AND i_obj_name IS NOT INITIAL.
      ls_e071-pgmid    = 'R3TR'.
      ls_e071-object   = i_object.
      ls_e071-obj_name = i_obj_name.

      CALL FUNCTION 'SCWB_GET_TADIR_REM'
        EXPORTING
          is_e071      = ls_e071
        IMPORTING
          es_tadir_key = ls_tadir_key
          es_tadir     = ls_tadir.
      IF ls_tadir_key IS INITIAL.
        ls_e071-pgmid   = 'LIMU'.
        CALL FUNCTION 'SCWB_GET_TADIR_REM'
          EXPORTING
            is_e071      = ls_e071
          IMPORTING
            es_tadir_key = ls_tadir_key
            es_tadir     = ls_tadir.
      ENDIF.

      IF ls_tadir IS INITIAL.
        l_mode = 'Create'.                                  "#EC NOTEXT
      ELSE.
        l_mode = 'Change'.                                  "#EC NOTEXT
        "check if object has been manually modified by customer.
        SELECT COUNT( * ) FROM adiraccess WHERE pgmid    EQ ls_tadir-pgmid
                                            AND object   EQ ls_tadir-object
                                            AND obj_name EQ ls_tadir-obj_name.
        IF sy-dbcnt NE 0.
          MESSAGE w666(01) WITH ls_tadir-object ls_tadir-obj_name 'was manually changed before.' 'Please adjust manually afterwards' INTO sy-lisel. "#EC *
          CALL METHOD log_message.
        ENDIF.
      ENDIF.
    ENDIF.

    IF i_text IS INITIAL.
      CALL METHOD get_object_text
        EXPORTING
          i_object   = i_object
          i_obj_name = i_obj_name
        RECEIVING
          r_text     = l_text.

      CONCATENATE l_mode l_text INTO l_text SEPARATED BY space.
    ELSE.
      l_text = i_text.
    ENDIF.

    IF l_text IS NOT INITIAL.
      IF i_langu IS NOT INITIAL.
        MESSAGE s666(01) WITH l_text '( language' i_langu ')' INTO l_text. "#EC NOTEXT
      ENDIF.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_msgty = 'I'
          i_text  = l_text
        EXCEPTIONS
          OTHERS  = 0.
    ENDIF.

  ENDMETHOD.                    "set_context




ENDCLASS.                    "lcl_wb IMPLEMENTATION


INITIALIZATION.
  "Determine proposal for tranportation layer based on existing package
  "SELECT SINGLE pdevclass FROM tdevc INTO trnlayer WHERE devclass EQ '....'.

START-OF-SELECTION.
  PERFORM action.

*&---------------------------------------------------------------------*
FORM action.

  CALL METHOD lcl_wb=>init   "#EC *
*    EXPORTING
*       i_note          = '    "<note which indicates that report is not needed anymore>
     EXCEPTIONS
        stop_processing = 1
        others          = 2.
  IF sy-subrc EQ 0 and ( update IS NOT INITIAL OR testrun IS NOT INITIAL ).

  APPEND 'SEPA_MNDID_FM'                          TO gt_tabname.   "#EC NOTEXT
  APPEND 'SEPA_MNDID_FM_T'                        TO gt_tabname.   "#EC NOTEXT
  APPEND 'SEPA_NR_CUST'                           TO gt_tabname.   "#EC NOTEXT

"- DOMA DOMD ---------------------------------------------------------------------------- Domains
"-- LIMU DOMD SEPA_FM_DESC (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_doma   "#EC *
    EXPORTING
      i_domname    = 'SEPA_FM_DESC'
      i_devclass   = 'FIN_SEPA_EN'
      i_datatype   = 'CHAR'
      i_lowercase  = 'X'
      i_langu      = 'E'
      i_ddtext     = 'SEPA MNDID Function Module Description'
      i_leng       = '000050':

      .


"-- LIMU DOMD SEPA_NRCUST_B2B (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_doma   "#EC *
    EXPORTING
      i_domname    = 'SEPA_NRCUST_B2B'
      i_devclass   = 'FIN_SEPA_EN'
      i_datatype   = 'CHAR'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Number Range Customizing: B2B Mandate, Mapping SEPA_B2B'
      i_leng       = '000001':

      i_domvalue   = 'Y'
      i_valpos     = '0001'
      i_valtext    = 'Yes',                                                                                        "#EC NOTEXT
      i_domvalue   = 'N'
      i_valpos     = '0002'
      i_valtext    = 'No'                                                                                          "#EC NOTEXT
      .


"-- LIMU DOMD SEPA_NRCUST_B2B (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_doma   "#EC *
    EXPORTING
      i_domname    = 'SEPA_NRCUST_B2B'
      i_devclass   = 'FIN_SEPA_EN'
      i_datatype   = 'CHAR'
      i_langu      = 'D'
      i_ddtext     = ''
      i_leng       = '000001':

      i_domvalue   = 'Y'
      i_valpos     = '0001'
      i_valtext    = 'Ja',                                                                                         "#EC NOTEXT
      i_domvalue   = 'N'
      i_valpos     = '0002'
      i_valtext    = 'Nein'                                                                                        "#EC NOTEXT
      .

"- DTEL DTED ---------------------------------------------------------------------- Data elements
"-- LIMU DTED SEPA_FM_DESC (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_dtel    "#EC *
    EXPORTING
      i_rollname  = 'SEPA_FM_DESC'
      i_devclass  = 'FIN_SEPA_EN'
      i_domname   = 'SEPA_FM_DESC'
      i_logflag   = ''
      i_headlen   = '04'
      i_scrlen1   = '10'
      i_scrlen2   = '15'
      i_scrlen3   = '20'

      i_langu     = 'E'
      i_ddtext    = 'SEPA MNDID Function Module Description'
      i_reptext   = 'Desc'
      i_scrtext_s = 'Desc'
      i_scrtext_m = 'Description'
      i_scrtext_l = 'FM Description'.


"-- LIMU DTED SEPA_FM_DESC (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_dtel    "#EC *
    EXPORTING
      i_rollname  = 'SEPA_FM_DESC'
      i_devclass  = 'FIN_SEPA_EN'
      i_domname   = 'SEPA_FM_DESC'
      i_logflag   = ''
      i_headlen   = '04'
      i_scrlen1   = '10'
      i_scrlen2   = '15'
      i_scrlen3   = '20'

      i_langu     = 'D'
      i_ddtext    = 'Beschreibung des Funktionsbausteins SEPA MNDID'
      i_reptext   = 'Bsch'
      i_scrtext_s = 'Beschreib.'
      i_scrtext_m = 'Beschreibung'
      i_scrtext_l = 'Fuba-Beschreibung'.


"-- LIMU DTED SEPA_NRCUST_B2B (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_dtel    "#EC *
    EXPORTING
      i_rollname  = 'SEPA_NRCUST_B2B'
      i_devclass  = 'FIN_SEPA_EN'
      i_domname   = 'SEPA_NRCUST_B2B'
      i_logflag   = ''
      i_headlen   = '03'
      i_scrlen1   = '10'
      i_scrlen2   = '15'
      i_scrlen3   = '20'

      i_langu     = 'E'
      i_ddtext    = 'SEPA Number Range Customizing: B2B Mandate, Mapping SEPA_B2B'
      i_reptext   = 'B2B'
      i_scrtext_s = 'B2B Mand.'
      i_scrtext_m = 'B2B Mandate'
      i_scrtext_l = 'B2B Mandate'.


"-- LIMU DTED SEPA_NRCUST_B2B (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_dtel    "#EC *
    EXPORTING
      i_rollname  = 'SEPA_NRCUST_B2B'
      i_devclass  = 'FIN_SEPA_EN'
      i_domname   = 'SEPA_NRCUST_B2B'
      i_logflag   = ''
      i_headlen   = '03'
      i_scrlen1   = '10'
      i_scrlen2   = '15'
      i_scrlen3   = '20'

      i_langu     = 'D'
      i_ddtext    = 'SEPA Nummernkreis-Customizing: B2B-Mandat, Mapping SEPA_B2B'
      i_reptext   = 'B2B'
      i_scrtext_s = 'B2B-Mand.'
      i_scrtext_m = 'B2B-Mandat'
      i_scrtext_l = 'B2B-Mandat'.


"-- LIMU DOCU DESEPA_NRCUST_B2B (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_dtel_docu     "#EC *
    EXPORTING
      i_rollname  = 'SEPA_NRCUST_B2B'
      i_langu     = 'E'
      i_line      =:'U1 &DEFINITION&',                                                                             "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &USE&',                                                                                    "#EC NOTEXT
                    'AS Business-to-Business Mandate',                                                             "#EC NOTEXT
                    'U1 &DEPENDENCIES&',                                                                           "#EC NOTEXT
                    'AS ID of mandate as business-to business mandate. For B2B mandates specific',                 "#EC NOTEXT
                    '   rules apply regarding the deadlines and return option of payments.',                       "#EC NOTEXT
                    'AS Payments with a B2B mandate are processed by the bank 1 day after',                        "#EC NOTEXT
                    '   presentation.',                                                                            "#EC NOTEXT
                    'AS For payments with core mandates of type <LS>Recurring Mandate</> 2 days',                  "#EC NOTEXT
                    '   apply. For payments with core mandates of type <LS>One-Time Mandate</>',                   "#EC NOTEXT
                    '   and for the first payment with a core mandate of type <LS>Recurring',                      "#EC NOTEXT
                    '   Mandate</> 5 days apply.',                                                                 "#EC NOTEXT
                    'AS When the system tries to match a number range interval with a mandate,',                   "#EC NOTEXT
                    '   since the value for B2B, either X or empty, is a Boolean, and an empty',                   "#EC NOTEXT
                    '   Boolean value matches any Boolean value, X is transformed to Y and empty',                 "#EC NOTEXT
                    '   is transformed to N.',                                                                     "#EC NOTEXT
                    'U1 &EXAMPLE&',                                                                                "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU DESEPA_NRCUST_B2B (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_dtel_docu     "#EC *
    EXPORTING
      i_rollname  = 'SEPA_NRCUST_B2B'
      i_langu     = 'D'
      i_line      =:'U1 &DEFINITION&',                                                                             "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &Use&',                                                                                    "#EC NOTEXT
                    'AS Business-to-Business-Mandat',                                                              "#EC NOTEXT
                    'U1 &DEPENDENCIES&',                                                                           "#EC NOTEXT
                    'AS Kennzeichnung des Mandats als Business-to-Business-Mandat. Für',                           "#EC NOTEXT
                    '   B2B-Mandate gelten spezifische Regelungen bzgl. der Fristen und der',                      "#EC NOTEXT
                    '   Rückgabemöglichkeiten von Zahlungen.',                                                     "#EC NOTEXT
                    'AS Zahlungen mit B2B-Mandat werden von der Bank bereits 1 Tag nach',                          "#EC NOTEXT
                    '   Einreichung verarbeitet.',                                                                 "#EC NOTEXT
                    'AS Für Zahlungen mit Core-Mandaten vom Typ "wiederkehrendes Mandat" gelten',                  "#EC NOTEXT
                    '   2 Tage, für Zahlungen mit Core-Mandaten vom Typ "Einmal-Mandat" und für',                  "#EC NOTEXT
                    '   die erste Zahlung mit einem Core-Mandat vom Typ "wiederkehrendes Mandat"',                 "#EC NOTEXT
                    '   gelten 5 Tage.',                                                                           "#EC NOTEXT
                    'AS Sie können angeben, ob das System den Nummernkreis in Abhängigkeit des',                   "#EC NOTEXT
                    '   B2B-Mandats ermitteln soll oder nicht. Wenn Sie keine Präferenz haben,',                   "#EC NOTEXT
                    '   lassen Sie das Feld leer.',                                                                "#EC NOTEXT
                    'U1 &EXAMPLE&',                                                                                "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"- SHLP SHLD ----------------------------------------------------------------------- Search helps
"-- LIMU SHLD SH_SEPA_MNDID_FM (language: EN , source: EBJ )
  DATA: ls_dd31v TYPE dd31v, lt_dd31v TYPE TABLE OF dd31v.
  DATA: ls_dd32p TYPE dd32p, lt_dd32p TYPE TABLE OF dd32p.
  DATA: ls_dd33v TYPE dd33v, lt_dd33v TYPE TABLE OF dd33v.
  DATA: ls_dd36m TYPE dd36m, lt_dd36m TYPE TABLE OF dd36m.

  CLEAR: lt_dd31v, lt_dd32p, lt_dd33v.
  CLEAR ls_dd32p.
  ls_dd32p-shlpname        = 'SH_SEPA_MNDID_FM'.
  ls_dd32p-fieldname       = 'FM_NAME'.
  ls_dd32p-flposition      = '0001'.
  ls_dd32p-rollname        = 'SEPA_FNAME_MNDID'.
  ls_dd32p-shlpoutput      = 'X'.
  ls_dd32p-shlpselpos      = '02'.
  ls_dd32p-shlplispos      = '01'.
  APPEND ls_dd32p TO lt_dd32p.

  CLEAR ls_dd32p.
  ls_dd32p-shlpname        = 'SH_SEPA_MNDID_FM'.
  ls_dd32p-fieldname       = 'FM_DESC'.
  ls_dd32p-flposition      = '0002'.
  ls_dd32p-rollname        = 'SEPA_FM_DESC'.
  ls_dd32p-shlpinput       = 'X'.
  ls_dd32p-shlpselpos      = '01'.
  ls_dd32p-shlplispos      = '02'.
  ls_dd32p-rollchange      = 'X'.
  APPEND ls_dd32p TO lt_dd32p.

  CALL METHOD lcl_wb=>create_search_help     "#EC *
    EXPORTING
      i_shlpname   = 'SH_SEPA_MNDID_FM'
      i_langu      = 'E'
      i_ddtext     = 'Search Help For SEPA Mandate Reference ID Function Modules'
      i_devclass   = 'FIN_SEPA_EN'
      i_issimple   = 'X'
      i_elemexi    = ''
      i_nofields   = ''
      i_attachexi  = ''
      i_selmethod  = 'SEPA_MNDID_FM'
      i_selmtype   = 'X'
      i_texttab    = 'SEPA_MNDID_FM_T'
      i_hotkey     = ''
      i_dialogtype = 'D'
      it_dd31v     = lt_dd31v
      it_dd32p     = lt_dd32p
      it_dd33v     = lt_dd33v.


"-- LIMU SHLD SH_SEPA_MNDID_FM (language: DE , source: V7T )
  CLEAR: lt_dd31v, lt_dd32p, lt_dd33v.
  CLEAR ls_dd32p.
  ls_dd32p-shlpname        = 'SH_SEPA_MNDID_FM'.
  ls_dd32p-fieldname       = 'FM_NAME'.
  ls_dd32p-flposition      = '0001'.
  ls_dd32p-rollname        = 'SEPA_FNAME_MNDID'.
  ls_dd32p-shlpoutput      = 'X'.
  ls_dd32p-shlpselpos      = '02'.
  ls_dd32p-shlplispos      = '01'.
  APPEND ls_dd32p TO lt_dd32p.

  CLEAR ls_dd32p.
  ls_dd32p-shlpname        = 'SH_SEPA_MNDID_FM'.
  ls_dd32p-fieldname       = 'FM_DESC'.
  ls_dd32p-flposition      = '0002'.
  ls_dd32p-rollname        = 'SEPA_FM_DESC'.
  ls_dd32p-shlpinput       = 'X'.
  ls_dd32p-shlpselpos      = '01'.
  ls_dd32p-shlplispos      = '02'.
  ls_dd32p-rollchange      = 'X'.
  APPEND ls_dd32p TO lt_dd32p.

  CALL METHOD lcl_wb=>create_search_help     "#EC *
    EXPORTING
      i_shlpname   = 'SH_SEPA_MNDID_FM'
      i_langu      = 'D'
      i_ddtext     = 'Suchhife für Funktionsbausteine für SEPA-Mandats-Referenz-ID'
      i_devclass   = 'FIN_SEPA_EN'
      i_issimple   = 'X'
      i_elemexi    = ''
      i_nofields   = ''
      i_attachexi  = ''
      i_selmethod  = 'SEPA_MNDID_FM'
      i_selmtype   = 'X'
      i_texttab    = 'SEPA_MNDID_FM_T'
      i_hotkey     = ''
      i_dialogtype = 'D'
      it_dd31v     = lt_dd31v
      it_dd32p     = lt_dd32p
      it_dd33v     = lt_dd33v.


"-- LIMU SHLD SH_SEPA_MNDID_FM_WOT (language: EN , source: EBJ )
  CLEAR: lt_dd31v, lt_dd32p, lt_dd33v.
  CLEAR ls_dd32p.
  ls_dd32p-shlpname        = 'SH_SEPA_MNDID_FM_WOT'.
  ls_dd32p-fieldname       = 'FM_NAME'.
  ls_dd32p-flposition      = '0001'.
  ls_dd32p-rollname        = 'SEPA_FNAME_MNDID'.
  ls_dd32p-shlpoutput      = 'X'.
  ls_dd32p-shlpselpos      = '02'.
  ls_dd32p-shlplispos      = '01'.
  APPEND ls_dd32p TO lt_dd32p.

  CALL METHOD lcl_wb=>create_search_help     "#EC *
    EXPORTING
      i_shlpname   = 'SH_SEPA_MNDID_FM_WOT'
      i_langu      = 'E'
      i_ddtext     = 'SH For SEPA Mandate Reference ID Function Modules W/O Desc'
      i_devclass   = 'FIN_SEPA_EN'
      i_issimple   = 'X'
      i_elemexi    = ''
      i_nofields   = ''
      i_attachexi  = ''
      i_selmethod  = 'SEPA_MNDID_FM'
      i_selmtype   = 'T'
      i_texttab    = 'SEPA_MNDID_FM_T'
      i_hotkey     = ''
      i_dialogtype = 'D'
      it_dd31v     = lt_dd31v
      it_dd32p     = lt_dd32p
      it_dd33v     = lt_dd33v.


"-- LIMU SHLD SH_SEPA_MNDID_FM_WOT (language: DE , source: V7T )
  CLEAR: lt_dd31v, lt_dd32p, lt_dd33v.
  CLEAR ls_dd32p.
  ls_dd32p-shlpname        = 'SH_SEPA_MNDID_FM_WOT'.
  ls_dd32p-fieldname       = 'FM_NAME'.
  ls_dd32p-flposition      = '0001'.
  ls_dd32p-rollname        = 'SEPA_FNAME_MNDID'.
  ls_dd32p-shlpoutput      = 'X'.
  ls_dd32p-shlpselpos      = '02'.
  ls_dd32p-shlplispos      = '01'.
  APPEND ls_dd32p TO lt_dd32p.

  CALL METHOD lcl_wb=>create_search_help     "#EC *
    EXPORTING
      i_shlpname   = 'SH_SEPA_MNDID_FM_WOT'
      i_langu      = 'D'
      i_ddtext     = 'Suchhife für Fuba für SEPA-Mandats-Ref.-ID ohne zahl. Bukrs'
      i_devclass   = 'FIN_SEPA_EN'
      i_issimple   = 'X'
      i_elemexi    = ''
      i_nofields   = ''
      i_attachexi  = ''
      i_selmethod  = 'SEPA_MNDID_FM'
      i_selmtype   = 'T'
      i_texttab    = 'SEPA_MNDID_FM_T'
      i_hotkey     = ''
      i_dialogtype = 'D'
      it_dd31v     = lt_dd31v
      it_dd32p     = lt_dd32p
      it_dd33v     = lt_dd33v.

"- TABL TABD ----------------------------------------------------------------------------- Tables
"-- LIMU TABD SEPA_MNDID_FM (language: EN , source: EBJ )
  DATA: lt_dd36  TYPE TABLE OF dd36m, ls_dd36 TYPE dd36m,
        lt_dd05m TYPE TABLE OF dd05m, ls_dd05m TYPE dd05m,
        ls_dd08v TYPE dd08v.

  CALL METHOD lcl_wb=>create_table     "#EC *
    EXPORTING
      i_tabname    = 'SEPA_MNDID_FM'
      i_devclass   = 'FIN_SEPA_EN'
      i_exclass    = '1'
      i_authclass  = '00'
      i_tabclass   = 'TRANSP'
      i_contflag   = 'E'
      i_tabkat     = '0'
      i_tabart     = 'APPL2'
      i_bufallow   = 'X'
      i_pufferung  = 'X'
      i_schfeldanz = '000'
      i_speichpuff = ''
      i_protokoll  = 'X'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Generate Function Modules'.

  CLEAR ls_dd08v.
  ls_dd08v-tabname         = 'SEPA_MNDID_FM'.
  ls_dd08v-fieldname       = 'FM_NAME'.
  ls_dd08v-tabname         = 'SEPA_MNDID_FM'.
  ls_dd08v-fieldname       = 'FM_NAME'.
  ls_dd08v-checktable      = 'TFDIR'.

  REFRESH lt_dd05m.
  CLEAR ls_dd05m.
  ls_dd05m-tabname         = 'SEPA_MNDID_FM'.
  ls_dd05m-fieldname       = 'FM_NAME'.
  ls_dd05m-fortable        = 'SEPA_MNDID_FM'.
  ls_dd05m-forkey          = 'FM_NAME'.
  ls_dd05m-checktable      = 'TFDIR'.
  ls_dd05m-checkfield      = 'FUNCNAME'.
  ls_dd05m-primpos         = '0001'.
  APPEND ls_dd05m TO lt_dd05m.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_MNDID_FM'
      i_fieldname = 'FM_NAME'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_FNAME_MNDID'
      is_dd08v    = ls_dd08v
      it_dd05m    = lt_dd05m
      i_context   = ''.


"-- LIMU TABD SEPA_MNDID_FM_T (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_table     "#EC *
    EXPORTING
      i_tabname    = 'SEPA_MNDID_FM_T'
      i_devclass   = 'FIN_SEPA_EN'
      i_exclass    = '1'
      i_authclass  = '00'
      i_tabclass   = 'TRANSP'
      i_contflag   = 'E'
      i_tabkat     = '0'
      i_tabart     = 'APPL2'
      i_bufallow   = 'X'
      i_pufferung  = 'X'
      i_schfeldanz = '000'
      i_speichpuff = ''
      i_protokoll  = 'X'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Generate Function Modules Text Table'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_MNDID_FM_T'
      i_fieldname = 'SPRAS'
      i_keyflag   = 'X'
      i_languflag = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SPRAS'
      i_context   = ''.

  CLEAR ls_dd08v.
  ls_dd08v-tabname         = 'SEPA_MNDID_FM_T'.
  ls_dd08v-fieldname       = 'FM_NAME'.
  ls_dd08v-tabname         = 'SEPA_MNDID_FM_T'.
  ls_dd08v-fieldname       = 'FM_NAME'.
  ls_dd08v-checktable      = 'SEPA_MNDID_FM'.
  ls_dd08v-frkart          = 'TEXT'.
  ls_dd08v-card            = 'N'.
  ls_dd08v-cardleft        = '1'.

  REFRESH lt_dd05m.
  CLEAR ls_dd05m.
  ls_dd05m-tabname         = 'SEPA_MNDID_FM_T'.
  ls_dd05m-fieldname       = 'FM_NAME'.
  ls_dd05m-fortable        = 'SEPA_MNDID_FM_T'.
  ls_dd05m-forkey          = 'FM_NAME'.
  ls_dd05m-checktable      = 'SEPA_MNDID_FM'.
  ls_dd05m-checkfield      = 'FM_NAME'.
  ls_dd05m-primpos         = '0001'.
  APPEND ls_dd05m TO lt_dd05m.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_MNDID_FM_T'
      i_fieldname = 'FM_NAME'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_FNAME_MNDID'
      is_dd08v    = ls_dd08v
      it_dd05m    = lt_dd05m
      i_context   = 'SPRAS'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_MNDID_FM_T'
      i_fieldname = 'FM_DESC'
      i_rollname  = 'SEPA_FM_DESC'
      i_context   = 'FM_NAME'.


"-- LIMU TABD SEPA_NR_CUST (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_table     "#EC *
    EXPORTING
      i_tabname    = 'SEPA_NR_CUST'
      i_devclass   = 'FIN_SEPA_EN'
      i_exclass    = '1'
      i_authclass  = '00'
      i_tabclass   = 'TRANSP'
      i_contflag   = 'C'
      i_tabkat     = '0'
      i_tabart     = 'APPL2'
      i_bufallow   = 'X'
      i_pufferung  = 'X'
      i_schfeldanz = '000'
      i_speichpuff = ''
      i_protokoll  = 'X'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Number Range Customizing'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'MANDAT'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'MANDT'
      i_context   = ''.

  CLEAR ls_dd08v.
  ls_dd08v-tabname         = 'SEPA_NR_CUST'.
  ls_dd08v-fieldname       = 'KTOKD'.
  ls_dd08v-tabname         = 'SEPA_NR_CUST'.
  ls_dd08v-fieldname       = 'KTOKD'.
  ls_dd08v-checktable      = 'T077D'.

  REFRESH lt_dd05m.
  CLEAR ls_dd05m.
  ls_dd05m-tabname         = 'SEPA_NR_CUST'.
  ls_dd05m-fieldname       = 'KTOKD'.
  ls_dd05m-fortable        = 'SEPA_NR_CUST'.
  ls_dd05m-forkey          = 'MANDAT'.
  ls_dd05m-checktable      = 'T077D'.
  ls_dd05m-checkfield      = 'MANDT'.
  ls_dd05m-primpos         = '0001'.
  APPEND ls_dd05m TO lt_dd05m.

  CLEAR ls_dd05m.
  ls_dd05m-tabname         = 'SEPA_NR_CUST'.
  ls_dd05m-fieldname       = 'KTOKD'.
  ls_dd05m-fortable        = 'SEPA_NR_CUST'.
  ls_dd05m-forkey          = 'KTOKD'.
  ls_dd05m-checktable      = 'T077D'.
  ls_dd05m-checkfield      = 'KTOKD'.
  ls_dd05m-primpos         = '0002'.
  APPEND ls_dd05m TO lt_dd05m.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'KTOKD'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'KTOKD'
      is_dd08v    = ls_dd08v
      it_dd05m    = lt_dd05m
      i_context   = 'MANDAT'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'B2B'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_NRCUST_B2B'
      i_context   = 'KTOKD'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'PAY_TYPE'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_PAY_TYPE'
      i_context   = 'B2B'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'ORGF1'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_ORG_ID'
      i_context   = 'PAY_TYPE'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'ORGF2'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_ORG_ID'
      i_context   = 'ORGF1'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'ORGF3'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_ORG_ID_LONG'
      i_context   = 'ORGF2'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'ORGF4'
      i_keyflag   = 'X'
      i_notnull   = 'X'
      i_rollname  = 'SEPA_ORG_ID_LONG'
      i_context   = 'ORGF3'.

  CALL METHOD lcl_wb=>add_field_to_table    "#EC *
    EXPORTING
      i_tabname   = 'SEPA_NR_CUST'
      i_fieldname = 'NRRANGENR'
      i_rollname  = 'NRNR'
      i_context   = 'ORGF4'.

"- VIEW VIED ------------------------------------------------------------------------------ Views
"-- LIMU VIED V_SEPA_MND_CUST (language: EN , source: EBJ )
  DATA: ls_dd26v TYPE dd26v, lt_dd26v TYPE TABLE OF dd26v.
  DATA: ls_dd27p TYPE dd28j, lt_dd27p TYPE TABLE OF dd28j.
  DATA: ls_dd28v TYPE dd28v, lt_dd28v TYPE TABLE OF dd28v.

  CLEAR: lt_dd26v, lt_dd27p, lt_dd28v.
  CLEAR ls_dd26v.
  ls_dd26v-viewname        = 'V_SEPA_MND_CUST'.
  ls_dd26v-tabname         = 'SEPA_CUST'.
  ls_dd26v-tabpos          = '0001'.
  ls_dd26v-fortabname      = 'SEPA_CUST'.
  APPEND ls_dd26v TO lt_dd26v.

  CLEAR ls_dd28v.
  ls_dd28v-condname        = 'V_SEPA_MND_CUST'.
  ls_dd28v-position        = '0001'.
  ls_dd28v-tabname         = 'SEPA_CUST'.
  ls_dd28v-fieldname       = 'ANWND'.
  ls_dd28v-operator        = 'EQ'.
  ls_dd28v-constants       = '''F'''.
  APPEND ls_dd28v TO lt_dd28v.

  CALL METHOD lcl_wb=>create_view     "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_CUST'
      i_devclass   = 'FIN_SEPA_EN'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Function Module Selection'
      i_aggtype    = 'V'
      i_roottab    = 'SEPA_CUST'
      i_viewclass  = 'C'
      i_viewgrant  = ''
      i_globalflag = 'X'
      it_dd26v     = lt_dd26v[]
      it_dd28v     = lt_dd28v[].

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_CUST'
      i_viewfield  = 'MANDT'
      i_tabname    = 'SEPA_CUST'
      i_fieldname  = 'MANDT'
      i_keyflag    = 'X'
      i_rollname   = 'MANDT'
      i_context    = ''.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_CUST'
      i_viewfield  = 'ANWND'
      i_tabname    = 'SEPA_CUST'
      i_fieldname  = 'ANWND'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_ANWND'
      i_context    = 'MANDT'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_CUST'
      i_viewfield  = 'FNAME_MNDID'
      i_tabname    = 'SEPA_CUST'
      i_fieldname  = 'FNAME_MNDID'
      i_rollname   = 'SEPA_FNAME_MNDID'
      i_context    = 'ANWND'.


"-- LIMU VIED V_SEPA_MND_FM (language: EN , source: EBJ )
  CLEAR: lt_dd26v, lt_dd27p, lt_dd28v.
  CLEAR ls_dd26v.
  ls_dd26v-viewname        = 'V_SEPA_MND_FM'.
  ls_dd26v-tabname         = 'SEPA_MNDID_FM'.
  ls_dd26v-tabpos          = '0001'.
  ls_dd26v-fortabname      = 'SEPA_MNDID_FM'.
  APPEND ls_dd26v TO lt_dd26v.

  CALL METHOD lcl_wb=>create_view     "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_FM'
      i_devclass   = 'FIN_SEPA_EN'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Generate Function Modules Desc'
      i_contflag   = 'E'
      i_aggtype    = 'V'
      i_roottab    = 'SEPA_MNDID_FM'
      i_viewclass  = 'C'
      i_viewgrant  = ''
      i_globalflag = 'X'
      it_dd26v     = lt_dd26v[]
      it_dd28v     = lt_dd28v[].

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_FM'
      i_viewfield  = 'FM_NAME'
      i_tabname    = 'SEPA_MNDID_FM'
      i_fieldname  = 'FM_NAME'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_FNAME_MNDID'
      i_context    = ''.


"-- LIMU VIED V_SEPA_MND_FM_T (language: EN , source: EBJ )
  CLEAR: lt_dd26v, lt_dd27p, lt_dd28v.
  CLEAR ls_dd26v.
  ls_dd26v-viewname        = 'V_SEPA_MND_FM_T'.
  ls_dd26v-tabname         = 'SEPA_MNDID_FM_T'.
  ls_dd26v-tabpos          = '0001'.
  ls_dd26v-fortabname      = 'SEPA_MNDID_FM_T'.
  APPEND ls_dd26v TO lt_dd26v.

  CALL METHOD lcl_wb=>create_view     "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_FM_T'
      i_devclass   = 'FIN_SEPA_EN'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Generate Function Modules'
      i_contflag   = 'E'
      i_aggtype    = 'V'
      i_roottab    = 'SEPA_MNDID_FM_T'
      i_viewclass  = 'C'
      i_viewgrant  = ''
      i_globalflag = 'X'
      it_dd26v     = lt_dd26v[]
      it_dd28v     = lt_dd28v[].

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_FM_T'
      i_viewfield  = 'SPRAS'
      i_tabname    = 'SEPA_MNDID_FM_T'
      i_fieldname  = 'SPRAS'
      i_keyflag    = 'X'
      i_rollname   = 'SPRAS'
      i_context    = ''.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_FM_T'
      i_viewfield  = 'FM_NAME'
      i_tabname    = 'SEPA_MNDID_FM_T'
      i_fieldname  = 'FM_NAME'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_FNAME_MNDID'
      i_context    = 'SPRAS'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_MND_FM_T'
      i_viewfield  = 'FM_DESC'
      i_tabname    = 'SEPA_MNDID_FM_T'
      i_fieldname  = 'FM_DESC'
      i_rollname   = 'SEPA_FM_DESC'
      i_context    = 'FM_NAME'.


"-- LIMU VIED V_SEPA_NR_CUST (language: EN , source: EBJ )
  CLEAR: lt_dd26v, lt_dd27p, lt_dd28v.
  CLEAR ls_dd26v.
  ls_dd26v-viewname        = 'V_SEPA_NR_CUST'.
  ls_dd26v-tabname         = 'SEPA_NR_CUST'.
  ls_dd26v-tabpos          = '0001'.
  ls_dd26v-fortabname      = 'SEPA_NR_CUST'.
  APPEND ls_dd26v TO lt_dd26v.

  CALL METHOD lcl_wb=>create_view     "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_devclass   = 'FIN_SEPA_EN'
      i_langu      = 'E'
      i_ddtext     = 'SEPA Mandate ID: Number Range Customizing'
      i_aggtype    = 'V'
      i_roottab    = 'SEPA_NR_CUST'
      i_viewclass  = 'C'
      i_viewgrant  = ''
      i_globalflag = 'X'
      it_dd26v     = lt_dd26v[]
      it_dd28v     = lt_dd28v[].

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'MANDAT'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'MANDAT'
      i_keyflag    = 'X'
      i_rollname   = 'MANDT'
      i_context    = ''.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'KTOKD'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'KTOKD'
      i_keyflag    = 'X'
      i_rollname   = 'KTOKD'
      i_context    = 'MANDAT'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'B2B'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'B2B'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_NRCUST_B2B'
      i_context    = 'KTOKD'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'PAY_TYPE'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'PAY_TYPE'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_PAY_TYPE'
      i_context    = 'B2B'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'ORGF1'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'ORGF1'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_ORG_ID'
      i_rdonly     = 'H'
      i_context    = 'PAY_TYPE'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'ORGF2'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'ORGF2'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_ORG_ID'
      i_rdonly     = 'H'
      i_context    = 'ORGF1'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'ORGF3'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'ORGF3'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_ORG_ID_LONG'
      i_rdonly     = 'H'
      i_context    = 'ORGF2'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'ORGF4'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'ORGF4'
      i_keyflag    = 'X'
      i_rollname   = 'SEPA_ORG_ID_LONG'
      i_rdonly     = 'H'
      i_context    = 'ORGF3'.

  CALL METHOD lcl_wb=>add_field_to_view    "#EC *
    EXPORTING
      i_viewname   = 'V_SEPA_NR_CUST'
      i_viewfield  = 'NRRANGENR'
      i_tabname    = 'SEPA_NR_CUST'
      i_fieldname  = 'NRRANGENR'
      i_rollname   = 'NRNR'
      i_context    = 'ORGF4'.

"- MSAG ------------------------------------------------------------------------- Message Classes
"-- R3TR MSAG FIN_SEPA_EN (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_message_class     "#EC *
    EXPORTING
      i_devclass  = 'FIN_SEPA_EN'
      i_msgid     = 'FIN_SEPA_EN'
      i_text      = 'FIN_SEPA_EN'
      i_langu     = 'E'.


"-- LIMU MESS FIN_SEPA_EN001 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '001':
      i_langu     = 'E'
      i_selfdef   = ''
      i_text      = 'Maintain number range interval &1 for paying company code &2'.


"-- LIMU MESS FIN_SEPA_EN001 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '001':
      i_langu     = 'D'
      i_selfdef   = ''
      i_text      = 'Nummernkreisintervall &1 für zahlenden Buchungskreis &2 pflegen'.


"-- LIMU MESS FIN_SEPA_EN002 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '002':
      i_langu     = 'E'
      i_selfdef   = ''
      i_text      = 'No entry in Customizing table for number ranges matches current mandate'.


"-- LIMU MESS FIN_SEPA_EN002 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '002':
      i_langu     = 'D'
      i_selfdef   = ''
      i_text      = 'Kein zu Mandat passender Eintrag in Customizing-Tabelle für Nummernkreise'.


"-- LIMU MESS FIN_SEPA_EN003 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '003':
      i_langu     = 'E'
      i_selfdef   = ''
      i_text      = 'Maintain number range interval &1 for paying company code &2'.


"-- LIMU MESS FIN_SEPA_EN003 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '003':
      i_langu     = 'D'
      i_selfdef   = ''
      i_text      = 'Nummernkreisintervall &1 für zahlenden Buchungskreis &2 pflegen'.


"-- LIMU MESS FIN_SEPA_EN004 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '004':
      i_langu     = 'E'
      i_selfdef   = '3'
      i_text      = 'Number range interval &1 cannot be external for paying company code &2'.


"-- LIMU MESS FIN_SEPA_EN004 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '004':
      i_langu     = 'D'
      i_selfdef   = '3'
      i_text      = 'Nummernkreisintervall &1 kann für zahlenden Bukrs &2 nicht extern sein'.


"-- LIMU MESS FIN_SEPA_EN005 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '005':
      i_langu     = 'E'
      i_selfdef   = '3'
      i_text      = 'Mandate ID not generated; aborted after 20 failed attempts'.


"-- LIMU MESS FIN_SEPA_EN005 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_message     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '005':
      i_langu     = 'D'
      i_selfdef   = '3'
      i_text      = 'Mandats-ID nicht generiert;nach 20 fehlgeschlagenen Versuchen abgebrochen'.

"-- LIMU DOCU NAFIN_SEPA_EN001 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_mess_docu     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '001'
      i_langu     = 'E'
      i_line      =:'U1 &CAUSE&',                                                                                  "#EC NOTEXT
                    'AS You choose to generate a mandate ID using the number range interval &v1&',                 "#EC NOTEXT
                    '=  . However, you have not yet maintained the interval for the current',                      "#EC NOTEXT
                    '   paying company code &v2&.',                                                                "#EC NOTEXT
                    'U1 &SYSTEM_RESPONSE&',                                                                        "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &WHAT_TO_DO&',                                                                             "#EC NOTEXT
                    'AS Use transaction',                                                                          "#EC NOTEXT
                    '=   <DS:TRAS.SEPA_NR_MT>SEPA_NR_MT</> to maintain the number range interval',                 "#EC NOTEXT
                    '   in the number range object SEPA_MNDID for the current paying company',                     "#EC NOTEXT
                    '   code.',                                                                                    "#EC NOTEXT
                    'U1 &SYS_ADMIN&',                                                                              "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU NAFIN_SEPA_EN001 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_mess_docu     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '001'
      i_langu     = 'D'
      i_line      =:'U1 &CAUSE&',                                                                                  "#EC NOTEXT
                    'AS Sie möchten eine Mandats-ID mithilfe des Nummernkreisintervalls &v1&',                     "#EC NOTEXT
                    '   generieren. Sie haben für den vorliegenden zahlenden Buchungskreis &v2&',                  "#EC NOTEXT
                    '   jedoch noch kein Intervall gepflegt.',                                                     "#EC NOTEXT
                    'U1 &SYSTEM_RESPONSE&',                                                                        "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &WHAT_TO_DO&',                                                                             "#EC NOTEXT
                    'AS Pflegen Sie in der Transaktion <DS:TRAS.SEPA_NR_MT>SEPA_NR_MT</> das',                     "#EC NOTEXT
                    '   Nummernkreisintervall im Nummernkreisobjekt SEPA_MNDID für den',                           "#EC NOTEXT
                    '   vorliegenden Buchungskreis.',                                                              "#EC NOTEXT
                    'U1 &SYS_ADMIN&',                                                                              "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU NAFIN_SEPA_EN002 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_mess_docu     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '002'
      i_langu     = 'E'
      i_line      =:'U1 &CAUSE&',                                                                                  "#EC NOTEXT
                    'AS You choose to generate a mandate ID using the number range. However, no',                  "#EC NOTEXT
                    '   entry in the Customizing table for number ranges matches the current',                     "#EC NOTEXT
                    '   mandate.',                                                                                 "#EC NOTEXT
                    'U1 &SYSTEM_RESPONSE&',                                                                        "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &WHAT_TO_DO&',                                                                             "#EC NOTEXT
                    '*  Use transaction',                                                                          "#EC NOTEXT
                    '=   <DS:TRAS.SEPA_NR_CUST>SEPA_NR_CUST</> to maintain your Customizing',                      "#EC NOTEXT
                    '   settings so that at least one entry matches the current mandate.',                         "#EC NOTEXT
                    'U1 &SYS_ADMIN&',                                                                              "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU NAFIN_SEPA_EN002 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_mess_docu     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '002'
      i_langu     = 'D'
      i_line      =:'U1 &CAUSE&',                                                                                  "#EC NOTEXT
                    'AS Sie möchten eine Mandats-ID mithilfe des Nummernkreises generieren. Für',                  "#EC NOTEXT
                    '   den Nummernkreis ist jedoch kein zum vorliegenden Mandat passender',                       "#EC NOTEXT
                    '   Eintrag in der Customizing-Tabelle vorhanden.',                                            "#EC NOTEXT
                    'U1 &SYSTEM_RESPONSE&',                                                                        "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &WHAT_TO_DO&',                                                                             "#EC NOTEXT
                    '*  Pflegen Sie in der Transaktion <DS:TRAS.SEPA_NR_CUST>SEPA_NR_CUST</>',                     "#EC NOTEXT
                    '   Ihre Customizing-Einstellungen, sodass mindestens ein Eintrag zum',                        "#EC NOTEXT
                    '   vorliegenden Mandat passt.',                                                               "#EC NOTEXT
                    'U1 &SYS_ADMIN&',                                                                              "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU NAFIN_SEPA_EN003 (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_mess_docu     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '003'
      i_langu     = 'E'
      i_line      =:'U1 &CAUSE&',                                                                                  "#EC NOTEXT
                    'AS You choose to generate a mandate ID using the number range interval &v1&',                 "#EC NOTEXT
                    '=  . However, you have not yet maintained the interval for the current',                      "#EC NOTEXT
                    '   paying company code &v2&.',                                                                "#EC NOTEXT
                    'U1 &SYSTEM_RESPONSE&',                                                                        "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &WHAT_TO_DO&',                                                                             "#EC NOTEXT
                    '*  Use transaction',                                                                          "#EC NOTEXT
                    '=   <DS:TRAS.SEPA_NR_MT>SEPA_NR_MT</> to maintain the number range interval',                 "#EC NOTEXT
                    '   &v1& in number range object SEPA_MNDID for the current paying company',                    "#EC NOTEXT
                    '   code.',                                                                                    "#EC NOTEXT
                    'U1 &SYS_ADMIN&',                                                                              "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU NAFIN_SEPA_EN003 (language: DE , source: V7T )
  CALL METHOD lcl_wb=>create_mess_docu     "#EC *
    EXPORTING
      i_msgid     = 'FIN_SEPA_EN'
      i_msgno     = '003'
      i_langu     = 'D'
      i_line      =:'U1 &CAUSE&',                                                                                  "#EC NOTEXT
                    'AS Sie möchten eine Mandats-ID mithilfe des Nummernkreisintervalls &v1&',                     "#EC NOTEXT
                    '   generieren. Sie haben für den vorliegenden zahlenden Buchungskreis &v2&',                  "#EC NOTEXT
                    '   jedoch noch kein Intervall gepflegt.',                                                     "#EC NOTEXT
                    'U1 &SYSTEM_RESPONSE&',                                                                        "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U1 &WHAT_TO_DO&',                                                                             "#EC NOTEXT
                    '*  Pflegen Sie in der Transaktion <DS:TRAS.SEPA_NR_MT>SEPA_NR_MT</> das',                     "#EC NOTEXT
                    '   Nummernkreisintervall im Nummernkreisobjekt SEPA_MNDID für den',                           "#EC NOTEXT
                    '   vorliegenden Buchungskreis.',                                                              "#EC NOTEXT
                    'U1 &SYS_ADMIN&',                                                                              "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.

"- FUGR -------------------------------------------------------------------------- Function Group
"-- R3TR FUGR SEPA_MNDID_CUST (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_function_group   "#EC *
    EXPORTING
      i_devclass   = 'FIN_SEPA_EN'
      i_area       = 'SEPA_MNDID_CUST'
      i_langu      = 'E'
      i_appl       = 'S'
      i_text       = 'SEPA Mandate Reference ID Customizing'.


"-- R3TR FUGR SEPA_MNDID_GEN (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_function_group   "#EC *
    EXPORTING
      i_devclass   = 'FIN_SEPA_EN'
      i_area       = 'SEPA_MNDID_GEN'
      i_langu      = 'E'
      i_appl       = 'S'
      i_text       = 'SEPA Mandate Reference ID Generation FMs'.


"-- LIMU DOCU FUFI_APAR_MANDATE_GET_CUST_NR (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_other_docu     "#EC *
    EXPORTING
      i_devclass  = 'FIN_SEPA_EN'
      i_docname   = 'FUFI_APAR_MANDATE_GET_CUST_NR'
      i_typ       = 'T'
      i_langu     = 'E'
      i_line      =:'U4 &FUNCTIONALITY&',                                                                          "#EC NOTEXT
                    'AS This function module determines which number range interval to use on',                    "#EC NOTEXT
                    '   the basis of the mandate that you want to create.',                                        "#EC NOTEXT
                    'U4 &EXAMPLE&',                                                                                "#EC NOTEXT
                    '*  You have created the following two entries in the Customizing table',                      "#EC NOTEXT
                    '   using transaction',                                                                        "#EC NOTEXT
                    '=   <DS:TRAN.SEPA_NR_CUST>SEPA_NR_CUST</>:',                                                  "#EC NOTEXT
                    'K5 Account Group,,B2B,,Transaction Type   Number Range Interval',                             "#EC NOTEXT
                    'T5 ,,,,,,,,,,  N8',                                                                           "#EC NOTEXT
                    'T5 DEBI,,          Y,,1,,,,  N7',                                                             "#EC NOTEXT
                    '*',                                                                                           "#EC NOTEXT
                    'AS The mandate you want to create has the following field values:',                           "#EC NOTEXT
                    'K1 Field,,Value',                                                                             "#EC NOTEXT
                    'T1 Account Group,,DEBI',                                                                      "#EC NOTEXT
                    'T1 B2B,,X',                                                                                   "#EC NOTEXT
                    'T1 Transaction Type,,1',                                                                      "#EC NOTEXT
                    'AS If all the field values of an entry in the Customizing table are empty,',                  "#EC NOTEXT
                    '   the corresponding number range interval matches all mandates. All the',                    "#EC NOTEXT
                    '   field values of the first entry in the Customizing table are empty.',                      "#EC NOTEXT
                    '   Therefore, number range interval N8 matches all mandates.',                                "#EC NOTEXT
                    'AS If one or more fields of an entry in the Customizing table are not',                       "#EC NOTEXT
                    '   empty, the corresponding number range interval matches a mandate if all',                  "#EC NOTEXT
                    '   their field values match. However, since the value for B2B, either X or',                  "#EC NOTEXT
                    '   empty, is a Boolean, and an empty Boolean value matches any Boolean',                      "#EC NOTEXT
                    '   value, X is transformed to Y and empty is transformed to N. Therefore,',                   "#EC NOTEXT
                    '   number range interval N7 also matches the mandate you want to create.',                    "#EC NOTEXT
                    'AS We then determine which entry to use on the basis of their priority. The',                 "#EC NOTEXT
                    '   priority is determined by the sum of all the weights of an entry: the',                    "#EC NOTEXT
                    '   higher the sum, the higher the priority. The weight of an empty field is',                 "#EC NOTEXT
                    '   0. The weight of a field with a valid value is shown in the following',                    "#EC NOTEXT
                    '   table:',                                                                                   "#EC NOTEXT
                    'K1 Field,,Weight',                                                                            "#EC NOTEXT
                    'T1 Account Group,,64',                                                                        "#EC NOTEXT
                    'T1 B2B,,32',                                                                                  "#EC NOTEXT
                    'T1 Transaction Type,,16',                                                                     "#EC NOTEXT
                    'T1 (The following fields are by default hidden in the maintenance view)',                     "#EC NOTEXT
                    'T1 OrgF1,,8',                                                                                 "#EC NOTEXT
                    'T1 OrgF2,,4',                                                                                 "#EC NOTEXT
                    'T1 OrgF3,,2',                                                                                 "#EC NOTEXT
                    'T1 OrgF4,,1',                                                                                 "#EC NOTEXT
                    'AS We can see that the second entry has a higher priority. Therefore,',                       "#EC NOTEXT
                    '   number range interval N7 is used to generate a mandate ID.',                               "#EC NOTEXT
                    'U4 &HINTS&',                                                                                  "#EC NOTEXT
                    'AS If you have not created any entry in the Customizing table or if no',                      "#EC NOTEXT
                    '   entry matches the mandate you want to create, an error message appears',                   "#EC NOTEXT
                    '   to remind you to either maintain the Customizing table or to correct the',                 "#EC NOTEXT
                    '   field values of the mandate.',                                                             "#EC NOTEXT
                    'U4 &FURTHER_SOURCES_OF_INF&',                                                                 "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.


"-- LIMU DOCU FUFI_APAR_MANDATE_MNDID_SERVICE (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_other_docu     "#EC *
    EXPORTING
      i_devclass  = 'FIN_SEPA_EN'
      i_docname   = 'FUFI_APAR_MANDATE_MNDID_SERVICE'
      i_typ       = 'T'
      i_langu     = 'E'
      i_line      =:'U4 &FUNCTIONALITY&',                                                                          "#EC NOTEXT
                    'AS You use this function module to generate a mandate ID for a newly',                        "#EC NOTEXT
                    '   created mandate.',                                                                         "#EC NOTEXT
                    'AS This function module gets a number via the determined number range',                       "#EC NOTEXT
                    '   interval according to the current mandate. It then checks whether the',                    "#EC NOTEXT
                    '   number has been used by another mandate. If yes, the function module',                     "#EC NOTEXT
                    '   continues searching for a new number and checks it again until it finds',                  "#EC NOTEXT
                    '   an available number and then returns that number.',                                        "#EC NOTEXT
                    'AS Note that the function module checks a maximum of 20 numbers. After 20',                   "#EC NOTEXT
                    '   failed attempts, an error message appears.',                                               "#EC NOTEXT
                    'AS Meanwhile, the mandate ID will be handled with a passed handler. For',                     "#EC NOTEXT
                    '   example, the handler can return the mandate ID directly or concatenate',                   "#EC NOTEXT
                    '   the ID with a prefix such as the paying company code. You can also use',                   "#EC NOTEXT
                    '   your own handler by implementing an already defined interface.',                           "#EC NOTEXT
                    'U4 &EXAMPLE&',                                                                                "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U4 &HINTS&',                                                                                  "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    'U4 &FURTHER_SOURCES_OF_INF&',                                                                 "#EC NOTEXT
                    'AS',                                                                                          "#EC NOTEXT
                    space.

"- TRAN ---------------------------------------------------------------------------- Transactions
"-- R3TR TRAN SEPA_MND_FM_CUST (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_transaction   "#EC *
    EXPORTING
      i_tcode     = 'SEPA_MND_FM_CUST'
      i_devclass  = 'FIN_SEPA_EN'
      i_langu     = 'E'
      i_text      = 'SEPA MNDID Generation FM Customizing'
      i_param     = '/*SM30 VIEWNAME=V_SEPA_MND_CUST;UPDATE=X;'
      i_type      = 'P'.

  CALL METHOD lcl_wb=>create_transaction   "#EC *
"-- R3TR TRAN SEPA_MND_FM_MT (language: EN , source: EBJ )
    EXPORTING
      i_tcode     = 'SEPA_MND_FM_MT'
      i_devclass  = 'FIN_SEPA_EN'
      i_langu     = 'E'
      i_text      = 'SEPA MNDID Generation FM Maintenance'
      i_param     = '/*SM34 VCLDIR-VCLNAME=VC_SEPA_MND_FM;UPDATE=X;'
      i_type      = 'P'.

  CALL METHOD lcl_wb=>create_transaction   "#EC *
"-- R3TR TRAN SEPA_NR_CUST (language: EN , source: EBJ )
    EXPORTING
      i_tcode     = 'SEPA_NR_CUST'
      i_devclass  = 'FIN_SEPA_EN'
      i_langu     = 'E'
      i_text      = 'SEPA Number Range Customizing'
      i_param     = '/*SM30 VIEWNAME=V_SEPA_NR_CUST;UPDATE=X;'
      i_type      = 'P'.

  CALL METHOD lcl_wb=>create_transaction   "#EC *
"-- R3TR TRAN SEPA_NR_MT (language: EN , source: EBJ )
    EXPORTING
      i_tcode     = 'SEPA_NR_MT'
      i_devclass  = 'FIN_SEPA_EN'
      i_langu     = 'E'
      i_text      = 'SEPA Number Range Maintenance'
      i_program   = 'SAPMSNUM'
      i_dynpro    = '0100'
      i_javagui   = ''
      i_htmlgui   = ''
      i_wingui    = ''
      i_param     = '/NSNUM TNRO-OBJECT=SEPA_MNDID'
      i_type      = 'P'.


"-- R3TR TOBJ V_SEPA_MND_CUSTV (language: EN , source: EBJ )

"-- R3TR TOBJ V_SEPA_MND_FMV (language: EN , source: EBJ )

"-- R3TR TOBJ V_SEPA_MND_FM_TV (language: EN , source: EBJ )

"-- R3TR TOBJ V_SEPA_NR_CUSTV (language: EN , source: EBJ )

"-- R3TR TABU TVIMF (language: DE , source: EBJ )
  DATA: lt_tvimf TYPE TABLE OF tvimf, ls_tvimf TYPE tvimf.

  CLEAR ls_tvimf.
  ls_tvimf-tabname  = 'V_SEPA_NR_CUST'.                               "#EC NOTEXT
  ls_tvimf-event    = '02'.                                           "#EC NOTEXT
  ls_tvimf-formname = 'AFTER_SAVE'.                                   "#EC NOTEXT
  APPEND ls_tvimf TO lt_tvimf.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_tabname  = 'TVIMF'
      it_entries = lt_tvimf.

"-- R3TR VCLS VC_SEPA_MND_FM (language: EN , source: EBJ )
  CALL METHOD lcl_wb=>create_logical_object "#EC *
    EXPORTING
      i_object   = 'VCLS'
      i_obj_name = 'VC_SEPA_MND_FM'
      i_devclass = 'FIN_SEPA_EN'
      i_langu    = 'E'
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc eq 0.

  DATA: lt_vcldir TYPE TABLE OF vcldir, ls_vcldir TYPE vcldir.

  CLEAR ls_vcldir.
  ls_vcldir-vclname    = 'VC_SEPA_MND_FM'.                            "#EC NOTEXT
  ls_vcldir-changedate = sy-datum.                                    "#EC NOTEXT
  ls_vcldir-author     = sy-uname.                                    "#EC NOTEXT
  APPEND ls_vcldir TO lt_vcldir.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLDIR'
      it_entries = lt_vcldir.

  DATA: lt_vcldirt TYPE TABLE OF vcldirt, ls_vcldirt TYPE vcldirt.

  CLEAR ls_vcldirt.
  ls_vcldirt-spras   = 'D'.                                           "#EC NOTEXT
  ls_vcldirt-vclname = 'VC_SEPA_MND_FM'.                              "#EC NOTEXT
  ls_vcldirt-text    = 'SEPA-Mandat Referenz-ID: Funktionsbausteine generieren'. "#EC NOTEXT
  APPEND ls_vcldirt TO lt_vcldirt.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLDIRT'
      it_entries = lt_vcldirt.

  CLEAR lt_vcldirt[].

  CLEAR ls_vcldirt.
  ls_vcldirt-spras   = 'E'.                                           "#EC NOTEXT
  ls_vcldirt-vclname = 'VC_SEPA_MND_FM'.                              "#EC NOTEXT
  ls_vcldirt-text    = 'SEPA Mandate Reference ID: Generate Function Modules'. "#EC NOTEXT
  APPEND ls_vcldirt TO lt_vcldirt.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLDIRT'
      it_entries = lt_vcldirt.

  DATA: lt_vclstruc TYPE TABLE OF vclstruc, ls_vclstruc TYPE vclstruc.

  CLEAR ls_vclstruc.
  ls_vclstruc-vclname    = 'VC_SEPA_MND_FM'.                          "#EC NOTEXT
  ls_vclstruc-object     = 'V_SEPA_MND_FM'.                           "#EC NOTEXT
  ls_vclstruc-predobject = 'V_SEPA_MND_FM'.                           "#EC NOTEXT
  ls_vclstruc-objpos     = '01'.                                      "#EC NOTEXT
  ls_vclstruc-dependency = 'R'.                                       "#EC NOTEXT
  ls_vclstruc-startobj   = 'X'.                                       "#EC NOTEXT
  APPEND ls_vclstruc TO lt_vclstruc.

  CLEAR ls_vclstruc.
  ls_vclstruc-vclname    = 'VC_SEPA_MND_FM'.                          "#EC NOTEXT
  ls_vclstruc-object     = 'V_SEPA_MND_FM_T'.                         "#EC NOTEXT
  ls_vclstruc-predobject = 'V_SEPA_MND_FM'.                           "#EC NOTEXT
  ls_vclstruc-objpos     = '02'.                                      "#EC NOTEXT
  ls_vclstruc-dependency = 'S'.                                       "#EC NOTEXT
  APPEND ls_vclstruc TO lt_vclstruc.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLSTRUC'
      it_entries = lt_vclstruc.

  DATA: lt_vclstruct TYPE TABLE OF vclstruct, ls_vclstruct TYPE vclstruct.

  CLEAR ls_vclstruct.
  ls_vclstruct-spras      = 'D'.                                      "#EC NOTEXT
  ls_vclstruct-vclname    = 'VC_SEPA_MND_FM'.                         "#EC NOTEXT
  ls_vclstruct-object     = 'V_SEPA_MND_FM'.                          "#EC NOTEXT
  ls_vclstruct-objecttext = 'SEPA-Mandats-ID Fuba-Generierung'.       "#EC NOTEXT
  APPEND ls_vclstruct TO lt_vclstruct.

  CLEAR ls_vclstruct.
  ls_vclstruct-spras      = 'D'.                                      "#EC NOTEXT
  ls_vclstruct-vclname    = 'VC_SEPA_MND_FM'.                         "#EC NOTEXT
  ls_vclstruct-object     = 'V_SEPA_MND_FM_T'.                        "#EC NOTEXT
  ls_vclstruct-objecttext = 'Beschreibung des Funktionsbausteins'.    "#EC NOTEXT
  APPEND ls_vclstruct TO lt_vclstruct.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLSTRUCT'
      it_entries = lt_vclstruct.

  CLEAR lt_vclstruct[].

  CLEAR ls_vclstruct.
  ls_vclstruct-spras      = 'E'.                                      "#EC NOTEXT
  ls_vclstruct-vclname    = 'VC_SEPA_MND_FM'.                         "#EC NOTEXT
  ls_vclstruct-object     = 'V_SEPA_MND_FM'.                          "#EC NOTEXT
  ls_vclstruct-objecttext = 'SEPA Mandate ID Generation FM'.          "#EC NOTEXT
  APPEND ls_vclstruct TO lt_vclstruct.

  CLEAR ls_vclstruct.
  ls_vclstruct-spras      = 'E'.                                      "#EC NOTEXT
  ls_vclstruct-vclname    = 'VC_SEPA_MND_FM'.                         "#EC NOTEXT
  ls_vclstruct-object     = 'V_SEPA_MND_FM_T'.                        "#EC NOTEXT
  ls_vclstruct-objecttext = 'FM Description'.                         "#EC NOTEXT
  APPEND ls_vclstruct TO lt_vclstruct.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLSTRUCT'
      it_entries = lt_vclstruct.

  DATA: lt_vclstrudep TYPE TABLE OF vclstrudep, ls_vclstrudep TYPE vclstrudep.

  CLEAR ls_vclstrudep.
  ls_vclstrudep-vclname    = 'VC_SEPA_MND_FM'.                        "#EC NOTEXT
  ls_vclstrudep-object     = 'V_SEPA_MND_FM'.                         "#EC NOTEXT
  ls_vclstrudep-predobject = 'V_SEPA_MND_FM'.                         "#EC NOTEXT
  APPEND ls_vclstrudep TO lt_vclstrudep.

  CLEAR ls_vclstrudep.
  ls_vclstrudep-vclname    = 'VC_SEPA_MND_FM'.                        "#EC NOTEXT
  ls_vclstrudep-object     = 'V_SEPA_MND_FM_T'.                       "#EC NOTEXT
  ls_vclstrudep-objfield   = 'FM_NAME'.                               "#EC NOTEXT
  ls_vclstrudep-predobject = 'V_SEPA_MND_FM'.                         "#EC NOTEXT
  ls_vclstrudep-predfield  = 'FM_NAME'.                               "#EC NOTEXT
  APPEND ls_vclstrudep TO lt_vclstrudep.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_logical_object = 'X'
      i_tabname  = 'VCLSTRUDEP'
      it_entries = lt_vclstrudep.

  ENDIF.
"------------------------------------------------------------------------------------------------
"- Changes which might be part of manual instruction ---------------------- Not supported Objects
"------------------------------------------------------------------------------------------------
  " VCLS VC_SEPA_MND_FM

"-- R3TR TOBJ VC_SEPA_MND_FMC (language: EN , source: EBJ )

"-- R3TR TABU TRESC (language: DE , source: EBJ )
  DATA: lt_tresc TYPE TABLE OF tresc, ls_tresc TYPE tresc.

  CLEAR ls_tresc.
  ls_tresc-object     = 'TABU'.                                       "#EC NOTEXT
  ls_tresc-tabname    = 'SEPA_MNDID_FM'.                              "#EC NOTEXT
  ls_tresc-fieldname  = 'FM_NAME'.                                    "#EC NOTEXT
  ls_tresc-keylow     = 'Y*'.                                         "#EC NOTEXT
  ls_tresc-lastname   = sy-uname.                                     "#EC NOTEXT
  ls_tresc-lastdate   = sy-datum.                                     "#EC NOTEXT
  APPEND ls_tresc TO lt_tresc.

  CLEAR ls_tresc.
  ls_tresc-object     = 'TABU'.                                       "#EC NOTEXT
  ls_tresc-tabname    = 'SEPA_MNDID_FM'.                              "#EC NOTEXT
  ls_tresc-fieldname  = 'FM_NAME'.                                    "#EC NOTEXT
  ls_tresc-keylow     = 'Z*'.                                         "#EC NOTEXT
  ls_tresc-lastname   = sy-uname.                                     "#EC NOTEXT
  ls_tresc-lastdate   = sy-datum.                                     "#EC NOTEXT
  APPEND ls_tresc TO lt_tresc.

  CLEAR ls_tresc.
  ls_tresc-object     = 'TABU'.                                       "#EC NOTEXT
  ls_tresc-tabname    = 'SEPA_MNDID_FM_T'.                            "#EC NOTEXT
  ls_tresc-fieldname  = 'FM_NAME'.                                    "#EC NOTEXT
  ls_tresc-keylow     = 'Y*'.                                         "#EC NOTEXT
  ls_tresc-lastname   = sy-uname.                                     "#EC NOTEXT
  ls_tresc-lastdate   = sy-datum.                                     "#EC NOTEXT
  APPEND ls_tresc TO lt_tresc.

  CLEAR ls_tresc.
  ls_tresc-object     = 'TABU'.                                       "#EC NOTEXT
  ls_tresc-tabname    = 'SEPA_MNDID_FM_T'.                            "#EC NOTEXT
  ls_tresc-fieldname  = 'FM_NAME'.                                    "#EC NOTEXT
  ls_tresc-keylow     = 'Z*'.                                         "#EC NOTEXT
  ls_tresc-lastname   = sy-uname.                                     "#EC NOTEXT
  ls_tresc-lastdate   = sy-datum.                                     "#EC NOTEXT
  APPEND ls_tresc TO lt_tresc.


  CALL METHOD lcl_wb=>create_table_entries "#EC *
    EXPORTING
      i_tabname  = 'TRESC'
      it_entries = lt_tresc.

  ENDIF.

  CALL METHOD lcl_wb=>activate.

  IF genview IS NOT INITIAL OR testrun IS NOT INITIAL.
    "Call SE54 and generate without changing the screens
    CALL METHOD lcl_wb=>generate_maint_view  "#EC *
      EXPORTING
        i_devclass    = 'FIN_SEPA_EN'
        i_objectname  = 'V_SEPA_MND_CUST'
        i_objecttype  = 'V'
        i_masterlang  = 'E'
        i_auth_group  = 'FC02'
        i_func_group  = 'SEPA_MNDID_CUST'
        i_dynp_list   = '0002'
        i_dynp_detail = '0004'.

    CALL METHOD lcl_wb=>generate_maint_view  "#EC *
      EXPORTING
        i_devclass    = 'FIN_SEPA_EN'
        i_objectname  = 'V_SEPA_MND_FM'
        i_objecttype  = 'V'
        i_masterlang  = 'E'
        i_auth_group  = 'FC02'
        i_func_group  = 'SEPA_MNDID_CUST'
        i_dynp_list   = '0003'
        i_dynp_detail = '0000'.

    CALL METHOD lcl_wb=>generate_maint_view  "#EC *
      EXPORTING
        i_devclass    = 'FIN_SEPA_EN'
        i_objectname  = 'V_SEPA_MND_FM_T'
        i_objecttype  = 'V'
        i_masterlang  = 'E'
        i_auth_group  = 'FC02'
        i_func_group  = 'SEPA_MNDID_CUST'
        i_dynp_list   = '0005'
        i_dynp_detail = '0000'.

    CALL METHOD lcl_wb=>generate_maint_view  "#EC *
      EXPORTING
        i_devclass    = 'FIN_SEPA_EN'
        i_objectname  = 'V_SEPA_NR_CUST'
        i_objecttype  = 'V'
        i_masterlang  = 'E'
        i_auth_group  = 'FC02'
        i_func_group  = 'SEPA_MNDID_CUST'
        i_dynp_list   = '0001'
        i_dynp_detail = '0000'.

    CALL METHOD lcl_wb=>generate_maint_view  "#EC *
      EXPORTING
        i_devclass    = 'FIN_SEPA_EN'
        i_objectname  = 'VC_SEPA_MND_FM'
        i_objecttype  = 'C'
        i_masterlang  = 'E'
        i_auth_group  = ''
        i_func_group  = ''
        i_dynp_list   = '0000'
        i_dynp_detail = '0000'.

  ENDIF.

  CALL METHOD lcl_wb=>display_log.
ENDFORM.
