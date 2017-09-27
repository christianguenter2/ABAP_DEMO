*&---------------------------------------------------------------------*
*& Report  Z_TEST_QM_INFO_COORD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_qm_info_coord.

PARAMETERS: p_qmnum TYPE qmnum OBLIGATORY.

*----------------------------------------------------------------------*
*       CLASS lcl_applcation DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_applcation DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.                    "lcl_applcation DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_applcation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_applcation IMPLEMENTATION.
  METHOD start.
    DATA: number      TYPE bapi2078_nothdre-notif_no,
          t_notifactv TYPE STANDARD TABLE OF bapi2078_notactvi,
          notifactv   LIKE LINE OF t_notifactv,
*          t_notfulltxt TYPE STANDARD TABLE OF bapi2078_notfulltxti,
*          notfulltxt   LIKE LINE OF t_notfulltxt,
          lt_return    TYPE bapiret2_tab.

    number = p_qmnum.

    notifactv-act_sort_no = '1'.
    notifactv-act_code    = '0060'.
    notifactv-act_codegrp = 'Q2'.
    INSERT notifactv INTO TABLE t_notifactv.

*    notfulltxt-objkey     = 0001.
*    notfulltxt-objtype    = 'QMMA'.
*    notfulltxt-format_col = '*'.
*    notfulltxt-text_line  = '4719457490A64ABE85368019D7BC1347'.
*    INSERT notfulltxt INTO TABLE t_notfulltxt.
*    notfulltxt-text_line = '8A4C4EE39DD04106AE728D846CA581BF'.
*    INSERT notfulltxt INTO TABLE t_notfulltxt.

    CALL FUNCTION 'BAPI_QUALNOT_ADD_DATA'
      EXPORTING
        number    = number
      TABLES
        notifactv = t_notifactv
        return    = lt_return.

    DATA: tlines TYPE STANDARD TABLE OF tline,
          tline  TYPE tline.

    tline-tdformat = '*'.
    tline-tdline   = 'XXX'.
    INSERT tline INTO TABLE tlines.
    tline-tdline = 'YYY'.
    INSERT tline INTO TABLE tlines.

    READ TABLE t_notifactv INTO notifactv INDEX 1.

    CALL FUNCTION 'IQS0_ADD_ACTIVITY_LONGTEXT'
      EXPORTING
        i_qmnum       = number
        i_manum       = notifactv-act_key
      TABLES
        t_inlines     = tlines
      EXCEPTIONS
        show_messages = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
    ENDIF.

    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
      TABLES
        ss_bapiret2 = lt_return.

    CALL FUNCTION 'BAPI_QUALNOT_SAVE'
      EXPORTING
        number = number    " Meldungsnummer
      TABLES
        return = lt_return.

    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
      TABLES
        ss_bapiret2 = lt_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_applcation IMPLEMENTATION

START-OF-SELECTION.
  lcl_applcation=>start( ).
