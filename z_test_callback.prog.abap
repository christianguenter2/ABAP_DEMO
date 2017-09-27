*&---------------------------------------------------------------------*
*& Report  Z_TEST_CALLBACK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_callback.

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_pf_status USING lt_extab TYPE kkblo_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING lt_extab
                                      OF PROGRAM 'Z_TEST_CALLBACK'.
ENDFORM.                    "set_pf_status

*&---------------------------------------------------------------------*
*&      Form  Z_TEST_CALLBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_test USING ucomm    TYPE sy-ucomm
                  selfield TYPE slis_selfield.

  FIELD-SYMBOLS: <outtab>    TYPE INDEX TABLE,
                 <line>      TYPE any,
                 <tabname>   TYPE csequence,
                 <fname>     TYPE csequence,
                 <old_value> TYPE any,
                 <new_value> TYPE any,
                 <tabkey>    TYPE any.

  DATA: lv_zehs_mat_detail   TYPE zehs_mat_detail,
        lv_mat_charact_txt   TYPE z_ehs_mat_charact_txt,
        lv_mat_value_txt_old TYPE z_ehs_mat_value_txt,
        lv_mat_value_txt_new TYPE z_ehs_mat_value_txt,
        title                TYPE char255,
        text1                TYPE char255,
        text2                TYPE char255.

  CHECK ucomm = '&TEST'.

  ASSIGN ('(SAPLSLVC_FULLSCREEN)T_OUTTAB[]') TO <outtab>.
  CHECK sy-subrc = 0.

  READ TABLE <outtab> ASSIGNING <line>
                      INDEX selfield-tabindex.
  CHECK sy-subrc = 0.

  ASSIGN COMPONENT 'TABNAME' OF STRUCTURE <line> TO <tabname>.
  CHECK sy-subrc = 0.
  ASSIGN COMPONENT 'FNAME' OF STRUCTURE <line> TO <fname>.
  CHECK sy-subrc = 0.
  ASSIGN COMPONENT 'F_OLD' OF STRUCTURE <line> TO <old_value>.
  CHECK sy-subrc = 0.
  ASSIGN COMPONENT 'F_NEW' OF STRUCTURE <line> TO <new_value>.
  CHECK sy-subrc = 0.
  ASSIGN COMPONENT 'TABKEY' OF STRUCTURE <line> TO <tabkey>.
  CHECK sy-subrc = 0.

  CHECK <tabname> = 'ZEHS_MAT_DETAIL'.

  lv_zehs_mat_detail = <tabkey>.

  IF <fname> CP 'APP_VALUE_FREE_*'.
    SELECT SINGLE mat_charact_txt FROM zehs_mat_md_20t
           INTO lv_mat_charact_txt
           WHERE mat_charact = lv_zehs_mat_detail-app_charact
           AND   spras       = sy-langu.

    title = |Merkmal { lv_mat_charact_txt }|.

    SELECT SINGLE mat_value_txt FROM zehs_mat_md_21t
           INTO lv_mat_value_txt_old
           WHERE mat_value = <old_value>
           AND   spras     = sy-langu.
    IF sy-subrc <> 0.
      lv_mat_value_txt_old = <old_value>.
    ENDIF.

    SELECT SINGLE mat_value_txt FROM zehs_mat_md_21t
           INTO lv_mat_value_txt_new
           WHERE mat_value = <new_value>
           AND   spras     = sy-langu.
    IF sy-subrc <> 0.
      lv_mat_value_txt_new = <new_value>.
    ENDIF.

    text1 = |Alt: { lv_mat_value_txt_old }|.
    text2 = |Neu: { lv_mat_value_txt_new }|.

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        titel        = title
        textline1    = text1
        textline2    = text2
*       start_column = 25
*       start_row    =           6
      .
  ENDIF.

ENDFORM.                    "Z_TEST_CALLBACK
