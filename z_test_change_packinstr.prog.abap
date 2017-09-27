*&---------------------------------------------------------------------*
*& Report  Z_TEST_CHANGE_PACKINSTR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_change_packinstr.

DATA: pikp_imp    TYPE pikp,
      cmvhupo_imp TYPE cmvhupo,
      pikp_exp    TYPE pikp,
      cmvhupo_exp TYPE cmvhupo,
      pipo_tab    TYPE STANDARD TABLE OF pipo,
      packkps_tab TYPE STANDARD TABLE OF packkps.


*pikp_imp-piid     = 'JfJLBS33If3X08002WO1UG'.
pikp_imp-piid     = '00000000000090054566'.
cmvhupo_imp-nodia = 'X'.

CALL FUNCTION 'VHUPO_PACK_INSTRUCTION_SHOW'
  EXPORTING
    pikp_imp          = pikp_imp
    cmvhupo_imp       = cmvhupo_imp
  IMPORTING
    pikp_exp          = pikp_exp
    cmvhupo_exp       = cmvhupo_exp
  TABLES
    pipo_tab          = pipo_tab
    packkps_tab       = packkps_tab
  EXCEPTIONS
    locked            = 1
    not_found         = 2
    data_missed       = 3
    data_wrong        = 4
    quantity_wrong    = 5
    missed_pos_type   = 6
    no_new_number     = 7
    baseunit_wrong    = 8
    not_saved         = 9
    inconsistent      = 10
    marked_for_delete = 11
    exsist            = 12
    material_unkown   = 13
    quantity_changed  = 14
    clint_unkown      = 15
    OTHERS            = 16.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DELETE pipo_tab FROM 4 TO 6.

FIELD-SYMBOLS: <pipo> LIKE LINE OF pipo_tab.

READ TABLE pipo_tab ASSIGNING <pipo> INDEX 2.
IF sy-subrc = 0.
  <pipo>-trgqty = <pipo>-trgqty + 1.
ENDIF.

cmvhupo_exp-nodia = 'X'.

CALL FUNCTION 'VHUPO_PACK_INSTRUCTION_CHANGE'
  EXPORTING
    pikp_imp          = pikp_exp
    cmvhupo_imp       = cmvhupo_exp
    commit            = abap_true
  IMPORTING
    pikp_exp          = pikp_exp
    cmvhupo_exp       = cmvhupo_exp
  TABLES
    pipo_tab          = pipo_tab
    packkps_tab       = packkps_tab
  EXCEPTIONS
    locked            = 1
    not_found         = 2
    data_missed       = 3
    data_wrong        = 4
    quantity_wrong    = 5
    missed_pos_type   = 6
    no_new_number     = 7
    baseunit_wrong    = 8
    not_saved         = 9
    inconsistent      = 10
    marked_for_delete = 11
    unit_needed       = 12
    unit_wrong        = 13
    spras_needed      = 14
    spras_exsist      = 15
    content_needed    = 16
    OTHERS            = 17.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

COMMIT WORK.
