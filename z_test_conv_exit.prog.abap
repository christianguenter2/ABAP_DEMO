*&---------------------------------------------------------------------*
*& Report  Z_TEST_CONV_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_conv_exit.

PARAMETERS: p_doknr TYPE doknr OBLIGATORY.

cl_reca_ddic_services=>do_conv_exit(
  EXPORTING
    if_output          = abap_true    " Bool: TRUE=OUTPUT, FALSE=INPUT
  CHANGING
    cd_field           = p_doknr
    EXCEPTIONS
      error              = 1
      OTHERS             = 2 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

cl_demo_output=>display_data( p_doknr ).
