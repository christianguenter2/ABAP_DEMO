*&---------------------------------------------------------------------*
*& Report  Z_TEST_SHLP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_shlp.

PARAMETERS: p_mat_ar TYPE z_ehs_mat_area OBLIGATORY,
            p_mat_2 TYPE z_ehs_mat_area.

AT SELECTION-SCREEN ON p_mat_ar.
  zcl_bc_shlp=>check_field_val_against_shlp(
    EXPORTING
      i_field_value = p_mat_ar
    EXCEPTIONS
      invalid_value = 1 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

AT SELECTION-SCREEN ON p_mat_2.
  zcl_bc_shlp=>check_field_val_against_shlp(
    EXPORTING
      i_field_value = p_mat_2
    EXCEPTIONS
      invalid_value = 1 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
