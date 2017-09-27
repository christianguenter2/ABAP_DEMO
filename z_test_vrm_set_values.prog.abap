*&---------------------------------------------------------------------*
*& Report  Z_TEST_VRM_SET_VALUES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_vrm_set_values.

PARAMETERS: p_test TYPE sstring AS LISTBOX VISIBLE LENGTH 30.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_test.
  DATA: values TYPE vrm_values,
        value  LIKE LINE OF values.

  value-text = '1'.
  value-key  = 'Test'.
  INSERT value INTO TABLE values.

  value-text = '2'.
  value-key  = 'Hallo Welt'.
  INSERT value INTO TABLE values.

  value-text = '3'.
  value-key  = '12345'.
  INSERT value INTO TABLE values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_TEST'
      values          = values    " Wertetabelle für ID
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
  ENDIF.
