*&---------------------------------------------------------------------*
*& Report  Z_TEST_ZAHLUNGSBEDINGUNGEN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_zahlungsbedingungen.

PARAMETERS: p_zterm TYPE t052-zterm OBLIGATORY,
            p_spras TYPE sy-langu OBLIGATORY.

START-OF-SELECTION.
  DATA: lt_t052  TYPE STANDARD TABLE OF t052,
        lt_zbtxt TYPE stringtab .

  FIELD-SYMBOLS: <t052>  LIKE LINE OF lt_t052,
                 <zbtxt> LIKE LINE OF lt_zbtxt.

  SELECT * FROM t052 INTO TABLE lt_t052
           WHERE zterm = p_zterm.

  LOOP AT lt_t052 ASSIGNING <t052>.
    CLEAR lt_zbtxt.

    CALL FUNCTION 'ME_PRINT_T052'
      EXPORTING
        i_t052    = <t052>    " G/L account number
        language  = p_spras    " G/L account number
      TABLES
        zbtxt     = lt_zbtxt    " G/L account number
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    ENDIF.

    LOOP AT lt_zbtxt ASSIGNING <zbtxt>.
      WRITE: / <zbtxt>.
    ENDLOOP.
  ENDLOOP.
