*&---------------------------------------------------------------------*
*& Report z_test_create_foreign_mode
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_create_foreign_mode.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    CALL FUNCTION 'TH_CREATE_FOREIGN_MODE'
      EXPORTING
        client           = sy-mandt
        user             = sy-uname
        tcode            = 'SE11'
      EXCEPTIONS
        user_not_found   = 1
        cant_create_mode = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
