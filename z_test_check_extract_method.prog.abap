*&---------------------------------------------------------------------*
*& Report z_test_check_extract_method
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_check_extract_method.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    CHECK sy-subrc = 0.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

ENDCLASS.
