CLASS zcl_test_check_extract_method DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      start.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.
ENDCLASS.



CLASS zcl_test_check_extract_method IMPLEMENTATION.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

    CHECK sy-subrc = 0.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

ENDCLASS.
