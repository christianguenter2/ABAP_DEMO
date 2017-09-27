*&---------------------------------------------------------------------*
*& Report z_test_returncode
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_returncode.

CLASS test_returncode DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: start.

  PRIVATE SECTION.

    METHODS _do_something.

ENDCLASS.

CLASS test_returncode IMPLEMENTATION.

  METHOD start.

    _do_something( ).

    cl_demo_output=>write( |{ sy-subrc }| ).

    CALL FUNCTION 'Z_TEST_DO_SOMETHING'.

    cl_demo_output=>write( |{ sy-subrc }| ).

    cl_demo_output=>display(  ).

  ENDMETHOD.


  METHOD _do_something.

    sy-subrc = 7.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_returncode( )->start( ).
