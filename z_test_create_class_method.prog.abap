*&---------------------------------------------------------------------*
*& Report z_test_create_class_method
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_create_class_method.

CLASS test_class_method DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: test.

  PRIVATE SECTION.
    CLASS-METHODS _test.

ENDCLASS.

CLASS test_class_method IMPLEMENTATION.

  METHOD test.

    _test( ).

  ENDMETHOD.

  METHOD _test.

  ENDMETHOD.

ENDCLASS.
