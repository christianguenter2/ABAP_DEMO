*&---------------------------------------------------------------------*
*& Report  Z_TEST_MISSING_PARAM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_missing_param.

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: test.
ENDCLASS.                    "lcl_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD test.
    WRITE: / 'Test'.
  ENDMETHOD.                    "test
ENDCLASS.                    "lcl_test IMPLEMENTATION


START-OF-SELECTION.
  DATA: params TYPE abap_parmbind_tab,
        param  TYPE abap_parmbind,
        text   TYPE string VALUE 'Test'.

  param-kind = cl_abap_objectdescr=>importing.
  param-name = 'Test'.
  GET REFERENCE OF text INTO param-value.
  INSERT param INTO TABLE params.

  TRY .
      CALL METHOD lcl_test=>('TEST')
        PARAMETER-TABLE params.
    CATCH  cx_sy_dyn_call_param_not_found.

  ENDTRY.
