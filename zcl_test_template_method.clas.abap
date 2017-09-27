CLASS zcl_test_template_method DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF co_type,
                 one   TYPE string VALUE `one`,
                 two   TYPE string VALUE `two`,
                 three TYPE string VALUE `three`,
                 four  TYPE string VALUE `four`,
                 five  TYPE string VALUE `five`,
                 six   TYPE string VALUE `six`,
                 seven TYPE string VALUE `seven`,
               END OF co_type.

    METHODS:
      run
        IMPORTING i_type          TYPE string
        RETURNING VALUE(r_result) TYPE string.

  PRIVATE SECTION.

    DATA mo_template TYPE REF TO lcl_template.

ENDCLASS.



CLASS ZCL_TEST_TEMPLATE_METHOD IMPLEMENTATION.


  METHOD run.

    mo_template = lcl_template=>create( i_type ).

    r_result = mo_template->run( ).

  ENDMETHOD.
ENDCLASS.
