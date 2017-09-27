*&---------------------------------------------------------------------*
*& Report z_test_2017_04_12
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_04_12.

CLASS lcx_error DEFINITION FINAL
                INHERITING FROM cx_static_check  .

  PUBLIC SECTION.
    CLASS-METHODS:
      raise_text
        IMPORTING
          i_text TYPE csequence
        RAISING
          lcx_error.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE textid OPTIONAL
          previous LIKE previous OPTIONAL
          text     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA:
      _text TYPE string.

ENDCLASS.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      start.

  PRIVATE SECTION.
    CLASS-DATA:
      _instance TYPE REF TO controller.

    METHODS:
      _test_returning
        RETURNING
          VALUE(r_result) TYPE string
        RAISING
          lcx_error,

      _test_exporting
        EXPORTING
          e1        TYPE string
          VALUE(e2) TYPE string
        RAISING
          lcx_error.

ENDCLASS.

CLASS lcx_error IMPLEMENTATION.

  METHOD constructor.

    super->constructor( textid = textid previous = previous ).
    _text = text.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _text IS NOT INITIAL THEN _text
                     ELSE super->get_text( ) ).

  ENDMETHOD.

  METHOD raise_text.

    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        text = i_text.

  ENDMETHOD.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    r_instance = _instance = COND #( WHEN _instance IS NOT BOUND THEN NEW controller( )
                                     ELSE _instance ).

  ENDMETHOD.

  METHOD start.

    DATA: result TYPE string,
          var1   TYPE string,
          var2   TYPE string.

    result = |Test|.

    TRY.
        result = _test_returning( ).

      CATCH lcx_error INTO DATA(error).
    ENDTRY.

    cl_demo_output=>write( result ).

    var1 = |Test1|.
    var2 = |Test2|.

    TRY.
        _test_exporting(
          IMPORTING
            e1 = var1
            e2 = var2 ).

      CATCH lcx_error INTO error.
    ENDTRY.

    cl_demo_output=>write( var1 ).
    cl_demo_output=>write( var2 ).

    cl_demo_output=>display(  ).

  ENDMETHOD.

  METHOD _test_returning.

    r_result = |Hallo Welt!|.

    lcx_error=>raise_text( |Fehler!!!| ).

  ENDMETHOD.


  METHOD _test_exporting.

    cl_demo_output=>write( e1 ).
    cl_demo_output=>write( e2 ).

    CLEAR: e1, e2.

    lcx_error=>raise_text( |Fehler!!!| ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).
