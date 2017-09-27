*&---------------------------------------------------------------------*
*& Report z_test_reporting_5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting_5.

PARAMETERS: table TYPE string OBLIGATORY.

CLASS lcx_not_implemented DEFINITION
                          INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          !textid   LIKE textid OPTIONAL
          !previous LIKE previous OPTIONAL
          i_table   TYPE csequence ,

      get_text REDEFINITION.

  PRIVATE SECTION  .
    DATA: _table TYPE string.

ENDCLASS.

CLASS lcl_reporting_base DEFINITION CREATE PUBLIC ABSTRACT.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_table           TYPE any OPTIONAL
      RETURNING
        VALUE(r_instance) TYPE REF TO lcl_reporting_base
      RAISING
        lcx_not_implemented.

    METHODS: start.

  PROTECTED SECTION.

    METHODS:
      _select ABSTRACT,
      _processing ABSTRACT.

    DATA: table TYPE REF TO data.

  PRIVATE SECTION.

    METHODS: _display.

ENDCLASS.

CLASS lcl_t100 DEFINITION CREATE PUBLIC
               INHERITING FROM lcl_reporting_base.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS:
      _select REDEFINITION,
      _processing REDEFINITION.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS lcx_not_implemented IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
      EXPORTING
        textid   = textid
        previous = previous ).

    _table = i_table.

  ENDMETHOD.

  METHOD get_text.

    result = |{ _table } not yet implemented|.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_reporting_base IMPLEMENTATION.

  METHOD create.

    r_instance = COND #( WHEN i_table = |T100| THEN NEW lcl_t100( )
                         ELSE THROW lcx_not_implemented( i_table ) ).

  ENDMETHOD.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

  METHOD _display.

    ASSIGN table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t100 IMPLEMENTATION.

  METHOD _select.

    SELECT *
           FROM t100
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).
      " do some processing here
    ENDLOOP.

    table = REF #( t100_tab ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      lcl_reporting_base=>create( table )->start( ).
    CATCH lcx_not_implemented INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
