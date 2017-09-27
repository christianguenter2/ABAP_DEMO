*&---------------------------------------------------------------------*
*& Report z_test_reporting_6
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reporting_6.

PARAMETERS: table TYPE string OBLIGATORY,
            demo  RADIOBUTTON GROUP r1 DEFAULT 'X',
            alv   RADIOBUTTON GROUP r1,
            write RADIOBUTTON GROUP r1.

INTERFACE lif_view.

  METHODS:
    display
      IMPORTING
        i_table TYPE REF TO data.

ENDINTERFACE.

CLASS lcx_not_implemented DEFINITION CREATE PUBLIC
                          INHERITING FROM cx_static_check.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          !textid   LIKE textid OPTIONAL
          !previous LIKE previous OPTIONAL
          table     TYPE csequence OPTIONAL,

      get_text REDEFINITION.

  PRIVATE SECTION.
    DATA _table TYPE string.

ENDCLASS.

CLASS lcl_abstract_reporting DEFINITION ABSTRACT.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        i_table           TYPE csequence
        i_view            TYPE REF TO lif_view
      RETURNING
        VALUE(r_instance) TYPE REF TO lcl_abstract_reporting
      RAISING
        lcx_not_implemented.

    METHODS:
      constructor
        IMPORTING
          i_view TYPE REF TO lif_view,

      start.

  PROTECTED SECTION.
    METHODS:
      _display,
      _select ABSTRACT,
      _processing ABSTRACT.

    DATA: table TYPE REF TO data.

  PRIVATE SECTION.
    DATA: view  TYPE REF TO lif_view.

ENDCLASS.

CLASS lcl_t100 DEFINITION CREATE PUBLIC
               INHERITING FROM lcl_abstract_reporting.

  PROTECTED SECTION.
    METHODS:
      _select REDEFINITION,
      _processing REDEFINITION.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

ENDCLASS.

CLASS lcl_t000 DEFINITION FINAL
               INHERITING FROM lcl_abstract_reporting.

  PROTECTED SECTION.
    METHODS: _select REDEFINITION,
      _processing REDEFINITION.

  PRIVATE SECTION.
    DATA: t000_tab TYPE STANDARD TABLE OF t000.

ENDCLASS.

CLASS lcl_demo_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_alv_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcl_write_view DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.

ENDCLASS.

CLASS lcx_not_implemented IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
      EXPORTING
        textid   = textid
        previous = previous ).

    _table = table.

  ENDMETHOD.

  METHOD get_text.

    result = COND #( WHEN _table IS NOT INITIAL THEN |{ _table } not yet implemented|
                     ELSE super->get_text( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_abstract_reporting IMPLEMENTATION.

  METHOD constructor.

    me->view = i_view.

  ENDMETHOD.

  METHOD create.

    r_instance = COND #( WHEN i_table = |T100| THEN NEW lcl_t100( i_view )
                         WHEN i_table = |T000| THEN NEW lcl_t000( i_view )
                         ELSE THROW lcx_not_implemented( table = i_table ) ).

  ENDMETHOD.

  METHOD _display.

    view->display( table ).

  ENDMETHOD.

  METHOD start.

    _select( ).
    _processing( ).
    _display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t100 IMPLEMENTATION.

  METHOD _processing.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t000>).
      " do some processing
    ENDLOOP.

    table = REF #( t100_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t100
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_t000 IMPLEMENTATION.

  METHOD _processing.

    table = REF #( t000_tab ).

  ENDMETHOD.

  METHOD _select.

    SELECT *
           FROM t000
           INTO TABLE @t000_tab.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN i_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    cl_demo_output=>display( <table> ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_view IMPLEMENTATION.

  METHOD lif_view~display.

    ASSIGN i_table->* TO FIELD-SYMBOL(<table>).
    ASSERT sy-subrc = 0.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = <table> ).

        alv->display( ).

      CATCH cx_salv_msg INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_write_view IMPLEMENTATION.

  METHOD lif_view~display.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE.

    ASSIGN i_table->* TO <table>.
    ASSERT sy-subrc = 0.

    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).

      DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <line> ) ).

      WRITE: /.

      LOOP AT struct_descr->components ASSIGNING FIELD-SYMBOL(<component>).

        ASSIGN COMPONENT <component>-name OF STRUCTURE <line> TO FIELD-SYMBOL(<field>).
        WRITE: |{ <field> }|.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      DATA: view TYPE REF TO lif_view.

      view = COND #( WHEN demo  = abap_true THEN NEW lcl_demo_view( )
                     WHEN alv   = abap_true THEN NEW lcl_alv_view( )
                     WHEN write = abap_true THEN NEW lcl_write_view( )
                     ELSE THROW lcx_not_implemented( ) ).

      lcl_abstract_reporting=>create( i_table = table
                                      i_view  = view )->start( ).
    CATCH lcx_not_implemented INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
