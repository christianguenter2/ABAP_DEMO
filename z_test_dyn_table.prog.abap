*&---------------------------------------------------------------------*
*& Report  Z_TEST_DYN_TABLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_dyn_table.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_cols  TYPE i OBLIGATORY DEFAULT 3,
            p_lines TYPE i OBLIGATORY DEFAULT 1,
            p_view  TYPE zcl_dyn_table_view_abs_factory=>ty_class AS LISTBOX VISIBLE LENGTH 60 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_instance)
                                          TYPE REF TO lcl_application,
                   at_selection_screen_output.
    METHODS: start.

  PRIVATE SECTION.
    METHODS:_create_dynamic_table RETURNING value(r_dref) TYPE REF TO data,
            _fill_lines CHANGING ct_table TYPE STANDARD TABLE,
            _display CHANGING ct_table TYPE STANDARD TABLE.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance.
  ENDMETHOD.                    "GET_INSTANCE

  METHOD at_selection_screen_output.
    DATA: lt_class  TYPE zcl_dyn_table_view_abs_factory=>tty_class,
          lt_values TYPE vrm_values,
          lv_value  LIKE LINE OF lt_values.

    FIELD-SYMBOLS: <class> LIKE LINE OF lt_class.

    lt_class = zcl_dyn_table_view_abs_factory=>get_all_sublasses( ).

    LOOP AT lt_class ASSIGNING <class>.
      lv_value-key = <class>-clsname.
      lv_value-text = <class>-description.
      INSERT lv_value INTO TABLE lt_values.
    ENDLOOP.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_VIEW'    " Name der Wertemenge
        values          = lt_values    " Wertetabelle für ID
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "at_selection_screen_output

  METHOD start.
    DATA: lr_table TYPE REF TO data.
    FIELD-SYMBOLS: <table> TYPE STANDARD TABLE.

    CHECK: p_cols  > 0,
           p_lines > 0.

    lr_table = _create_dynamic_table( ).
    ASSIGN lr_table->* TO <table>.

    _fill_lines( CHANGING ct_table = <table> ).

    _display( CHANGING ct_table = <table> ).
  ENDMETHOD.                    "start

  METHOD _create_dynamic_table.
    r_dref = zcl_dyn_table_controller=>create_dynamic_table( i_cols = p_cols ).
  ENDMETHOD.                    "_create_dynamic_table

  METHOD _fill_lines.
    zcl_dyn_table_controller=>fill_table(
        EXPORTING
          i_cols = p_cols
          i_rows = p_lines
        CHANGING
          ct_table = ct_table ).
  ENDMETHOD.                    "_fill_lines

  METHOD _display.
    zcl_dyn_table_view_abs_factory=>get_instance( p_view-clsname )->display_table( CHANGING ct_table = ct_table ).
  ENDMETHOD.                    "_display
ENDCLASS.                    "lcl_application IMPLEMENTATION

AT SELECTION-SCREEN OUTPUT.
  lcl_application=>at_selection_screen_output( ).

START-OF-SELECTION.
  lcl_application=>get_instance( )->start( ).
