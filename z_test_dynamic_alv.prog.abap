*&---------------------------------------------------------------------*
*& Report z_test_dynamic_alv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_dynamic_alv.

DATA(struct_descr) = cl_abap_structdescr=>create(
                        VALUE #( ( name = 'TEST'
                                   type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'CHAR100' ) ) ) ) ).

DATA(table_descr) = cl_abap_tabledescr=>create( struct_descr ).

DATA: table TYPE REF TO data,
      line  TYPE REF TO data.

FIELD-SYMBOLS: <table> TYPE ANY TABLE.

CREATE DATA: table TYPE HANDLE table_descr,
             line  TYPE HANDLE struct_descr.

ASSIGN: table->* TO <table>,
        line->*  TO FIELD-SYMBOL(<line>).

ASSIGN COMPONENT 'TEST' OF STRUCTURE <line> TO FIELD-SYMBOL(<field>).
<field> = |Hallo Welt!|.

INSERT <line> INTO TABLE <table>.

DATA: lt_fieldcatalog TYPE lvc_t_fcat.
DATA(alv) = NEW cl_gui_alv_grid( i_parent = cl_gui_container=>default_screen ).

TRY.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = DATA(salv)
      CHANGING
        t_table        = <table> ).

    cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      EXPORTING
        r_columns      = salv->get_columns( )
        r_aggregations = salv->get_aggregations( )
      RECEIVING
        t_fieldcatalog = lt_fieldcatalog ).

  CATCH cx_salv_error INTO DATA(error).    "
    MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.

alv->set_table_for_first_display(
  CHANGING
    it_outtab                     = <table>
    it_fieldcatalog               = lt_fieldcatalog    " Feldkatalog
  EXCEPTIONS
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    OTHERS                        = 4 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE sy-msgty.
  RETURN.
ENDIF.

CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  SET SCREEN 0.
ENDMODULE.
