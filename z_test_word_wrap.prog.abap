*&---------------------------------------------------------------------*
*& Report  Z_TEST_WORD_WRAP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_word_wrap.

*----------------------------------------------------------------------*
*       CLASS lcl_bc_string_splitter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bc_string_splitter DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      split_string_to_table
        IMPORTING
          i_string TYPE string
        EXPORTING
          et_text_table TYPE STANDARD TABLE
        RAISING
          zcx_lo_error.
ENDCLASS.                    "lcl_bc_string_splitter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_bc_string_splitter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_bc_string_splitter IMPLEMENTATION.
  METHOD split_string_to_table.
    DATA: out_lines   TYPE STANDARD TABLE OF char20,
          dref        TYPE REF TO data,
          elem_descr  TYPE REF TO cl_abap_elemdescr,
          table_descr TYPE REF TO cl_abap_tabledescr,
          len         TYPE i.

    FIELD-SYMBOLS: <text>      TYPE any,
                   <text_line> TYPE clike.

    elem_descr ?= cl_abap_elemdescr=>get_c( strlen( i_string ) ).
    CREATE DATA dref TYPE HANDLE elem_descr.
    ASSIGN dref->* TO <text>.
    <text> = i_string.

    table_descr ?= cl_abap_tabledescr=>describe_by_data( et_text_table ).
    elem_descr  ?= table_descr->get_table_line_type( ).

    CALL FUNCTION 'RKD_WORD_WRAP'
      EXPORTING
        textline            = <text>
        outputlen           = elem_descr->output_length
      TABLES
        out_lines           = et_text_table
      EXCEPTIONS
        outputlen_too_large = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      zcx_lo_error=>raise_syst_message( ).
    ENDIF.
  ENDMETHOD.                    "split_string_to_table
ENDCLASS.                    "lcl_bc_string_splitter IMPLEMENTATION

START-OF-SELECTION.

  DATA: text    TYPE string VALUE 'Dies ist ein Test für einen langen String der in Zeilen der Länge 10 gesplitted werden soll',
        lt_text TYPE STANDARD TABLE OF char20.

  lcl_bc_string_splitter=>split_string_to_table(
                            EXPORTING
                              i_string = text
                            IMPORTING
                              et_text_table = lt_text ).

  cl_demo_output=>display_data( lt_text ).
