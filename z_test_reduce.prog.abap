*&---------------------------------------------------------------------*
*& Report z_test_reduce
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_reduce.

CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      main.

  PRIVATE SECTION.
    CLASS-METHODS:
      reverse
        IMPORTING
          i_table        TYPE stringtab
        RETURNING
          VALUE(r_table) TYPE stringtab,

      add_slash_at_second_pos
        IMPORTING
          i_table        TYPE stringtab
        RETURNING
          VALUE(r_table) TYPE stringtab.

ENDCLASS.

CLASS demo IMPLEMENTATION.

  METHOD main.

    DATA(html_commands) = VALUE string_table(
                                ( `<html>` )
                                ( `<body>` )
                                ( `<p>` ) ).
    cl_demo_output=>write( html_commands ).

    DATA(text) = concat_lines_of( html_commands )
              && 'Hallo Welt'
              && concat_lines_of( reverse( add_slash_at_second_pos( html_commands ) ) ).

    cl_demo_output=>display( text ).

  ENDMETHOD.

  METHOD reverse.

    LOOP AT i_table ASSIGNING FIELD-SYMBOL(<line>).
      INSERT <line> INTO r_table INDEX 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD add_slash_at_second_pos.

    r_table = VALUE #( FOR <line> IN i_table
                       ( <line>(1) && '/' && <line>+1 ) ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).
