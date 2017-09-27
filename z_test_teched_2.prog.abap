*&---------------------------------------------------------------------*
*& Report z_test_teched_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_teched_2.

CLASS test_grouping DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _test
      IMPORTING
        i_text          TYPE string
      EXPORTING
        e_text          TYPE string
      CHANGING
        c_text          TYPE string
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS _group_value
      IMPORTING
        i_group         TYPE any
      RETURNING
        VALUE(r_result) TYPE i.

ENDCLASS.

CLASS test_grouping IMPLEMENTATION.

  METHOD start.

    TYPES: BEGIN OF ty_test,
             i TYPE i,
             s TYPE string,
           END OF ty_test,
           tty_test TYPE STANDARD TABLE OF ty_test
                      WITH NON-UNIQUE DEFAULT KEY.

    DATA(table) = VALUE tty_test( ( i = 1 s = 'Test'    )
                                  ( i = 2 s = 'isntsre' )
                                  ( i = 1 s = 'asinte'  )
                                  ( i = 3 s = 'Test'    )
                                  ( i = 1 s = 'Test'    )
                                  ( i = 2 s = 'xxx'     )
                                  ( i = 1 s = 'Test'    ) ).

    DATA(x)    = ||.
    DATA(text) = ||.

    DATA(y) = _test( EXPORTING i_text = 'Test'
                     IMPORTING e_text = text
                     CHANGING  c_text = x ).

*    _test( EXPORTING i_text = 'Test'
*           IMPORTING e_text = DATA(text)
*           CHANGING  c_text = x
*           RECEIVING r_text = DATA(y) ).

    LOOP AT table ASSIGNING FIELD-SYMBOL(<group>)
         GROUP BY _group_value( <group> ) INTO DATA(group).

      cl_demo_output=>begin_section( |Begin { group } values are: \n| ).

      LOOP AT GROUP group ASSIGNING FIELD-SYMBOL(<group_line>).

        cl_demo_output=>write( |{ <group_line>-s }\n| ).

      ENDLOOP.

    ENDLOOP.

    cl_demo_output=>display(  ).

  ENDMETHOD.


  METHOD _test.

  ENDMETHOD.


  METHOD _group_value.

    r_result = 1.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW test_grouping( )->start( ).
