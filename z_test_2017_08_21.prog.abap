*&---------------------------------------------------------------------*
*& Report z_test_2017_08_21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_08_21.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    DATA: t100_tab TYPE STANDARD TABLE OF t100.

    METHODS:
      _select,
      _process,
      _display.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    _select( ).
    _process( ).
    _display( ).

  ENDMETHOD.

  METHOD _select.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @t100_tab
           UP TO 100 ROWS.

  ENDMETHOD.

  METHOD _process.

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

  ENDMETHOD.

  METHOD _display.

    DATA: alv   TYPE REF TO cl_salv_table,
          error TYPE REF TO cx_salv_msg.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table      = t100_tab ).

        alv->display( ).

      CATCH cx_salv_msg INTO error.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
