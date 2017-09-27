*&---------------------------------------------------------------------*
*& Report z_test_2017_03_22_2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_22_2.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      start.

  PRIVATE SECTION.
    CLASS-DATA _instance TYPE REF TO controller.

    TYPES: tty_t100 TYPE STANDARD TABLE OF t100
                    WITH NON-UNIQUE DEFAULT KEY.

    DATA: t100_tab TYPE tty_t100.

    METHODS:
      _select,
      _process,
      _display.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.


  METHOD start.

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
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = DATA(alv)
          CHANGING
            t_table        = t100_tab ).

        DATA(tab2) = VALUE tty_t100( FOR t100 IN t100_tab
                                     ( t100 ) ).

        alv->display( ).

      CATCH cx_salv_error INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).
