*&---------------------------------------------------------------------*
*& Report z_test_2017_03_23
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_23.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.
    METHODS start.

  PRIVATE SECTION.
    CLASS-DATA:
      _instance TYPE REF TO controller.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    IF _instance IS NOT BOUND.

      _instance = NEW controller( ).

    ENDIF.

    r_instance = _instance.

  ENDMETHOD.

  METHOD start.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @DATA(t100_tab)
           UP TO 100 ROWS.

    SELECT FROM t100
           FIELDS *
           INTO TABLE @DATA(t000_tab)
           UP TO 100 ROWS.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).
