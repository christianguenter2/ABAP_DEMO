*&---------------------------------------------------------------------*
*& Report z_test_2017_03_18
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_03_18.

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

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    DATA x	TYPE string.
    DATA y	TYPE i.
    DATA y1 TYPE i.
    DATA y2 TYPE i.
    DATA y3 TYPE i.
    DATA y4 TYPE i.
    DATA y5 TYPE i.
    DATA y6 TYPE i.
    DATA y7 TYPE i.
    DATA y8 TYPE i.
    DATA y9 TYPE i.

    DATA: z    TYPE string,
          test TYPE string.

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

    LOOP AT t100_tab ASSIGNING FIELD-SYMBOL(<t100>).

    ENDLOOP.

    cl_demo_output=>display( t100_tab ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  controller=>get_instance( )->start( ).
