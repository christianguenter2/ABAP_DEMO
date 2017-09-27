*&---------------------------------------------------------------------*
*& Report z_test_2017_04_21
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_2017_04_21.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(r_instance) TYPE REF TO controller.

    METHODS:
      start.

  PRIVATE SECTION.
    CLASS-DATA:
      _instance TYPE REF TO controller.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD get_instance.

    r_instance = _instance = COND #( WHEN _instance IS NOT BOUND THEN NEW controller( )
                                     ELSE _instance ).

    CREATE OBJECT r_instance.

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
