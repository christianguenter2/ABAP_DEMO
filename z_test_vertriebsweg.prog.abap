REPORT z_test_vertriebsweg.

*----------------------------------------------------------------------*
*       CLASS lcl_test_vertriebsweg DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_vertriebsweg DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_name,
             kunnr  TYPE kna1-kunnr,
             ktokd  TYPE kna1-ktokd,
             vtweg1 TYPE vtweg,
             vtweg2 TYPE vtweg,
           END OF ty_name,
           tty_name TYPE STANDARD TABLE OF ty_name
                         WITH NON-UNIQUE DEFAULT KEY.

    CLASS-METHODS:
      create
        RETURNING value(r_instance) TYPE REF TO lcl_test_vertriebsweg.

    METHODS: start.

  PRIVATE SECTION.

    DATA: lt_kunnr TYPE tty_name.

    METHODS:
     _select,
     _display.

ENDCLASS.                    "lcl_test_vertriebsweg DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test_vertriebsweg IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_vertriebsweg IMPLEMENTATION.

  METHOD create.

    CREATE OBJECT r_instance.

  ENDMETHOD.                    "create

  METHOD start.

    _select( ).
    _display( ).

  ENDMETHOD.                    "start

  METHOD _select.

    SELECT kna1~kunnr
           kna1~ktokd
           k1~vtweg AS vtweg1
           k2~vtweg AS vtweg2
           FROM knvv AS k1
           INNER JOIN knvv AS k2 ON  k1~kunnr = k2~kunnr
                                 AND k1~vkorg = k2~vkorg
           INNER JOIN kna1 ON k1~kunnr = kna1~kunnr
           INTO CORRESPONDING FIELDS OF TABLE lt_kunnr
           WHERE k1~vtweg <> k2~vtweg.

    DELETE ADJACENT DUPLICATES FROM lt_kunnr COMPARING kunnr.

  ENDMETHOD.                    "_select

  METHOD _display.

    DATA: lo_alv       TYPE REF TO cl_salv_table,
          lo_alv_error TYPE REF TO cx_salv_error.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_alv
          CHANGING
            t_table        = lt_kunnr ).

        lo_alv->get_columns( )->set_optimize( ).
        lo_alv->display( ).

      CATCH cx_salv_msg INTO lo_alv_error.
        MESSAGE lo_alv_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "_display

ENDCLASS.                    "lcl_test_vertriebsweg IMPLEMENTATION

START-OF-SELECTION.
  lcl_test_vertriebsweg=>create( )->start( ).
