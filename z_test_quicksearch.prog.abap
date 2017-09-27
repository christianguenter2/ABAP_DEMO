REPORT z_test_quicksearch.

PARAMETERS: query TYPE string OBLIGATORY.

CLASS lcl_quicksearch DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: start RAISING cx_static_check.

  PRIVATE SECTION.
    DATA: objects TYPE if_adt_object_reference_ch=>ty_object_references,
          alv     TYPE REF TO cl_salv_table.
    METHODS:
      _quicksearch RAISING cx_static_check,
      _display RAISING cx_static_check.
ENDCLASS.

CLASS lcl_quicksearch IMPLEMENTATION.
  METHOD start.
    _quicksearch( ).
    _display( ).
  ENDMETHOD.

  METHOD _quicksearch.
    NEW cl_ris_hana_quick_search( )->execute_quick_search(
          EXPORTING
            iv_query               = query
          IMPORTING
            et_object_references   = objects ).
  ENDMETHOD.

  METHOD _display.
    cl_salv_table=>factory(
      IMPORTING
       r_salv_table   = alv
      CHANGING
        t_table        = objects ).

    alv->get_columns( )->set_optimize( ).
    alv->display( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TRY.
      NEW lcl_quicksearch( )->start(  ).
    CATCH cx_static_check INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
