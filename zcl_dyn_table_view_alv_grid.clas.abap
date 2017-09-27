*----------------------------------------------------------------------*
*       CLASS ZCL_DYN_TABLE_VIEW_ALV_GRID DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_DYN_TABLE_VIEW_ALV_GRID definition
  public
  inheriting from ZCL_DYN_TABLE_VIEW_ABS_FACTORY
  final
  create public .

public section.

  methods ZIF_DYN_TABLE_VIEW~DISPLAY_TABLE
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: lo_alv TYPE REF TO cl_salv_table.
ENDCLASS.



CLASS ZCL_DYN_TABLE_VIEW_ALV_GRID IMPLEMENTATION.


  METHOD display_table.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table   = lo_alv    " Basisklasse einfache ALV Tabellen
      CHANGING
        t_table        = ct_table ).

    lo_alv->get_functions( )->set_all( ).

    lo_alv->get_columns( )->set_optimize( ).

    lo_alv->display( ).
  ENDMETHOD.                    "display_table
ENDCLASS.
