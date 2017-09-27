class ZCL_MAT_D_MATERIAL_TEXT definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MAT_D_MATERIAL_TEXT IMPLEMENTATION.


METHOD /bobf/if_frw_determination~execute.
  DATA: lt_data TYPE zmat_t_material_link,
        data    TYPE REF TO data,
        lv_makt TYPE makt.

  FIELD-SYMBOLS: <data> LIKE LINE OF lt_data.

  io_read->retrieve(
    EXPORTING
      iv_node                 = is_ctx-node_key
      it_key                  = it_key
      iv_fill_data            = abap_true
    IMPORTING
      et_data                 = lt_data ).

  LOOP AT lt_data ASSIGNING <data>.
    CALL FUNCTION 'MAKT_SINGLE_READ'
      EXPORTING
        matnr      = <data>-matnr
        spras      = sy-langu
      IMPORTING
        wmakt      = lv_makt
      EXCEPTIONS
        wrong_call = 1
        not_found  = 2
        OTHERS     = 3.

    MOVE-CORRESPONDING lv_makt TO <data>.

    GET REFERENCE OF <data> INTO data.

    io_modify->update(
      EXPORTING
        iv_node           = is_ctx-node_key
        iv_key            = <data>-key
        is_data           = data ).
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
