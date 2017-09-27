*----------------------------------------------------------------------*
*       CLASS ZCL_MAT_D_OUTPUTTEXT_DETERMINA DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_mat_d_outputtext_determina DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_superclass
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
      REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MAT_D_OUTPUTTEXT_DETERMINA IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    DATA: lt_data  TYPE zmat_t_root,
          lv_combi TYPE zmat_s_root,
          data     TYPE REF TO data.

    FIELD-SYMBOLS: <data> LIKE LINE OF lt_data.

    io_read->retrieve(
      EXPORTING
        iv_node                 = is_ctx-node_key
        it_key                  = it_key
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 = lt_data ).

    LOOP AT lt_data ASSIGNING <data>.
      CLEAR: lv_combi.
      MOVE-CORRESPONDING <data> TO lv_combi.

      zcl_ehs_material_functions=>add_domain_texts(
        CHANGING
          c_struct = lv_combi ).

      GET REFERENCE OF lv_combi INTO data.

      io_modify->update(
        EXPORTING
          iv_node           = is_ctx-node_key
          iv_key            = <data>-key
          is_data           = data ).
    ENDLOOP.

  ENDMETHOD.                    "/bobf/if_frw_determination~execute
ENDCLASS.
