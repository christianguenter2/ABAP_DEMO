*----------------------------------------------------------------------*
*       CLASS ZCL_MAT_D_NUMBER_RANGE_FOR_MAT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_mat_d_number_range_for_mat DEFINITION
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



CLASS ZCL_MAT_D_NUMBER_RANGE_FOR_MAT IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    DATA: lt_data TYPE zmat_t_root,
          data    TYPE REF TO data,
          lo_rand TYPE REF TO cl_abap_random,
          seed    TYPE i.
    FIELD-SYMBOLS: <data> LIKE LINE OF lt_data.

    io_read->retrieve(
      EXPORTING
        iv_node                 = is_ctx-node_key
        it_key                  = it_key
      IMPORTING
        et_data                 = lt_data ).

    READ TABLE lt_data ASSIGNING <data> INDEX 1.
    CHECK sy-subrc = 0.

    CHECK <data>-mat_no IS INITIAL.

    seed          = sy-timlo.
    <data>-mat_no = cl_abap_random=>create( seed )->int( ).

    GET REFERENCE OF <data> INTO data.

    io_modify->update(
      EXPORTING
        iv_node = is_ctx-node_key
        iv_key  = <data>-key
        is_data = data ).
  ENDMETHOD.                    "/BOBF/IF_FRW_DETERMINATION~EXECUTE
ENDCLASS.
