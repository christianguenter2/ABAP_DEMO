class ZCL_FP_EXTENSION_FIELD_CONV_EX definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_FP_EXT_FIELD_CONV_EXIT .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FP_EXTENSION_FIELD_CONV_EX IMPLEMENTATION.


METHOD if_badi_fp_ext_field_conv_exit~format_value_conv_exit.
  DATA: lo_type_desc TYPE REF TO cl_abap_typedescr,
        name         TYPE string.

  CHECK iv_formname = 'Z_TEST_BARCODE'.

  lo_type_desc = cl_abap_typedescr=>describe_by_data( iv_value ).

  name = lo_type_desc->get_relative_name( ).

  IF name = 'EXIDV'.
    cv_result_value = iv_value.
  ENDIF.
ENDMETHOD.
ENDCLASS.
