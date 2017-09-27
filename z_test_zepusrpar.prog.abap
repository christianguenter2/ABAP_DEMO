*&---------------------------------------------------------------------*
*& Report  Z_TEST_ZEPUSRPAR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_zepusrpar.

PARAMETERS: p_kunnr TYPE kunnr OBLIGATORY.

TYPES: BEGIN OF ty_kunnr,
         kunnr TYPE kunnr,
       END OF ty_kunnr.

DATA: lv_objid   TYPE zepusrpar-objid,
      lt_kunnr   TYPE STANDARD TABLE OF ty_kunnr,
      lt_r_kunnr TYPE RANGE OF kunnr.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = p_kunnr
  IMPORTING
    output = lv_objid.

SELECT parva FROM zepusrpar
             INTO TABLE lt_kunnr
             WHERE objtype = 'KNA1'
             AND   parid   = 'KUN'
             AND   objid   = lv_objid.

IF sy-subrc = 0.
  zcl_bc_convert_to_range=>convert_generic(
     EXPORTING
       it_table       = lt_kunnr
       i_column       = 'KUNNR'
     IMPORTING
       et_range_table = lt_r_kunnr ).
ENDIF.

IF sy-subrc = 0.

ENDIF.
