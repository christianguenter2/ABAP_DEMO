*&---------------------------------------------------------------------*
*& Report  Z_TEST_PACKING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_packing.

PARAMETERS: packnr TYPE pdt_packnr OBLIGATORY.

DATA: pdpara TYPE pdt_pdpara,
*     qty_per_tophu   TYPE pdt_qty,
*     act_pgid        TYPE packgood-pgid,
*     tophunum        TYPE pdt_hunum,
*     tophu_itemno    TYPE pdt_itemno,
*     dialog_flag     TYPE c,
      ret_hunum     TYPE pdt_hunum,
      packqtys      TYPE pdt_t_packqty,
      item_in_tophu TYPE pdt_huitem,
      prthus        TYPE pdt_prthus.

pdpara-packmode    = 2.
pdpara-requ_status = 1.

CALL FUNCTION 'VHUPIHU_CREATE_HU_PROTOTYPE'
  EXPORTING
    pdpara                 = pdpara
    packnr                 = packnr
*   qty_per_tophu          = qty_per_tophu
*   act_pgid               = act_pgid
*   tophunum               = tophunum
*   tophu_itemno           = tophu_itemno
*   dialog_flag            = dialog_flag
  IMPORTING
    ret_hunum              = ret_hunum
*   packqtys               = packqtys
*   item_in_tophu          = item_in_tophu
  CHANGING
    prthus                 = prthus
  EXCEPTIONS
    creation_failed        = 1
    packinstr_not_found    = 2
    packinstr_deleted      = 3
    unit_conversion_failed = 4
    packinstr_cyclic       = 5
    packstatus_violation   = 6
    join_group_violation   = 7
    OTHERS                 = 8.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
ENDIF.

DATA: l_treetc             TYPE pdt_treetc.

PERFORM hutree_create  IN PROGRAM saplvhusubsc
                       USING prthus
                       CHANGING l_treetc.


DATA: lo_alv TYPE REF TO cl_salv_table.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = lo_alv
  CHANGING
    t_table        = l_treetc-tc_rows ).

lo_alv->display( ).
