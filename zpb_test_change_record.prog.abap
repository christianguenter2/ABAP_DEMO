*&---------------------------------------------------------------------*
*& Report  ZPB_TEST_CHANGE_RECORD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zpb_test_change_record.

DATA: key   TYPE zif_pb_types=>key,
      model TYPE REF TO zcl_pb_model,
      error TYPE REF TO /bobf/cx_frw.

FIELD-SYMBOLS: <cdoc_header> LIKE LINE OF model->change_documents_header.

key-number  = '00000074'.
key-version = '01'.

TRY.
    model = zcl_pb_model=>read( key ).

    " Do 3 Changes, adding material links
    model->add_material_link( '01800180' ).
    model->add_material_link( '31700000' ).
    model->add_material_link( '01800181' ).

    model->save( ).
  CATCH /bobf/cx_frw INTO error.
    zcl_pb_view_error=>display( error ).
ENDTRY.

WAIT UP TO 2 SECONDS.

" Show Change Docs, 3 Change Documents were expectet, but only 1 occurs
" same if you look direct to tables CDHDR/CDPOS
READ TABLE model->change_documents_header ASSIGNING <cdoc_header> INDEX 1.
CHECK sy-subrc = 0.

zcl_ehs_functions=>display_change_doc(
  EXPORTING
    i_objectclass = zif_pb_types=>co_object_class_cdoc
    i_objectid    = |{ <cdoc_header>-objectid }| ).


" Now let's do 3 independent changes
TRY.
    model->add_material_link( '01800180' ).
    model->save( ).

    model->add_material_link( '31700000' ).
    model->save( ).

    model->add_material_link( '01800181' ).
    model->save( ).
  CATCH /bobf/cx_frw INTO error.
    zcl_pb_view_error=>display( error ).
ENDTRY.

WAIT UP TO 2 SECONDS.

" Now 3 change docs appear
READ TABLE model->change_documents_header ASSIGNING <cdoc_header> INDEX 1.
CHECK sy-subrc = 0.

zcl_ehs_functions=>display_change_doc(
  EXPORTING
    i_objectclass = zif_pb_types=>co_object_class_cdoc
    i_objectid    = |{ <cdoc_header>-objectid }| ).
