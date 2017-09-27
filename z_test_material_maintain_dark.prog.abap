*&---------------------------------------------------------------------*
*& Report  Z_TEST_MATERIAL_MAINTAIN_DARK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_material_maintain_dark.

DATA: headdata       TYPE bapimathead,
      plantdata      TYPE bapi_marc,
      plantdatax     TYPE bapi_marcx,
      return         TYPE bapiret2,
      returnmessages TYPE STANDARD TABLE OF bapi_matreturn2.

headdata-material = '12345678'.
headdata-mrp_view = abap_true.

plantdata-plant      = 'MX01'.
plantdata-pur_status = '04'.

plantdatax-plant      = 'MX01'.
plantdatax-pur_status = abap_true.

CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
  EXPORTING
    headdata       = headdata    " Kopfsegment mit Steuerungsinformationen
    plantdata      = plantdata    " Werksspezifische Materialdaten
    plantdatax     = plantdatax    " Update-Informationen zu PLANTDATA
  IMPORTING
    return         = return    " Rückgabeparameter
  TABLES
    returnmessages = returnmessages.    " Alle Meldungen

cl_demo_output=>write_data( return ).
cl_demo_output=>write_data( returnmessages ).
cl_demo_output=>display( ).
