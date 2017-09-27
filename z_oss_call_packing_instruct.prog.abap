REPORT z_oss_call_packing_instruct.

DATA: pikey      TYPE bapihupiheader-packinstrid,
      header_ext TYPE bapihupiheader,
      positions  TYPE STANDARD TABLE OF bapihupiposition,
      return     TYPE STANDARD TABLE OF bapiret2.

pikey = '00000000000090000010'.

CALL FUNCTION 'BAPI_HU_PI_READ'
  EXPORTING
    pikey      = pikey    " Identifikationsnummer der Packvorschrift
  IMPORTING
    header_ext = header_ext    " Packvorschriftenkopf
  TABLES
    positions  = positions    " Packvorschriftpositionen
    return     = return.    " Returnparameter

" During this call the exception pi_check_wrong is raised in the
" BADI Implementation ZCL_IM__PL_PACK_INSTR_BADI.
" Why isn't the error shown in return?
" Why is the pi saved?
CALL FUNCTION 'BAPI_HU_PI_CHANGE'
  EXPORTING
    pikey     = pikey    " Identifikationsnummer der Packvorschrift
    header    = header_ext
  TABLES
    positions = positions    " Packvorschriftpositionen
    return    = return.    " Returnparameter

cl_demo_output=>display_data( return ).
