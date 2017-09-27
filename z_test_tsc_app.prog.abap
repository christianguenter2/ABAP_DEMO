*&---------------------------------------------------------------------*
*& Report  Z_TEST_TSC_APP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_TSC_APP.

    DATA: sguid               TYPE string,
*     sdatetime           TYPE string,
     skundennummer       TYPE string,
*     svorname            TYPE string,
*     snachname           TYPE string,
*     stelefonnummer      TYPE string,
*     semailadresse       TYPE string,
     sbetreff            TYPE string,
     scomment            TYPE string,
*     smediadata          TYPE string,
*     smediamimetype      TYPE string,
*     wait_after_commit   TYPE char01,
     itype               TYPE zztsc_return_type,
     iqmnum              TYPE string.



call FUNCTION 'Z_CS_CREATE_SIMPLE'
  EXPORTING
    sguid             = sguid
*    sdatetime         = sdatetime
    skundennummer     = skundennummer
*    svorname          = svorname
*    snachname         = snachname
*    stelefonnummer    = stelefonnummer
*    semailadresse     = semailadresse
    sbetreff          = sbetreff
    scomment          = scomment
*    smediadata        = smediadata
*    smediamimetype    = smediamimetype
*    wait_after_commit = wait_after_commit
*  IMPORTING
*    itype             = itype
*    iqmnum            = iqmnum
  .
