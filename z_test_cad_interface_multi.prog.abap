REPORT z_test_cad_interface_multi.

PARAMETERS: dokar	TYPE dokar OBLIGATORY,
            doknr	TYPE doknr OBLIGATORY,
            dokvr	TYPE dokvr OBLIGATORY,
            doktl	TYPE doktl_d OBLIGATORY.

DATA: lt_dvsdata    TYPE STANDARD TABLE OF /ctcdi/dvsdata_st,
      lt_doc_search TYPE STANDARD TABLE OF /ctcdi/doc_s001_st,
      doc_search    LIKE LINE OF lt_doc_search,
      lt_doc_found  TYPE STANDARD TABLE OF /ctcdi/doc_s001_st,
      doc_found     LIKE LINE OF lt_doc_found.

CALL FUNCTION '/CTCDI/DOC_READ_DATA'
  EXPORTING
    iv_dokar   = dokar
    iv_doknr   = doknr
    iv_doktl   = doktl
    iv_dokvr   = dokvr
  TABLES
    tt_dvsdata = lt_dvsdata.

cl_demo_output=>display_data( lt_dvsdata ).

doc_search-dokar = dokar.
doc_search-doknr = doknr.
doc_search-dokvr = dokvr.
doc_search-doktl = doktl.
doc_search-fieldname = 'ALLGEMEINTOLERANZ-DE'.
INSERT doc_search INTO TABLE lt_doc_search.

CALL FUNCTION '/CTCDI/DOC_READ_MULTIDETAIL'
  TABLES
    tt_doc_search = lt_doc_search
    tt_doc_found  = lt_doc_found.

cl_demo_output=>display_data( lt_doc_found ).
