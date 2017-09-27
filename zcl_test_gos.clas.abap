*----------------------------------------------------------------------*
*       CLASS zcl_test_gos DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_test_gos DEFINITION
  PUBLIC
  INHERITING FROM cl_gos_service
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: execute REDEFINITION.

  PROTECTED SECTION.
    METHODS: check_status REDEFINITION.

ENDCLASS.



CLASS ZCL_TEST_GOS IMPLEMENTATION.


  METHOD check_status.

    super->check_status(
      EXPORTING
        is_lporb  = is_lporb    " Lokale Persistente Objektreferenz - BOR kompatibel
        is_object = is_object    " obsolet: wird gelöscht
      IMPORTING
        ep_status = ep_status    " (aktiv/inaktiv/unsichtbar)
        ep_icon   = ep_icon    ).

    CHECK sy-uname = 'GUENTERC'.

    ep_status = 0.

  ENDMETHOD.                    "check_status


  METHOD execute.

    DATA: data_tab      TYPE tabl1024_t,
          filelength    TYPE i,
          length        TYPE num12,
          toaom         TYPE toaom,
          archiv_doc_id TYPE sapb-sapadokid,
          object_id     TYPE sapb-sapobjid,
          manager       TYPE REF TO cl_gos_manager,
          obj           TYPE borident.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = 'C:\temp\Test.pdf'
        filetype                = 'BIN'    " Dateityp (Ascii, Binär)
      IMPORTING
        filelength              = filelength    " Dateilänge
      CHANGING
        data_tab                = data_tab
      EXCEPTIONS
        OTHERS                  = 19 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    SELECT SINGLE *
           FROM toaom
           INTO toaom
           WHERE sap_object = 'VBRK'
           AND   ar_object  = 'ZSDINVPDF'.

    ASSERT sy-subrc = 0.

    length = filelength.

    CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
      EXPORTING
        archiv_id                = toaom-archiv_id
        document_type            = toaom-doc_type
        length                   = length
      IMPORTING
        archiv_doc_id            = archiv_doc_id    " Identifikation des abzulegenden Dokumentes
      TABLES
        binarchivobject          = data_tab    " Puffertabelle zum speichern von Tabellendaten bis Länge 1024
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        blocked_by_policy        = 4
        OTHERS                   = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    object_id = gs_lporb-instid.

    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        arc_doc_id            = archiv_doc_id
        ar_object             = toaom-ar_object
        object_id             = object_id
        sap_object            = toaom-sap_object    " Aufrufende Applikation
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    obj-objkey  = gs_lporb-instid.
    obj-objtype = gs_lporb-typeid.

    CREATE OBJECT manager
      EXPORTING
        is_object    = obj
        ip_no_commit = 'R'
      EXCEPTIONS
        others       = 1.

    manager->unpublish( ).

    CREATE OBJECT manager
      EXPORTING
        is_object    = obj
        ip_no_commit = 'R'
      EXCEPTIONS
        others       = 1.

    manager->start_service_direct(
      EXPORTING
        ip_service       = 'VIEW_ATTA'
        is_object        = obj
      EXCEPTIONS
        no_object        = 1
        object_invalid   = 2
        execution_failed = 3
        OTHERS           = 4 ).

    MESSAGE 'Anlage angehängt' TYPE 'S'.

  ENDMETHOD.                    "execute
ENDCLASS.
