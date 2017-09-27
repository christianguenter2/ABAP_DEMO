*&---------------------------------------------------------------------*
*& Report  Z_TEST_GOS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_gos.

PARAMETERS: p_file TYPE string OBLIGATORY.

*DATA: lo_gos_doc TYPE REF TO cl_gos_document_service,
*      object     TYPE borident.
*
*CREATE OBJECT lo_gos_doc.
*
*object-objkey  = '0020500000'.
*object-objtype = 'BUS2031'.
*
*lo_gos_doc->create_attachment(
*  EXPORTING
*    is_object     = object
**  IMPORTING
**    ep_attachment = ep_attachment    " Attachment created
*).
*
*COMMIT WORK.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: rc         TYPE i,
        file_table TYPE filetable.

  cl_gui_frontend_services=>file_open_dialog(
    CHANGING
      file_table              = file_table    " Tabelle, die selektierte Dateien enthält
      rc                      = rc    " Rückgabewert: Anzahl Dateien oder -1 falls Fehler auftritt
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

  READ TABLE file_table INTO p_file INDEX 1.

START-OF-SELECTION.
  DATA: folder_id        TYPE soodk,
        data_tab         TYPE solix_tab,
        lt_objcont       TYPE soli_tab,
        lt_objhead       TYPE soli_tab,
        lv_objhead       LIKE LINE OF lt_objhead,
        object_hd_change TYPE sood1,
        xstring          TYPE xstring.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = p_file
      filetype                = 'BIN'    " Dateityp (Ascii, Binär)
    CHANGING
      data_tab                = data_tab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
  ENDIF.

  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region                = 'B'
    IMPORTING
      folder_id             = folder_id    " Gefundene ID der Wurzelmappe
    EXCEPTIONS
      communication_failure = 1
      owner_not_exist       = 2
      system_failure        = 3
      x_error               = 4
      OTHERS                = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    RETURN.
  ENDIF.

  CONCATENATE '&SO_FILENAME=' 'Test.pdf' INTO lv_objhead.
  APPEND lv_objhead TO lt_objhead.
  lv_objhead = '&SO_FORMAT=BIN' .
  APPEND lv_objhead TO lt_objhead.

  object_hd_change-file_ext = 'PDF'.
  object_hd_change-objdes   = 'Test'.
  object_hd_change-objsns   = 'O'.
  object_hd_change-objlen   = lines( lt_objcont ) * 510.

  DATA: object_id TYPE soodk.

  CALL FUNCTION 'SO_SOLIXTAB_TO_SOLITAB'
    EXPORTING
      ip_solixtab = data_tab
    IMPORTING
      ep_solitab  = lt_objcont.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
  ENDIF.

  CALL FUNCTION 'SO_CONVERT_CONTENTS_BIN'
    EXPORTING
      it_contents_bin = lt_objcont
    IMPORTING
      et_contents_bin = lt_objcont.

  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id                  = folder_id    " ID der Mappe
      object_type                = 'EXT'
      object_hd_change           = object_hd_change
    IMPORTING
      object_id                  = object_id
    TABLES
      objcont                    = lt_objcont
      objhead                    = lt_objhead    " Spezieller Kopf des Objekts
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      dl_name_exist              = 4
      folder_not_exist           = 5
      folder_no_authorization    = 6
      object_type_not_exist      = 7
      operation_no_authorization = 8
      owner_not_exist            = 9
      parameter_error            = 10
      substitute_not_active      = 11
      substitute_not_defined     = 12
      system_failure             = 13
      x_error                    = 14
      OTHERS                     = 15.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    RETURN.
  ENDIF.

  DATA: obj_rolea    TYPE borident,
        obj_roleb    TYPE borident,
        relationtype TYPE breltyp-reltype,
        document     TYPE sood4.

  obj_rolea-objkey  = '0020500000'.
  obj_rolea-objtype = 'BUS2031'.

  document-foltp = folder_id-objtp.
  document-folyr = folder_id-objyr.
  document-folno = folder_id-objno.
  MOVE-CORRESPONDING object_id TO document.

  obj_roleb-objkey  = document(34).
  obj_roleb-objtype = 'MESSAGE'.

  CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
    EXPORTING
      obj_rolea      = obj_rolea    " Rolle Objekt A
      obj_roleb      = obj_roleb    " Rolle Objekt B
      relationtype   = 'ATTA'
    EXCEPTIONS
      no_model       = 1
      internal_error = 2
      unknown        = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    RETURN.
  ENDIF.

  COMMIT WORK.
