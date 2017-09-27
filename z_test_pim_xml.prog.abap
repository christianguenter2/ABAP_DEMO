*&---------------------------------------------------------------------*
*& Report  ZSDLH190                                                   *
*&---------------------------------------------------------------------*
*  Kurzbeschreibung:                                                   *
*  SD: Produktlabel
*  Erstellt am  26.04.2012                                             *
*           von HEZELLUD                                               *
*----------------------------------------------------------------------*
* Nr     Änderer    Datum     Änderung
*
*----------------------------------------------------------------------*
REPORT z_test_pim_xml NO STANDARD PAGE HEADING
                      LINE-SIZE 255 MESSAGE-ID ma.



*----------------------------------------------------------------------*
* Daten für ALV
*----------------------------------------------------------------------*
INCLUDE: zbcaralv01.


* Hilfsfelder
DATA: h_repid           LIKE sy-repid,
      h_save(1)         TYPE c,
      h_exit(1)         TYPE c,
      h_user_command    TYPE slis_formname VALUE 'USER_COMMAND-ALV',
      h_callback_status TYPE slis_formname VALUE 'SET_PF_STATUS'.

* Strukturen
DATA: s_layout   TYPE slis_layout_alv, "Layout der Liste
      s_fieldcat TYPE slis_t_fieldcat_alv, "Feldkatalog der Liste
      s_variant  LIKE disvariant. " Layout der Liste
* Konstanten
DATA: c_unit_ea    LIKE t006-msehi VALUE 'EA',
      c_preis_in   LIKE a006-pltyp VALUE 'IN',
      c_char1_x    TYPE char1 VALUE 'X',
      c_kappl_v    TYPE kappl VALUE 'V',
      c_waerk_inr  TYPE waers VALUE 'INR',
      c_kschl_pr00 TYPE kschl VALUE 'PR00',
      c_vtweg_01   TYPE vtweg VALUE '01',
      c_cust_care  TYPE char10 VALUE 'CCareMRP'.
"LH004+
* Fehlermeldung
DATA: g_zeile       TYPE char50,
      g_zeile1      TYPE char50,
      g_fehler      TYPE char1,
      g_name1       TYPE name1,
      gs_tvko       TYPE tvko,
      gs_adrc       TYPE adrc,
      gs_a530       TYPE a530,
      g_maktx       TYPE maktx,
      g_menge       TYPE char20,
      g_matnr       TYPE matnr,
      g_wert        TYPE kwert,
      g_menge2      TYPE fkimg,
      g_wertdr      TYPE char25,
      g_sofort      TYPE char1,
      g_cust_number TYPE ad_tlnmbr,
      g_pmatn       TYPE mvke-pmatn, "LH002+
      g_adrnr       TYPE adrc-addrnumber. "LH004+
DATA: gs_usr01      TYPE usr01.

DATA: w_cx_root TYPE REF TO cx_root,
      mesg      TYPE string.

DATA: gt_adtel_tab TYPE TABLE OF adtel,
      gs_adtel_tab TYPE adtel.
*
DATA: g_kbetr                     TYPE kbetr.

DATA: lo_http_client TYPE REF TO if_http_client,
      lv_return_code TYPE sy-subrc.
DATA: g_bild2        TYPE string,
      g_type         TYPE string VALUE 'JPG',
      g_type2        TYPE string.

DATA: gxx_url TYPE string.

DATA: gv_img_xstring_logo TYPE string,
      gv_img_xstring      TYPE xstring.

DATA: gs_val_m003 TYPE api_val_r.
DATA: gs_val_m002 TYPE api_val_r.
DATA: gs_val_m004 TYPE api_val_r.
DATA: gs_val_m005 TYPE api_val_r.

DATA: gs_t178t      TYPE t178t.
DATA: g_menge_druck TYPE char15,
      g_ean11       TYPE ean11,
      g_landx       TYPE landx,
      g_landx_druck TYPE char25.
* Formular
DATA: retcode                     LIKE sy-subrc.            "Returncode
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Datendeklaration
*----------------------------------------------------------------------*
* Tabellen
TABLES: smp_dyntxt,
        sscrfields.

*----------------------------------------------------------------------*
* Selektionsmaske
*----------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK selection
                  WITH FRAME
                 TITLE text-001.
PARAMETERS:

             p_matnr LIKE  mara-matnr
               OBLIGATORY
               DEFAULT '39010000',
             p_menge LIKE mara-inhal
               OBLIGATORY
               DEFAULT '1',
             p_meins LIKE mara-meins
               OBLIGATORY
               DEFAULT 'ST',
             p_vkorg LIKE vbak-vkorg
               OBLIGATORY
               DEFAULT 'DE01',
             p_werks LIKE marc-werks
               OBLIGATORY
               DEFAULT 'DE01',
             p_vtweg LIKE vbak-vtweg
               OBLIGATORY
               DEFAULT '01'.



SELECTION-SCREEN END OF BLOCK selection.

* Druckparameter
SELECTION-SCREEN BEGIN OF BLOCK druckparameter
                  WITH FRAME
                 TITLE text-002.
* Drucker
PARAMETERS:  p_dest LIKE itcpo-tddest
               OBLIGATORY
               DEFAULT 'Y1LH',
* Anzahl Copys
              p_copy  LIKE itcpo-tdcopies
               OBLIGATORY
               DEFAULT '1',
* Sofortdruck
             p_sofort LIKE itcpo-tdimmed,
*               DEFAULT c_char1_x,
             p_dialog TYPE char1
               DEFAULT 'X',

* Bezeichnung
             p_bez  LIKE itcpo-tdcovtitle
               DEFAULT 'Product-Label'.

SELECTION-SCREEN END OF BLOCK druckparameter.

*----------------------------------------------------------------------*
*  Bei Selektion                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.



*----------------------------------------------------------------------*
* Initialisierung
*----------------------------------------------------------------------*

INITIALIZATION.
* Drucktaste Help
  smp_dyntxt-icon_id   = '@5E@'. "ICON_SYSTEM_EXTENDED_HELP
  smp_dyntxt-quickinfo = text-999.
  MOVE smp_dyntxt TO sscrfields-functxt_01.



  CLEAR: gs_usr01.
  CALL FUNCTION 'GET_PRINT_PARAM'
    EXPORTING
      i_bname = sy-uname
    IMPORTING
      e_usr01 = gs_usr01.

  IF gs_usr01-spdb EQ 'H'.
    CLEAR: p_sofort.
  ELSE.
    p_sofort = c_char1_x.
  ENDIF.

*----------------------------------------------------------------------*
* Eingegebene Daten prüfen                                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF sscrfields-ucomm = 'FC01'.
    CALL FUNCTION 'Z_DOKUMENTATION_AUFRUFEN'
      EXPORTING
        i_repid = 'ZSDLH188'
        i_langu = sy-langu.
  ENDIF.

  IF sscrfields-ucomm IS INITIAL.                           " KH001+
    sscrfields-ucomm = 'ONLI'.                              " KH001+
  ENDIF.                                                    " KH001+

*----------------------------------------------------------------------*
*  Beginn der Selektion                                                *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM pruefen_material.
  PERFORM get_marc.
  PERFORM get_klassif.
  PERFORM get_bild
    USING
      p_matnr
    CHANGING
      gv_img_xstring_logo
      gxx_url.

*  PERFORM formular_drucken.

*----------------------------------------------------------------------*
*  Ende der Selektion                                                  *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  CLEAR p_matnr.

*----------------------------------------------------------------------*
*  Neue Seite                                                          *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

*----------------------------------------------------------------------*
*  Neue Seite bei Neuaufbau                                            *
*----------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.


*&---------------------------------------------------------------------*
*&      Form status_anzeigen using h_text.
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM status_anzeigen USING h_text.
  IF sy-batch = space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = h_text.
  ELSE.
    GET TIME.
    WRITE:/ sy-uzeit, h_text.
  ENDIF.
ENDFORM.                               "status_anzeigen

*---------------------------------------------------------------------*
*       FORM set_pf_status                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*  -->  ENDFORM                                                       *
*---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING rt_extab.
ENDFORM.                    "SET_PF_STATUS
*&      Form  nachricht_ausgeben
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nachricht_ausgeben USING p_matnr  TYPE matnr
                              p_zeile  TYPE char50
                              p_zeile1 TYPE char50.
  CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
    EXPORTING
      titel     = p_zeile
      textline1 = p_zeile1.
ENDFORM.                    " nachricht_ausgeben
*&---------------------------------------------------------------------*
*&      Form  PRUEFEN_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_MATNR  text
*----------------------------------------------------------------------*
FORM pruefen_material.

  CLEAR: g_ean11.
  SELECT SINGLE ean11 FROM marm INTO g_ean11
         WHERE matnr = p_matnr
           AND meinh = p_meins.

ENDFORM.                    " PRUEFEN_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  FORMULAR_DRUCKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM formular_drucken.

  DATA: lv_forname      TYPE funcname,
        gv_fm_name      TYPE rs38l_fnam,
        ls_outputparams TYPE sfpoutputparams,
        ls_docparams    TYPE sfpdocparams,
        ls_result       TYPE sfpjoboutput.

  CLEAR: g_menge_druck.
  WRITE p_menge TO g_menge_druck UNIT p_meins.
  CONDENSE g_menge_druck.

* Titel im Spool.
  ls_outputparams-covtitle = p_bez.
* Anzahl Labels                                         "LH001+
  ls_outputparams-copies   = p_copy.                    "LH001+
* neuer Spoolauftrag
  ls_outputparams-reqimm   = c_char1_x.
* Druckdialog
  ls_outputparams-nodialog = p_dialog.
* Drucker
  ls_outputparams-dest     = p_dest.
* sofortdruck
  ls_outputparams-reqimm   = p_sofort.
* Formularname
  lv_forname =   'Z_PRODUKTETIKETTEN'.
* Funktionsbaustein ermitteln
  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = lv_forname
        IMPORTING
          e_funcname = gv_fm_name.

    CATCH cx_root INTO w_cx_root.
      mesg = w_cx_root->get_text( ).
      IF NOT mesg IS INITIAL.
        g_zeile  = mesg(36).
        CONCATENATE g_zeile p_matnr
             INTO g_zeile1 SEPARATED BY space.
        PERFORM nachricht_ausgeben USING p_matnr
                                  g_zeile
                                  g_zeile1.
        g_fehler = c_char1_x.

        RETURN.
      ENDIF.
  ENDTRY.
* Open print job
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outputparams
    EXCEPTIONS
      OTHERS          = 1.
  IF sy-subrc EQ 0.
    CALL FUNCTION gv_fm_name
      EXPORTING
        /1bcdwb/docparams   = ls_docparams
        iv_img_xstring_logo = gv_img_xstring_logo
        iv_type             = g_type2
        is_val_m002         = gs_val_m002
        is_val_m003         = gs_val_m003
        is_val_m004         = gs_val_m004
        is_val_m005         = gs_val_m005
        i_matnr             = p_matnr
        i_menge_druck       = g_menge_druck
        i_ean11             = g_ean11
        i_landx_druck       = g_landx_druck
      EXCEPTIONS
        usage_error         = 1
        system_error        = 2
        internal_error      = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      g_zeile  = text-205.
      CONCATENATE text-206 p_matnr
          INTO g_zeile1 SEPARATED BY space.
      PERFORM nachricht_ausgeben
        USING
          p_matnr
          g_zeile
          g_zeile1.
      g_fehler = c_char1_x.
      RETURN.
    ELSE.
      CALL FUNCTION 'FP_JOB_CLOSE'
        IMPORTING
          e_result       = ls_result
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
        g_zeile  = text-205.
        CONCATENATE text-206 p_matnr
           INTO g_zeile1 SEPARATED BY space.
        PERFORM nachricht_ausgeben
          USING
            p_matnr
            g_zeile
            g_zeile1.
        g_fehler = c_char1_x.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " FORMULAR_DRUCKEN
*&---------------------------------------------------------------------*
*&      Form  LESEN_WERK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LIEFNR  text
*----------------------------------------------------------------------*
FORM lesen_werk  USING    p_liefnr  TYPE t001w-werks
                 CHANGING p_name1   TYPE t001w-name1.

  SELECT SINGLE name1 FROM t001w
           INTO p_name1
           WHERE werks = p_liefnr.


ENDFORM.                    " LESEN_WERK
*&---------------------------------------------------------------------*
*&      Form  LESEN_TVKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VKORG  text
*      <--P_G_ADRNR  text
*----------------------------------------------------------------------*
FORM lesen_tvko  USING    p_vkorg TYPE vkorg
                 CHANGING p_tvko  TYPE tvko.

  CLEAR: gs_tvko.
  SELECT SINGLE * FROM tvko
           INTO gs_tvko
           WHERE vkorg = p_vkorg.


ENDFORM.                    " LESEN_TVKO
*&---------------------------------------------------------------------*
*&      Form  LESEN_ADRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_TVKO  text
*      <--P_GS_ADRC  text
*----------------------------------------------------------------------*
FORM lesen_adrc  USING    p_gs_tvko  TYPE tvko
                 CHANGING p_gs_adrc  TYPE adrc.

  CLEAR: p_gs_adrc.
  SELECT SINGLE * FROM adrc
           INTO gs_adrc
           WHERE addrnumber = p_gs_tvko-adrnr
             AND date_from  = '00010101'
             AND nation     = ' '.


ENDFORM.                    " LESEN_ADRC
*&---------------------------------------------------------------------*
*&      Form  LESEN_MAKT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_ADRC  text
*      <--P_G_MAKTX  text
*----------------------------------------------------------------------*
FORM lesen_makt  USING    p_gs_adrc  TYPE adrc
                          p_matnr    TYPE matnr
                 CHANGING p_maktx    TYPE maktx
                          p_menge    TYPE char20.

  CLEAR: p_maktx.
  SELECT SINGLE maktx FROM makt INTO p_maktx
        WHERE matnr = p_matnr
          AND spras = p_gs_adrc-langu.

ENDFORM.                    " LESEN_MAKT
*&---------------------------------------------------------------------*
*&      Form  GET_BILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*       Fetch image in a two step process.
*       First call REST service to get XML that holds the image URL
*       Then use that URL to fetch the image data
*----------------------------------------------------------------------*
FORM get_bild
  USING
    i_matnr                       TYPE matnr
  CHANGING
    e_logo_base64                 TYPE string
    e_url                         TYPE string.

  DATA: l_uri  TYPE string,
        l_data TYPE xstring.


  g_type2 = to_upper( 'image'
                   && '/'
                   && g_type ).

*=====================================================================*
* Create HTTP client and make connection to PIM
*---------------------------------------------------------------------*
*  TRY.
*     Create the HTTP client
  cl_http_client=>create_by_destination(
    EXPORTING
      destination              = 'PIM_PRODUCTION'
    IMPORTING
      client                   = lo_http_client
    EXCEPTIONS
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      OTHERS                   = 6
         ).
  IF sy-subrc <> 0.
*         Implement suitable error handling here
  ENDIF.

*     Set method to GET
  lo_http_client->request->set_method(
      method = if_http_request=>co_request_method_get
         ).

*     set protocol version
  lo_http_client->request->set_version(
      version = if_http_request=>co_protocol_version_1_1
         ).

*     set request URI
  l_uri = '/hgpim/rest/media/'
       && i_matnr
       && '/17?drawingType=v'.

  PERFORM make_http_request
    USING
      l_uri
    CHANGING
      lo_http_client
      l_data.


*     get image URL from XML data
  PERFORM get_image_url_from_xml
    USING
      l_data
    CHANGING
      l_uri.

*     with the new URI, get the image data
  PERFORM make_http_request
    USING
      l_uri
    CHANGING
      lo_http_client
      l_data.

*     close the http connection
  lo_http_client->close(
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2 ).

*    CATCH cx_root.
*  ENDTRY.

  CALL FUNCTION 'SSFC_BASE64_ENCODE'
    EXPORTING
      bindata                  = l_data
*     BINLENG                  = BINLENG
    IMPORTING
      b64data                  = e_logo_base64
    EXCEPTIONS
      ssf_krn_error            = 1
      ssf_krn_noop             = 2
      ssf_krn_nomemory         = 3
      ssf_krn_opinv            = 4
      ssf_krn_input_data_error = 5
      ssf_krn_invalid_par      = 6
      ssf_krn_invalid_parlen   = 7
      OTHERS                   = 8.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.
*---------------------------------------------------------------------*

  e_url = l_uri.

ENDFORM.                    " GET_BILD
*&---------------------------------------------------------------------*
*&      Form  GET_MVKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_marc.
  DATA: l_herkl TYPE herkl.
  SELECT SINGLE herkl FROM marc
         INTO l_herkl
         WHERE matnr = p_matnr
           AND werks = p_werks
           AND matnr = p_matnr
           AND beskz = 'E'.
  IF sy-subrc EQ 0.
    SELECT SINGLE landx FROM t005t INTO g_landx
            WHERE spras = 'E'
             AND land1 = l_herkl.
    IF sy-subrc EQ 0.
      CONCATENATE 'Made in'  g_landx INTO g_landx_druck
       SEPARATED BY space.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_MVKE
*&---------------------------------------------------------------------*
*&      Form  GET_KLASSIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_klassif .
  DATA: lv_ob_key TYPE api_ob_key,
        lt_ob_key TYPE STANDARD TABLE OF api_ob_key
                  WITH NON-UNIQUE DEFAULT KEY.

  DATA: lt_val   TYPE STANDARD TABLE OF api_val_r
                 WITH NON-UNIQUE DEFAULT KEY
                 WITH NON-UNIQUE SORTED KEY secondary_key
                      COMPONENTS charact.

  lv_ob_key-field = 'MATNR'.
  lv_ob_key-value = p_matnr.
  APPEND lv_ob_key TO lt_ob_key.

  CALL FUNCTION 'CACL_OBJECT_READ_VALIDATION'
    EXPORTING
      object_type              = 'MARA'
      class_type               = '001'
      class                    = 'K308'
      language                 = syst-langu
      with_unassigned_characts = space
      i_execute_dep            = ' '
    TABLES
      object_identification    = lt_ob_key
      charact_values           = lt_val
    EXCEPTIONS
      error                    = 1
      warning                  = 2
      OTHERS                   = 3.

  READ TABLE lt_val INTO gs_val_m003
       WITH TABLE KEY secondary_key
       COMPONENTS charact = 'M003'.
  READ TABLE lt_val INTO gs_val_m002
       WITH TABLE KEY secondary_key
       COMPONENTS charact = 'M002'.
  READ TABLE lt_val INTO gs_val_m004
       WITH TABLE KEY secondary_key
       COMPONENTS charact = 'M004'.
  READ TABLE lt_val INTO gs_val_m005
       WITH TABLE KEY secondary_key
       COMPONENTS charact = 'M005'.

ENDFORM.                    " GET_KLASSIF
*&---------------------------------------------------------------------*
*&      Form  GET_IMAGE_URL_FROM_XML
*&---------------------------------------------------------------------*
*       Gets the xml of the provided URI and parses it to get the
*       images destination URL
*----------------------------------------------------------------------*
FORM get_image_url_from_xml
  USING
    i_xml_data                    TYPE xstring
  CHANGING
    e_image_url                   TYPE string.

  DATA: lt_urls TYPE stringtab.

  CALL TRANSFORMATION z_test_bild_pim
    SOURCE XML  i_xml_data
    RESULT urls = lt_urls.

  READ TABLE lt_urls INTO e_image_url
                     INDEX 1.
ENDFORM.                    " GET_XML
*&---------------------------------------------------------------------*
*&      Form  MAKE_HTTP_REQUEST
*&---------------------------------------------------------------------*
*       Performs a HTTP call to the specified URI with the given
*       HTTP client / session
*----------------------------------------------------------------------*
FORM make_http_request
  USING
    l_uri                         TYPE string
  CHANGING
    co_http_client                TYPE REF TO if_http_client
    e_data                        TYPE xstring.

  cl_http_utility=>set_request_uri(
    request = lo_http_client->request
    uri     = l_uri ).

  lo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5 ).

  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).

  e_data = lo_http_client->response->get_data( ).

ENDFORM.                    " MAKE_HTTP_REQUEST

*----------------------------------------------------------------------*
*       CLASS lcl_unit_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_unit_test DEFINITION FOR TESTING
                    INHERITING FROM cl_aunit_assert
                    DURATION SHORT
                    RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA: matnr       TYPE matnr,
          logo_base64 TYPE string,
          url         TYPE string.
    METHODS: setup,
             _given_valid_matnr,
             _when_perform_get_bild,
             _then_url_shd_be_filled,
             test_get_bild FOR TESTING.
ENDCLASS.                    "lcl_unit_test DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_unit_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_unit_test IMPLEMENTATION.
  METHOD setup.

  ENDMETHOD.                    "setup

  METHOD _given_valid_matnr.
    matnr = '39010000'.
  ENDMETHOD.                    "_given_valid_matnr

  METHOD _when_perform_get_bild.
    PERFORM get_bild
      USING
        matnr
      CHANGING
        logo_base64
        url .
  ENDMETHOD.                    "_when_perform_get_bild

  METHOD _then_url_shd_be_filled.
    assert_equals(
      EXPORTING
        exp                  = `/medias/sys_master/celum_assets/17__avg01230_tif.jpg?1`
        act                  = url ).
  ENDMETHOD.                    "_then_url_shd_be_filled

  METHOD test_get_bild.
    _given_valid_matnr( ).
    _when_perform_get_bild( ).
    _then_url_shd_be_filled( ).
  ENDMETHOD.                    "test_get_bild
ENDCLASS.                    "lcl_unit_test IMPLEMENTATION
