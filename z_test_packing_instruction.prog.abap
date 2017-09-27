*&---------------------------------------------------------------------*
*& Report  Z_TEST_PACKING_INSTRUCTION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_packing_instruction.

*PARAMETERS: p_pikey TYPE bapihupiheader-packinstrid OBLIGATORY.

*DATA: header_ext TYPE bapihupiheader,
*      positions  TYPE STANDARD TABLE OF bapihupiposition,
*      lt_return  TYPE STANDARD TABLE OF bapiret2.
*
*CALL FUNCTION 'BAPI_HU_PI_READ'
*  EXPORTING
*    pikey      = p_pikey    " Identifikationsnummer der Packvorschrift
*  IMPORTING
*    header_ext = header_ext    " Packvorschriftenkopf
*  TABLES
*    positions  = positions    " Packvorschriftpositionen
*    return     = lt_return.
*
*CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
*  TABLES
*    ss_bapiret2 = lt_return.

*DATA: it_kompv         TYPE vsep_t_kompv,
*      kompv            LIKE LINE OF it_kompv,
*      ef_existing_data TYPE xfeld,
*      et_messages      TYPE huitem_messages_t,
*      et_header        TYPE hum_hu_header_t.
*
*kompv-matnr = '01800180'.
*INSERT kompv INTO TABLE it_kompv.
*
*CALL FUNCTION 'V51P_PACKING_DETERMINATION'
*  EXPORTING
*    it_kompv         = it_kompv    " anlegen Beleg/ Fcode
*  IMPORTING
*    ef_existing_data = ef_existing_data    " anlegen Beleg/ Fcode
*    et_messages      = et_messages    " anlegen Beleg/ Fcode
*    et_header        = et_header    " Tabellentyp für Handling Units
*  EXCEPTIONS
*    packinstruction  = 1
*    OTHERS           = 2.
*
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.

PARAMETERS: p_matnr TYPE komgp-matnr OBLIGATORY,
            p_werks TYPE komgp-werks OBLIGATORY,
            p_lifnr TYPE komgp-lifnr OBLIGATORY,
            p_scheme TYPE t683-kalsm OBLIGATORY DEFAULT 'Z00001'.

DATA: i_komgp           TYPE komgp,
*     i_dialog
      i_scheme          TYPE t683-kalsm,
*     i_protocol
*     i_requ_status     TYPE prthukp-packstatu,
*     i_date            TYPE packgood-valdat_req,
*     i_loadcarr_req    TYPE packgood-loadcarr_req,
      ex_komgp        TYPE komgp,
      ex_requ_status  TYPE prthukp-packstatu,
      ex_date         TYPE packgood-valdat_req,
      ex_loadcarr_req TYPE packgood-loadcarr_req,
      ch_packnr       TYPE packkp-packnr,
      ch_kondp        TYPE kondp.

i_komgp-matnr = p_matnr.
i_komgp-werks = p_werks.
i_komgp-lifnr = p_lifnr.
i_scheme      = p_scheme.

CALL FUNCTION 'VHUPOSEL_PACK_INST_DETERMINE'
  EXPORTING
    i_dialog            = abap_false
    i_scheme            = i_scheme    " Findungsschema
*   i_protocol          = SPACE    " Flag, Findungsprotokoll
    i_komgp             = i_komgp    " Merkmalsfelder des Belegkopfes
*   i_requ_status       = C_YELLOW    " erforderlicher Packstatus
*   i_date              = SY-DATUM    " erforderliches Gültigkeitsdatum
*   i_loadcarr_req      = i_loadcarr_req    " erforderlicher Ladungsträger
  IMPORTING
    ex_komgp            = ex_komgp    " Merkmalsfelder der Findung
    ex_requ_status      = ex_requ_status    " Merkmalsfelder der Findung
    ex_date             = ex_date    " Gültigkeitsdatum der Findung
    ex_loadcarr_req     = ex_loadcarr_req    " Ladungsträger der Findung
  CHANGING
    ch_packnr           = ch_packnr    " Packvorschrift
    ch_kondp            = ch_kondp    " Kopf der Findung
  EXCEPTIONS
    packinstr_not_found = 1
    packinstr_deleted   = 2
    OTHERS              = 3.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DATA: packnr_i         TYPE packkp-packnr,
      pre_lock_mode_i  TYPE lockpara-lock_mode,
      post_lock_mode_i TYPE lockpara-lock_mode,
      all_langu_i      TYPE cmvhupo-nodia,
      packkp_e         TYPE packkp,
      db_status_e      TYPE cmvhupodb-db_status,
      tpackpo          TYPE STANDARD TABLE OF packpo,
      tpackkps         TYPE STANDARD TABLE OF packkps.

CALL FUNCTION 'VHUPODB_PACKOBJ_READ_INT'
  EXPORTING
    packnr_i           = ch_packnr
    pre_lock_mode_i    = pre_lock_mode_i
    post_lock_mode_i   = post_lock_mode_i
    all_langu_i        = all_langu_i
    get_items          = abap_true
    get_text           = abap_true
  IMPORTING
    packkp_e           = packkp_e
    db_status_e        = db_status_e    " Database status of packing object
  TABLES
    tpackpo            = tpackpo    " Database status of packing object
    tpackkps           = tpackkps    " Database status of packing object
  EXCEPTIONS
    pobj_in_update_run = 1
    foreign_lock       = 2
    error              = 3
    OTHERS             = 4.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DATA: lo_alv TYPE REF TO cl_salv_table.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = lo_alv
  CHANGING
    t_table        = tpackpo ).

lo_alv->display( ).
