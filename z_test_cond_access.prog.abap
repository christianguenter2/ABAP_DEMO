*&---------------------------------------------------------------------*
*& Report  Z_TEST_COND_ACCESS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_cond_access.

DATA: application        TYPE t685-kappl,
      condition_type     TYPE t685-kschl,
      header_comm_area   TYPE komk,
      position_comm_area TYPE komp,
      koprt_i            TYPE koprt,
      t682i              TYPE t682i,
      condition_records  TYPE STANDARD TABLE OF a000.

application    = 'V'.
condition_type = 'JCST'.

SELECT SINGLE * FROM t682i
                INTO t682i
                WHERE kozgf = 'JCST'.

header_comm_area-aland = 'IN'.
header_comm_area-wkreg = '13'.
header_comm_area-taxk2 = '1'.
header_comm_area-prsdt = sy-datum.

position_comm_area-taxm2 = '1'.

CALL FUNCTION 'SD_COND_ACCESS'
  EXPORTING
    application          = application
    condition_type       = condition_type    " Konditionsart
    header_comm_area     = header_comm_area    " Kommunikationsstruktur (Kopf)
    position_comm_area   = position_comm_area    " Kommunikationsstruktur (Position)
    protocol_access      = abap_false
    t682i_i              = t682i    " Zugriff
    koprt_i              = koprt_i    " Protokollschnittstelle (Konditionen)
  TABLES
    condition_records    = condition_records    " Gefundene Konditionssätze (Form A000, B000, ...)
  EXCEPTIONS
    field_is_initial     = 1
    not_read_unqualified = 2
    read_but_not_found   = 3
    read_but_blocked     = 4
    t682z_missing        = 5
    t681v_missing        = 6
    t681z_missing        = 7
    mva_error            = 8
    OTHERS               = 9.

IF sy-subrc <> 0.
  DATA: subrc TYPE syst-subrc.
  subrc = sy-subrc.
  cl_demo_output=>display_data( subrc ).
ELSE.
  cl_demo_output=>display_data( condition_records ).
ENDIF.

FIELD-SYMBOLS: <cond> LIKE LINE OF condition_records.

READ TABLE condition_records ASSIGNING <cond>
                             INDEX 1.
IF sy-subrc = 0.

ENDIF.

DATA: pi_t_kschl TYPE STANDARD TABLE OF wpkschl,
      px_t_xvake TYPE STANDARD TABLE OF vakekond,
      pe_t_vake  TYPE STANDARD TABLE OF vakekond,
      pe_t_scale TYPE STANDARD TABLE OF condscale,
      pe_i_vake  TYPE vakekond.

CALL FUNCTION 'CONDITION_RECORD_READ'
  EXPORTING
*   pi_kvewe        = 'A'
    pi_kappl        = 'V'
    pi_kschl        = 'JCST'
*   pi_kotabnr      = pi_kotabnr
*   pi_bufrd        = pi_bufrd
    pi_i_komk       = header_comm_area
    pi_i_komp       = position_comm_area
*   pi_scale_read   = pi_scale_read    " Kennz.: Staffeln zur Kondition lesen
*   pi_kalsm        = pi_kalsm    " Schema (Preisfindung, Nachrichten, Kontenfindung, ...)
  IMPORTING
    pe_i_vake       = pe_i_vake    " Schema (Preisfindung, Nachrichten, Kontenfindung, ...)
  TABLES
    pi_t_kschl      = pi_t_kschl    " POS-Schnittstelle: Tabelle für Konditionsarten
    px_t_xvake      = px_t_xvake    " Struktur aus VAKE und Konditionssatz
    pe_t_vake       = pe_t_vake    " Struktur aus VAKE und Konditionssatz
    pe_t_scale      = pe_t_scale    " Tabelle mit Staffeln
  EXCEPTIONS
    no_record_found = 1
    OTHERS          = 2.

IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
