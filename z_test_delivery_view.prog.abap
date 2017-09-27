*&---------------------------------------------------------------------*
*& Report  Z_TEST_DELIVERY_VIEW
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_delivery_view.

DATA: comwa      TYPE vbco3,
      vbdkl_tab  TYPE TABLE OF vbdkl,
      vbdpl_tab  TYPE TABLE OF vbdpl,
      vbplk_tab  TYPE TABLE OF vbplk,
      vbplp_tab  TYPE TABLE OF vbplp,
      vbpls_tab  TYPE TABLE OF vbpls,
      vbdkl_tab2 TYPE TABLE OF vbdkl,
      vbdpl_tab2 TYPE TABLE OF vbdpl,
      vbplk_tab2 TYPE TABLE OF vbplk,
      vbplp_tab2 TYPE TABLE OF vbplp,
      vbpls_tab2 TYPE TABLE OF vbpls.

*comwa-tknum = '0001520430'.
comwa-vbeln = '0087891948'.
comwa-packd = abap_true.

CALL FUNCTION 'SD_DELIVERY_VIEW'
  EXPORTING
    comwa            = comwa
    edi              = abap_true
  TABLES
    vbdkl_tab        = vbdkl_tab    " EDI-Verarbeitung
    vbdpl_tab        = vbdpl_tab    " EDI-Verarbeitung
    vbplk_tab        = vbplk_tab    " EDI-Verarbeitung
    vbplp_tab        = vbplp_tab    " EDI-Verarbeitung
    vbpls_tab        = vbpls_tab    " EDI-Verarbeitung
  EXCEPTIONS
    object_not_found = 1
    OTHERS           = 2.

comwa-vbeln = '0087897242'.
comwa-packd = abap_true.

CALL FUNCTION 'SD_DELIVERY_VIEW'
  EXPORTING
    comwa            = comwa
    edi              = abap_true
  TABLES
    vbdkl_tab        = vbdkl_tab2    " EDI-Verarbeitung
    vbdpl_tab        = vbdpl_tab2    " EDI-Verarbeitung
    vbplk_tab        = vbplk_tab2    " EDI-Verarbeitung
    vbplp_tab        = vbplp_tab2    " EDI-Verarbeitung
    vbpls_tab        = vbpls_tab2    " EDI-Verarbeitung
  EXCEPTIONS
    object_not_found = 1
    OTHERS           = 2.

DATA: nast            TYPE nast,
      vttk            TYPE vttkvb,
      materi          TYPE brgew,
      t_zsummenblatt  TYPE TABLE OF zsummenblatt,
      t_zsummenblatt2 TYPE TABLE OF zsummenblatt2,
      t_zsummenblatt4 TYPE TABLE OF zsummenblatt4,
      t_zsummenblatt3 TYPE TABLE OF zsummenblatt3.

APPEND LINES OF: vbdkl_tab2 TO vbdkl_tab,
                 vbdpl_tab2 TO vbdpl_tab,
                 vbplk_tab2 TO vbplk_tab,
                 vbplp_tab2 TO vbplp_tab,
                 vbpls_tab2 TO vbpls_tab.

CALL FUNCTION 'Z_SUMMEN_BLATT'
  EXPORTING
    i_nast          = nast    " Nachrichtenstatus
    i_vttk          = vttk    " Transportkopf
  IMPORTING
    p_materi        = materi    " Bruttogewicht
  TABLES
    tvbplk          = vbplk_tab    " Handling Unit Kopfdaten Komunikationsstruktur
    t_zsummenblatt  = t_zsummenblatt    " HG: Übergabestruktur Replenishment für SmartForms Kundenauft
    t_zsummenblatt2 = t_zsummenblatt2    " HG: Summenblatt Packete auf Transportbelegen
    t_zsummenblatt4 = t_zsummenblatt4    " HG: Summenblatt Anzahl Kartons ZD Partner auf Packliste
    t_zsummenblatt3 = t_zsummenblatt3    " HG: Summenblatt Verladen auf TRansport
    tvbdkl          = vbdkl_tab    " Dokumentenkopfview Lieferschein
    tvbplp          = vbplp_tab.    " Handling Unit Inhaltsdaten Kommunikationsstruktur

cl_demo_output=>display_data( materi ).
