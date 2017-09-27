REPORT z_test_reca_gui.


DATA:
*      id_key_field           TYPE recafieldname,
*      it_key_field           TYPE re_t_fieldname,
      it_f4value              TYPE STANDARD TABLE OF vbak,
*      it_mark                TYPE standard table,
*      it_hide_field          TYPE re_t_fieldname,
*      it_sort_field          TYPE re_t_fieldname,
*      if_multi               TYPE abap_bool,
*      id_checkbox_field      TYPE recafieldname,
*      id_checkbox_field_ref  TYPE recafieldname,
*      id_title               TYPE csequence,
*      id_repid               TYPE syrepid,
*      id_repid_handle        TYPE salv_s_layout_key-handle,
*      if_readonly            TYPE abap_bool,
*      if_classic_layout      TYPE abap_bool,
*      if_layout_default      TYPE abap_bool value abap_true,
      ro_f4_instance          TYPE REF TO cl_reca_gui_f4_popup.


SELECT * FROM vbak INTO TABLE it_f4value
                   UP TO 10 ROWS.

cl_reca_gui_f4_popup=>factory_grid(
  EXPORTING
*    id_key_field          = id_key_field    " Feldname: Ergebnisschlüssel
*    it_key_field          = it_key_field    " Feldnamen: Mehrere Ergebnisschlüssel
    it_f4value            = it_f4value    " Tabelle mit Werten
*    it_mark               = it_mark    " Tabelle der vormarkierten Zeile(n)
*    it_hide_field         = it_hide_field    " Feldnamen: Versteckte Felder
*    it_sort_field         = it_sort_field    " Feldnamen: Sortierung (Aufwärts!)
*    if_multi              = if_multi    " Bool: Multi-Select
*    id_checkbox_field     = id_checkbox_field    " Feldname: Zusätzliche Checkbox
*    id_checkbox_field_ref = id_checkbox_field_ref    " Feldname: Checkbox nur aktiv, wenn REF gefüllt
*    id_title              = id_title    " Titel des F4-Popups
*    id_repid              = id_repid    " REPID: Falls Varianten gewünscht werden
*    id_repid_handle       = id_repid_handle    " VerwaltungsID für Mehrfachaufrufe aus dem selben Programm
*    if_readonly           = if_readonly    " Bool: Nur Anzeigemodus
*    if_classic_layout     = if_classic_layout    " Bool: Als klassische Liste ausgeben
*    if_layout_default     = ABAP_TRUE    " Bool: Klassich/Control F4 Default verwenden
  RECEIVING
    ro_f4_instance        = ro_f4_instance    " F4 Hilfe Instanz
).

ro_f4_instance->display( ).
