REPORT z_test_html_text.

TABLES: stxh.

SELECT-OPTIONS: s_obj   FOR stxh-tdobject,
                s_name  FOR stxh-tdname,
                s_id    FOR stxh-tdid,
                s_spras FOR stxh-tdspras.

DATA: error TYPE REF TO zcx_lo_error.

TRY.
    zcl_bc_text_functions=>show_texts_as_popup(
      EXPORTING
        name_ranges     = s_name[]
        object_ranges   = s_obj[]
        id_ranges       = s_id[]
        language_ranges = s_spras[] ).

  CATCH zcx_lo_error INTO error.
    MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
