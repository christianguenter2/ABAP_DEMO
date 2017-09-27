REPORT zz_extract_subtable.

TYPES: arbg_tab TYPE STANDARD TABLE OF t100-arbgb WITH NON-UNIQUE DEFAULT KEY,
       t100_tab TYPE STANDARD TABLE OF t100 WITH NON-UNIQUE KEY arbgb
                     WITH NON-UNIQUE SORTED KEY secondary_key COMPONENTS arbgb.

* Extract a standard table of elementary line type arbgb
* from a table of some 100 t100 rows
* containing the distinct values that appear in column t100-arbgb

* Different ways to do it, with old and with new syntax.

START-OF-SELECTION.
  PERFORM start.

* ---
FORM start.

  DATA: lt_t100 TYPE t100_tab,
        lt_arbg TYPE arbg_tab.

  SELECT * UP TO 1000 ROWS FROM t100 INTO TABLE lt_t100.

  PERFORM with_collect USING lt_t100 CHANGING lt_arbg.
  PERFORM add_result USING `with collect` lt_arbg.

  PERFORM with_explicit_aux_hash USING lt_t100 CHANGING lt_arbg.
  PERFORM add_result USING `with auxiliary hash` lt_arbg.

  PERFORM with_value_for USING lt_t100 CHANGING lt_arbg.
  PERFORM add_result USING `with value for` lt_arbg.

  PERFORM with_reduce_for USING lt_t100 CHANGING lt_arbg.
  PERFORM add_result USING `with reduce for` lt_arbg.

  PERFORM with_grouping USING lt_t100 CHANGING lt_arbg.
  PERFORM add_result USING `with grouping` lt_arbg.

  PERFORM with_filter USING lt_t100 CHANGING lt_arbg.
  PERFORM add_result USING `with filter` lt_arbg.

  cl_demo_output=>display(  ).

ENDFORM.

FORM with_collect USING it_t100 TYPE t100_tab
                  CHANGING ct_arbgb TYPE arbg_tab.
  CLEAR ct_arbgb.
  LOOP AT it_t100 ASSIGNING FIELD-SYMBOL(<t100>).
    COLLECT <t100>-arbgb INTO ct_arbgb.
  ENDLOOP.

ENDFORM.

FORM with_explicit_aux_hash USING it_t100 TYPE t100_tab
                            CHANGING ct_arbgb TYPE arbg_tab.

  DATA: lt_arbg TYPE HASHED TABLE OF t100-arbgb WITH UNIQUE KEY table_line.

  LOOP AT it_t100 ASSIGNING FIELD-SYMBOL(<t100>).
    INSERT <t100>-arbgb INTO TABLE lt_arbg.
  ENDLOOP.
  ct_arbgb = lt_arbg.

ENDFORM.

FORM with_value_for USING it_t100 TYPE t100_tab
                    CHANGING ct_arbgb TYPE arbg_tab.

  ct_arbgb = VALUE #( FOR <t100> IN it_t100
                      ( <t100>-arbgb ) ). " Inserting into a hash table will dump on multiple values.
  " No way to avoid this
  SORT ct_arbgb.
  DELETE ADJACENT DUPLICATES FROM ct_arbgb.

ENDFORM.

FORM with_reduce_for USING it_t100 TYPE t100_tab
                     CHANGING ct_arbgb TYPE arbg_tab.

  ct_arbgb = REDUCE #( INIT names TYPE arbg_tab
                       FOR <t100> IN it_t100
                       NEXT names =
                         COND #(
                           WHEN line_exists( names[ table_line = <t100>-arbgb ] ) THEN names
                           ELSE VALUE #( BASE names ( <t100>-arbgb ) ) ) ).
ENDFORM.

FORM with_grouping USING it_t100 TYPE t100_tab
                   CHANGING ct_arbgb TYPE arbg_tab.

  CLEAR ct_arbgb.
  LOOP AT it_t100 ASSIGNING FIELD-SYMBOL(<t100>)
                  GROUP BY <t100>-arbgb
                  ASCENDING
                  ASSIGNING FIELD-SYMBOL(<lv_group>).
    APPEND <lv_group> TO ct_arbgb.
  ENDLOOP.

ENDFORM.

FORM with_filter USING it_t100 TYPE t100_tab
                 CHANGING ct_arbgb TYPE arbg_tab.

  CLEAR ct_arbgb.

  DATA: lt_arbg TYPE HASHED TABLE OF t100-arbgb WITH UNIQUE KEY table_line.

  lt_arbg = VALUE #( FOR <t100> IN it_t100
                      ( <t100>-arbgb ) ). " Inserting into a hash table will dump on multiple values.

ENDFORM.

FORM add_result USING iv_name TYPE string
                      it_arbgb TYPE arbg_tab.
  cl_demo_output=>write_data( name = iv_name value = concat_lines_of( table = it_arbgb sep = ',' ) ).
ENDFORM.
