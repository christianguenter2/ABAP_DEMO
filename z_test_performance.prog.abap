REPORT z_test_performance.

TYPES: re_st_tabix TYPE SORTED TABLE OF sytabix
                        WITH UNIQUE KEY table_line.

TYPES: BEGIN OF huge_tab_line_type,
         tab TYPE re_st_tabix,
       END OF huge_tab_line_type,

       huge_tab_type TYPE STANDARD TABLE OF huge_tab_line_type.

CONSTANTS big_number TYPE int4 VALUE '10000'.

DATA: huge_tab      TYPE huge_tab_type,
      huge_tab_line TYPE huge_tab_line_type,
      tmp_tab       TYPE huge_tab_line_type-tab,
      t1            TYPE int4,
      t2            TYPE int4,
      t3            TYPE int4.

huge_tab_line-tab = VALUE #( FOR i = 0 WHILE i < big_number ( i ) ).

huge_tab = VALUE #( FOR j = 0 WHILE j < big_number ( huge_tab_line ) ).

CLEAR huge_tab_line.

GET RUN TIME FIELD t1.

LOOP AT huge_tab ASSIGNING FIELD-SYMBOL(<huge_tab_line>).
  tmp_tab = <huge_tab_line>-tab.
ENDLOOP.

GET RUN TIME FIELD t2.

LOOP AT huge_tab INTO huge_tab_line.
  tmp_tab = huge_tab_line-tab.
ENDLOOP.

GET RUN TIME FIELD t3.

WRITE: / 'FS: ', |{ ( t2 - t1 ) }|, 'microsecs'.
WRITE: / 'WA: ', |{ ( t3 - t2 ) }|, 'microsecs'.
