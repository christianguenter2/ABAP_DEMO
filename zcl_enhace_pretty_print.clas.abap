class ZCL_ENHACE_PRETTY_PRINT definition
  public
  final
  create public .

public section.

  type-pools SEDI .
  class-methods PRETTY_PRINT
    changing
      !CT_SOURCE type SEDI_SOURCE .
protected section.
private section.

  class-methods DELETE_WHITESPACE
    importing
      !I_COUNT type I
      !I_MAX_L_POS type I
    changing
      !C_TEXT type STRING .
  class-methods INSERT_WHITESPACE
    importing
      !I_COUNT type I
    changing
      !C_TEXT type CSEQUENCE .
ENDCLASS.



CLASS ZCL_ENHACE_PRETTY_PRINT IMPLEMENTATION.


METHOD delete_whitespace.
  DATA: lv_eq_pos TYPE i.

  lv_eq_pos = find( val = c_text
                    sub = `=` ).

  c_text = replace( val  = c_text
                    off  = lv_eq_pos - ( lv_eq_pos - i_max_l_pos ) + 1
                    len  = i_count
                    with = `` ).
ENDMETHOD.


METHOD insert_whitespace.
  DATA: whitespace TYPE string.

  DO i_count TIMES.
    whitespace = whitespace && ` `.
  ENDDO.

  whitespace = whitespace && `=`.

  c_text = replace( val  = c_text
                    sub  = `=`
                    with = whitespace ).

ENDMETHOD.


METHOD pretty_print.
  TYPES: BEGIN OF ty_data,
           index     TYPE i,
           s         TYPE string,
           pos       TYPE i,
           max_l_pos TYPE i,
           changed   TYPE abap_bool,
         END OF ty_data.

  DATA: lt_data    TYPE STANDARD TABLE OF ty_data,
        lv_data    LIKE LINE OF lt_data,
        save_tabix TYPE i.

  DATA: whitespace TYPE string,
        lv_eq_pos  TYPE i.

  FIELD-SYMBOLS: <source> LIKE LINE OF ct_source,
                 <data>   LIKE LINE OF lt_data,
                 <data2>  LIKE LINE OF lt_data.

  DATA: lt_tokens TYPE stokesx_tab,
        lt_stm    TYPE sstmnt_tab.

  FIELD-SYMBOLS: <stm>   LIKE LINE OF lt_stm,
                 <token> LIKE LINE OF lt_tokens.

  SCAN ABAP-SOURCE ct_source
       TOKENS      INTO lt_tokens
       STATEMENTS  INTO lt_stm
       WITH ANALYSIS WITH COMMENTS.

  LOOP AT ct_source ASSIGNING <source>.
    lv_data-index = sy-tabix.
    lv_data-s     = <source>.
    lv_data-pos   = find( regex = `[^\s]\s+=`
                          val   = lv_data-s ).
    INSERT lv_data INTO TABLE lt_data.
  ENDLOOP.

  LOOP AT lt_data ASSIGNING <data>.
    save_tabix = sy-tabix.

    IF <data>-pos = -1.
      CONTINUE.
    ENDIF.

    WHILE sy-subrc = 0.
      READ TABLE lt_data ASSIGNING <data2>
                         INDEX save_tabix - sy-index.
      IF sy-subrc = 0.
        <data>-max_l_pos = nmax( val1 = <data>-pos
                                 val2 = <data2>-pos
                                 val3 = <data>-max_l_pos ).
      ENDIF.
    ENDWHILE.

    LOOP AT lt_data ASSIGNING <data2> FROM save_tabix.
      IF <data2>-pos = -1.
        EXIT.
      ENDIF.
      <data>-max_l_pos = nmax( val1 = <data>-pos
                               val2 = <data2>-pos
                               val3 = <data>-max_l_pos ).
    ENDLOOP.
  ENDLOOP.

  DATA: valid TYPE abap_bool.

  LOOP AT lt_data ASSIGNING <data>
                  WHERE max_l_pos IS NOT INITIAL
                  AND   pos <> -1
                  AND   changed = abap_false.
    CLEAR: whitespace, valid.

    LOOP AT lt_stm ASSIGNING <stm>
                   WHERE type = 'A'.
      LOOP AT lt_tokens ASSIGNING <token>
                        FROM <stm>-from
                        TO   <stm>-to.
        IF <token>-row = <data>-index.
          valid = abap_true.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    CHECK valid = abap_true.

    lv_eq_pos = find( val = <data>-s
                      sub = `=` ).

    IF <data>-max_l_pos - lv_eq_pos >= -1.
      insert_whitespace( EXPORTING i_count = <data>-max_l_pos - lv_eq_pos + 2
                         CHANGING  c_text  = <data>-s ).
    ELSE.
      delete_whitespace( EXPORTING i_count = nmax( val1 = lv_eq_pos - <data>-max_l_pos - 2
                                                   val2 = 0 )
                                   i_max_l_pos = <data>-max_l_pos
                         CHANGING  c_text  = <data>-s ).
    ENDIF.
  ENDLOOP.

  LOOP AT ct_source ASSIGNING <source>.
    save_tabix = sy-tabix.
    READ TABLE lt_data ASSIGNING <data>
                       WITH KEY index = save_tabix.
    IF sy-subrc = 0.
      <source> = <data>-s.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
