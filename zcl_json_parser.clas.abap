class ZCL_JSON_PARSER definition
  public
  final
  create public .

*"* public components of class ZCL_JSON_PARSER
*"* do not include other source files here!!!
public section.

  types:
    begin of zut_hash_element,
           key type string.
           include type zut_data as value.
  types: end of zut_hash_element .
  types:
    zut_hash_tab type hashed table of zut_hash_element
          with unique key key .
  types:
    zut_array_tab
          type standard table of zut_data .

  methods PARSE
    importing
      !IV_JSON type STRING
    returning
      value(ES_DATA) type ZUT_DATA
    raising
      ZCX_PARSE_ERROR .
protected section.
*"* protected components of class ZCL_JSON_PARSER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_JSON_PARSER
*"* do not include other source files here!!!

methods MAP_CHAR
    importing
      !IV_CHAR type CSEQUENCE
    returning
      value(EV_MAPPED_CHAR) type CHAR01 .
  methods GET_ANY
    importing
      !IV_JSON type STRING
    exporting
      !ES_DATA type ZUT_DATA
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_ELEMENT
    importing
      !IV_JSON type STRING
    exporting
      !ES_ELEMENT type ZUT_HASH_ELEMENT
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_ARRAY
    importing
      !IV_JSON type STRING
    exporting
      !ET_ARRAY type ZUT_ARRAY_TAB
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_HASH
    importing
      !IV_JSON type STRING
    exporting
      !ET_HASH type ZUT_HASH_TAB
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_KEY
    importing
      !IV_JSON type STRING
    exporting
      !EV_KEY type STRING
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_NAME
    importing
      !IV_JSON type STRING
    exporting
      !EV_NAME type STRING
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_NUMBER
    importing
      !IV_JSON type STRING
    exporting
      !EV_NUMBER type ref to DATA
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_BOOLEAN
    importing
      !IV_JSON type STRING
    exporting
      !EV_BOOLEAN type FLAG
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_STRING
    importing
      !IV_JSON type STRING
      !IV_DELIMITER type CHAR01 optional
    exporting
      !EV_STRING type STRING
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_SYMBOL
    importing
      !IV_JSON type STRING
      !IV_SYMBOL type CSEQUENCE
    exporting
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods SKIP_WHITESPACE
    importing
      !IV_JSON type STRING
    changing
      !CV_POS type I .
ENDCLASS.



CLASS ZCL_JSON_PARSER IMPLEMENTATION.


method GET_ANY.
  data: ls_data      type zut_data,
        lv_pos       type i.

  field-symbols: <lv_boolean> type flag,
                 <lv_string>  type string,
                 <lt_hash>    type zut_hash_tab,
                 <lt_array>   type zut_array_tab.

  ev_found = space.

  lv_pos = cv_pos.

  skip_whitespace( exporting iv_json = iv_json
                   changing  cv_pos  = lv_pos ).
  check lv_pos < strlen( iv_json ).

  case iv_json+lv_pos(1).
    when '"' or `'`.
      create data ls_data-data type string.
      assign ls_data-data->* to <lv_string>.
      ls_data-type = 'S'.
      call method get_string
        exporting
          iv_json   = iv_json
        importing
          ev_string = <lv_string>
          ev_found  = ev_found
        changing
          cv_pos    = lv_pos.

    when '{'.
      create data ls_data-data type zut_hash_tab.
      assign ls_data-data->* to <lt_hash>.
      ls_data-type = 'h'.
      call method get_hash
        exporting
          iv_json  = iv_json
        importing
          et_hash  = <lt_hash>
          ev_found = ev_found
        changing
          cv_pos   = lv_pos.

    when '['.
      create data ls_data-data type zut_array_tab.
      assign ls_data-data->* to <lt_array>.
      ls_data-type = 'a'.
      call method get_array
        exporting
          iv_json  = iv_json
        importing
          et_array = <lt_array>
          ev_found = ev_found
        changing
          cv_pos   = lv_pos.

    when 't' or 'f'.
      create data ls_data-data type flag.
      assign ls_data-data->* to <lv_boolean>.
      ls_data-type = 'B'.
      call method get_boolean
        exporting
          iv_json    = iv_json
        importing
          ev_boolean = <lv_boolean>
          ev_found   = ev_found
        changing
          cv_pos     = lv_pos.

    when others.
      if iv_json+lv_pos(1) ca '0123456789-'.
        ls_data-type = 'N'.
        call method get_number
          exporting
            iv_json   = iv_json
          importing
            ev_number = ls_data-data
            ev_found  = ev_found
          changing
            cv_pos    = lv_pos.
      endif.
  endcase.

  if ev_found = 'X'.
    cv_pos  = lv_pos.
    es_data = ls_data.
  endif.

endmethod.


method GET_ARRAY.
  data: ls_element  type zut_data,
        lv_pos      type i,
        lv_found    type flag,
        lv_key      type string.

  clear ev_found.
  lv_pos = cv_pos.


  get_symbol( exporting iv_json = iv_json
                        iv_symbol = '['
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

  while lv_found eq 'X'.

    call method get_any
      exporting
        iv_json  = iv_json
      importing
        es_data  = ls_element
        ev_found = lv_found
      changing
        cv_pos   = lv_pos.

    if lv_found = 'X'.

      insert ls_element into table et_array.

      call method get_symbol
        exporting
          iv_json   = iv_json
          iv_symbol = ','
        importing
          ev_found  = lv_found
        changing
          cv_pos    = lv_pos.

    endif.

  endwhile.


  get_symbol( exporting iv_json = iv_json
                        iv_symbol = ']'
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

  ev_found = 'X'.
  cv_pos = lv_pos.
endmethod.


method GET_BOOLEAN.
  DATA: lv_symbol TYPE string,
         lv_boolean TYPE flag.


  CHECK cv_pos < STRLEN( iv_json ).

  CASE iv_json+cv_pos(1).
    WHEN 't'.
      lv_symbol = 'true'.
      lv_boolean = 'X'.
    WHEN 'f'.
      lv_symbol = 'false'.
      lv_boolean = space.
  ENDCASE.

  CALL METHOD get_symbol
    EXPORTING
      iv_json   = iv_json
      iv_symbol = lv_symbol
    IMPORTING
      ev_found  = ev_found
    CHANGING
      cv_pos    = cv_pos.

  IF ev_found = 'X'.
    ev_boolean = lv_boolean.
  ENDIF.
endmethod.


method GET_ELEMENT.
  data: lv_pos     type i,
        lv_found   type flag,
        ls_element type zut_hash_element.

  clear: ev_found, es_element.

  lv_pos = cv_pos.

* Schlüssel
  call method get_key
    exporting
      iv_json  = iv_json
    importing
      ev_key   = ls_element-key
      ev_found = lv_found
    changing
      cv_pos   = lv_pos.
  check lv_found eq 'X'.

* Doppelpunkt
  get_symbol( exporting iv_json = iv_json
                        iv_symbol = ':'
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

* Wert
  call method get_any
    exporting
      iv_json  = iv_json
    importing
      es_data  = ls_element-value
      ev_found = lv_found
    changing
      cv_pos   = lv_pos.

  if lv_found = 'X'.
    ev_found   = 'X'.
    cv_pos     = lv_pos.
    es_element = ls_element.
  endif.
endmethod.


method GET_HASH.
    data: ls_element  type zut_hash_element,
        lv_pos      type i,
        lv_found    type flag,
        lv_key      type string.

  clear ev_found.
  lv_pos = cv_pos.


  get_symbol( exporting iv_json = iv_json
                        iv_symbol = '{'
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

  while lv_found eq 'X'.

    call method get_element
      exporting
        iv_json    = iv_json
      importing
        es_element = ls_element
        ev_found   = lv_found
      changing
        cv_pos     = lv_pos.

    if lv_found = 'X'.

      insert ls_element into table et_hash.

      call method get_symbol
        exporting
          iv_json   = iv_json
          iv_symbol = ','
        importing
          ev_found  = lv_found
        changing
          cv_pos    = lv_pos.

    endif.

  endwhile.


  get_symbol( exporting iv_json = iv_json
                        iv_symbol = '}'
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

  ev_found = 'X'.
  cv_pos = lv_pos.
endmethod.


method GET_KEY.
   data: lv_pos          type i,
        lv_found        type flag.

  ev_found = space.
  check cv_pos < strlen( iv_json ).

  lv_pos = cv_pos.

* Ist der Key als String notiert?
  call method get_string
    exporting
      iv_json   = iv_json
    importing
      ev_string = ev_key
      ev_found  = lv_found
    changing
      cv_pos    = lv_pos.

  if lv_found = space.
* Zweiter Versuch: Symbolischer Name
    call method get_name
      exporting
        iv_json  = iv_json
      importing
        ev_name  = ev_key
        ev_found = lv_found
      changing
        cv_pos   = lv_pos.
  endif.

  if lv_found = 'X'.
    ev_found = lv_found.
    cv_pos   = lv_pos.
  endif.
endmethod.


method GET_NAME.
  data: lv_name         type string,
        lv_length       type i.

  ev_found = ev_name = space.
  check cv_pos < strlen( iv_json ).

  find regex '^\s*([a-z_]\w*)' in section offset cv_pos of iv_json
                           submatches lv_name
                           match length lv_length.
  if sy-subrc eq 0.
    ev_found = 'X'.
    cv_pos   = cv_pos + lv_length.
    ev_name  = lv_name.
  endif.

endmethod.


method GET_NUMBER.
   data: lv_pos    type i,
        lv_exp    type string,
        lv_length type i.

  field-symbols: <lv_number> type numeric.

  clear ev_number.
  ev_found = space.
  lv_pos = cv_pos.

  find regex '^\s*([\d.-]+(e-?\d+)?)' in iv_json+lv_pos submatches lv_exp match length lv_length.
  if sy-subrc eq 0.
    add lv_length to lv_pos.
* Ganze Zahl?
    if lv_exp co '0123456789'.
      create data ev_number type i.
    else.
      find regex '^\d*\.\d+|\d+\.\d*$' in lv_exp.
      if sy-subrc eq 0.
        create data ev_number type f.
      endif.
    endif.

    if ev_number is bound.
      assign ev_number->* to <lv_number>.
* Hier überlassen wir die Feinheiten des Parsings dem ABAP-Befehl MOVE:
      <lv_number> = lv_exp.
      ev_found = 'X'.
    endif.

  endif.


  if ev_found = 'X'.
    cv_pos = lv_pos.
  endif.
endmethod.


method GET_STRING.
   data: lv_pos             type i,
        lv_delimiter(1)    type c,
        lv_char(1)         type c,
        lv_mapped_char(1)  type c.

  ev_found = space.

  lv_pos = cv_pos.
  call method skip_whitespace
    exporting
      iv_json = iv_json
    changing
      cv_pos  = lv_pos.

  check lv_pos < strlen( iv_json ).

  if iv_delimiter is not initial.
    lv_delimiter = iv_delimiter.
    check iv_json+lv_pos(1) eq lv_delimiter.
  else.
    lv_delimiter = iv_json+lv_pos(1).
    check lv_delimiter ca `'"`.
  endif.


  do.

    add 1 to lv_pos.

    if strlen( iv_json ) <= lv_pos.
      exit.
    endif.

* Escaped sequences finden und auflösen
    find regex `^\\(['"/bfnrt\\])` in section offset lv_pos of iv_json submatches lv_char.
    if sy-subrc eq 0.
      if lv_char ca `bfnrt`.
        lv_mapped_char = map_char( lv_char ).
      else.
        lv_mapped_char = lv_char.  " Else auch hier, wg. Performance
      endif.
      concatenate ev_string lv_mapped_char into ev_string.
      add 1 to lv_pos.
      continue.
    endif.

    if iv_json+lv_pos(1) eq lv_delimiter.
      ev_found = 'X'.
      exit.
    endif.

    concatenate ev_string iv_json+lv_pos(1) into ev_string.

  enddo.

  if ev_found = 'X'.
    add 1 to lv_pos.
    cv_pos = lv_pos.
  endif.
endmethod.


method GET_SYMBOL.
  clear ev_found.

  skip_whitespace(
    exporting iv_json = iv_json
    changing  cv_pos  = cv_pos ).

  check cv_pos < strlen( iv_json ).

  if iv_json+cv_pos cs iv_symbol and sy-fdpos = 0.
    ev_found = 'X'.
    cv_pos = cv_pos + strlen( iv_symbol ).
  endif.
endmethod.


method MAP_CHAR.
   case iv_char.
    when 'b'.
      ev_mapped_char = cl_abap_char_utilities=>backspace.
    when 'f'.
      ev_mapped_char = cl_abap_char_utilities=>form_feed.
    when 'n'.
      ev_mapped_char = cl_abap_char_utilities=>newline.
    when 'r'.
      ev_mapped_char = cl_abap_char_utilities=>Cr_lf(1).
    when 't'.
      ev_mapped_char = cl_abap_char_utilities=>horizontal_tab.
    when others.
      ev_mapped_char = iv_char.
  endcase.
endmethod.


method PARSE.
  data: lv_pos type i,
        lv_found type flag.

  clear es_data.
  check iv_json is not initial.

* Einen JSON-Ausdruck auswerten
  get_any( exporting iv_json = iv_json
           importing es_data = es_data
                     ev_found = lv_found
           changing  cv_pos  = lv_pos ).

  if lv_found eq space or
* Hintendran darf nichts mehr kommen
    lv_pos < strlen( iv_json ).
    find regex '\S' in section offset lv_pos of iv_json.
    if sy-subrc eq 0.
      raise exception type zcx_parse_error.
    endif.
  endif.
endmethod.


method SKIP_WHITESPACE.
    data: lv_pos type i.

  find regex '(\S|\Z)' in section offset cv_pos of iv_json match offset lv_pos.
  if sy-subrc eq 0.
    cv_pos = lv_pos.
  endif.
endmethod.
ENDCLASS.
