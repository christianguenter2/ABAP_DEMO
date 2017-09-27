CLASS zcl_2017_03_01_selections DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          !i_current_report TYPE rsvar-report OPTIONAL
        RAISING
          zcx_2017_03_01,

      get
        IMPORTING
          !i_name           TYPE csequence
        EXPORTING
          !et_select_option TYPE ANY TABLE
          e_parameter       TYPE any
        RAISING
          zcx_2017_03_01 .

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_selection,
        name  TYPE string,
        kind  TYPE rsscr_kind,
        value TYPE REF TO data,
      END OF ty_selection,

      tty_selection TYPE HASHED TABLE OF ty_selection
                                WITH UNIQUE KEY name .

    DATA: mt_selections TYPE tty_selection .

ENDCLASS.

CLASS zcl_2017_03_01_selections IMPLEMENTATION.

  METHOD constructor.

    DATA: selection_table TYPE STANDARD TABLE OF rsparams.

    DATA(current_report) = COND #( WHEN i_current_report IS SUPPLIED THEN i_current_report
                                   ELSE cl_abap_syst=>get_current_program( ) ).

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = current_report    " Programm für den Sel. angezeigt werden sollen
      TABLES
        selection_table = selection_table    " Tabelle mit Ranges-Struktur die Sel. enthält.
      EXCEPTIONS
        OTHERS          = 3.

    IF sy-subrc <> 0.
      zcx_2017_03_01=>raise_syst( ).
    ENDIF.

    LOOP AT selection_table ASSIGNING FIELD-SYMBOL(<selection>).

      DATA(fname) = SWITCH #( <selection>-kind
                      WHEN 'P' THEN |({ current_report }){ <selection>-selname }|
                      WHEN 'S' THEN |({ current_report }){ <selection>-selname }[]|
                      ELSE THROW zcx_2017_03_01( text = |Invalid type kind { <selection>-kind }| ) ).

      ASSIGN (fname) TO FIELD-SYMBOL(<value>).
      ASSERT sy-subrc = 0.

      INSERT VALUE #( name  = <selection>-selname
                      kind  = <selection>-kind
                      value = REF #( <value> ) )
             INTO TABLE mt_selections.

    ENDLOOP.

  ENDMETHOD.

  METHOD get.

    ASSIGN mt_selections[ name = i_name ] TO FIELD-SYMBOL(<selection>).
    IF sy-subrc <> 0.
      zcx_2017_03_01=>raise_text( |Invalid Select-Option/Paramter { i_name }| ).
    ENDIF.

    ASSIGN <selection>-value->* TO FIELD-SYMBOL(<value>).
    ASSERT sy-subrc = 0.

    CASE <selection>-kind.
      WHEN 'S'.
        et_select_option = <value>.
      WHEN 'P'.
        e_parameter = <value>.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
