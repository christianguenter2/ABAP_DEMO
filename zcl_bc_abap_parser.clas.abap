CLASS zcl_bc_abap_parser DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_reference_information,
        tabname   TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
      END OF ty_reference_information .

    METHODS constructor
      IMPORTING
        !i_repid TYPE sy-repid .
    METHODS get_type
      IMPORTING
        !i_structure_name TYPE csequence
        !i_field_name     TYPE csequence
      RETURNING
        VALUE(r_type)     TYPE string .
    METHODS get_reference_information
      IMPORTING
        !i_compound_type               TYPE csequence
      RETURNING
        VALUE(r_reference_information) TYPE ty_reference_information .
    METHODS get_field_for_type
      IMPORTING
        !i_structure_name TYPE csequence
        !i_type           TYPE csequence
      RETURNING
        VALUE(r_field)    TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA source TYPE stringtab .
    DATA tokens TYPE stokes_tab .
    DATA statements TYPE sstmnt_tab .

    METHODS _get_tabix_of_string_in_struct
      IMPORTING
        !i_structure_name             TYPE csequence
        !i_field_name                 TYPE csequence
      RETURNING
        VALUE(r_tabix_of_field_token) TYPE syst-tabix .
ENDCLASS.



CLASS zcl_bc_abap_parser IMPLEMENTATION.


  METHOD constructor.

    TYPES: tty_char10 TYPE STANDARD TABLE OF char10
                           WITH DEFAULT KEY.

    DATA: keywords TYPE tty_char10,
          keyword  LIKE LINE OF keywords.

    READ REPORT i_repid INTO source.
    ASSERT sy-subrc = 0.

    keyword = 'DATA'.
    INSERT keyword INTO TABLE keywords.

    keyword = 'TYPE'.
    INSERT keyword INTO TABLE keywords.

    SCAN ABAP-SOURCE source KEYWORDS FROM keywords
                            WITH DECLARATIONS
                            TOKENS INTO tokens
                            STATEMENTS INTO statements.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_field_for_type.

    DATA: tabix_of_type_token TYPE syst-tabix,
          token               TYPE stokes.

    _get_tabix_of_string_in_struct(
      EXPORTING
        i_structure_name       = i_structure_name
        i_field_name           = i_type
      RECEIVING
        r_tabix_of_field_token = tabix_of_type_token ).

    tabix_of_type_token = tabix_of_type_token - 2.

    READ TABLE tokens INDEX tabix_of_type_token
                      INTO token.
    ASSERT sy-subrc = 0.

    r_field = token-str.

  ENDMETHOD.


  METHOD get_reference_information.

    DATA: tabname   TYPE string,
          fieldname TYPE string.

    FIND FIRST OCCURRENCE OF '-'
         IN i_compound_type.

    ASSERT sy-subrc = 0.

    SPLIT i_compound_type AT '-'
                          INTO tabname fieldname.

    SELECT SINGLE reftable reffield
           FROM dd03l
           INTO r_reference_information
           WHERE tabname   = tabname
           AND   fieldname = fieldname.

    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_type.

    DATA: tabix_of_field_token TYPE syst-tabix,
          type_token           TYPE stokes.

    _get_tabix_of_string_in_struct(
      EXPORTING
        i_structure_name       = i_structure_name
        i_field_name           = i_field_name
      RECEIVING
        r_tabix_of_field_token = tabix_of_field_token ).

    tabix_of_field_token = tabix_of_field_token + 2.

    READ TABLE tokens INTO type_token
                      INDEX tabix_of_field_token.

    ASSERT sy-subrc = 0.

    r_type = type_token-str.

  ENDMETHOD.


  METHOD _get_tabix_of_string_in_struct.

    DATA: token_of_type TYPE stokes.

    FIELD-SYMBOLS: <statement> LIKE LINE OF statements.

    READ TABLE tokens INTO token_of_type
                      WITH KEY str = i_structure_name.

    ASSERT token_of_type IS NOT INITIAL.

    LOOP AT statements ASSIGNING <statement>
                       WHERE from >= token_of_type-row.
      EXIT.
    ENDLOOP.

    ASSERT sy-subrc = 0.

    LOOP AT tokens TRANSPORTING NO FIELDS
                   WHERE row >= <statement>-from
                   AND   row <= <statement>-to
                   AND   str = i_field_name.
      r_tabix_of_field_token  = sy-tabix.
    ENDLOOP.

    ASSERT sy-subrc = 0.

  ENDMETHOD.

ENDCLASS.
