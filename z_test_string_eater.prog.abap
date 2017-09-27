REPORT z_test_string_eater.

CLASS lcl_string_eater DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor IMPORTING i_text TYPE string.
    METHODS has_next RETURNING VALUE(r_has_next) TYPE abap_bool.
    METHODS next RETURNING VALUE(r_next) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: _text  TYPE string,
          _items TYPE stringtab.

ENDCLASS.

CLASS lcl_string_eater IMPLEMENTATION.
  METHOD constructor.
    _text = i_text.
    SPLIT _text AT ` `
                INTO TABLE _items.
  ENDMETHOD.

  METHOD has_next.
    r_has_next = COND #( WHEN lines( _items ) > 0 THEN abap_true
                         ELSE abap_false ).
  ENDMETHOD.

  METHOD next.
    r_next = _items[ 1 ].
    DELETE _items INDEX 1.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(eater) = NEW lcl_string_eater( `Dies ist ein Test` ).

  WHILE eater->has_next( ).
    cl_demo_output=>write( eater->next( ) ).
  ENDWHILE.

  cl_demo_output=>display( ).
