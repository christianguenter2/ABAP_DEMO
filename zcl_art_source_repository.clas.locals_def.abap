*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
class lcl_source_corer definition final create private.

  PUBLIC SECTION.
    interfaces if_art_source_corer.
    CLASS-METHODS create
      RETURNING
        value(r_result) TYPE REF TO lcl_source_corer.

  PRIVATE SECTION.

    methods spaces
      importing
        i_count         type i
      returning
        value(r_result) type string.

endclass.
