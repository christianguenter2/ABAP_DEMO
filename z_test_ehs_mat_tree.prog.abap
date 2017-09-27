REPORT z_test_ehs_mat_tree.

PARAMETERS: mat_no TYPE z_ehs_mat_no OBLIGATORY,
            version TYPE z_ehs_mat_version OBLIGATORY.

*----------------------------------------------------------------------*
*       CLASS lcl_test_tree DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_tree DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_instance) TYPE REF TO lcl_test_tree.

    METHODS: start,
             pbo.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: instance TYPE REF TO lcl_test_tree.
    DATA:       lo_mat   TYPE REF TO zcl_ehs_material,
                lo_tree  TYPE REF TO zcl_ehs_material_tree.
ENDCLASS.                    "lcl_test_tree DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_test_tree IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_tree IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance.
    ENDIF.

    r_instance = instance.
  ENDMETHOD.                    "get_instance

  METHOD start.
    CALL SCREEN 0100.
  ENDMETHOD.                    "start

  METHOD pbo.
    DATA: error  TYPE REF TO zcx_ehs_material.

    TRY.
        CREATE OBJECT lo_mat
          EXPORTING
            i_mat_no      = mat_no    " HG: EHS - Werkstoffe
            i_mat_version = version.    " HG: EHS - Version

        lo_mat->read_material( ).
      CATCH zcx_ehs_material INTO error.    " Ausnahmeklasse Werkstoffe
        MESSAGE error TYPE 'I'.
        RETURN.
    ENDTRY.

    CREATE OBJECT lo_tree.

    "lo_tree->create_control_menu( ).
    lo_tree->build_tree( lo_mat ).
  ENDMETHOD.                    "pbo
ENDCLASS.                    "lcl_test_tree IMPLEMENTATION

START-OF-SELECTION.
  lcl_test_tree=>get_instance( )->start( ).
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  lcl_test_tree=>get_instance( )->pbo( ).
ENDMODULE.                 " STATUS_0100  OUTPUT
