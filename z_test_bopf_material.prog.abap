*&---------------------------------------------------------------------*
*& Report  Z_TEST_BOPF_MATERIAL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_bopf_material.

PARAMETER: p_mat_no TYPE z_ehs_mat_no OBLIGATORY DEFAULT '00000001',
           p_mat_v  TYPE z_ehs_mat_version OBLIGATORY DEFAULT '01'.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS: start.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: lo_bopf_model TYPE REF TO zcl_bopf_material_model,
          lo_error      TYPE REF TO /bobf/cx_frw.

    TRY.
        CREATE OBJECT lo_bopf_model.

        lo_bopf_model->load(
          EXPORTING
            i_mat_no      = p_mat_no
            i_mat_version = p_mat_v ).

        lo_bopf_model->set_mat_txt( |{ sy-timlo }| ).
        lo_bopf_model->save( ).
      CATCH /bobf/cx_frw INTO lo_error.
        lo_bopf_model->display_messages( lo_error->mo_message ).
        RETURN.
    ENDTRY.

    DATA: text TYPE string.
    text = |{ lo_bopf_model->root-mat_no }\n{ lo_bopf_model->root-mat_version }\n| &&
           |{ lo_bopf_model->root-mat_txt }|.
    WRITE: / text.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
