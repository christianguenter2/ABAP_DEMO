*"* use this source file for your ABAP unit test classes
*----------------------------------------------------------------------*
*       CLASS lcl_test_bopf_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_test_bopf_model DEFINITION FOR TESTING DURATION SHORT
                                                 RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS: setup RAISING /bobf/cx_frw,
             teardown RAISING /bobf/cx_frw,
             test_create_obligatory FOR TESTING RAISING /bobf/cx_frw,
             test_create_obligatory2 FOR TESTING,
             test_load FOR TESTING,
             test_load_invalid FOR TESTING,
             test_set_mat_txt FOR TESTING RAISING /bobf/cx_frw,
             test_get_material_for_mat_no FOR TESTING RAISING /bobf/cx_frw,
             test_has_changes FOR TESTING RAISING /bobf/cx_frw.

    DATA: lo_bopf_model TYPE REF TO zcl_bopf_material_model.
ENDCLASS.                    "lcl_test_bopf_model DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_test_bopf_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test_bopf_model IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT lo_bopf_model.
  ENDMETHOD.                    "setup

  METHOD test_create_obligatory.                  "test_basic
    DATA: ls_mat TYPE zmat_s_root_d.

    ls_mat-mat_type       = '010'.
    ls_mat-mat_subtype    = '010'.
    ls_mat-mat_expression = '010'.
    lo_bopf_model->create( ls_mat ).
  ENDMETHOD.                    "test_create

  METHOD test_create_obligatory2.
    DATA: ls_mat TYPE zmat_s_root_d.

    TRY.
        lo_bopf_model->create( ls_mat ).
      CATCH /bobf/cx_frw.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.                    "test_create_obligatory2

  METHOD test_get_material_for_mat_no.
    DATA: key TYPE /bobf/conf_key.

    key = lo_bopf_model->get_material_for_mat_no( i_mat_no      = '00000001'
                                                  i_mat_version = '01' ).

    cl_abap_unit_assert=>assert_not_initial( key ).
  ENDMETHOD.                    "test_get_material_for_mat_no


  METHOD test_load.
    lo_bopf_model->load( i_mat_no      = '00000001'
                         i_mat_version = '01' ).

    cl_abap_unit_assert=>assert_not_initial( act = lo_bopf_model->root ).
  ENDMETHOD.                    "test_modify

  METHOD test_load_invalid.
    TRY.
        lo_bopf_model->load( i_mat_no      = '1100001'
                             i_mat_version = '01' ).
      CATCH /bobf/cx_frw.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.                    "test_modify

  METHOD test_set_mat_txt.
    DATA: timlo TYPE syst-timlo.

    timlo = sy-timlo.

    lo_bopf_model->load( i_mat_no      = '00000011'
                         i_mat_version = '01' ).

*    lo_bopf_model->set_head( |{ timlo }| ).
    lo_bopf_model->save( ).

    lo_bopf_model->init( ).
    CLEAR lo_bopf_model.

    CREATE OBJECT lo_bopf_model.
    lo_bopf_model->load( i_mat_no      = '00000011'
                         i_mat_version = '01' ).

    cl_abap_unit_assert=>assert_equals(
            act = lo_bopf_model->root-mat_txt
            exp = timlo ).
  ENDMETHOD.                    "test_modify

  METHOD teardown.
    lo_bopf_model->init( ).
  ENDMETHOD.                    "teardown

  METHOD test_has_changes.
    lo_bopf_model->load( i_mat_no      = '00000011'
                         i_mat_version = '01' ).
*    lo_bopf_model->set_mat_txt( |{ sy-timlo }| ).

    cl_abap_unit_assert=>assert_true( lo_bopf_model->has_changes( ) ).
  ENDMETHOD.                    "test_has_changes
ENDCLASS.                    "lcl_test_bopf_model IMPLEMENTATION
