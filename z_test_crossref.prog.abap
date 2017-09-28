*&---------------------------------------------------------------------*
*& Report z_test_crossref
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_crossref.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: run.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    TYPES: BEGIN OF ty_ddls_name.
        INCLUDE TYPE ddsymtab.
    TYPES: END OF ty_ddls_name,
    tty_ddls_names TYPE STANDARD TABLE OF ty_ddls_name
                      WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_s_masdepwa,
             depname   TYPE dd02l-tabname,
             deptyp(4),
             deplocal  TYPE dd02l-as4local,  "Version der abhängigen
             refname   TYPE dd02l-tabname,
             reftyp(4),
             kind(1),
           END OF ty_s_masdepwa.

    DATA(ddls_names) = VALUE tty_ddls_names( ( CONV #( 'ZI_TE_FLIGHT' ) ) ).

    DATA: deptab TYPE STANDARD TABLE OF ty_s_masdepwa
                         WITH NON-UNIQUE DEFAULT KEY.

    PERFORM ('DDLS_GET_DEP') IN PROGRAM ('RADMASDL') TABLES ddls_names deptab.

    cl_demo_output=>display( deptab ).

*
*    DATA: lv_find_obj_cls TYPE euobj-id,
*          lt_findstrings  TYPE TABLE OF rsfind,
*          lt_founds       TYPE STANDARD TABLE OF rsfindlst,
*          lt_scope        TYPE STANDARD TABLE OF seu_obj.
*
*    APPEND 'ZI_TE_FLIGHT' TO lt_findstrings.
*    lv_find_obj_cls = 'DDLS'.
*    APPEND 'DDLS' TO lt_scope.
*
*    CALL FUNCTION 'RS_EU_CROSSREF'
*      EXPORTING
*        i_find_obj_cls           = lv_find_obj_cls
*      TABLES
*        i_findstrings            = lt_findstrings
*        o_founds                 = lt_founds
*        i_scope_object_cls       = lt_scope
*      EXCEPTIONS
*        not_executed             = 1
*        not_found                = 2
*        illegal_object           = 3
*        no_cross_for_this_object = 4
*        batch                    = 5
*        batchjob_error           = 6
*        wrong_type               = 7
*        object_not_exist         = 8
*        OTHERS                   = 9.
*
*    cl_demo_output=>display( lt_founds ).
*
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
