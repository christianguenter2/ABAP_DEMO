*&---------------------------------------------------------------------*
*& Report  Z_TEST_WHERE_USED
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_where_used.

DATA: i_find_obj_cls     TYPE euobj-id,
      o_scope_obj_cls    TYPE euobj-id,
      i_findstrings      TYPE stringtab,
      o_founds           TYPE STANDARD TABLE OF rsfindlst,
      o_findstrings      TYPE stringtab,
      i_scope_object_cls TYPE stringtab.

INSERT `Z_ERWEITERUNG_AUSFUEHREN` INTO TABLE i_findstrings.

INSERT: `P`  INTO TABLE i_scope_object_cls,
        `YT` INTO TABLE i_scope_object_cls,
        `WO` INTO TABLE i_scope_object_cls,
        `XH` INTO TABLE i_scope_object_cls,
        `XS` INTO TABLE i_scope_object_cls,
        `3I` INTO TABLE i_scope_object_cls,
        `WS` INTO TABLE i_scope_object_cls,
        `KI` INTO TABLE i_scope_object_cls,
        `DH` INTO TABLE i_scope_object_cls,
        `GE` INTO TABLE i_scope_object_cls,
        `C`  INTO TABLE i_scope_object_cls,
        `OM` INTO TABLE i_scope_object_cls.

CALL FUNCTION 'RS_EU_CROSSREF'
  EXPORTING
     i_find_obj_cls                     = 'FUNC'
*     i_scope_obj_cls                    = i_scope_obj_cls
     rekursiv                           = abap_false
*     i_answer                           = i_answer
*     i_actual_include                   = i_actual_include
*     no_dialog                          = no_dialog
*     expand_source_in_batch_mode        = expand_source_in_batch_mode
     expand_source_in_online_mode       = abap_true
*     without_text                       = without_text
*     with_generated_objects             = with_generated_objects
*     i_full_name                        = i_full_name
*     i_scope_objkey                     = i_scope_objkey
* IMPORTING
*     o_scope_obj_cls                    = o_scope_obj_cls
*     o_answer                           = o_answer
*     o_hits                             = o_hits
 TABLES
     i_findstrings                      = i_findstrings
     o_founds                           = o_founds
     o_findstrings                      = o_findstrings
*     i_scope_objects                    = i_scope_objects
*     i_actual_source                    = i_actual_source
     i_scope_object_cls                 = i_scope_object_cls
*     i_scope_devclass                   = i_scope_devclass
*     i_exclude_scope_object_cls         = i_exclude_scope_object_cls
 EXCEPTIONS
     not_executed                       = 1
     not_found                          = 2
     illegal_object                     = 3
     no_cross_for_this_object           = 4
     batch                              = 5
     batchjob_error                     = 6
     wrong_type                         = 7
     object_not_exist                   = 8
     OTHERS                             = 9.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE sy-msgty.
ENDIF.

cl_demo_output=>write_data( o_founds ).
cl_demo_output=>write_data( o_findstrings ).
cl_demo_output=>display( ).
