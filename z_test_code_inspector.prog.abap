*&---------------------------------------------------------------------*
*& Report z_test_code_inspector
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_code_inspector.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: run.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

*    DATA: ok        TYPE sychar01,
*          result    TYPE REF TO cl_ci_check_result,

    TRY.

        NEW cl_ci_objectset( )->create(
          EXPORTING
            p_user              = sy-uname
            p_name              = |TEST_CG|
          RECEIVING
            p_ref               = DATA(p_ref)    " Referenz auf Objektmenge
          EXCEPTIONS
            objs_already_exists = 1
            objs_not_exists     = 2
            locked              = 3
            error_in_enqueue    = 4
            not_authorized      = 5
            OTHERS              = 6 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  DISPLAY LIKE sy-msgty.
          RETURN.
        ENDIF.

        p_ref->save_objectset(
          EXPORTING
*            p_text                = p_text    " Code Inspector: Elementtext (Chk, ChkV, ObjM, Inspec)
            p_tadir               = VALUE #( sodevc = VALUE #( ( sign = 'I' option = 'EQ' low = '$tmp' ) ) )
*            p_rest                = p_rest    " Code Inspector: Objekt-Menge aus Ergebnis einer Inspektion
*            p_fugrs               = p_fugrs    " Code inspector: Selektion nach Funktionsgruppe
*            p_class               = p_class    " Code inspector: Selektion nach Klasse/Interface
*            p_repos               = p_repos    " Code inspector: Selektion nach TADIR-Programm
*            p_ddics               = p_ddics    " Code Inspector: Selektion nach DDIC-Objekten
*            p_typps               = p_typps    " Code inspector: Selektion nach Typgruppen
*            p_anyos               = p_anyos    " Code Inspector: Selektionsoptionen für allg. TADIR-Objekt
*            p_wdyns               = p_wdyns    " Code Inspector: Selektion nach Web Dynpro Komponente
*            p_sel_flags           = p_sel_flags    " Code Inspector: Langstruktur für Selektionsflags Objektmenge
*            p_only_selection      = p_only_selection    " CI: nur Selektionsschirm sichern
*            p_deldate             = p_deldate    " Code Inspector:  Löschdatum
          EXCEPTIONS
            no_valid_selection    = 1
            missing_program_param = 2
            not_enqueued          = 3
            not_authorized        = 4
            OTHERS                = 5 ).

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  DISPLAY LIKE sy-msgty.
          RETURN.
        ENDIF.



        NEW cl_ci_check( )->object_set(
          EXPORTING
            p_variant 	= 'SYNTAX_CHECK'
            p_objectset = |TEST_CG|
          IMPORTING
            p_ok				= DATA(ok)
            p_result		= DATA(result) ).

*        NEW cl_ci_check( )->object_list(
*          EXPORTING
*            p_variant = 'SYNTAX_CHECK'
*            p_objects = VALUE #( ( devclass = '$tmp'
*                                   objtype  = 'PROG'
*                                   objname  = 'Z_TEST_SYNTAX_CHECK' ) )
*          IMPORTING
*            p_ok			= DATA(ok)
*            p_result	= DATA(result) ).

*        NEW cl_ci_check( )->selection(
*          EXPORTING
*            p_variant 				= 'SYNTAX_CHECK'
*            p_tadir 					= VALUE #( sodevc = VALUE #( ( sign = 'I' option = 'EQ' low = '$TMP' ) ) )
*            p_function_groups = VALUE #( )
*            p_reports 				= VALUE #( )
*            p_classes 				= VALUE #( )
*            p_ddic_types			= VALUE #( )
*            p_type_pools			= VALUE #( )
*            p_any_objects     = value #( )
*          IMPORTING
*            p_ok							= DATA(ok)
*            p_result					= DATA(result) ).

      CATCH cx_root INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    cl_demo_output=>write( ok ).

    result->get_result(
      IMPORTING
        p_result = DATA(lt_result)
        p_list   = DATA(list) ).

    cl_demo_output=>write( list ).
    cl_demo_output=>display(  ).

*    DATA: ok        TYPE sychar01,
*          result    TYPE REF TO cl_ci_check_result,
*          lt_result TYPE scit_rest,
*          list      TYPE scit_alvlist.
*
*    TRY.
*        NEW cl_ci_check( )->single(
*          EXPORTING
*            p_variant_user = ''
*            p_variant 		 = 'SYNTAX_CHECK'
*            p_obj_type		 = 'PROG'
*            p_obj_name		 = 'Z_TEST_SYNTAX_CHECK'
*          IMPORTING
*            p_ok					 = ok
*            p_result			 = result ).
*
*      CATCH cx_root INTO DATA(error).
*        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*    ENDTRY.
*
*    cl_demo_output=>write( ok ).
*
*    result->get_result(
*      IMPORTING
*        p_result = lt_result
*        p_list   = list ).
*
*    cl_demo_output=>write( list ).
*    cl_demo_output=>display(  ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->run( ).
