*&---------------------------------------------------------------------*
*& Report  Z_TEST_PLM_COPY_AND_REPLACE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_plm_copy_and_replace.

PARAMETERS: dokar TYPE bapi_doc_aux-doctype OBLIGATORY,
            doknr TYPE bapi_doc_aux-docnumber OBLIGATORY,
            dokvr TYPE bapi_doc_aux-docpart OBLIGATORY,
            doktl TYPE bapi_doc_aux-docversion OBLIGATORY,
            view  TYPE seoclsname AS LISTBOX OBLIGATORY VISIBLE LENGTH 30.

INTERFACE lif_model DEFERRED.

*----------------------------------------------------------------------*
*       INTERFACE lif_view
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_view.
  METHODS: show IMPORTING i_model TYPE REF TO lif_model.
ENDINTERFACE.                    "lif_view

*----------------------------------------------------------------------*
*       INTERFACE lif_model
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_model.
  METHODS: create_from_source.

  DATA: doctype    TYPE bapi_doc_aux-doctype READ-ONLY,
        docnumber  TYPE bapi_doc_aux-docnumber READ-ONLY,
        docpart    TYPE bapi_doc_aux-docpart READ-ONLY,
        docversion TYPE bapi_doc_aux-docversion READ-ONLY,
        return     TYPE bapiret2 READ-ONLY.
ENDINTERFACE.                    "lif_model
*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
          IMPORTING i_view TYPE REF TO lif_view
                    i_model TYPE REF TO lif_model
          RETURNING value(r_instance) TYPE REF TO lcl_application.
    METHODS:
      constructor
          IMPORTING i_view TYPE REF TO lif_view
                    i_model TYPE REF TO lif_model,
               create_from_source.

  PRIVATE SECTION.
    DATA: _view  TYPE REF TO lif_view,
          _model TYPE REF TO lif_model.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_model.
    CLASS-METHODS: get_instance RETURNING value(r_instance) TYPE REF TO lif_model.
ENDCLASS.                    "lcl_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS demo_output_view DEFINITION. " Demo Output
  PUBLIC SECTION.
    INTERFACES: lif_view.
ENDCLASS.                    "lcl_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_list_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS list_view DEFINITION. " List Output
  PUBLIC SECTION.
    INTERFACES: lif_view.
ENDCLASS.                    "lcl_list_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_popup_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS popup_view DEFINITION. " Popup Output
  PUBLIC SECTION.
    INTERFACES: lif_view.
ENDCLASS.                    "lcl_popup_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING value(r_view) TYPE REF TO lif_view.
ENDCLASS.                    "lcl_view_factory DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_instance
      EXPORTING
        i_view  = i_view
        i_model = i_model.
  ENDMETHOD.                    "get_instance

  METHOD create_from_source.
    _model->create_from_source( ).
    _view->show( _model ).
  ENDMETHOD.                    "create_from_source

  METHOD constructor.
    _view  = i_view.
    _model = i_model.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.
  METHOD lif_model~create_from_source.
    DATA: newdocumentnumber  TYPE bapi_doc_aux-docnumber VALUE '*',
          newdocumentpart    TYPE bapi_doc_aux-docpart VALUE '000',
          newdocumentversion TYPE bapi_doc_aux-docversion,
          copyobjectlinks    TYPE STANDARD TABLE OF bapi_doc_drad_select.

    CALL FUNCTION 'BAPI_DOCUMENT_CREATEFROMSRC2'
      EXPORTING
        refdocumenttype    = dokar
        refdocumentnumber  = doknr
        refdocumentpart    = dokvr
        refdocumentversion = doktl
        newdocumentnumber  = newdocumentnumber
        newdocumentpart    = newdocumentpart
        newdocumentversion = newdocumentversion
        copyclassification = abap_true
        copyoriginals      = abap_true
        copydocbom         = abap_true
      IMPORTING
        doctype            = lif_model~doctype
        docnumber          = lif_model~docnumber
        docpart            = lif_model~docpart
        docversion         = lif_model~docversion
        return             = lif_model~return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDMETHOD.                    "lif_model~create_from_source

  METHOD get_instance.
    CREATE OBJECT r_instance TYPE lcl_model.
  ENDMETHOD.                    "get_instance
ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS demo_output_view IMPLEMENTATION.
  METHOD lif_view~show.
    DATA: out TYPE REF TO cl_demo_output.

    CREATE OBJECT out TYPE cl_demo_output.

    out->write_data( i_model->return ).
    out->write_data( i_model->doctype ).
    out->write_data( i_model->docnumber ).
    out->write_data( i_model->docpart ).
    out->write_data( i_model->docversion ).
    out->display( ).
  ENDMETHOD.                    "lif_view~SHOW
ENDCLASS.                    "lcl_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_list_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS list_view IMPLEMENTATION.
  METHOD lif_view~show.
    WRITE: / i_model->return-message,
           / i_model->docnumber,
           / i_model->doctype,
           / i_model->docversion,
           / i_model->docpart.
  ENDMETHOD.                    "lif_view~show
ENDCLASS.                    "lcl_list_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_popup_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS popup_view IMPLEMENTATION.
  METHOD lif_view~show.
    DATA: text TYPE string.

    text = |{ i_model->return-message } | &&
           |{ i_model->docnumber } | &&
           |{ i_model->doctype } | &&
           |{ i_model->docversion } | &&
           |{ i_model->docpart } |.

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        textline1 = text.
  ENDMETHOD.                    "lif_view~SHOW
ENDCLASS.                    "lcl_popup_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view_factory IMPLEMENTATION.
  METHOD get_instance.
    CREATE OBJECT r_view TYPE (view).
  ENDMETHOD.                    "get_instance
ENDCLASS.                    "lcl_view_factory IMPLEMENTATION

AT SELECTION-SCREEN OUTPUT.
  zcl_bc_dynp_utilities=>vrm_set_values_with_locl_intf( id        = 'VIEW'
                                                        interface = 'LIF_VIEW' ).

START-OF-SELECTION.
  lcl_application=>get_instance( i_model = lcl_model=>get_instance( )
                                 i_view  = lcl_view_factory=>get_instance( )
                )->create_from_source( ).
