*&---------------------------------------------------------------------*
*& Report  Z_TEST_RECA_MSG_LIST_ENH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_reca_msg_list_enh.

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS  lcl_application IMPLEMENTATION.
  METHOD start.
    DATA: lo_log TYPE REF TO cl_reca_message_list.

    lo_log ?= cf_reca_message_list=>create( ).

    lo_log->add_free_text( 'Dies ist ein Test' ).

    lo_log->add_free_text( i_text = 'Weiterer Test'
                           i_msgty = 'E' ).

    lo_log->add_free_text( 'Dies ist ein ziemlich langer Test Text' &&
                           |2. Zeile | &&
                           |In einer außerordentlichen Sondersitzung hat die UNO-Generalversammlung heute in New York die sofortige Auflösung der Organisation beschlossen. Der Zusammenschluss aus 193| ).

    CALL FUNCTION 'RECA_GUI_MSGLIST_POPUP'
      EXPORTING
        io_msglist = lo_log.
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_application IMPLEMENTATION

START-OF-SELECTION.
  lcl_application=>start( ).
