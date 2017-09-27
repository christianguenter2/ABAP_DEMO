REPORT z_dock_drop.
*** Selection screen
PARAMETERS: p_url     TYPE string.

INITIALIZATION.
  PERFORM create_objects.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_navigate_complete
          FOR EVENT navigate_complete
          OF cl_gui_html_viewer
            IMPORTING url sender.
ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_navigate_complete.
    p_url = url.
    sender->go_back( ). "If object should not be displayed
  ENDMETHOD.                    "handle_sapevent
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECTS
*&---------------------------------------------------------------------*
FORM create_objects.
*** Data
  DATA cl_docker      TYPE REF TO cl_gui_docking_container.
  DATA t_events       TYPE cntl_simple_events.
  DATA wa_event       LIKE LINE OF t_events.
  DATA event_receiver TYPE REF TO lcl_event_receiver.
  DATA t_html         TYPE STANDARD TABLE OF text8192
                       WITH NON-UNIQUE DEFAULT KEY.
  DATA html    LIKE LINE OF t_html.
  DATA url     TYPE text1000.
  DATA cl_html TYPE REF TO cl_gui_html_viewer.

*** create docker
  CREATE OBJECT cl_docker
    EXPORTING
      extension               = 300
      side                    = cl_gui_docking_container=>dock_at_bottom
      no_autodef_progid_dynnr = 'X'.

*** create HTML
  CREATE OBJECT cl_html
    EXPORTING
      parent = cl_docker.

*** Register NAVIGATE_COMPLETE
  CALL METHOD cl_html->get_registered_events
    IMPORTING
      events = t_events.
  wa_event-eventid    = cl_gui_html_viewer=>m_id_navigate_complete.
  wa_event-appl_event = 'X'.
  READ TABLE t_events TRANSPORTING NO FIELDS
        WITH KEY eventid = wa_event-eventid.
  IF sy-subrc <> 0.
    APPEND wa_event TO t_events.
  ENDIF.
  CALL METHOD cl_html->set_registered_events
    EXPORTING
      events = t_events.
  SET HANDLER lcl_event_receiver=>handle_navigate_complete FOR cl_html.

*** Build HTML-Code
  CONCATENATE  '<html>'
               '<head><title>Dropzone</title>'
               '</head>'
               '<body bgcolor="#22FF22">'
               '<p><font size= 30>hierher mit den Bildern!!</font></p>'
               '</body>'
               '</html>'
          INTO html.
  APPEND html TO t_html.

*** load HTML
  CALL METHOD cl_html->load_data
    IMPORTING
      assigned_url = url
    CHANGING
      data_table   = t_html.

*** Show HTML page
  CALL METHOD cl_html->show_url
    EXPORTING
      url = url.
ENDFORM.                    " CREATE_OBJECTS
