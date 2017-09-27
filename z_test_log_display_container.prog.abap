REPORT z_test_log_display_container.

PARAMETER: p_test TYPE abap_bool AS CHECKBOX.

*call FUNCTION 'BAL_CNTL_CREATE'
*  EXPORTING
*    i_container            =
**    i_s_display_profile    =     " Display Profile
**    i_t_log_handle         =     " Restrict display by log handle
**    i_t_msg_handle         =     " Restrict display by message handle
**    i_s_log_filter         =     " Restrict display by log filter
**    i_s_msg_filter         =     " Restrict display by message filter
**    i_t_log_context_filter =     " Restrict display by log context filter
**    i_t_msg_context_filter =     " Restrict display by message context filter
**    i_srt_by_timstmp       = SPACE
**  IMPORTING
**    e_control_handle       =     " Application Log: Control Display Handle
**    e_no_data_available    =
**    e_no_authority         =
**  EXCEPTIONS
**    profile_inconsistent   = 1
**    internal_error         = 2
**    others                 = 3
*  .
*IF sy-subrc <> 0.
** MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.

INITIALIZATION.

START-OF-SELECTION.

  DATA(log_filter) = VALUE bal_s_lfil( aldate = VALUE #( ( sign   = 'I'
                                                           option = 'EQ'
                                                           low    = sy-datum ) )
*                                     altime = VALUE #( ( sign   = 'I'
*                                                         option = 'BT'
*                                                         low    = sy-uzeit - 200
*                                                         high   = sy-uzeit ) )
                                       aluser = VALUE #( ( sign   = 'I'
                                                           option = 'EQ'
                                                           low    = sy-uname ) )
                                       object = VALUE #( ( sign   = 'I'
                                                           option = 'EQ'
                                                           low    = '/UI2/BE' ) ) ).

  DATA: log_header TYPE balhdr_t.

  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
*     i_client           = SY-MANDT    " Client to be searched
      i_s_log_filter     = log_filter
*     i_t_sel_field      =     " Errors to Be Read
    IMPORTING
      e_t_log_header     = log_header
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  DATA: go_docking TYPE REF TO cl_gui_docking_container.

  CREATE OBJECT go_docking
    EXPORTING
*     parent                      =     " Parent container
*     repid                       =     " Report to Which This Docking Control is Linked
*     dynnr                       =     " Screen to Which This Docking Control is Linked
      side                        = cl_gui_docking_container=>dock_at_bottom    " Side to Which Control is Docked
      extension                   = 250    " Control Extension
*     style                       =     " Windows Style Attributes Applied to This Docking Container
*     lifetime                    = LIFETIME_DEFAULT    " Lifetime
*     caption                     =     " Caption
*     metric                      = 0    " Metric
*     ratio                       =     " Percentage of Screen: Takes Priority Over EXTENSION
*     no_autodef_progid_dynnr     =     " Don't Autodefined Progid and Dynnr?
*     name                        =     " Name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA(log_handles) = VALUE bal_t_logh( FOR wa IN log_header
                                          ( wa-log_handle ) ).

  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_handle     = log_handles    " Alternative 2: Table of log handles
    EXCEPTIONS
      no_logs_specified  = 1
      log_not_found      = 2
      log_already_loaded = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'BAL_CNTL_CREATE'
    EXPORTING
      i_container          = go_docking
      i_t_log_handle       = log_handles    " Restrict display by log handle
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      OTHERS               = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cl_demo_output=>display_data( log_header ).

  WRITE: 'Test'.

  call FUNCTION 'BAL_DSP_LOG_TEXTFORM'
    EXPORTING
*      i_log_handle        =     " Application Log: Log Handle
      it_log_handle       = log_handles    " Application Log: Log Handle Table
*      if_load             = SPACE    " Load logs from database first
*      if_print            = SPACE    " Output in new spool request
*      is_print_parameters =     " Structure for Passing Spool Parameters
*    IMPORTING
*      e_spool_number      =     " Number of Generated Spool Request
*    EXCEPTIONS
*      log_not_found       = 1
*      print_error         = 2
*      others              = 3
    .
  IF sy-subrc <> 0.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  CALL FUNCTION 'BAL_DSP_LOG_PRINT'
*    EXPORTING
**     i_s_print_options    =     " Print options
**     i_s_list_append      = SPACE    " Output of a list, consisting of n blocks
**     i_s_display_profile  =     " Display profile (only message list relevant)
*      i_t_log_handle       = log_handles    " Restrict display by log handle
**     i_t_msg_handle       =     " Restrict display by message handle
**     i_s_log_filter       =     " Restrict display by log filter
**     i_s_msg_filter       =     " Restrict display by message filter
**     i_t_log_context_filter =     " Restrict display by log context filter
**     i_t_msg_context_filter =     " Restrict display by message context filter
**     i_srt_by_timstmp     = SPACE    " Sort Logs by Timestamp ('X') or Log Number (SPACE)
*    EXCEPTIONS
*      profile_inconsistent = 1
*      internal_error       = 2
*      no_data_available    = 3
*      no_authority         = 4
*      OTHERS               = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
