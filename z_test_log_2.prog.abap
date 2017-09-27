REPORT z_test_log_2.

CLASS lcl_log DEFINITION INHERITING FROM cl_sbal_logger.

  PUBLIC SECTION.
    METHODS:
      if_logger~add_message REDEFINITION.


  PRIVATE SECTION.
    DATA: mt_messages TYPE bal_tt_msg.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD if_logger~add_message.

    INSERT VALUE #( msgty     = i_msgty
                    msgid     = i_msgid
                    msgno     = i_msgno
                    msgv1     = i_msgv1
                    msgv2     = i_msgv2
                    msgv3     = i_msgv3
                    msgv4     = i_msgv4
                    probclass = i_probcl ) INTO TABLE mt_messages.

    super->if_logger~add_message(
      EXPORTING
        i_msgid  = i_msgid    " Message Class
        i_msgno  = i_msgno    " Text field
        i_probcl = i_probcl    " Application Log: Message Problem Class
        i_msgty  = i_msgty    " Message Type
        i_msgv1  = i_msgv1    " Message Variable
        i_msgv2  = i_msgv2    " Message Variable
        i_msgv3  = i_msgv3    " Message Variable
        i_msgv4  = i_msgv4 ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(lo_log) = NEW lcl_log( i_category    = 'WEBDYNPRO'
                              i_subcategory = 'RUNTIME' ).


  DATA(if_log) = CAST if_sbal_logger( lo_log ).

  lo_log->if_logger~add_message( i_msgid  = 'BC'
                                 i_msgno  = '701'
                                 i_msgty  = 'S'
                                 i_msgv1  = 'Test' ).

  if_log->add_message( i_msgid  = 'BC'
                       i_msgno  = '701'
                       i_msgty  = 'E'
                       i_msgv1  = 'Noch ein Test' ).

  if_log->finalize( ).

  COMMIT WORK.
