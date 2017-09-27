REPORT z_test_mail.

TRY.
    " create mail document
    DATA(mail) = cl_document_bcs=>create_document( i_text    = VALUE #( ( CONV so_text255( 'Testmail' ) ) )
                                                   i_type    = 'RAW'
                                                   i_subject = 'Testsubject' ).

    DATA(send_request) = cl_bcs=>create_persistent( ).
    send_request->set_document( mail ).
    send_request->set_sender( cl_cam_address_bcs=>create_internet_address( 'test@test.de' ) ) ##NO_TEXT.
    send_request->add_recipient( cl_cam_address_bcs=>create_internet_address( 'test@test.de' ) ).
    send_request->set_send_immediately( abap_true ).

  CATCH cx_bcs INTO DATA(ex_bcs).
    MESSAGE ex_bcs TYPE ex_bcs->msgty.
ENDTRY.

" do not forget to commit, otherwise the mail will not be sent!
IF send_request->send( i_with_error_screen = abap_true ) = abap_true.
  COMMIT WORK.
ENDIF.
