*&---------------------------------------------------------------------*
*& Report z_test_mail_leading_zero
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_mail_leading_zero.

CLASS controller DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      start.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD start.

    TYPES: BEGIN OF ty_matnr,
             matnr TYPE matnr,
           END OF ty_matnr,
           tty_matnr TYPE STANDARD TABLE OF matnr
                          WITH NON-UNIQUE DEFAULT KEY.

    DATA(lt_matnr) = VALUE tty_matnr( ( '01800180' )
                                      ( '00180018' )  ).

    DATA(string) = concat_lines_of( table = lt_matnr sep = cl_abap_char_utilities=>cr_lf ).

    TRY.
        DATA(bcs) = cl_bcs=>create_persistent( ).

        DATA(document) = cl_document_bcs=>create_document( i_type 	 = 'TXT'
                                                           i_subject = 'Test' ).

        document->add_attachment( i_attachment_type 	 = 'CSV'
                                  i_attachment_subject = 'test.csv'
                                  i_att_content_text	 = cl_bcs_convert=>string_to_soli( string ) ).

        bcs->set_document( document ).
        bcs->add_recipient( cl_cam_address_bcs=>create_internet_address( i_address_string = 'christian.guenter@hansgrohe.com') ).
        bcs->send( ).

        COMMIT WORK.

      CATCH cx_bcs INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  NEW controller( )->start( ).
