REPORT z_test_pb_send_reject_mail.

PARAMETERS: p_no   TYPE zpb_s_head-number OBLIGATORY,
            p_vers TYPE zpb_s_head-version OBLIGATORY.

START-OF-SELECTION.
  DATA: key       TYPE zif_pb_types=>key,
        error     TYPE REF TO /bobf/cx_frw,
        lo_error  TYPE REF TO zcx_lo_error,
        bcs_error TYPE REF TO cx_bcs.

  key-number  = p_no.
  key-version = p_vers.

  TRY.
      zcl_pb_mail=>new( io_model = zcl_pb_model=>read( key )
                        i_type   = zif_pb_types=>co_mail_type-invalid_bom_mail
                )->send_mail( ).

      COMMIT WORK.

    CATCH cx_bcs INTO bcs_error.
      MESSAGE bcs_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    CATCH /bobf/cx_frw INTO error.
      zcl_pb_view_error=>display( error ).
      RETURN.
    CATCH zcx_lo_error INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.
