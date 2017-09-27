*&---------------------------------------------------------------------*
*& Report  Z_TEST_HOSS_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_hoss_data.
PARAMETERS: p_trkorr TYPE trkorr OBLIGATORY,
            p_url TYPE string OBLIGATORY,
            p_resp TYPE iw_respnsb OBLIGATORY,
            p_status TYPE ztrckeck OBLIGATORY.

zcl_bc_transport_request_hoss=>change_hoss_data(
  EXPORTING
    i_transport_request = p_trkorr
    i_hoss_url          = p_url
    i_hoss_responsible  = p_resp
    i_status            = p_status
  EXCEPTIONS
    invalid_attribute   = 1
    db_access_error     = 2
    OTHERS              = 3 ).

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
