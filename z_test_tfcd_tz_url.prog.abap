*&---------------------------------------------------------------------*
*& Report  Z_TEST_TFCD_TZ_URL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_tfcd_tz_url.

DATA: lt_data TYPE STANDARD TABLE OF /tfcd/tz_url WITH NON-UNIQUE DEFAULT KEY.

SELECT * FROM /tfcd/tz_url
         CLIENT SPECIFIED
         INTO TABLE lt_data.

cl_demo_output=>display_data( lt_data ).
