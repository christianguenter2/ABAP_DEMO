*&---------------------------------------------------------------------*
*& Report  Z_TEST_TANGRO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_tangro.

DATA: lt_field TYPE HASHED TABLE OF /ssc/gui_field_t
                    WITH UNIQUE KEY dynprog fieldname langu,
      ls_field TYPE /ssc/gui_field_t,
      langu    TYPE sy-langu.

SELECT * FROM /ssc/gui_field_t
         INTO TABLE lt_field.

WRITE: / sy-subrc.

ls_field-dynprog   = '/SSC/SAPLM000000000000000059'.
ls_field-fieldname = 'FIELD_SD_VSNMR'.

SELECT SINGLE * FROM /ssc/cui_field_t INTO ls_field
       WHERE dynprog = ls_field-dynprog
       AND fieldname = ls_field-fieldname
       AND langu     = sy-langu.

WRITE: / sy-subrc.

langu = sy-langu.

SELECT SINGLE * FROM /ssc/cui_field_t INTO ls_field
       WHERE dynprog = ls_field-dynprog
       AND fieldname = ls_field-fieldname
       AND langu     = langu.

WRITE: / sy-subrc.
