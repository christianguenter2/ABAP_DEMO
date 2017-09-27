*&---------------------------------------------------------------------*
*& Report z_test_verbuchung
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_verbuchung.

CALL FUNCTION 'Z_TEST_VERBUCHUNG_1'
  IN UPDATE TASK.

COMMIT WORK.

IF sy-subrc = 0.
ENDIF.
