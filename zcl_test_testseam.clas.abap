CLASS zcl_test_testseam DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_answer,
             a TYPE char01,
             b TYPE char01,
           END OF ty_answer.

    METHODS:
      start
        RETURNING VALUE(r_answer) TYPE ty_answer.

ENDCLASS.



CLASS ZCL_TEST_TESTSEAM IMPLEMENTATION.


  METHOD start.

    DATA(itab) = VALUE stringtab( ( `Test` ) ).

    READ TABLE itab ASSIGNING FIELD-SYMBOL(<line2>)
                    INDEX 1.
    IF sy-subrc = 0.

    ENDIF.

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<line>).

      IF <line> = `Test`.

      ELSE.

      ENDIF.

    ENDLOOP.

    TEST-SEAM test_popup.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = 'Test'
        IMPORTING
          answer         = r_answer-a
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.

    END-TEST-SEAM.

    TEST-SEAM test_popup2.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = 'Test'
        IMPORTING
          answer         = r_answer-b
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.

    END-TEST-SEAM.

  ENDMETHOD.
ENDCLASS.
