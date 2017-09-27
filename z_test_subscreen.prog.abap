**&---------------------------------------------------------------------*
**& Report  Z_TEST_SUBSCREEN
**&
**&---------------------------------------------------------------------*
**&
**&
**&---------------------------------------------------------------------*
*
REPORT z_test_subscreen.
*
*SELECTION-SCREEN BEGIN OF SCREEN 0100 AS SUBSCREEN.
*PARAMETERS: p_test TYPE char01.
*SELECTION-SCREEN END OF SCREEN 0100.
*
*SELECTION-SCREEN BEGIN OF SCREEN 0200 AS SUBSCREEN.
*PARAMETERS: p_test2 TYPE char02.
*SELECTION-SCREEN END OF SCREEN 0200.
*
*SELECTION-SCREEN BEGIN OF TABBED BLOCK b1 FOR 2 LINES.
*SELECTION-SCREEN TAB (40) tab1 user-command ucomm1 default screen 0100.
*SELECTION-SCREEN TAB (40) tab2 user-command ucomm2 default screen 0200.
*SELECTION-SCREEN END OF BLOCK b1.
*
*AT SELECTION-SCREEN.
*  b1-dynnr = 0200.

DATA: save_active_tab TYPE string.

SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
PARAMETERS: p1 TYPE c LENGTH 10,
            p2 TYPE c LENGTH 10,
            p3 TYPE c LENGTH 10.
SELECTION-SCREEN END OF SCREEN 100.

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
PARAMETERS: q1 TYPE c LENGTH 10,
            q2 TYPE c LENGTH 10,
            q3 TYPE c LENGTH 10.
SELECTION-SCREEN END OF SCREEN 200.

SELECTION-SCREEN: BEGIN OF TABBED BLOCK mytab FOR 10 LINES,
                  TAB (20) button1 USER-COMMAND push1,
                  TAB (20) button2 USER-COMMAND push2,
                  END OF BLOCK mytab.

INITIALIZATION.
  button1 = 'Selection Screen 1'.
  button2 = 'Selection Screen 2'.
  mytab-prog = sy-repid.
  mytab-dynnr = 100.
  mytab-activetab = 'PUSH1'.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      CASE sy-ucomm.
        WHEN 'PUSH1'.
          mytab-dynnr = 100.
          save_active_tab = 'PUSH1'.
        WHEN 'PUSH2'.
          mytab-dynnr = 200.
          save_active_tab = 'PUSH2'.
        WHEN OTHERS.
          ...
      ENDCASE.
  ENDCASE.

START-OF-SELECTION.
  EXPORT save_active_tab = save_active_tab TO MEMORY ID 'Z_TEST_SUBSCREEN_TAB'.

INITIALIZATION.
  IMPORT save_active_tab = save_active_tab FROM MEMORY ID 'Z_TEST_SUBSCREEN_TAB'.
  mytab-activetab = save_active_tab.
