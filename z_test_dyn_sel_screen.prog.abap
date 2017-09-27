*&---------------------------------------------------------------------*
*& Report  Z_TEST_DYN_SEL_SCREEN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_dyn_sel_screen.

parameter: p_1 type char01.

DEFINE def_par.
  case &1.
    when 1.
       %_p_1_%_app_%-text = &1.
    when 2.
      parameter: p_2 type char01.
      %_p_2_%_app_%-text = &1.
    when 3.
      parameter: p_3 type char01.
      %_p_3_%_app_%-text = &1.
  endcase.
END-OF-DEFINITION.

INITIALIZATION.
  DO 3 TIMES.
    def_par: sy-index.
  ENDDO.
