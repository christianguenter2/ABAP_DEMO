*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>RSTPDA_SCRIPT_TEMPLATE</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super.
  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
             init      REDEFINITION,
             script    REDEFINITION,
             end       REDEFINITION.

  PRIVATE SECTION.
    DATA: range_it TYPE tpda_range_it.
ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD prologue.
  ENDMETHOD.                    "prolog

  METHOD init.
    CALL FUNCTION 'TPDA_SCRIPT_CALL_SEL_SCREEN'
      EXPORTING
        p_screen   = if_tpda_sel_screens=>c_scr_filter
      IMPORTING
        p_range_it = range_it.
  ENDMETHOD.                    "init

  METHOD script.
    trace->add_src_info_ext( i_filter = range_it[] ).
  ENDMETHOD.                    "script

  METHOD end.
  ENDMETHOD.                    "end

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
