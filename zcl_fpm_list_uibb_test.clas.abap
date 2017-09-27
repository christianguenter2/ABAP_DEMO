class ZCL_FPM_LIST_UIBB_TEST definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_LIST .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FPM_LIST_UIBB_TEST IMPLEMENTATION.


method IF_FPM_GUIBB_LIST~CHECK_CONFIG.
endmethod.


method IF_FPM_GUIBB_LIST~FLUSH.
endmethod.


method IF_FPM_GUIBB_LIST~GET_DATA.
endmethod.


method IF_FPM_GUIBB_LIST~GET_DEFAULT_CONFIG.
endmethod.


method IF_FPM_GUIBB_LIST~GET_DEFINITION.
  data: field_description like LINE OF et_field_description.

  field_description-name = 'Test'.
endmethod.


method IF_FPM_GUIBB_LIST~PROCESS_EVENT.
endmethod.


method IF_FPM_GUIBB~GET_PARAMETER_LIST.
endmethod.


method IF_FPM_GUIBB~INITIALIZE.
endmethod.
ENDCLASS.
