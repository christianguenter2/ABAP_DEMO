*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE _append_to_itab.
  get REFERENCE OF &1 INTO dref.
  APPEND dref to itab.
END-OF-DEFINITION.
