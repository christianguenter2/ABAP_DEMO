*"* use this source file for any macro definitions you need
*"* in the implementation part of the class


DEFINE _set_data.
  if root-&1 <> i_head-&1.
    root-&1 = i_head-&1.
    something_changed = abap_true.
  endif.
END-OF-DEFINITION.
