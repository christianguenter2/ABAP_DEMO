REPORT z_test_ref.

DATA(mfa) = NEW zcl_bc_debugger_scripts( ).

DATA(ref) = REF #( mfa ).

IF sy-subrc = 0.

ENDIF.
