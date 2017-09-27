CLASS zcl_rosetta_gcd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      get_lcm IMPORTING val1         TYPE i
                        val2         TYPE i
              RETURNING VALUE(r_gcd) TYPE i,
      get_gcd IMPORTING val1         TYPE i
                        val2         TYPE i
              RETURNING VALUE(r_lcd) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ROSETTA_GCD IMPLEMENTATION.


  METHOD get_gcd.
    r_lcd = abs( val1 * val2 ) / get_lcm( val1 = val1
                                          val2 = val2 ).
  ENDMETHOD.


  METHOD get_lcm.
    DATA: finished TYPE abap_bool.
    DATA(index) =  CONV i( nmax( val1 = val1 val2 = val2 ) ).

    WHILE finished = abap_false.
      IF index MOD val1 = 0 AND index MOD val2 = 0.
        finished = abap_true.
      ELSE.
        index = index + 1.
      ENDIF.
    ENDWHILE.
    r_gcd = index.
  ENDMETHOD.
ENDCLASS.
