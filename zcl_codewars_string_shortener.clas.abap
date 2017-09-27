CLASS zcl_codewars_string_shortener DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: co_default_glue TYPE string VALUE '...'.

    CLASS-METHODS shorten
      IMPORTING
        i_text_to_shorten TYPE string
        i_length          TYPE i
        i_glue            TYPE string DEFAULT co_default_glue
      RETURNING
        VALUE(r_result)   TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CODEWARS_STRING_SHORTENER IMPLEMENTATION.


  METHOD shorten.
    DATA(length_of_input_string) = strlen( i_text_to_shorten ).
    DATA(glue_length)  = strlen( i_glue ).

    IF i_text_to_shorten IS INITIAL.
      RETURN.
    ENDIF.

    IF i_length < 0 OR i_length > length_of_input_string.
      r_result = i_text_to_shorten.
      RETURN.
    ENDIF.

    IF  i_length <= ( glue_length + 1 ).
      r_result = substring( val = i_text_to_shorten
                            off = 0
                            len = i_length ).
      RETURN.
    ENDIF.

    DATA(left_length)  = ( i_length - glue_length ) / 2.
    DATA(right_length) = COND #( WHEN ( length_of_input_string MOD 2 ) = 1 THEN left_length
                                 ELSE left_length + 1 ).
    DATA(right_offset) = length_of_input_string - right_length.

    r_result = substring( val = i_text_to_shorten
                          off = 0
                          len = left_length )
            && i_glue
            && substring( val = i_text_to_shorten
                          off = right_offset
                          len = right_length ).
  ENDMETHOD.
ENDCLASS.
