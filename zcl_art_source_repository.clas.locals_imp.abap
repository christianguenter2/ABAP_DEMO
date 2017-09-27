*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

class lcl_source_corer implementation.

  method create.
    create object r_result.
  endmethod.


  method if_art_source_corer~grain_procedures.
    data: tokens     type table of stokesx,
          procedure_token type stokesx,
          endprocedure_token type stokesx,
          statements type table of sstmnt,
          keywords   type table of char10,
          procedure_start_statement  like line of statements,
          procedure_end_statement  like line of statements,
          loop_count type i,
          length_to_end_of_proc_stmnt type i,
          procedure_name_token like line of tokens.

    FIELD-SYMBOLS: <code_line> LIKE LINE OF r_result.

    append 'METHOD' to keywords.
    append 'ENDMETHOD' to keywords.
    append 'FUNCTION' to keywords.
    append 'ENDFUNCTION' to keywords.
    append 'FORM' to keywords.
    append 'ENDFORM' to keywords.

    r_result = i_source_code.

    scan abap-source r_result tokens into tokens statements into statements with analysis keywords from keywords.

    do lines( statements ) / 2 times.
      loop_count = sy-index.
      read table statements index loop_count * 2 - 1 into procedure_start_statement.
      read table statements index loop_count * 2     into procedure_end_statement.

      read table tokens index procedure_start_statement-from into procedure_token.
      read table tokens index procedure_start_statement-from + 1 into procedure_name_token.
      read table tokens index procedure_end_statement-from into endprocedure_token.

      "constructors must contain the call to the super constructor in case of inheritance
      "else the compiler struggles over this syntax error
      if procedure_token-str = 'METHOD' and procedure_name_token-str = 'CONSTRUCTOR'.
        continue.
      endif.

      if  i_cursor_position is bound
      and i_cursor_position->starts_after_row_col( i_row = procedure_token-row i_col = procedure_token-col + 0 ) = abap_true
      and i_cursor_position->ends_before_row_col( i_row = endprocedure_token-row i_col = endprocedure_token-col + 0 ) = abap_true.
        continue.
      endif.

      length_to_end_of_proc_stmnt = procedure_start_statement-tcol + 1.

      if procedure_start_statement-trow = endprocedure_token-row.

        read table r_result index procedure_start_statement-trow assigning <code_line>.


        <code_line> = |{ <code_line>(length_to_end_of_proc_stmnt) }{ spaces( endprocedure_token-col - length_to_end_of_proc_stmnt ) }{ <code_line>+endprocedure_token-col }|.

      else.

        read table r_result index procedure_start_statement-trow assigning <code_line>.

*        if strlen( <code_line> ) < length_to_end_of_proc_stmnt and sy-uname = 'LANGMARK0'.
*          break-point.
*        endif.

        <code_line> = <code_line>(length_to_end_of_proc_stmnt).

        read table r_result index endprocedure_token-row assigning <code_line>.
        <code_line> = |{ spaces( endprocedure_token-col + 0 ) }{ <code_line>+endprocedure_token-col }|.

        loop at r_result from procedure_start_statement-trow + 1 to endprocedure_token-row - 1 assigning <code_line>.
          clear <code_line>.
        endloop.

      endif.

    enddo.

  endmethod.


  method spaces.

    do i_count times.
      r_Result = |{ r_result } |.
    enddo.

  endmethod.

endclass.
