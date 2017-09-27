*&---------------------------------------------------------------------*
*& Report z_test_linked_list
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_linked_list.

TYPES: BEGIN OF ty_cell,
         value TYPE string,
         next  TYPE REF TO data,
       END OF ty_cell.

FORM walk_cells USING i_cell TYPE ty_cell.

  DATA(cell) = REF #( i_cell ).

  WHILE cell->next IS BOUND.
    WRITE: / cell->value.
    cell = CAST #( cell->next ).
  ENDWHILE.

  WRITE: / cell->value.

ENDFORM.

FORM add_cell CHANGING c_current_cell TYPE ty_cell
                       c_next_cell    TYPE ty_cell.

  DATA(current_next) = c_current_cell-next.

  c_current_cell-next = REF #( c_next_cell ).
  c_next_cell-next = current_next.

ENDFORM.

START-OF-SELECTION.

  DATA(cell1)       = VALUE ty_cell( value = '1. Zelle' ).
  DATA(cell2)       = VALUE ty_cell( value = '2. Zelle' ).
  DATA(cell3)       = VALUE ty_cell( value = '3. Zeile' ).
  DATA(random_cell) = VALUE ty_cell( value = cl_abap_random=>create( CONV #( sy-uzeit ) )->int( ) ).

  PERFORM add_cell CHANGING: cell1 cell2,
                             cell2 cell3,
                             cell2 random_cell.

  PERFORM walk_cells USING cell1.
