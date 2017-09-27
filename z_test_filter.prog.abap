*&---------------------------------------------------------------------*
*& Report z_test_filter
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_filter.

TYPES: BEGIN OF grid_element,
         row   TYPE i,
         col   TYPE i,
         value TYPE string,
       END OF grid_element,
       grid_type TYPE SORTED TABLE OF grid_element
                             WITH UNIQUE KEY row col.

DATA(grid) = VALUE grid_type(
  ( row = 1  col = 1 value = |Element 1,1| )
  ( row = 1  col = 2 value = |Element 1,2| )
  ( row = 2  col = 1 value = |Element 2,1| )
  ( row = 2  col = 2 value = |Element 2,2| )
  ( row = 3  col = 1 value = |Element 3,1| )
  ( row = 3  col = 2 value = |Element 3,2| ) ).

DATA(filtered_grid) = VALUE grid_type( FOR line IN grid
                                       WHERE ( row <= 2 AND col <= 2 )
                                       ( line ) ).

*DATA(filtered_grid) = FILTER grid_type( grid WHERE row <= 2  AND col <= 2 ).

cl_demo_output=>display( filtered_grid ).

*DATA(sum) = REDUCE i( INIT result = 0
*                      FOR line IN table WHERE ( row <= 2 AND col <= 2 )
*                      NEXT result = result + line-row ).

*cl_demo_output=>display( sum ).
