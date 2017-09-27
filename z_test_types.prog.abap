*&---------------------------------------------------------------------*
*& Report z_test_types
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test_types.

TYPES: BEGIN OF t1,
         f1 TYPE char2,
         f2 TYPE char10,
       END OF t1.

TYPES: BEGIN OF t2,
         f0 TYPE char2,
         f1 TYPE char2,
         f2 TYPE char10,
       END OF t2.

DATA: _1 TYPE t1,
      _2 TYPE t2.

_1 = VALUE #( f1 = '12' f2 = 'Welt').
_2 = _1.

cl_demo_output=>new(  )->write( _1 )->write( _2 )->display( ).
