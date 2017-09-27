*&---------------------------------------------------------------------*
*& Report  Z_TEST_PATH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_path.

TABLES: path.

SELECT-OPTIONS: s_path FOR path-pathextern LOWER CASE.

DATA: paths TYPE STANDARD TABLE OF path.

SELECT * FROM path INTO TABLE paths
         WHERE pathextern IN s_path.

cl_demo_output=>display_data( paths ).
