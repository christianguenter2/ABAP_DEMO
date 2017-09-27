*&---------------------------------------------------------------------*
*& Report  Z_TEST_UZEIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_TEST_UZEIT.

data: datum TYPE syst-datum,
      zeit TYPE syst-uzeit,
      timestamp TYPE timestampl,
      tz TYPE tzonref-tzone VALUE 'UTC+8'.

zeit = sy-uzeit.

WRITE:/ zeit.

get TIME STAMP FIELD timestamp.

write: / timestamp.

CONVERT time STAMP timestamp
        TIME ZONE tz
        INTO date datum
             time zeit.

WRITE: / datum.
WRITE: / zeit.
