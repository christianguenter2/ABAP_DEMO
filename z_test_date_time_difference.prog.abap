*&---------------------------------------------------------------------*
*& Report  Z_TEST_DATE_TIME_DIFFERENCE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_test_date_time_difference.

DATA: date1    TYPE sy-datum,
      date2    TYPE sy-datum,
      time1    TYPE sy-uzeit,
      time2    TYPE sy-uzeit,
      datediff TYPE p DECIMALS 2,
      timediff TYPE p DECIMALS 2,
      text     TYPE string.

date1 = sy-datum.
date2 = sy-datum.
time1 = sy-uzeit.
time2 = sy-uzeit.

DO 10 TIMES.
  ADD 1 TO date2.
  ADD sy-index TO time2.

  CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
    EXPORTING
      date1            = date1
      time1            = time1
      date2            = date2
      time2            = time2
    IMPORTING
      datediff         = datediff
      timediff         = timediff
    EXCEPTIONS
      invalid_datetime = 1
      OTHERS           = 2.

  text = | Date1: { date1 DATE = USER }, Time1 { time1 TIME = USER }, Date 2 { date2 DATE = USER }, Time2 { time2 TIME = USER }, Datediff { datediff }, Timediff { timediff }, DateTimeDiff  { datediff * 24 + timediff } |.
  WRITE:/ text.
ENDDO.
