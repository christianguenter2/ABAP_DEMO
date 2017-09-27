* SAP Initialisierungen

INCLUDE rpcedtd0_hrforms_init.

* Anpassungen an kundeneigene InfoStars ********************************
*
* 1. InfoStars mit Lohnarten - nur Monatswerte, keine Jahreswerte
*    Die InfoStars sind vom MetaStar PAYROLL_RESULT (für Lohnarten
*    aus der RT) abgeleitet und enthalten zumindest die Dimensionen:
*    - FORPERIOD
*    - INPERIODE
*    - WAGETYPE
*    Der InfoStar könnte z.B. "ZMEINS" heißen.
*
* 2. InfoStars mit Lohnarten getrennt nach Monats- und Jahreswerten
*    a.) Die InfoStars mit Monatswerten sind vom MetaStar PAYROLL_RESULT
*        (für Lohnarten aus der RT) abgeleitet und enthalten zumindest
*        die Dimensionen:
*        - FORPERIOD
*        - INPERIODE
*        - WAGETYPE
*        Der InfoStar könnte z.B. "ZLOHNART" heißen.
*    b.) Die InfoStars mit Jahreswerten sind von einem der folgenden
*        beiden MetaStars abgeleitet:
*        - CUMULAYTED_PAY (für Lohnarten aus der CRT)
*        - CUM_TAX_PAY_DE (für Lohnarten aus der SCRT)
*        enthalten zumindest die Dimensionen
*        - FORPERIOD
*        - INPERIODE
*        - WAGETYPE
*        und tragen den Namen des InfoStars mit dem Monatswerten plus
*        der Endung "_JHR".
*        Der InfoStar könnte dann z.B. "ZLOHNART_JHR" heißen.
* 2. Andere InfoStars müssen vor der Anzeige im Formular eventuell
*    noch entsprechend sortiert werden.
*
* Beispielcoding für die kundeneigenen InfoStars ZLOHNART, ZLOHNART_JHR
* und ZINFO unter Verwendung der Routinen aus dem Standard-Include:
*
* CUSTOMER_INFOSTAR zmeins. "nur Monatswerte
* CUSTOMER_INFOSTAR_SET zlohnart. "implizit auch mit 'zlohnart_jhr'


************************************************************************
* Customer Initialisierungen
* Stegmann & Brucker Unternehmensberatung GmbH
************************************************************************

* Macros
DEFINE zfeldsymbole.  " Feldsymbole für InfoStars anlegen
  field-symbols <z_&1> like line of hrdata-star_&1.
END-OF-DEFINITION.

DEFINE fill_betrg.  "Kundentabelle füllen,
  "zeilenbasierte Werte (lfd, einm., Monat, Jahr)
  loop at hrdata-star_&1 assigning <z_&1>.
    read table &2 assigning <z_betrg>
      with key forperiod_key = <z_&1>-forperiod_key
               inperiod_key  = <z_&1>-inperiod_key
             wagetype_key-wagetype(3) = <z_&1>-wagetype_key-wagetype(3).
    if sy-subrc ne 0.
      append initial line to &2 assigning <z_betrg>.
      <z_betrg>-forperiod_key = <z_&1>-forperiod_key.
      <z_betrg>-inperiod_key = <z_&1>-inperiod_key.
      <z_betrg>-wagetype_key = <z_&1>-wagetype_key.
      <z_betrg>-wagetype_sort = <z_&1>-wagetype_sort.
      <z_betrg>-wagetype_longtext = <z_&1>-wagetype_longtext.
      replace ', EZ' in <z_betrg>-wagetype_longtext with ``.
      replace ', lfd.' in <z_betrg>-wagetype_longtext with ``.
    endif.

    case &3.
      when 'BJ'.
      <z_betrg>-betrg_m_jhr = <z_betrg>-betrg_m_jhr + <z_&1>-pay_amount.
      when 'BM'.
        <z_betrg>-betrg_m = <z_betrg>-betrg_m + <z_&1>-pay_amount.
      when 'J'.  "Jahreswerte auch in Jahreswerte schreiben
        if  <z_&1>-wagetype_key-wagetype+3(1) eq 'E'.  "Einmalzahlung?
          <z_betrg>-betrg_e_jhr = <z_betrg>-betrg_e_jhr
                                + <z_&1>-pay_amount.
          <z_betrg>-betrg_m_jhr = <z_betrg>-betrg_m_jhr
                                + <z_&1>-pay_amount.
        else. "lfd. Zahlung
          <z_betrg>-betrg_l_jhr = <z_betrg>-betrg_l_jhr
                                + <z_&1>-pay_amount.
          <z_betrg>-betrg_m_jhr = <z_betrg>-betrg_m_jhr
                                + <z_&1>-pay_amount.
        endif.
      when others. "Monatswerte
        if  <z_&1>-wagetype_key-wagetype+3(1) eq 'E'.  "Einmalzahlung?
          <z_betrg>-betrg_e = <z_betrg>-betrg_e + <z_&1>-pay_amount.
          <z_betrg>-betrg_m = <z_betrg>-betrg_m + <z_&1>-pay_amount.
        else. "lfd. Zahlung
          <z_betrg>-betrg_l = <z_betrg>-betrg_l + <z_&1>-pay_amount.
          <z_betrg>-betrg_m = <z_betrg>-betrg_m + <z_&1>-pay_amount.
        endif.
    endcase.
  endloop.
END-OF-DEFINITION.
**********************************************************************
* data definition
FIELD-SYMBOLS  <z_betrg> TYPE lt_betrg.

zfeldsymbole:
abzuege_st_jhr, abzuege_st,
abzuege_sv_jhr, abzuege_sv,
aganteil_jhr, aganteil,
brutto_jhr, brutto,
stbrutto_jhr, stbrutto,
svbrutto_jhr, svbrutto.

fill_betrg abzuege_st g_star_abzuege 'M'.
fill_betrg abzuege_st_jhr g_star_abzuege 'J'.
fill_betrg abzuege_sv g_star_abzuege 'M'.
fill_betrg abzuege_sv_jhr g_star_abzuege 'J'.

fill_betrg brutto g_star_brutto 'BM'.
fill_betrg brutto_jhr g_star_brutto 'BJ'.
fill_betrg stbrutto g_star_brutto 'M'.
fill_betrg stbrutto_jhr g_star_brutto 'J'.
fill_betrg svbrutto g_star_brutto 'M'.
fill_betrg svbrutto_jhr g_star_brutto 'J'.


* Feldsymbole für InfoStars anlegen.                      "YMLN1380777e
DEFINE feldsymbole.
  field-symbols <ls_&1> like line of hrdata-star_&1.
END-OF-DEFINITION.

feldsymbole:
  basisbezuege2,
  zusaetze_ez,
  zusaetze_gwv,
  zeitbezuege_p3b.


lgart_classify:
  basisbezuege2,
  zusaetze_ez,
  zusaetze_gwv,
  zeitbezuege_p3b.

"Lohnartentexte anpassen
DATA ls_zusaetze_ez   LIKE LINE OF hrdata-star_zusaetze_ez.
DATA c_pay_number(5).
LOOP AT hrdata-star_zusaetze_ez INTO ls_zusaetze_ez.
  CASE ls_zusaetze_ez-wagetype_key-wagetype.
    WHEN '4010'.
      c_pay_number = ls_zusaetze_ez-pay_number.

      CONCATENATE 'ZEP-Abschlag' c_pay_number '%'
         INTO ls_zusaetze_ez-wagetype_longtext
         SEPARATED BY space.
      REPLACE ' %' WITH '%' INTO ls_zusaetze_ez-wagetype_longtext.
      REPLACE '0%' WITH '%' INTO ls_zusaetze_ez-wagetype_longtext.
      REPLACE '.0%' WITH '%' INTO ls_zusaetze_ez-wagetype_longtext.
      REPLACE '.' WITH ',' INTO ls_zusaetze_ez-wagetype_longtext.
      CLEAR ls_zusaetze_ez-pay_number.
      MODIFY hrdata-star_zusaetze_ez FROM ls_zusaetze_ez.

    WHEN OTHERS.
      CASE ls_zusaetze_ez-wagetype_key-wagetype(3).
        WHEN '/5B'.
          CLEAR ls_zusaetze_ez-pay_number.
          MODIFY hrdata-star_zusaetze_ez FROM ls_zusaetze_ez.
      ENDCASE.
  ENDCASE.
ENDLOOP.


"Urlaubskontingente zusammenfassen
DATA ls_urlaub LIKE LINE OF hrdata-star_urlaub.
DATA: urlaub_tmp LIKE hrdata-star_urlaub .

REFRESH: urlaub_tmp.
LOOP AT hrdata-star_urlaub INTO ls_urlaub.
  CLEAR: ls_urlaub-quotatype_key-quota_type.
*  ls_urlaub-QUOTATYPE_text = 'Urlaub'.
*  clear ls_urlaub-QUOTATYPE_text.
  COLLECT ls_urlaub INTO urlaub_tmp.
ENDLOOP.

hrdata-star_urlaub[] = urlaub_tmp[].

"Wenn die Option Nur Rückrechungen angehakt ist,
"muss in den jeweiligen Perioden nach der Lohnart /553 gesucht werden.
"->wenn die nicht vorhanden ist-> Ergebnis löschen

* Schleife zur Eliminierung von irrelevanten Rückrechnungsperioden
DATA: ls_flag LIKE LINE OF hrdata-star_zhr_rr_flag.
LOOP AT hrdata-dim_forperiod INTO l_forperiod.
  AT LAST. EXIT. ENDAT. "letzte Fürperiode bleibt         "YMLN1450712
  flag_del = 'X'.
  LOOP AT hrdata-star_zhr_rr_flag INTO ls_flag
    WHERE zhr_rr_flag_key-forperiod EQ l_forperiod-key-period
      AND zhr_rr_flag_key-relevant EQ 'X'.
    CLEAR flag_del.
  ENDLOOP.
  IF flag_del = 'X'.
*   APPEND l_forperiod TO lt_del_forperiods.   "YML1723837c"YMLN1801060a
    DELETE hrdata-dim_forperiod INDEX sy-tabix.
    APPEND l_forperiod TO lt_del_forperiods.               "YMLN1801060a
  ENDIF.
ENDLOOP.


"Basisbezüge Summenlohnarten zusammenfassen
DATA ls_basisbezuege2 LIKE LINE OF hrdata-star_basisbezuege2.
DATA: basisbezuege2_tmp LIKE hrdata-star_basisbezuege2 .

REFRESH: basisbezuege2_tmp.
LOOP AT hrdata-star_basisbezuege2 INTO ls_basisbezuege2.
  CLEAR: ls_basisbezuege2-time_unit.
  COLLECT ls_basisbezuege2 INTO basisbezuege2_tmp.
ENDLOOP.

hrdata-star_basisbezuege2[] = basisbezuege2_tmp[].

"Auf dem Selektionsbild
"auch rückger. Perioden ? -Differenzendarstellung
"dann Überweisungen löschen
LOOP AT hrdata-star_zhr_rr_flag INTO ls_flag
  WHERE zhr_rr_flag_key-addret EQ 'X'.
  EXIT.
ENDLOOP.
IF sy-subrc EQ 0.
  "alle Zahlungen löschen ausser Originalmonat
  DELETE hrdata-star_ueberweisung
    WHERE forperiod_key-period NE g_inperiod(6).
ENDIF.
