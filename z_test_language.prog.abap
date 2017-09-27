REPORT z_test_language.

TYPES: w_pc_clang TYPE STANDARD TABLE OF langu.

DATA:
  "! list of languages
  doku_langus      TYPE w_pc_clang,

  "! selection range of languages
  doku_langu_range TYPE RANGE OF spras.

 types: tty_r_langus type range of langu.

data(langus) = tty_r_langus( for lang in doku_langus
                                ( sign = 'I'
                                  option = 'EQ'
                                  low = lang ) ).

SELECT 'I' AS sign,
       'EQ' AS option,
       spras AS low
       INTO CORRESPONDING FIELDS OF TABLE @doku_langu_range FROM t002
       WHERE spras in VALUE #( FOR lang IN doku_langus
                            ( sign   = 'I'
                              option = 'EQ'
                              low    = lang ) ).
