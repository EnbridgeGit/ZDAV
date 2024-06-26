FUNCTION Z_AW_FILE_ROWCOUNT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(NAME) LIKE  ZACTA-NAME OPTIONAL
*"             VALUE(ROWCOUNT) LIKE  ZACTA-TOTAL_ROW OPTIONAL
*"             VALUE(SET) LIKE  SONV-FLAG OPTIONAL
*"       EXPORTING
*"             VALUE(TOTAL_ROW) LIKE  ZACTA-TOTAL_ROW
*"----------------------------------------------------------------------
TABLES: ZACTA.
DATA: TEMPDATE LIKE ZACTA-UPDATE_DAT.

IF SET = 'X'.
   SELECT SINGLE TOTAL_ROW FROM ZACTA INTO TOTAL_ROW WHERE NAME = NAME.
   IF SY-SUBRC <> 0.
      ZACTA-NAME = NAME.
      ZACTA-TOTAL_ROW = ROWCOUNT.
      INSERT INTO ZACTA VALUES ZACTA.
   ELSE.
      UPDATE ZACTA SET TOTAL_ROW = ROWCOUNT WHERE NAME = NAME.
   ENDIF.
ELSE.
   SELECT SINGLE TOTAL_ROW FROM ZACTA INTO TOTAL_ROW WHERE NAME = NAME.
   IF SY-SUBRC <> 0.
      TOTAL_ROW = 0.
   ELSE.
      TEMPDATE = SY-DATUM - 3.
      DELETE FROM ZACTA WHERE UPDATE_DAT LT TEMPDATE.
   ENDIF.
ENDIF.
ENDFUNCTION.

FORM Z_AW_FILE_ROWCOUNT_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
