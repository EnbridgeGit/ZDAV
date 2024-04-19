FUNCTION Z_AW_GET_VERSION.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_FUNC) LIKE  RFCFUNC-FUNCNAME OPTIONAL
*"       EXPORTING
*"             VALUE(O_DI_VER) LIKE  RFCFUNC-FUNCNAME
*"       TABLES
*"              ENTRIES STRUCTURE  TAB512
*"----------------------------------------------------------------------

DATA VERSION(100).
DATA OUTPUT(512).
DATA FORM_NAME(100).
DATA PROGNAME LIKE SY-REPID.

PROGNAME = SY-REPID.
O_DI_VER = '6.5.1.0'.

REFRESH ENTRIES. CLEAR ENTRIES.

SELECT DISTINCT FUNCNAME
FROM FUPARAREF
INTO FUPARAREF-FUNCNAME
WHERE FUNCNAME LIKE 'Z#_AW#_%' ESCAPE '#'.
  CLEAR: FORM_NAME, VERSION.
  CONCATENATE FUPARAREF-FUNCNAME
              '_GETV' INTO FORM_NAME.
  MOVE FORM_NAME+0(30) TO FORM_NAME.
  PERFORM (FORM_NAME) IN PROGRAM (PROGNAME)
                      CHANGING VERSION IF FOUND.
  IF NOT VERSION IS INITIAL.
    CLEAR OUTPUT.
    CONCATENATE FUPARAREF-FUNCNAME
                '|'
                VERSION INTO OUTPUT.
    APPEND OUTPUT TO ENTRIES.
    CLEAR ENTRIES.
  ENDIF.
ENDSELECT.

ENDFUNCTION.

FORM Z_AW_GET_VERSION_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.