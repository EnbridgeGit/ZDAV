FUNCTION Z_AW_SYNTAX_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       EXPORTING
*"             VALUE(ERRORMESSAGE) LIKE  SY-MSGV1
*"       TABLES
*"              PROGRAM STRUCTURE  PROGTAB
*"----------------------------------------------------------------------

DATA: MESS(72), LIN(72), WRD(72).

SYNTAX-CHECK FOR PROGRAM MESSAGE MESS LINE LIN WORD WRD.
IF MESS <> SPACE.
  ERRORMESSAGE = MESS.
  EXIT.
ENDIF.

ENDFUNCTION.

FORM Z_AW_SYNTAX_CHECK_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
