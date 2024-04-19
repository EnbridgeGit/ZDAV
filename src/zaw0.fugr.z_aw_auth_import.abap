FUNCTION Z_AW_AUTH_IMPORT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(AUTHPROF) LIKE  USR10-PROFN
*"       TABLES
*"              AUTHOBJ STRUCTURE  LISTZEILE
*"       EXCEPTIONS
*"              SECURITY_PROFILE_DOESNOT_EXIST
*"----------------------------------------------------------------------

IF AUTHPROF <> SPACE.
  SELECT SINGLE * FROM USR10 WHERE PROFN = AUTHPROF AND AKTPS = 'A'.
  IF USR10-TYP = 'S'.
    SELECT * FROM UST10S WHERE PROFN = AUTHPROF
                         AND   AKTPS = 'A'.
       SELECT * FROM UST12 WHERE OBJCT = UST10S-OBJCT
                           AND   AUTH  = UST10S-AUTH
                           AND   AKTPS = 'A'.
          INT_AUTH-OBJECT = UST12-OBJCT.
          INT_AUTH-FIELD = UST12-FIELD.
          INT_AUTH-FROM_VALUE = UST12-VON.
          INT_AUTH-TO_VALUE   = UST12-BIS.
          APPEND INT_AUTH.
       ENDSELECT.
    ENDSELECT.
  ELSE.
    SELECT * FROM UST10C WHERE PROFN = AUTHPROF
                         AND AKTPS = 'A'.
      SELECT * FROM UST10S WHERE PROFN = UST10C-SUBPROF
                           AND   AKTPS = 'A'.
       SELECT * FROM UST12 WHERE OBJCT = UST10S-OBJCT
                           AND   AUTH  = UST10S-AUTH
                           AND   AKTPS = 'A'.
          INT_AUTH-OBJECT = UST12-OBJCT.
          INT_AUTH-FIELD = UST12-FIELD.
          INT_AUTH-FROM_VALUE = UST12-VON.
          INT_AUTH-TO_VALUE   = UST12-BIS.
          APPEND INT_AUTH.
       ENDSELECT.
      ENDSELECT.
    ENDSELECT.
  ENDIF.
ENDIF.

LOOP AT INT_AUTH.
   CONCATENATE INT_AUTH-OBJECT '|'
            INT_AUTH-FIELD '|'
            INT_AUTH-FROM_VALUE '|'
            INT_AUTH-TO_VALUE '|'
   INTO AUTHOBJ-ZEILE.
   APPEND AUTHOBJ.
ENDLOOP.


ENDFUNCTION.

FORM Z_AW_AUTH_IMPORT_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
