FUNCTION Z_AW_JOB_STATUS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(JOBNAME) LIKE  TBTCO-JOBNAME
*"             VALUE(JOBCOUNT) LIKE  TBTCO-JOBCOUNT
*"             VALUE(HOST) LIKE  TBTCO-BTCSYSREAX
*"       EXPORTING
*"             VALUE(STATUS) LIKE  TBTCO-STATUS
*"       EXCEPTIONS
*"              JOB_NOT_FOUND
*"----------------------------------------------------------------------

IF HOST <> SPACE.
  SELECT SINGLE STATUS FROM TBTCO INTO STATUS
                   WHERE JOBNAME = JOBNAME AND
                         JOBCOUNT = JOBCOUNT AND
                         BTCSYSREAX = HOST.

  IF SY-SUBRC <> 0.
    SELECT SINGLE STATUS FROM TBTCO INTO STATUS
                   WHERE JOBNAME = JOBNAME AND
                         JOBCOUNT = JOBCOUNT.

    IF SY-SUBRC <> 0.
       RAISE JOB_NOT_FOUND.
    ENDIF.
  ENDIF.
ELSE.
  SELECT SINGLE STATUS FROM TBTCO INTO STATUS
                   WHERE JOBNAME = JOBNAME AND
                         JOBCOUNT = JOBCOUNT.

  IF SY-SUBRC <> 0.
     RAISE JOB_NOT_FOUND.
  ENDIF.
ENDIF.

ENDFUNCTION.

FORM Z_AW_JOB_STATUS_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
