FUNCTION Z_AW_JOB_LOG.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(JOBCOUNT) LIKE  TBTCO-JOBCOUNT
*"             VALUE(JOBNAME) LIKE  TBTCO-JOBNAME
*"             VALUE(HOST) LIKE  TBTCO-BTCSYSREAX
*"       TABLES
*"              LOG STRUCTURE  BTCTLE
*"       EXCEPTIONS
*"              JOB_NOT_FOUND
*"              JOB_LOG_NOT_FOUND
*"              JOB_NUMBER_NOT_VALID
*"              LOGNAME_WRONG_FORMAT
*"              LOGNAME_MISSING
*"              LOG_HANDLE_ERROR
*"              LOG_IS_EMPTY
*"              LOG_NOT_FOUND
*"              TEMSE_CONVERSION_NOT_POSSIBLE
*"              TEMSE_ERROR
*"              TEMSE_FUNCTION_ERROR
*"----------------------------------------------------------------------

DATA: BEGIN OF LOG1 OCCURS 0.
       INCLUDE STRUCTURE BTCTL1.
DATA: END OF LOG1.
DATA LOGNAME LIKE TBTCO-JOBLOG.
*tables tbtco.

IF JOBCOUNT CN '0123456789'.
   RAISE JOB_NUMBER_NOT_VALID.
ELSE.
   SELECT SINGLE JOBNAME JOBLOG BTCSYSREAX INTO
       (JOBNAME,LOGNAME, HOST) FROM TBTCO
       WHERE JOBCOUNT = JOBCOUNT AND JOBNAME = JOBNAME.
   IF SY-SUBRC <> 0.
      RAISE JOB_NUMBER_NOT_VALID.
   ENDIF.
ENDIF.


CALL FUNCTION 'COMMON_LOG_READ_T100'
     EXPORTING
          CLIENT                        = SY-MANDT
          DECODE_MESSAGE                = 'X'
          LOGNAME                       = LOGNAME
     TABLES
          LOGTABLE                      = LOG1
     EXCEPTIONS
          LOGNAME_MISSING               = 1
          LOG_HANDLE_ERROR              = 2
          LOG_IS_EMPTY                  = 3
          LOG_NOT_FOUND                 = 4
          TEMSE_CONVERSION_NOT_POSSIBLE = 5
          TEMSE_ERROR                   = 6
          TEMSE_FUNCTION_ERROR          = 7.

CASE SY-SUBRC.
  WHEN 1. RAISE LOGNAME_MISSING.
  WHEN 2. RAISE LOG_HANDLE_ERROR.
  WHEN 3. RAISE LOG_IS_EMPTY.
  WHEN 4. RAISE LOG_NOT_FOUND.
  WHEN 5. RAISE TEMSE_CONVERSION_NOT_POSSIBLE.
  WHEN 6. RAISE TEMSE_ERROR.
  WHEN 7. RAISE TEMSE_FUNCTION_ERROR.
ENDCASE.

LOOP AT LOG1.
 MOVE LOG1-ENTERDATE TO LOG-ENTERDATE.
 MOVE LOG1-ENTERTIME TO LOG-ENTERTIME.
 MOVE LOG1-MSGTEXT TO LOG-LOGMESSAGE.
 APPEND LOG.
ENDLOOP.

ENDFUNCTION.

FORM Z_AW_JOB_LOG_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
