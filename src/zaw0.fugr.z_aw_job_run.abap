FUNCTION Z_AW_JOB_RUN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(PROGRAMNAME) LIKE  SY-REPID
*"             VALUE(JOBGROUP) LIKE  TBTCO-JOBGROUP
*"             VALUE(JOBNAME) LIKE  TBTCO-JOBNAME
*"             VALUE(HOST) LIKE  TBTCO-BTCSYSREAX
*"             VALUE(JOBCLASS) LIKE  TBTCO-JOBCLASS
*"       EXPORTING
*"             VALUE(JOBCOUNT) LIKE  TBTCO-JOBCOUNT
*"             VALUE(JOB_RELEASED) LIKE  BTCH0000-CHAR1
*"       TABLES
*"              SELTAB STRUCTURE  RSPARAMS
*"       EXCEPTIONS
*"              ABAP_PROGRAM_SYNTAX_ERROR
*"              CANT_CREATE_JOB
*"              INVALID_JOB_DATA
*"              JOBNAME_MISSING
*"              CANT_START_IMMEDIATE
*"              INVALID_STARTDATE
*"              JOB_CLOSE_FAILED
*"              JOB_NOSTEPS
*"              JOB_NOTEX
*"              LOCK_FAILED
*"              ABAP_PROGRAM_DOES_NOT_EXIST
*"----------------------------------------------------------------------
DATA: BEGIN OF PROGRAM OCCURS 0.
       INCLUDE STRUCTURE PROGTAB.
DATA: END OF PROGRAM.

SELECT SINGLE * FROM D010SINF WHERE PROG = PROGRAMNAME.
IF SY-SUBRC <> 0.
  RAISE ABAP_PROGRAM_DOES_NOT_EXIST.
ENDIF.
* if we need syntax check, do it here.
DATA: MESS(72), LIN(72), WRD(72).
READ REPORT PROGRAMNAME INTO PROGRAM.
  SYNTAX-CHECK FOR PROGRAM MESSAGE MESS LINE LIN WORD WRD.

IF MESS <> SPACE.
   RAISE ABAP_PROGRAM_SYNTAX_ERROR.
"  errormessage = mess.
  EXIT.
ENDIF.

  IF JOBNAME EQ SPACE.
     RAISE JOBNAME_MISSING.
  ENDIF.

  REFRESH GLOBAL_STEP_TBL.
  CLEAR   GLOBAL_STEP_TBL.
  CLEAR   GLOBAL_JOB.

  GLOBAL_JOB-JOBNAME   = JOBNAME.
  GLOBAL_JOB-JOBCLASS  = JOBCLASS.
  GLOBAL_JOB-NEWFLAG   = 'O'.
  GLOBAL_STEP_TBL-PROGRAM = 'RSBTCPT3'.
  GLOBAL_STEP_TBL-TYP     = BTC_ABAP.
  GLOBAL_STEP_TBL-STATUS  = BTC_SCHEDULED.
  GLOBAL_STEP_TBL-AUTHCKNAM = SY-UNAME.
  APPEND GLOBAL_STEP_TBL.

  CALL FUNCTION 'BP_JOB_CREATE'
       EXPORTING
            JOB_CR_DIALOG       = BTC_NO
            JOB_CR_HEAD_INP     = GLOBAL_JOB
       IMPORTING
            JOB_CR_HEAD_OUT     = GLOBAL_JOB
            JOB_CR_STDT_OUT     = GLOBAL_START_DATE
       TABLES
            JOB_CR_STEPLIST     = GLOBAL_STEP_TBL
       EXCEPTIONS
            INVALID_JOB_DATA    =  1
            OTHERS              = 99.

  CASE SY-SUBRC.
    WHEN 0.
      JOBCOUNT = GLOBAL_JOB-JOBCOUNT.
    WHEN 1.
      RAISE INVALID_JOB_DATA.
    WHEN OTHERS.
      RAISE CANT_CREATE_JOB.
  ENDCASE.


    SUBMIT (PROGRAMNAME)
        WITH SELECTION-TABLE SELTAB AND RETURN
           USER SY-UNAME
           VIA JOB JOBNAME NUMBER JOBCOUNT.
*          to sap-spool.
*            spool parameters user_print_params
*            archive parameters user_arc_parms
*             without spool dynpro.
*            exporting list to memory.

    CALL FUNCTION 'JOB_CLOSE'
         EXPORTING
*         AT_OPMODE                   = ' '
*         AT_OPMODE_PERIODIC          = ' '
*         CALENDAR_ID                 = ' '
*         EVENT_ID                    = ' '
*         EVENT_PARAM                 = ' '
*         EVENT_PERIODIC              = ' '
          JOBCOUNT                    = JOBCOUNT
          JOBNAME                     = JOBNAME
*         LASTSTRTDT                  = NO_DATE
*         LASTSTRTTM                  = NO_TIME
*         PRDDAYS                     = 0
*         PRDHOURS                    = 0
*         PRDMINS                     = 0
*         PRDMONTHS                   = 0
*         PRDWEEKS                    = 0
*         PREDJOB_CHECKSTAT           = ' '
*         PRED_JOBCOUNT               = ' '
*         PRED_JOBNAME                = ' '
*         SDLSTRTDT                   = NO_DATE
*         SDLSTRTTM                   = NO_TIME
*         STARTDATE_RESTRICTION       = BTC_PROCESS_ALWAYS
          STRTIMMED                   = 'X'
          TARGETSYSTEM                = HOST
*         START_ON_WORKDAY_NOT_BEFORE = SY-DATUM
*         START_ON_WORKDAY_NR         = 0
*         WORKDAY_COUNT_DIRECTION     = 0
         IMPORTING
              JOB_WAS_RELEASED            = JOB_RELEASED
         EXCEPTIONS
              CANT_START_IMMEDIATE        = 1
              INVALID_STARTDATE           = 2
              JOBNAME_MISSING             = 3
              JOB_CLOSE_FAILED            = 4
              JOB_NOSTEPS                 = 5
              JOB_NOTEX                   = 6
              LOCK_FAILED                 = 7.

CASE SY-SUBRC.
  WHEN 1.
    RAISE CANT_START_IMMEDIATE.
  WHEN 2.
    RAISE INVALID_STARTDATE.
  WHEN 3.
    RAISE JOBNAME_MISSING.
  WHEN 4.
    RAISE JOB_CLOSE_FAILED.
  WHEN 5.
    RAISE JOB_NOSTEPS.
  WHEN 6.
    RAISE JOB_NOTEX.
  WHEN 7.
    RAISE LOCK_FAILED.
ENDCASE.

ENDFUNCTION.

FORM Z_AW_JOB_RUN_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
