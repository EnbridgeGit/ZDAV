INCLUDE MS38LCOM.
DATA: IMPORT           VALUE '1',     "From LSCATTOP
        EXPORT           VALUE '2',
        ITAB             VALUE '3',
        EXCEPT           VALUE '4',
        IMP_STRUC_FIELD  VALUE '5',
        EXP_STRUC_FIELD  VALUE '6',
        ITAB_FIELD       VALUE '7',
        CHANGE           VALUE '8',
        CHA_STRUC_FIELD  VALUE '9'.
FIELD-SYMBOLS <F>.
DATA: BEGIN OF NAMETAB OCCURS 500.    "NAMETAB
          INCLUDE STRUCTURE DNTAB.
  DATA: END OF   NAMETAB.
DATA: BEGIN OF TABHEAD.               "TABHEAD
          INCLUDE STRUCTURE X030L.
  DATA: END OF   TABHEAD.
DATA: POSITION     LIKE SY-FDPOS,
      TABNAME_SAVE LIKE CATFU-TABNAME,
      COUNT        LIKE SY-TABIX,
      READINDEX    LIKE SY-TABIX.
TABLES: DD04L, FUPARAREF.

DATA PTYPE.
DATA PSTR LIKE DFIES OCCURS 0 WITH HEADER LINE.
DATA GOT_IT.
DATA FIELDNAME LIKE DCOBJDEF-NAME.

* The marker is used for parameters defined by DATA ELEMENTS
* for example, see ASSORT_LIST_TBD22_READ
DATA: MARKER LIKE CATFU-DDLEN VALUE 997899.

*---------------------------------------------------------------------*
*       FORM MOVE_IF_IMPORT                                           *
*---------------------------------------------------------------------*
FORM MOVE_IF_IMPORT TABLES PRMTAB STRUCTURE CATFU.
  LOOP AT IF_IMPORT.
    CLEAR PRMTAB.
    PRMTAB-PARAMTYPE = IMPORT.
    PRMTAB-NAME      = IF_IMPORT-PARAMETER.
* Check to see if the field is defined by TYPE
    IF IF_IMPORT-DBFIELD = SPACE
      AND IF_IMPORT-TYP <> SPACE.
      IF IF_IMPORT-TYP CA '-'.
        ASSIGN IF_IMPORT-TYP(SY-FDPOS) TO <F>.
        PRMTAB-TABNAME   = <F>.
        POSITION = SY-FDPOS + 1.
        ASSIGN IF_IMPORT-TYP+POSITION(*) TO <F>.
        PRMTAB-FIELDNAME = <F>.
        IF PRMTAB-FIELDNAME <> SPACE.
          PRMTAB-IMPDEFAULT = IF_IMPORT-DEFAULT.
          APPEND PRMTAB.
          CONTINUE.
        ENDIF.
      ELSE.
        PRMTAB-TABNAME = IF_IMPORT-TYP.
*        PRMTAB-PARAMTYPE = ITAB.
        IF PRMTAB-TABNAME(7) = 'REF TO '.
          SHIFT PRMTAB-TABNAME BY 7 PLACES.
        ENDIF.
        APPEND PRMTAB.
        CONTINUE.
      ENDIF.
    ENDIF.
* Check to see if the field is defined by LIKE
    IF IF_IMPORT-DBFIELD CA '-'.
      ASSIGN IF_IMPORT-DBFIELD(SY-FDPOS) TO <F>.
      PRMTAB-TABNAME   = <F>.
      POSITION = SY-FDPOS + 1.
      ASSIGN IF_IMPORT-DBFIELD+POSITION(*) TO <F>.
      PRMTAB-FIELDNAME = <F>.
      IF PRMTAB-FIELDNAME = SPACE.
        PERFORM MOVE_INITIAL TABLES PRMTAB.
      ENDIF.
      PRMTAB-IMPDEFAULT = IF_IMPORT-DEFAULT.
      APPEND PRMTAB.
    ELSE.
      PRMTAB-TABNAME = IF_IMPORT-DBFIELD.
      IF PRMTAB-TABNAME = SPACE.
        PERFORM MOVE_INITIAL TABLES PRMTAB.
        PRMTAB-IMPDEFAULT = IF_IMPORT-DEFAULT.
      ENDIF.
      APPEND PRMTAB.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MOVE_IF_CHANGE                                           *
*---------------------------------------------------------------------*
FORM MOVE_IF_CHANGE TABLES PRMTAB STRUCTURE CATFU.
  LOOP AT IF_CHANGE.
    CLEAR PRMTAB.
    PRMTAB-PARAMTYPE = CHANGE.
    PRMTAB-NAME      = IF_CHANGE-PARAMETER.
* Check to see if the field is defined by TYPE
    IF IF_CHANGE-DBFIELD = SPACE
      AND IF_CHANGE-TYP <> SPACE.
      IF IF_CHANGE-TYP CA '-'.
        ASSIGN IF_CHANGE-TYP(SY-FDPOS) TO <F>.
        PRMTAB-TABNAME   = <F>.
        POSITION = SY-FDPOS + 1.
        ASSIGN IF_CHANGE-TYP+POSITION(*) TO <F>.
        PRMTAB-FIELDNAME = <F>.
        IF PRMTAB-FIELDNAME <> SPACE.
          PRMTAB-IMPDEFAULT = IF_CHANGE-DEFAULT.
          APPEND PRMTAB.
          CONTINUE.
        ENDIF.
      ELSE.
        PRMTAB-TABNAME = IF_CHANGE-TYP.
*        PRMTAB-PARAMTYPE = ITAB.
        IF PRMTAB-TABNAME(7) = 'REF TO '.
          SHIFT PRMTAB-TABNAME BY 7 PLACES.
        ENDIF.
        APPEND PRMTAB.
        CONTINUE.
      ENDIF.
    ENDIF.
* Check to see if the field is defined by LIKE
    IF IF_CHANGE-DBFIELD CA '-'.
      ASSIGN IF_CHANGE-DBFIELD(SY-FDPOS) TO <F>.
      PRMTAB-TABNAME   = <F>.
      POSITION = SY-FDPOS + 1.
      ASSIGN IF_CHANGE-DBFIELD+POSITION(*) TO <F>.
      PRMTAB-FIELDNAME = <F>.
      IF PRMTAB-FIELDNAME = SPACE.
        PERFORM MOVE_INITIAL TABLES PRMTAB.
        PRMTAB-LOWERCASE = SPACE.
      ENDIF.
      PRMTAB-IMPDEFAULT = IF_CHANGE-DEFAULT.
      APPEND PRMTAB.
    ELSE.
      PRMTAB-TABNAME = IF_CHANGE-DBFIELD.
      IF PRMTAB-TABNAME = SPACE.
        PERFORM MOVE_INITIAL TABLES PRMTAB.
        PRMTAB-LOWERCASE = SPACE.
        PRMTAB-IMPDEFAULT = IF_CHANGE-DEFAULT.
      ENDIF.
      APPEND PRMTAB.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MOVE_IF_EXPORT                                           *
*---------------------------------------------------------------------*
FORM MOVE_IF_EXPORT TABLES PRMTAB STRUCTURE CATFU.
  LOOP AT IF_EXPORT.
    CLEAR PRMTAB.
    PRMTAB-PARAMTYPE = EXPORT.
    PRMTAB-NAME      = IF_EXPORT-PARAMETER.
* Check to see if the field is defined by TYPE
    IF IF_EXPORT-DBFIELD = SPACE
      AND IF_EXPORT-TYP <> SPACE.
      IF IF_EXPORT-TYP CA '-'.
        ASSIGN IF_EXPORT-TYP(SY-FDPOS) TO <F>.
        PRMTAB-TABNAME   = <F>.
        POSITION = SY-FDPOS + 1.
        ASSIGN IF_EXPORT-TYP+POSITION(*) TO <F>.
        PRMTAB-FIELDNAME = <F>.
        IF PRMTAB-FIELDNAME <> SPACE.
          APPEND PRMTAB.
          CONTINUE.
        ENDIF.
      ELSE.
        PRMTAB-TABNAME = IF_EXPORT-TYP.
*        PRMTAB-PARAMTYPE = ITAB.
        IF PRMTAB-TABNAME(7) = 'REF TO '.
          SHIFT PRMTAB-TABNAME BY 7 PLACES.
        ENDIF.
        APPEND PRMTAB.
        CONTINUE.
      ENDIF.
    ENDIF.
* Check to see if the field is defined by LIKE
    IF IF_EXPORT-DBFIELD CA '-'.
      ASSIGN IF_EXPORT-DBFIELD(SY-FDPOS) TO <F>.
      PRMTAB-TABNAME   = <F>.
      POSITION = SY-FDPOS + 1.
      ASSIGN IF_EXPORT-DBFIELD+POSITION(*) TO <F>.
      PRMTAB-FIELDNAME = <F>.
      IF PRMTAB-FIELDNAME = SPACE.
        PERFORM MOVE_INITIAL TABLES PRMTAB.
        PRMTAB-LOWERCASE = SPACE.
      ENDIF.
      APPEND PRMTAB.
    ELSE.
      PRMTAB-TABNAME = IF_EXPORT-DBFIELD.
      IF PRMTAB-TABNAME = SPACE.
        PERFORM MOVE_INITIAL TABLES PRMTAB.
        PRMTAB-LOWERCASE = SPACE.
      ENDIF.
      APPEND PRMTAB.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MOVE_IF_TABLES                                           *
*---------------------------------------------------------------------*
FORM MOVE_IF_TABLES TABLES PRMTAB STRUCTURE CATFU.
  LOOP AT IF_TABLES.
    CLEAR PRMTAB.
    PRMTAB-PARAMTYPE = ITAB.
    PRMTAB-NAME    = IF_TABLES-PARAMETER.
    PRMTAB-TABNAME = IF_TABLES-DBSTRUCT.
    IF PRMTAB-TABNAME = SPACE.
      PRMTAB-TABNAME = 'RSSOURCE'.
*     RAISE REF_STRUCTURE_MISSING.
    ENDIF.
    APPEND PRMTAB.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MOVE_IF_EXCEPT                                           *
*---------------------------------------------------------------------*
FORM MOVE_IF_EXCEPT TABLES PRMTAB STRUCTURE CATFU.
  LOOP AT IF_EXCEPT.
    CLEAR PRMTAB.
    PRMTAB-PARAMTYPE = EXCEPT.
    PRMTAB-NAME = IF_EXCEPT-EXCEPTION.
    APPEND PRMTAB.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM DDIC_INFO_GET_1                                          *
*---------------------------------------------------------------------*
*       get ddic-info on each each im-/export and itab-field          *
*---------------------------------------------------------------------*
FORM DDIC_INFO_GET_1 TABLES PRMTAB STRUCTURE CATFU.
  CLEAR TABNAME_SAVE.
  SORT PRMTAB BY TABNAME PARAMTYPE.
  COUNT = 0.
  READINDEX = 1.
  DESCRIBE TABLE PRMTAB LINES COUNT.
  WHILE READINDEX <= COUNT.
    READ TABLE PRMTAB INDEX READINDEX.
*   CHECK PRMTAB-FIELDNAME <> SPACE.
    IF PRMTAB-TABNAME <> SPACE AND PRMTAB-TABNAME <> TABNAME_SAVE.
      TABNAME_SAVE = PRMTAB-TABNAME.
      PERFORM NAMETAB_GET USING PRMTAB-TABNAME RC.
      IF RC = 0 OR RC = 8.
        LOOP AT PRMTAB
          WHERE TABNAME = TABNAME_SAVE.
          LOOP AT NAMETAB
            WHERE TABNAME   = PRMTAB-TABNAME
            AND   FIELDNAME = PRMTAB-FIELDNAME
            AND   LANGU     = SY-LANGU.
            PERFORM MOVE_NAMETAB TABLES PRMTAB.
            MODIFY PRMTAB.
          ENDLOOP.
        ENDLOOP.
* Do not raise exception - need to check for fields defined by data
* elements, etc.
*      ELSE.
*        RAISE NAMETAB_FAULT.
      ENDIF.
    ENDIF.
    READINDEX = READINDEX + 1.
  ENDWHILE.
  SORT PRMTAB.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM DDIC_INFO_GET_2                                          *
*---------------------------------------------------------------------*
*       solve all im-/export and itab-structures                      *
*       get ddic-info                                                 *
*---------------------------------------------------------------------*
FORM DDIC_INFO_GET_2 TABLES PRMTAB STRUCTURE CATFU.
  LOOP AT PRMTAB
    WHERE  PARAMTYPE = IMPORT OR PARAMTYPE = EXPORT OR
           PARAMTYPE = CHANGE OR PARAMTYPE = ITAB.
    CHECK PRMTAB-TABNAME   <> SPACE.
    CHECK PRMTAB-FIELDNAME =  SPACE.
    PERFORM NAMETAB_GET USING PRMTAB-TABNAME RC.
    IF RC = 0 OR RC = 8.
* Mark the original field name in PRMTAB to exclude
* it from future processing
      MOVE MARKER TO PRMTAB-INTLEN.
      MODIFY PRMTAB.
      CASE PRMTAB-PARAMTYPE.
        WHEN IMPORT.
          PRMTAB-PARAMTYPE = IMP_STRUC_FIELD.
        WHEN CHANGE.
          PRMTAB-PARAMTYPE = CHA_STRUC_FIELD.
        WHEN EXPORT.
          PRMTAB-PARAMTYPE = EXP_STRUC_FIELD.
        WHEN ITAB.
          PRMTAB-PARAMTYPE = ITAB_FIELD.
      ENDCASE.
      LOOP AT NAMETAB.
        PRMTAB-FIELDNAME = NAMETAB-FIELDNAME.
        PERFORM MOVE_NAMETAB TABLES PRMTAB.
        APPEND PRMTAB.
      ENDLOOP.
* Do not raise exception - need to check for fields defined by data
* elements, etc.
*    ELSE.
*      RAISE NAMETAB_FAULT.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM NAMETAB_GET                                              *
*---------------------------------------------------------------------*
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM NAMETAB_GET USING TABNAME RC.

* Let's make sure it is not a domain name
  SELECT DOMNAME
  INTO DD04L-DOMNAME
  FROM DD04L
  UP TO 1 ROWS
  WHERE ROLLNAME = TABNAME. "Get domain name first
  ENDSELECT.
  IF SY-SUBRC = 0.
    RC = 8.
  ELSE.
    CALL FUNCTION 'NAMETAB_GET'
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(FUNCNAME) LIKE  TFDIR-FUNCNAME
*"       TABLES
*"              PRMTAB STRUCTURE  CATFU
*"       EXCEPTIONS
*"              FM_NOT_FOUND
*"              NAMETAB_FAULT
*"              REF_FIELD_MISSING
*"              REF_STRUCTURE_MISSING
*"----------------------------------------------------------------------
         EXPORTING
              LANGU               = SY-LANGU
              TABNAME             = TABNAME
         IMPORTING
              HEADER              = TABHEAD
         TABLES
              NAMETAB             = NAMETAB
         EXCEPTIONS
              INTERNAL_ERROR      = 4
              NO_TEXTS_FOUND      = 8
              TABLE_HAS_NO_FIELDS = 12
              TABLE_NOT_ACTIV     = 16.
    RC = SY-SUBRC.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MOVE_NAMETAB                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MOVE_NAMETAB TABLES PRMTAB STRUCTURE CATFU.
  PRMTAB-DATATYPE  = NAMETAB-DATATYPE.
  PRMTAB-INTTYPE   = NAMETAB-INTTYPE.
  PRMTAB-DDLEN     = NAMETAB-DDLEN.
  PRMTAB-INTLEN    = NAMETAB-INTLEN.
  PRMTAB-DECIMALS  = NAMETAB-DECIMALS.
  PRMTAB-LOWERCASE = NAMETAB-LOWERCASE.
  PRMTAB-SIGN      = NAMETAB-SIGN.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MOVE_INITIAL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MOVE_INITIAL TABLES PRMTAB STRUCTURE CATFU.
  PRMTAB-DATATYPE  = 'CHAR'.
  PRMTAB-INTTYPE   = 'C'.
  PRMTAB-DDLEN     = 20.
  PRMTAB-INTLEN    = 201. "Special mark to distinguish from "real" C(20)
  PRMTAB-LOWERCASE = 'X'.
ENDFORM.

FUNCTION Z_AW_FUNCTION_GET.

  REFRESH: IF_IMPORT, IF_EXPORT, IF_TABLES, IF_EXCEPT, PRMTAB,
           IF_CHANGE.
  CLEAR:   IF_IMPORT, IF_EXPORT, IF_TABLES, IF_EXCEPT, PRMTAB,
           IF_CHANGE.

  CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING
          FUNCNAME = FUNCNAME
*     IMPORTING
*         GLOBAL_FLAG = I01
*         REMOTE_CALL = I02
*         UPDATE_TASK = I03
      TABLES
          EXCEPTION_LIST =   IF_EXCEPT
          EXPORT_PARAMETER = IF_EXPORT
          IMPORT_PARAMETER = IF_IMPORT
          CHANGING_PARAMETER = IF_CHANGE
          TABLES_PARAMETER = IF_TABLES
      EXCEPTIONS
          ERROR_MESSAGE = 01
          FUNCTION_NOT_FOUND = 02
          INVALID_NAME = 03.
  IF SY-SUBRC > 0.
    RAISE FM_NOT_FOUND.
  ENDIF.

  PERFORM MOVE_IF_IMPORT TABLES PRMTAB.
  PERFORM MOVE_IF_CHANGE TABLES PRMTAB.
  PERFORM MOVE_IF_EXPORT TABLES PRMTAB.
  PERFORM MOVE_IF_TABLES TABLES PRMTAB.
  PERFORM MOVE_IF_EXCEPT TABLES PRMTAB.

  LOOP AT PRMTAB WHERE TABNAME = 'SY  '.
    PRMTAB-TABNAME = 'SYST'.
    MODIFY PRMTAB.
  ENDLOOP.

*-- get ddic-information on each field i.e. all im-/export-parameters -*
  PERFORM DDIC_INFO_GET_1 TABLES PRMTAB.

*-- get ddic-information on each field in import/export-structures ----*
  PERFORM DDIC_INFO_GET_2 TABLES PRMTAB.

* Check for parameters defined by a data element

  LOOP AT PRMTAB WHERE TABNAME <> SPACE AND
       FIELDNAME = SPACE AND ( DDLEN = 0 OR INTLEN = 0 ).
    SELECT DOMNAME
    INTO DD04L-DOMNAME
    FROM DD04L
    UP TO 1 ROWS
    WHERE ROLLNAME = PRMTAB-TABNAME. "Get domain name first
    ENDSELECT.
    IF SY-SUBRC <> 4.
      SELECT *
      FROM DD01L
      UP TO 1 ROWS
      WHERE DOMNAME = DD04L-DOMNAME. "Get data element info
        IF SY-SUBRC <> 4.
          CLEAR DD03L-INTTYPE.
          SELECT INTTYPE
          INTO DD03L-INTTYPE
          FROM DD03L
          UP TO 1 ROWS
          WHERE DOMNAME = DD04L-DOMNAME.
          ENDSELECT.
          PRMTAB-DATATYPE = DD01L-DATATYPE.
          PRMTAB-INTTYPE  = DD03L-INTTYPE.
          PRMTAB-INTLEN = DD01L-LENG.
          PRMTAB-DECIMALS = DD01L-DECIMALS.
          PRMTAB-DDLEN = DD01L-OUTPUTLEN.
          PRMTAB-LOWERCASE = DD01L-LOWERCASE.
          PRMTAB-SIGN = DD01L-SIGNFLAG.
          MODIFY PRMTAB.
        ENDIF.
      ENDSELECT.
    ENDIF.
* If the marker is set, this field is a structure
*   - no further processing
    IF SY-SUBRC = 4 AND PRMTAB-INTLEN <> MARKER.
* This is not a data element
      CLEAR PRMTAB-TABNAME.
      MODIFY PRMTAB.
    ENDIF.
  ENDLOOP.

IF SY-SUBRC = 0.
   LOOP AT PRMTAB WHERE TABNAME IS INITIAL.
     MOVE PRMTAB-PARAMTYPE TO PTYPE.
     TRANSLATE PTYPE USING '1I2E3T4X5I6E7T8C9C'.
     SELECT STRUCTURE FROM FUPARAREF
     INTO FUPARAREF-STRUCTURE
     UP TO 1 ROWS
     WHERE FUNCNAME = FUNCNAME
       AND PARAMETER = PRMTAB-NAME
       AND PARAMTYPE = PTYPE
       AND TYPE = 'X'.
     ENDSELECT.
     IF SY-SUBRC = 0.
       CLEAR GOT_IT.
       IF SY-SAPRL(1) > '3'.
         MOVE FUPARAREF-STRUCTURE TO FIELDNAME.
         CALL FUNCTION 'DDIF_FIELDINFO_GET'
           EXPORTING
                TABNAME             = FIELDNAME
                ALL_TYPES           = 'X'
           IMPORTING
                DFIES_WA            = PSTR
           EXCEPTIONS
              OTHERS              = 1.
         IF SY-SUBRC = 0.
           READ TABLE PSTR INDEX 1.
           IF PSTR-INTLEN > 0.
             GOT_IT = 'X'.
             PRMTAB-DATATYPE  = FUPARAREF-STRUCTURE.
             PRMTAB-INTTYPE   = PSTR-INTTYPE.
             PRMTAB-DDLEN     = PSTR-LENG.
             PRMTAB-INTLEN    = PSTR-INTLEN.
             PRMTAB-DECIMALS  = PSTR-DECIMALS.
             PRMTAB-LOWERCASE = PSTR-LOWERCASE.
           ENDIF. " Read length
         ENDIF. " Call DDIF_FIELDINFO_GET
       ENDIF. " SAP version > 3
       IF GOT_IT IS INITIAL.
          CASE FUPARAREF-STRUCTURE. "Check common types
            when 'D' or 'DATS'.
              PRMTAB-DATATYPE  = 'DATS'.
              PRMTAB-INTTYPE   = 'D'.
              PRMTAB-DDLEN     = 8.
              PRMTAB-INTLEN    = 8.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'T' or 'TIMS'.
              PRMTAB-DATATYPE  = 'TIMS'.
              PRMTAB-INTTYPE   = 'T'.
              PRMTAB-DDLEN     = 6.
              PRMTAB-INTLEN    = 6.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'F' or 'FLTP'.
              PRMTAB-DATATYPE  = 'FLTP'.
              PRMTAB-INTTYPE   = 'F'.
              PRMTAB-DDLEN     = 16.
              PRMTAB-INTLEN    = 8.
              PRMTAB-DECIMALS  = 16.
              CLEAR PRMTAB-LOWERCASE.
            when 'ACCP'.
              PRMTAB-DATATYPE  = 'ACCP'.
              PRMTAB-INTTYPE   = 'N'.
              PRMTAB-DDLEN     = 6.
              PRMTAB-INTLEN    = 6.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'CLNT'.
              PRMTAB-DATATYPE  = 'CLNT'.
              PRMTAB-INTTYPE   = 'C'.
              PRMTAB-DDLEN     = 3.
              PRMTAB-INTLEN    = 3.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'CUKY'.
              PRMTAB-DATATYPE  = 'CUKY'.
              PRMTAB-INTTYPE   = 'C'.
              PRMTAB-DDLEN     = 5.
              PRMTAB-INTLEN    = 5.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'LANG'.
              PRMTAB-DATATYPE  = 'LANG'.
              PRMTAB-INTTYPE   = 'C'.
              PRMTAB-DDLEN     = 1.
              PRMTAB-INTLEN    = 1.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'PREC'.
              PRMTAB-DATATYPE  = 'PREC'.
              PRMTAB-INTTYPE   = 's'.
              PRMTAB-DDLEN     = 2.
              PRMTAB-INTLEN    = 2.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'UNIT'.
              PRMTAB-DATATYPE  = 'UNIT'.
              PRMTAB-INTTYPE   = 'C'.
              PRMTAB-DDLEN     = 3.
              PRMTAB-INTLEN    = 3.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'I' or 'INT4'.
              PRMTAB-DATATYPE  = 'INT4'.
              PRMTAB-INTTYPE   = 'I'.
              PRMTAB-DDLEN     = 10.
              PRMTAB-INTLEN    = 4.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'INT2'.
              PRMTAB-DATATYPE  = 'INT2'.
              PRMTAB-INTTYPE   = 'I'.
              PRMTAB-DDLEN     = 5.
              PRMTAB-INTLEN    = 2.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
            when 'INT1'.
              PRMTAB-DATATYPE  = 'INT1'.
              PRMTAB-INTTYPE   = 'I'.
              PRMTAB-DDLEN     = 3.
              PRMTAB-INTLEN    = 1.
              CLEAR PRMTAB-DECIMALS.
              CLEAR PRMTAB-LOWERCASE.
         ENDCASE.
       ENDIF. " Check for pre-defined types
     ENDIF. " Check FUPARAREF
     MODIFY PRMTAB.
   ENDLOOP.
ENDIF.

LOOP AT PRMTAB.
  IF PRMTAB-INTLEN EQ 0.
    IF PRMTAB-DDLEN > 0.
      MOVE PRMTAB-DDLEN TO PRMTAB-INTLEN.
    ELSE.
      IF PRMTAB-PARAMTYPE = '1' OR PRMTAB-PARAMTYPE = '2'
        OR PRMTAB-PARAMTYPE = '8'.
* Zero length for a scalar element - give up
        PERFORM MOVE_INITIAL TABLES PRMTAB.
      ENDIF.
    ENDIF.
  ELSE.
    IF PRMTAB-INTLEN <> MARKER AND PRMTAB-INTLEN <> 981 AND
      PRMTAB-INTLEN <> 201 AND
      ( PRMTAB-DATATYPE <> SPACE OR PRMTAB-INTTYPE <> SPACE ).
* All fields are already set - no further processing
      CONTINUE.
    ENDIF.
  ENDIF.
*  MOVE PRMTAB-PARAMTYPE TO PTYPE.
*  TRANSLATE PTYPE USING '1I2E3T4X5I6E7T8C9C'.
*  IF PTYPE EQ 'I' OR PTYPE EQ 'C'.
  IF PRMTAB-INTLEN <> MARKER.
    IF PRMTAB-PARAMTYPE = '1' OR PRMTAB-PARAMTYPE = '8'
       OR PRMTAB-PARAMTYPE = '2'.
* Put undefined (981) length only for input/changing/output scalar types

      IF PRMTAB-TABNAME = SPACE OR
         ( PRMTAB-TABNAME CA '-' AND PRMTAB-FIELDNAME = SPACE ).
        PRMTAB-DDLEN  = 981.
        PRMTAB-INTLEN = 981.
      ENDIF.
      IF PRMTAB-FIELDNAME = SPACE AND
         PRMTAB-INTTYPE = 'C' AND PRMTAB-INTLEN = 201
         AND PRMTAB-LOWERCASE = 'X'.
        PRMTAB-DDLEN  = 981.
        PRMTAB-INTLEN = 981.
      ENDIF.
    ENDIF.
  ELSE.
* Marker is set (this is a structure) - move on
    PRMTAB-INTLEN = 0.
    PRMTAB-DDLEN = 0.
    CLEAR: PRMTAB-LOWERCASE.
  ENDIF.
  IF PRMTAB-INTTYPE = 'C' AND PRMTAB-LOWERCASE = 'X' AND
    PRMTAB-INTLEN <> PRMTAB-DDLEN.
* This parameter info was created by MOVE_INITIAL
    PRMTAB-INTLEN = PRMTAB-DDLEN.
  ENDIF.
  MODIFY PRMTAB.
ENDLOOP.

ENDFUNCTION.

FORM Z_AW_FUNCTION_GET_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
