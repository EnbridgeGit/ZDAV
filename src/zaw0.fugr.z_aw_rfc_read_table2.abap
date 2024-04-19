FUNCTION Z_AW_RFC_READ_TABLE2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(QUERY_TABLE) LIKE  DD02L-TABNAME
*"             VALUE(DELIMITER) LIKE  SONV-FLAG OPTIONAL
*"             VALUE(NO_DATA) LIKE  SONV-FLAG OPTIONAL
*"             VALUE(ROWSKIPS) LIKE  SOID-ACCNT OPTIONAL
*"             VALUE(ROWCOUNT) LIKE  SOID-ACCNT OPTIONAL
*"       EXPORTING
*"             VALUE(OUT_TABLE) LIKE  DD02L-TABNAME
*"       TABLES
*"              OPTIONS STRUCTURE  RFC_DB_OPT
*"              FIELDS STRUCTURE  RFC_DB_FLD
*"              TBLOUT128 STRUCTURE  ZTAB128
*"              TBLOUT512 STRUCTURE  ZTAB512
*"              TBLOUT2048 STRUCTURE  ZTAB2048
*"              TBLOUT8192 STRUCTURE  ZTAB8192
*"              TBLOUT30000 STRUCTURE  ZTAB30K
*"       EXCEPTIONS
*"              TABLE_NOT_AVAILABLE
*"              TABLE_WITHOUT_DATA
*"              OPTION_NOT_VALID
*"              FIELD_NOT_VALID
*"              NOT_AUTHORIZED
*"              DATA_BUFFER_EXCEEDED
*"----------------------------------------------------------------------

statics pn like sy-cprog.

DATA: codetab(72) occurs 10 with header line,
      ln type i, msg(128),
      lf_sap_release type i.

move sy-saprl(1) to lf_sap_release.

if PN(2) <> '%_'.

if lf_sap_release >= 5.
   APPEND 'REPORT ZRTABLE1.' TO CODETAB.
   APPEND 'FORM Z_AW_RFC_READ_TABLE2_FORM' TO CODETAB.
   APPEND 'TABLES' TO CODETAB.
   APPEND 'OPTIONS STRUCTURE  RFC_DB_OPT' TO CODETAB.
   APPEND 'FIELDS STRUCTURE  RFC_DB_FLD' TO CODETAB.
   APPEND 'TBLOUT128 STRUCTURE  ZTAB128' TO CODETAB.
   APPEND 'TBLOUT512 STRUCTURE  ZTAB512' TO CODETAB.
   APPEND 'TBLOUT2048 STRUCTURE  ZTAB2048' TO CODETAB.
   APPEND 'TBLOUT8192 STRUCTURE  ZTAB8192' TO CODETAB.
   APPEND 'TBLOUT30000 STRUCTURE  ZTAB30K' TO CODETAB.
   APPEND 'USING' TO CODETAB.
   APPEND 'QUERY_TABLE LIKE  DD02L-TABNAME' TO CODETAB.
   APPEND 'DELIMITER LIKE  SONV-FLAG' TO CODETAB.
   APPEND 'NO_DATA LIKE  SONV-FLAG' TO CODETAB.
   APPEND 'ROWSKIPS LIKE  SOID-ACCNT' TO CODETAB.
   APPEND 'ROWCOUNT LIKE  SOID-ACCNT' TO CODETAB.
   APPEND 'CHANGING' TO CODETAB.
   APPEND 'OUTTAB LIKE  DD02L-TABNAME.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'DATA TABTYPE TYPE I.' TO CODETAB.
   APPEND 'CALL FUNCTION ''VIEW_AUTHORITY_CHECK''' TO CODETAB.
   APPEND 'EXPORTING' TO CODETAB.
   APPEND 'VIEW_ACTION                    = ''S''' TO CODETAB.
   APPEND 'VIEW_NAME              = QUERY_TABLE' TO CODETAB.
   APPEND 'EXCEPTIONS' TO CODETAB.
   APPEND 'NO_AUTHORITY                   = 2' TO CODETAB.
   APPEND 'NO_CLIENTINDEPENDENT_AUTHORITY = 2' TO CODETAB.
   APPEND 'NO_LINEDEPENDENT_AUTHORITY     = 2' TO CODETAB.
   APPEND 'OTHERS                         = 1.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF SY-SUBRC = 2.' TO CODETAB.
   APPEND 'RAISE NOT_AUTHORIZED.' TO CODETAB.
   APPEND 'ELSEIF SY-SUBRC = 1.' TO CODETAB.
   APPEND 'RAISE TABLE_NOT_AVAILABLE.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ---------------------------------------------' TO CODETAB.
   APPEND '*  find out about the structure of QUERY_TABLE' TO CODETAB.
   APPEND '* ---------------------------------------------' TO CODETAB.
   APPEND 'DATA BEGIN OF TABLE_STRUCTURE OCCURS 10.' TO CODETAB.
   APPEND 'INCLUDE STRUCTURE DFIES.' TO CODETAB.
   APPEND 'DATA END OF TABLE_STRUCTURE.' TO CODETAB.
   APPEND '"DATA TABLE_HEADER LIKE X030L.' TO CODETAB.
   APPEND 'DATA TABLE_TYPE TYPE DD02V-TABCLASS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'CALL FUNCTION ''DDIF_FIELDINFO_GET''' TO CODETAB.
   APPEND 'EXPORTING' TO CODETAB.
   APPEND 'TABNAME              = QUERY_TABLE' TO CODETAB.
   APPEND '*   FIELDNAME            = '' ''' TO CODETAB.
   APPEND '*   LANGU                = SY-LANGU' TO CODETAB.
   APPEND '*   LFIELDNAME           = '' ''' TO CODETAB.
   APPEND '*   ALL_TYPES            = '' ''' TO CODETAB.
   APPEND '*   GROUP_NAMES          = '' ''' TO CODETAB.
   APPEND 'IMPORTING' TO CODETAB.
   APPEND '*   X030L_WA             =' TO CODETAB.
   APPEND 'DDOBJTYPE            = TABLE_TYPE' TO CODETAB.
   APPEND '*   DFIES_WA             =' TO CODETAB.
   APPEND '*   LINES_DESCR          =' TO CODETAB.
   APPEND 'TABLES' TO CODETAB.
   APPEND 'DFIES_TAB            = TABLE_STRUCTURE' TO CODETAB.
   APPEND '*   FIXED_VALUES         =' TO CODETAB.
   APPEND 'EXCEPTIONS' TO CODETAB.
   APPEND 'NOT_FOUND            = 1' TO CODETAB.
   APPEND 'INTERNAL_ERROR       = 2' TO CODETAB.
   APPEND 'OTHERS               = 3' TO CODETAB.
   APPEND '.' TO CODETAB.
   APPEND 'IF SY-SUBRC <> 0.' TO CODETAB.
   APPEND 'RAISE TABLE_NOT_AVAILABLE.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'IF TABLE_TYPE = ''INTTAB''.' TO CODETAB.
   APPEND 'RAISE TABLE_WITHOUT_DATA.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* --------------------------------------------' TO CODETAB.
   APPEND '*  isolate first field of DATA as output field' TO CODETAB.
   APPEND '*  (i.e. allow for changes to structure DATA!)' TO CODETAB.
   APPEND '* --------------------------------------------' TO CODETAB.
   APPEND 'FIELD-SYMBOLS <D>.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ------------------------------------' TO CODETAB.
   APPEND '*  if FIELDS are not specified, read' TO CODETAB.
   APPEND '*  all available fields' TO CODETAB.
   APPEND '* ------------------------------------' TO CODETAB.
   APPEND 'DATA NUMBER_OF_FIELDS TYPE I.' TO CODETAB.
   APPEND 'DESCRIBE TABLE FIELDS LINES NUMBER_OF_FIELDS.' TO CODETAB.
   APPEND 'IF NUMBER_OF_FIELDS = 0.' TO CODETAB.
   APPEND 'LOOP AT TABLE_STRUCTURE.' TO CODETAB.
   APPEND 'MOVE TABLE_STRUCTURE-FIELDNAME' TO CODETAB.
   APPEND 'TO FIELDS-FIELDNAME.' TO CODETAB.
   APPEND 'APPEND FIELDS.' TO CODETAB.
   APPEND 'ENDLOOP.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ---------------------------------------------' TO CODETAB.
   APPEND '*  for each field which has to be read, copy' TO CODETAB.
   APPEND '*  structure information into tables FIELDS_INT' TO CODETAB.
   APPEND '* (internal use) and FIELDS (output)' TO CODETAB.
   APPEND '* ---------------------------------------------' TO CODETAB.
   APPEND 'DATA: BEGIN OF FIELDS_INT OCCURS 10,' TO CODETAB.
   APPEND 'FIELDNAME  LIKE TABLE_STRUCTURE-FIELDNAME,' TO CODETAB.
   APPEND 'TYPE       LIKE TABLE_STRUCTURE-INTTYPE,' TO CODETAB.
   APPEND 'DECIMALS   LIKE TABLE_STRUCTURE-DECIMALS,' TO CODETAB.
   APPEND 'LENGTH_SRC LIKE TABLE_STRUCTURE-INTLEN,' TO CODETAB.
   APPEND 'LENGTH_DST LIKE TABLE_STRUCTURE-LENG,' TO CODETAB.
   APPEND 'OFFSET_SRC LIKE TABLE_STRUCTURE-OFFSET,' TO CODETAB.
   APPEND 'OFFSET_DST LIKE TABLE_STRUCTURE-OFFSET,' TO CODETAB.
   APPEND 'END OF FIELDS_INT,' TO CODETAB.
   APPEND 'LINE_CURSOR TYPE I.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'LINE_CURSOR = 0.' TO CODETAB.
   APPEND '*  for each field which has to be read ...' TO CODETAB.
   APPEND 'LOOP AT FIELDS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'READ TABLE TABLE_STRUCTURE WITH' TO CODETAB.
   APPEND 'KEY FIELDNAME = FIELDS-FIELDNAME.' TO CODETAB.
   APPEND 'IF SY-SUBRC NE 0.' TO CODETAB.
   APPEND 'RAISE FIELD_NOT_VALID.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* compute the place for field contents in DATA rows:'
     TO CODETAB.
   APPEND '* if not first field in row, allow space' TO CODETAB.
   APPEND '* for delimiter' TO CODETAB.
   APPEND 'IF LINE_CURSOR <> 0.' TO CODETAB.
   APPEND 'IF NO_DATA EQ SPACE AND DELIMITER NE SPACE.' TO CODETAB.
   APPEND 'MOVE DELIMITER TO TBLOUT30000-WA+LINE_CURSOR.'
     TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'LINE_CURSOR = LINE_CURSOR + STRLEN( DELIMITER ).' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ... copy structure information into tables FIELDS_INT'
     TO CODETAB.
   APPEND '* (which is used internally during SELECT) ...' TO CODETAB.
   APPEND 'FIELDS_INT-FIELDNAME  = TABLE_STRUCTURE-FIELDNAME.'
     TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_SRC = TABLE_STRUCTURE-INTLEN.' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_DST = TABLE_STRUCTURE-LENG.' TO CODETAB.
   APPEND 'FIELDS_INT-OFFSET_SRC = TABLE_STRUCTURE-OFFSET.' TO CODETAB.
   APPEND 'FIELDS_INT-OFFSET_DST = LINE_CURSOR.' TO CODETAB.
   APPEND 'FIELDS_INT-TYPE       = TABLE_STRUCTURE-INTTYPE.' TO CODETAB.
   APPEND 'FIELDS_INT-DECIMALS   = TABLE_STRUCTURE-DECIMALS.'
     TO CODETAB.
   APPEND 'IF FIELDS_INT-TYPE = ''P''.' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_DST = FIELDS_INT-LENGTH_DST + 1.'
     TO CODETAB.
   APPEND 'IF FIELDS_INT-DECIMALS IS NOT INITIAL.' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_DST = FIELDS_INT-LENGTH_DST + 1.'
     TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '* compute the place for contents of next field' TO CODETAB.
   APPEND '* in DATA rows' TO CODETAB.
*  APPEND 'LINE_CURSOR = LINE_CURSOR + TABLE_STRUCTURE-LENG.'
*    TO CODETAB.
   APPEND 'LINE_CURSOR = LINE_CURSOR + FIELDS_INT-LENGTH_DST.'
     TO CODETAB.
   APPEND 'APPEND FIELDS_INT.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ... and into table FIELDS (which is output to' TO CODETAB.
   APPEND '* the caller)' TO CODETAB.
   APPEND 'FIELDS-FIELDTEXT = TABLE_STRUCTURE-FIELDTEXT.' TO CODETAB.
   APPEND 'FIELDS-TYPE      = TABLE_STRUCTURE-INTTYPE.' TO CODETAB.
   APPEND 'FIELDS-LENGTH    = FIELDS_INT-LENGTH_DST.' TO CODETAB.
   APPEND 'FIELDS-OFFSET    = FIELDS_INT-OFFSET_DST.' TO CODETAB.
   APPEND 'MODIFY FIELDS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDLOOP.' TO CODETAB.
   APPEND '* end of loop at FIELDS' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF LINE_CURSOR < 129.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT128''.' TO CODETAB.
   APPEND '  TABTYPE = 1.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(128) TO TBLOUT128-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT128 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 513.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT512''.' TO CODETAB.
   APPEND '  TABTYPE = 2.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(512) TO TBLOUT512-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT512 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 2049.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT2048''.' TO CODETAB.
   APPEND '  TABTYPE = 3.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(2048) TO TBLOUT2048-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT2048 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 8193.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT8192''.' TO CODETAB.
   APPEND '  TABTYPE = 4.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(8192) TO TBLOUT8192-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT8192 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 30001.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT30000''.' TO CODETAB.
   APPEND '  TABTYPE = 5.' TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT30000 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF NO_DATA EQ SPACE.' TO CODETAB.
   APPEND 'RAISE DATA_BUFFER_EXCEEDED.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ---------------------------------------------------'
     TO CODETAB.
   APPEND '*  read data from the database and copy relevant' TO CODETAB.
   APPEND '*  portions into DATA' TO CODETAB.
   APPEND '* ---------------------------------------------------'
     TO CODETAB.
   APPEND '* output data only if NO_DATA equals space (otherwise'
     TO CODETAB.
   APPEND '* the structure information in FIELDS is the only'
     TO CODETAB.
   APPEND '* result of the module)' TO CODETAB.
   APPEND 'IF NO_DATA EQ SPACE.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND
   'DATA: BEGIN OF WORK, align type f, BUFFER(30000), END OF WORK.'
    TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'FIELD-SYMBOLS: <WA> TYPE ANY, <COMP> TYPE ANY.' TO CODETAB.
   APPEND 'ASSIGN WORK-BUFFER TO <WA> CASTING TYPE (QUERY_TABLE).'
     TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF ROWCOUNT > 0.' TO CODETAB.
   APPEND 'ROWCOUNT = ROWCOUNT + ROWSKIPS.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'SELECT * FROM (QUERY_TABLE) INTO <WA>' TO CODETAB.
   APPEND 'WHERE (OPTIONS).' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF SY-DBCNT GT ROWSKIPS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '*   copy all relevant fields into DATA' TO CODETAB.
   APPEND '*   (output) table' TO CODETAB.
   APPEND 'LOOP AT FIELDS_INT.' TO CODETAB.
   APPEND 'IF FIELDS_INT-TYPE = ''P''.' TO CODETAB.
   APPEND 'ASSIGN COMPONENT FIELDS_INT-FIELDNAME' TO CODETAB.
   APPEND 'OF STRUCTURE <WA> TO <COMP>' TO CODETAB.
   APPEND 'TYPE     FIELDS_INT-TYPE' TO CODETAB.
   APPEND 'DECIMALS FIELDS_INT-DECIMALS.' TO CODETAB.
   APPEND 'ELSE.' TO CODETAB.
   APPEND 'ASSIGN COMPONENT FIELDS_INT-FIELDNAME' TO CODETAB.
   APPEND 'OF STRUCTURE <WA> TO <COMP>' TO CODETAB.
   APPEND 'TYPE     FIELDS_INT-TYPE.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'MOVE <COMP> TO' TO CODETAB.
   APPEND '<D>+FIELDS_INT-OFFSET_DST(FIELDS_INT-LENGTH_DST).'
     TO CODETAB.
   APPEND 'ENDLOOP.' TO CODETAB.
   APPEND '*   end of loop at FIELDS_INT' TO CODETAB.
   APPEND 'CASE TABTYPE.' TO CODETAB.
   APPEND '  WHEN 1.      APPEND TBLOUT128.' TO CODETAB.
   APPEND '  WHEN 2.      APPEND TBLOUT512.' TO CODETAB.
   APPEND '  WHEN 3.      APPEND TBLOUT2048.' TO CODETAB.
   APPEND '  WHEN 4.      APPEND TBLOUT8192.' TO CODETAB.
   APPEND '  WHEN OTHERS. APPEND TBLOUT30000.' TO CODETAB.
   APPEND 'ENDCASE.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF ROWCOUNT > 0 AND SY-DBCNT GE ROWCOUNT.' TO CODETAB.
   APPEND 'EXIT.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDSELECT.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDFORM.' TO CODETAB.
else.
   APPEND 'REPORT ZRTABLE1.' TO CODETAB.
   APPEND 'FORM Z_AW_RFC_READ_TABLE2_FORM' TO CODETAB.
   APPEND 'TABLES' TO CODETAB.
   APPEND 'OPTIONS STRUCTURE  RFC_DB_OPT' TO CODETAB.
   APPEND 'FIELDS STRUCTURE  RFC_DB_FLD' TO CODETAB.
   APPEND 'TBLOUT128 STRUCTURE  ZTAB128' TO CODETAB.
   APPEND 'TBLOUT512 STRUCTURE  ZTAB512' TO CODETAB.
   APPEND 'TBLOUT2048 STRUCTURE  ZTAB2048' TO CODETAB.
   APPEND 'TBLOUT8192 STRUCTURE  ZTAB8192' TO CODETAB.
   APPEND 'TBLOUT30000 STRUCTURE  ZTAB30K' TO CODETAB.
   APPEND 'USING' TO CODETAB.
   APPEND 'QUERY_TABLE LIKE  DD02L-TABNAME' TO CODETAB.
   APPEND 'DELIMITER LIKE  SONV-FLAG' TO CODETAB.
   APPEND 'NO_DATA LIKE  SONV-FLAG' TO CODETAB.
   APPEND 'ROWSKIPS LIKE  SOID-ACCNT' TO CODETAB.
   APPEND 'ROWCOUNT LIKE  SOID-ACCNT' TO CODETAB.
   APPEND 'CHANGING' TO CODETAB.
   APPEND 'OUTTAB LIKE  DD02L-TABNAME.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'DATA TABTYPE TYPE I.' TO CODETAB.
   APPEND 'CALL FUNCTION ''VIEW_AUTHORITY_CHECK''' TO CODETAB.
   APPEND 'EXPORTING' TO CODETAB.
   APPEND 'VIEW_ACTION                    = ''S''' TO CODETAB.
   APPEND 'VIEW_NAME               = QUERY_TABLE' TO CODETAB.
   APPEND 'EXCEPTIONS' TO CODETAB.
   APPEND 'NO_AUTHORITY                   = 2' TO CODETAB.
   APPEND 'NO_CLIENTINDEPENDENT_AUTHORITY = 3' TO CODETAB.
   APPEND 'TABLE_NOT_FOUND                = 4.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF SY-SUBRC = 2 OR SY-SUBRC = 3.' TO CODETAB.
   APPEND 'RAISE NOT_AUTHORIZED.' TO CODETAB.
   APPEND 'ELSEIF SY-SUBRC = 1.' TO CODETAB.
   APPEND 'RAISE TABLE_NOT_AVAILABLE.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ---------------------------------------------' TO CODETAB.
   APPEND '*  find out about the structure of QUERY_TABLE' TO CODETAB.
   APPEND '* ---------------------------------------------' TO CODETAB.
   APPEND 'DATA BEGIN OF TABLE_STRUCTURE OCCURS 10.' TO CODETAB.
   APPEND 'INCLUDE STRUCTURE DNTAB.' TO CODETAB.
   APPEND 'DATA END OF TABLE_STRUCTURE.' TO CODETAB.
   APPEND 'DATA TABLE_HEADER LIKE X030L.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'CALL FUNCTION ''NAMETAB_GET''' TO CODETAB.
   APPEND 'EXPORTING' TO CODETAB.
   APPEND 'TABNAME             = QUERY_TABLE' TO CODETAB.
   APPEND 'IMPORTING' TO CODETAB.
   APPEND 'HEADER              = TABLE_HEADER' TO CODETAB.
   APPEND 'TABLES' TO CODETAB.
   APPEND 'NAMETAB             = TABLE_STRUCTURE' TO CODETAB.
   APPEND 'EXCEPTIONS' TO CODETAB.
   APPEND 'TABLE_HAS_NO_FIELDS = 01' TO CODETAB.
   APPEND 'TABLE_NOT_ACTIV     = 02' TO CODETAB.
   APPEND 'INTERNAL_ERROR      = 03' TO CODETAB.
   APPEND 'NO_TEXTS_FOUND      = 04.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF SY-SUBRC > 1.' TO CODETAB.
   APPEND 'RAISE TABLE_NOT_AVAILABLE.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'IF SY-SUBRC = 1 OR TABLE_HEADER-TABFORM CN ''TCPV''.'
     TO CODETAB.
   APPEND 'RAISE TABLE_WITHOUT_DATA.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'DATA: TEMP(32).' TO CODETAB.
   APPEND '* ----------------------------------------------' TO CODETAB.
   APPEND '*  isolate first field of DATA as output field' TO CODETAB.
   APPEND '*  (i.e. allow for changes to structure DATA!)' TO CODETAB.
   APPEND '* ----------------------------------------------' TO CODETAB.
   APPEND 'FIELD-SYMBOLS <D>.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* -------------------------------------' TO CODETAB.
   APPEND '*  if FIELDS are not specified, read' TO CODETAB.
   APPEND '*  all available fields' TO CODETAB.
   APPEND '* -------------------------------------' TO CODETAB.
   APPEND 'DATA NUMBER_OF_FIELDS TYPE I.' TO CODETAB.
   APPEND 'DESCRIBE TABLE FIELDS LINES NUMBER_OF_FIELDS.' TO CODETAB.
   APPEND 'IF NUMBER_OF_FIELDS = 0.' TO CODETAB.
   APPEND 'LOOP AT TABLE_STRUCTURE.' TO CODETAB.
   APPEND 'MOVE TABLE_STRUCTURE-FIELDNAME' TO CODETAB.
   APPEND 'TO FIELDS-FIELDNAME.' TO CODETAB.
   APPEND 'APPEND FIELDS.' TO CODETAB.
   APPEND 'ENDLOOP.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ----------------------------------------------' TO CODETAB.
   APPEND '*  for each field which has to be read, copy' TO CODETAB.
   APPEND '*  structure information into tables FIELDS_INT' TO CODETAB.
   APPEND '*  (internal use) and FIELDS (output)' TO CODETAB.
   APPEND '* ----------------------------------------------' TO CODETAB.
   APPEND 'DATA: BEGIN OF FIELDS_INT OCCURS 10,' TO CODETAB.
   APPEND 'TYPE       LIKE TABLE_STRUCTURE-INTTYPE,' TO CODETAB.
   APPEND 'DECIMALS   LIKE TABLE_STRUCTURE-DECIMALS,' TO CODETAB.
   APPEND 'LENGTH_SRC LIKE TABLE_STRUCTURE-INTLEN,' TO CODETAB.
   APPEND 'LENGTH_DST LIKE TABLE_STRUCTURE-DDLEN,' TO CODETAB.
   APPEND 'OFFSET_SRC LIKE TABLE_STRUCTURE-OFFSET,' TO CODETAB.
   APPEND 'OFFSET_DST LIKE TABLE_STRUCTURE-OFFSET,' TO CODETAB.
   APPEND 'END OF FIELDS_INT,' TO CODETAB.
   APPEND 'LINE_CURSOR TYPE I.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'LINE_CURSOR = 0.' TO CODETAB.
   APPEND '*  for each field which has to be read ...' TO CODETAB.
   APPEND 'LOOP AT FIELDS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'READ TABLE TABLE_STRUCTURE' TO CODETAB.
   APPEND 'WITH KEY FIELDNAME = FIELDS-FIELDNAME.' TO CODETAB.
   APPEND 'IF SY-SUBRC NE 0.' TO CODETAB.
   APPEND 'RAISE FIELD_NOT_VALID.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* compute the place for field contents in DATA rows:'
     TO CODETAB.
   APPEND '* if not first field in row, allow space' TO CODETAB.
   APPEND '* for delimiter' TO CODETAB.
   APPEND 'IF LINE_CURSOR <> 0.' TO CODETAB.
   APPEND 'IF NO_DATA EQ SPACE AND DELIMITER NE SPACE.' TO CODETAB.
   APPEND 'MOVE DELIMITER TO TBLOUT30000-WA+LINE_CURSOR.'
     TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'LINE_CURSOR = LINE_CURSOR + STRLEN( DELIMITER ).' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ... copy structure information into tables' TO CODETAB.
   APPEND '* FIELDS_INT (which is used internally' TO CODETAB.
   APPEND '* during SELECT) ...' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_SRC = TABLE_STRUCTURE-INTLEN.' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_DST = TABLE_STRUCTURE-DDLEN.' TO CODETAB.
   APPEND 'FIELDS_INT-OFFSET_SRC = TABLE_STRUCTURE-OFFSET.' TO CODETAB.
   APPEND 'FIELDS_INT-OFFSET_DST = LINE_CURSOR.' TO CODETAB.
   APPEND 'FIELDS_INT-TYPE       = TABLE_STRUCTURE-INTTYPE.' TO CODETAB.
   APPEND 'FIELDS_INT-DECIMALS   = TABLE_STRUCTURE-DECIMALS.'
     TO CODETAB.
   APPEND 'IF FIELDS_INT-TYPE = ''P''.' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_DST = FIELDS_INT-LENGTH_DST + 1.'
     TO CODETAB.
   APPEND 'IF FIELDS_INT-DECIMALS <> 0.' TO CODETAB.
   APPEND 'FIELDS_INT-LENGTH_DST = FIELDS_INT-LENGTH_DST + 1.'
     TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '* compute the place for contents of next field' TO CODETAB.
   APPEND '* in DATA rows' TO CODETAB.
*  APPEND 'LINE_CURSOR = LINE_CURSOR + TABLE_STRUCTURE-DDLEN.'
*    TO CODETAB.
   APPEND 'LINE_CURSOR = LINE_CURSOR + FIELDS_INT-LENGTH_DST.'
     TO CODETAB.
   APPEND 'APPEND FIELDS_INT.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ... and into table FIELDS (which is output' TO CODETAB.
   APPEND '* to the caller)' TO CODETAB.
   APPEND 'FIELDS-FIELDTEXT = TABLE_STRUCTURE-FIELDTEXT.' TO CODETAB.
   APPEND 'FIELDS-TYPE      = TABLE_STRUCTURE-INTTYPE.' TO CODETAB.
   APPEND 'FIELDS-LENGTH    = FIELDS_INT-LENGTH_DST.' TO CODETAB.
   APPEND 'FIELDS-OFFSET    = FIELDS_INT-OFFSET_DST.' TO CODETAB.
   APPEND 'MODIFY FIELDS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDLOOP.' TO CODETAB.
   APPEND '* end of loop at FIELDS' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF LINE_CURSOR < 129.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT128''.' TO CODETAB.
   APPEND '  TABTYPE = 1.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(128) TO TBLOUT128-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT128 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 513.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT512''.' TO CODETAB.
   APPEND '  TABTYPE = 2.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(512) TO TBLOUT512-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT512 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 2049.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT2048''.' TO CODETAB.
   APPEND '  TABTYPE = 3.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(2048) TO TBLOUT2048-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT2048 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 8193.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT8192''.' TO CODETAB.
   APPEND '  TABTYPE = 4.' TO CODETAB.
   APPEND '  MOVE TBLOUT30000-WA+0(8192) TO TBLOUT8192-WA.'
     TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT8192 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF LINE_CURSOR < 30001.' TO CODETAB.
   APPEND '  OUTTAB = ''TBLOUT30000''.' TO CODETAB.
   APPEND '  TABTYPE = 5.' TO CODETAB.
   APPEND '  ASSIGN COMPONENT 0 OF STRUCTURE TBLOUT30000 TO <D>.'
     TO CODETAB.
   APPEND 'ELSEIF NO_DATA EQ SPACE.' TO CODETAB.
   APPEND 'RAISE DATA_BUFFER_EXCEEDED.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '* ----------------------------------------------' TO CODETAB.
   APPEND '*  read data from the database and copy relevant' TO CODETAB.
   APPEND '*  portions into DATA' TO CODETAB.
   APPEND '* ----------------------------------------------' TO CODETAB.
   APPEND '* output data only if NO_DATA equals space' TO CODETAB.
   APPEND '* (otherwise the structure information in FIELDS' TO CODETAB.
   APPEND '* is the only result of the module)' TO CODETAB.
   APPEND 'IF NO_DATA EQ SPACE.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'DATA: BEGIN OF WORK, BUFFER(30000), END OF WORK.' TO CODETAB.
   APPEND 'FIELD-SYMBOLS <F>.' TO CODETAB.
   APPEND 'DATA: DUMMY1 TYPE F, F1(8).' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF ROWCOUNT > 0.' TO CODETAB.
   APPEND 'ROWCOUNT = ROWCOUNT + ROWSKIPS.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'SELECT * FROM (QUERY_TABLE) INTO WORK' TO CODETAB.
   APPEND 'WHERE (OPTIONS).' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF SY-DBCNT GT ROWSKIPS.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND '*   copy all relevant fields' TO CODETAB.
   APPEND '*   into DATA (output) table' TO CODETAB.
   APPEND 'LOOP AT FIELDS_INT.' TO CODETAB.
   APPEND 'IF FIELDS_INT-TYPE = ''P''.' TO CODETAB.
   APPEND 'ASSIGN' TO CODETAB.
   APPEND 'WORK+FIELDS_INT-OFFSET_SRC(FIELDS_INT-LENGTH_SRC)'
     TO CODETAB.
   APPEND 'TO <F>' TO CODETAB.
   APPEND 'TYPE     FIELDS_INT-TYPE' TO CODETAB.
   APPEND 'DECIMALS FIELDS_INT-DECIMALS.' TO CODETAB.
   APPEND 'ELSEIF FIELDS_INT-TYPE = ''F''.' TO CODETAB.
   APPEND 'MOVE' TO CODETAB.
   APPEND 'WORK+FIELDS_INT-OFFSET_SRC(FIELDS_INT-LENGTH_SRC)'
     TO CODETAB.
   APPEND 'TO F1.' TO CODETAB.
   APPEND 'ASSIGN F1 TO <F> TYPE ''F''.' TO CODETAB.
   APPEND 'IF <F> = ''0.0''.' TO CODETAB.
   APPEND 'TEMP = ''0.0''.' TO CODETAB.
   APPEND 'ELSE.' TO CODETAB.
   APPEND 'WRITE <F> TO TEMP EXPONENT 0.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'CONDENSE TEMP NO-GAPS.' TO CODETAB.
   APPEND 'TRANSLATE TEMP USING '',.''.' TO CODETAB.
   APPEND 'ASSIGN TEMP TO <F>.' TO CODETAB.
   APPEND 'ELSE.' TO CODETAB.
   APPEND 'ASSIGN' TO CODETAB.
   APPEND 'WORK+FIELDS_INT-OFFSET_SRC(FIELDS_INT-LENGTH_SRC)'
     TO CODETAB.
   APPEND 'TO <F>' TO CODETAB.
   APPEND 'TYPE     FIELDS_INT-TYPE.' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND 'MOVE <F> TO' TO CODETAB.
   APPEND '<D>+FIELDS_INT-OFFSET_DST(FIELDS_INT-LENGTH_DST).'
     TO CODETAB.
   APPEND 'ENDLOOP.' TO CODETAB.
   APPEND '*   end of loop at FIELDS_INT' TO CODETAB.
   APPEND 'CASE TABTYPE.' TO CODETAB.
   APPEND '  WHEN 1.      APPEND TBLOUT128.' TO CODETAB.
   APPEND '  WHEN 2.      APPEND TBLOUT512.' TO CODETAB.
   APPEND '  WHEN 3.      APPEND TBLOUT2048.' TO CODETAB.
   APPEND '  WHEN 4.      APPEND TBLOUT8192.' TO CODETAB.
   APPEND '  WHEN OTHERS. APPEND TBLOUT30000.' TO CODETAB.
   APPEND 'ENDCASE.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'IF ROWCOUNT > 0 AND SY-DBCNT GE ROWCOUNT. EXIT. ENDIF.'
     TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDSELECT.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDIF.' TO CODETAB.
   APPEND '' TO CODETAB.
   APPEND 'ENDFORM.' TO CODETAB.
endif.

generate subroutine pool CODETAB name pn message msg line ln.
endif.

PERFORM Z_AW_RFC_READ_TABLE2_FORM IN PROGRAM (PN)
  TABLES
     OPTIONS
     FIELDS
     TBLOUT128
     TBLOUT512
     TBLOUT2048
     TBLOUT8192
     TBLOUT30000
  USING
     QUERY_TABLE
     DELIMITER
     NO_DATA
     ROWSKIPS
     ROWCOUNT
  CHANGING
     OUT_TABLE.

ENDFUNCTION.

FORM Z_AW_RFC_READ_TABLE2_GETV CHANGING VERSION TYPE C.

VERSION = '11.5.0.0'.

ENDFORM.
