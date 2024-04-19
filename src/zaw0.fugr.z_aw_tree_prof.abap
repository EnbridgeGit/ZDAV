FUNCTION Z_AW_TREE_PROF.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_GROUP) LIKE  DD04L-ROLLNAME
*"             VALUE(P_TABLE) LIKE  DD02D-TABNAME
*"       TABLES
*"              WRITES STRUCTURE  LISTZEILE
*"----------------------------------------------------------------------

statics pn like sy-cprog.

DATA: codetab(72) occurs 10 with header line,
      ln type i, msg(128),
      L_TABNAME LIKE DD02L-TABNAME,
      L_RC LIKE SY-SUBRC,
      LF_SAP_RELEASE TYPE I,
      LF_SAP_SUBVER TYPE I.

move sy-saprl(1) to lf_sap_release.
MOVE SY-SAPRL+1(1) TO LF_SAP_SUBVER.

SELECT TABNAME INTO L_TABNAME FROM DD02L UP TO 1 ROWS
WHERE TABNAME = 'T800S' AND AS4LOCAL = 'A'.
ENDSELECT.

L_RC = SY-SUBRC.

SELECT TABNAME INTO L_TABNAME FROM DD02L UP TO 1 ROWS
WHERE TABNAME = 'SETHEADER' AND AS4LOCAL = 'A'.
ENDSELECT.

IF ( SY-SUBRC <> 0 AND LF_SAP_RELEASE > 3 ) OR L_RC <> 0.
   EXIT.
ENDIF.

if PN(2) <> '%_'.
      APPEND 'REPORT ZTREERD1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'TABLES T800S.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'TABLES SETHEADER.' TO CODETAB.
  ENDIF.
      APPEND 'TABLES TKA01.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: begin of ITAB1 occurs 0,' TO CODETAB.
      APPEND 'PARENT_ID(30) TYPE C,' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'PARENT_DESC(64) TYPE C,' TO CODETAB.
  ELSE.
      APPEND 'PARENT_DES(64) TYPE C,' TO CODETAB.
  ENDIF.
      APPEND 'CHILD_ID(30) TYPE C,' TO CODETAB.
      APPEND 'CHILD_DESC(64) TYPE C,' TO CODETAB.
      APPEND 'VALUE_FROM(24) TYPE C,' TO CODETAB.
      APPEND 'VALUE_TO(24) TYPE C,' TO CODETAB.
      APPEND 'LEAF_FLAG(1) TYPE C,' TO CODETAB.
      APPEND 'ROOT_FLAG(1) TYPE C,' TO CODETAB.
      APPEND 'TREE_LEVEL(3) TYPE C,' TO CODETAB.
      APPEND 'CONT_AREA(30) TYPE C,' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'CHART_OF_ACCT(30) TYPE C,' TO CODETAB.
  ELSE.
      APPEND 'CHART_OF_A(30) TYPE C,' TO CODETAB.
  ENDIF.
      APPEND 'SET_TABLE(30) TYPE C.' TO CODETAB.
      APPEND 'DATA: end of ITAB1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'data: append_flag(1) value '' '',' TO CODETAB.
      APPEND '      cntbuf type i,' TO CODETAB.
      APPEND '      delimleng type i.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA:       P_GROUP LIKE  DD04L-ROLLNAME,' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND '            P_TABLE LIKE  SETHEADER-TABNAME.' TO CODETAB.
  ELSE.
      APPEND '            P_TABLE LIKE  T800S-TAB.' TO CODETAB.
  ENDIF.
      APPEND 'FORM Z_AW_TREE_PROF_FORM' TO CODETAB.
      APPEND '       TABLES' TO CODETAB.
      APPEND '              WRITES STRUCTURE  LISTZEILE' TO CODETAB.
      APPEND '       USING' TO CODETAB.
      APPEND '            P_GROUP1 LIKE  DD04L-ROLLNAME' TO CODETAB.
      APPEND '            P_TABLE1 LIKE  DD02D-TABNAME.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '       P_GROUP = P_GROUP1.' TO CODETAB.
      APPEND '       P_TABLE = P_TABLE1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'PERFORM FORM1.' TO CODETAB.
      APPEND 'PERFORM FORM2 TABLES WRITES.' TO CODETAB.
      APPEND 'FREE ITAB1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'ENDFORM.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'FORM FORM1.' TO CODETAB.
      APPEND 'DATA: $TABNAME LIKE SETHIER-TABNAME.' TO CODETAB.
      APPEND 'DATA: $FELD LIKE SETHIER-FIELDNAME.' TO CODETAB.
      APPEND '* DATA: $AREA LIKE TKA01-KOKRS.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'DATA: SETNR   LIKE SETHIER-SETID.' TO CODETAB.
      APPEND 'DATA: SETNR2  LIKE SETHIER-SETID.' TO CODETAB.
  ELSE.
      APPEND 'DATA: SETNR   LIKE RGSBS-SETNR.' TO CODETAB.
      APPEND 'DATA: SETNR2  LIKE SETHDR-SETID.' TO CODETAB.
  ENDIF.
      APPEND 'DATA: PARENT_ID LIKE SETHIER-SETID.' TO CODETAB.
      APPEND 'DATA: PARENT_DESC LIKE SETHIER-DESCRIPT.' TO CODETAB.
      APPEND 'DATA: IX TYPE I.' TO CODETAB.
      APPEND 'DATA: LC TYPE I.' TO CODETAB.
      APPEND 'DATA: INDEX TYPE I.' TO CODETAB.
      APPEND 'DATA: LEVEL TYPE I, P_LEVEL TYPE I, NODE TYPE I.'
        TO CODETAB.
      APPEND 'DATA: VCOUNT TYPE I.' TO CODETAB.
      APPEND 'DATA: MATCH_SETID TYPE I.' TO CODETAB.
      APPEND 'DATA: IDX TYPE I.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '$TABNAME = P_TABLE.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '$FELD = P_GROUP.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '* $AREA = P_AREA.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF ITAB_HIERARCHY OCCURS 0.' TO CODETAB.
      APPEND 'INCLUDE STRUCTURE SETHIER.' TO CODETAB.
      APPEND 'DATA: END OF ITAB_HIERARCHY.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: TYPE LIKE ITAB_HIERARCHY-TYPE.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF ITAB_VALUES OCCURS 0.' TO CODETAB.
      APPEND 'INCLUDE STRUCTURE SETVALUES.' TO CODETAB.
      APPEND 'DATA: END OF ITAB_VALUES.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF ITAB_SUPERSETS OCCURS 0.' TO CODETAB.
      APPEND 'INCLUDE STRUCTURE SETLIST.' TO CODETAB.
      APPEND 'DATA: END OF ITAB_SUPERSETS.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF Hex00,' TO CODETAB.
      APPEND 'X1(1) TYPE C VALUE SPACE,' TO CODETAB.
      APPEND 'END OF Hex00.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF HexFF,' TO CODETAB.
      APPEND 'X1(1) TYPE C VALUE SPACE,' TO CODETAB.
      APPEND 'END OF HexFF.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: NO_ROWS TYPE I.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF ITAB_SETS OCCURS 0.' TO CODETAB.
      APPEND 'include structure SETHIER.' TO CODETAB.
      APPEND 'DATA: END OF ITAB_SETS.' TO CODETAB.
      APPEND '' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'DATA: BEGIN OF ITAB_SETS40 OCCURS 0.' TO CODETAB.
      APPEND 'include structure SETHIER.' TO CODETAB.
      APPEND 'DATA: END OF ITAB_SETS40.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: LOC_SETCLASS LIKE SETHIER-SETCLASS,' TO CODETAB.
      APPEND 'LOC_SETCLASS_FATHER LIKE SETHIER-SETCLASS.' TO CODETAB.
      APPEND '' TO CODETAB.
  ENDIF.
  IF lf_sap_release < 5.
      APPEND 'DATA: BEGIN OF Hex_00,' TO CODETAB.
      APPEND 'X1(1) TYPE X VALUE ''00'',' TO CODETAB.
      APPEND 'END OF Hex_00.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'DATA: BEGIN OF Hex_FF,' TO CODETAB.
      APPEND 'X1(1) TYPE X VALUE ''FF'',' TO CODETAB.
      APPEND 'END OF Hex_FF.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'MOVE Hex_00 TO HEX00.' TO CODETAB.
      APPEND 'MOVE Hex_FF TO HEXFF.' TO CODETAB.
  ELSE.
      APPEND 'CLASS cl_abap_char_utilities DEFINITION LOAD.' TO CODETAB.
      APPEND 'CLEAR Hex00' TO CODETAB.
      APPEND '      WITH cl_abap_char_utilities=>minchar' TO CODETAB.
      APPEND '      IN CHARACTER MODE.' TO CODETAB.
      APPEND 'CLEAR HexFF' TO CODETAB.
      APPEND '      WITH cl_abap_char_utilities=>maxchar' TO CODETAB.
      APPEND '      IN CHARACTER MODE.' TO CODETAB.
  ENDIF.
  IF LF_SAP_RELEASE > 3.
      APPEND 'CALL FUNCTION ''G_TABLE_FIELD_GET_SETCLASS''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'TABNAME   = $TABNAME' TO CODETAB.
      APPEND 'FIELDNAME = $FELD' TO CODETAB.
      APPEND 'IMPORTING' TO CODETAB.
      APPEND 'SETCLASS  = LOC_SETCLASS' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS    = 0.' TO CODETAB.
      APPEND '' TO CODETAB.
  ENDIF.
      APPEND '* Fetch 3.0 hierarchy' TO CODETAB.
      APPEND '* SELECT SINGLE * FROM TKA01 WHERE KOKRS LIKE $AREA.'
        TO CODETAB.
      APPEND '* IF SY-SUBRC = 0.' TO CODETAB.
      APPEND 'SELECT * FROM T800S WHERE TAB = $TABNAME' TO CODETAB.
      APPEND 'AND FELD = $FELD' TO CODETAB.
      APPEND
        '* AND ( SEARCHFLD = TKA01-KOKRS OR SEARCHFLD = TKA01-KTOPL )'
        TO CODETAB.
      APPEND 'AND ( TYPE = ''S'' OR TYPE = ''B'' ).' TO CODETAB.
      APPEND 'IF ( T800S-SETNR CS ''0H'' ).' TO CODETAB.
      APPEND 'IF ( T800S-SEARCHFLD <> SPACE ).' TO CODETAB.
      APPEND 'MOVE T800S-SETNR TO ITAB_SETS-SETID.' TO CODETAB.
      APPEND 'MOVE T800S-SEARCHFLD TO ITAB_SETS-SEARCHFLD.' TO CODETAB.
      APPEND 'MOVE TKA01-KOKRS TO ITAB_SETS-KOKRS.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'MOVE TKA01-KTOPL TO ITAB_SETS-KTOPL.' TO CODETAB.
  ENDIF.
      APPEND 'APPEND ITAB_SETS.' TO CODETAB.
      APPEND 'ELSEIF $FELD = ''AUFNR''.' TO CODETAB.
      APPEND 'ITAB_SETS-SETID = T800S-SETNR.' TO CODETAB.
      APPEND 'ITAB_SETS-SEARCHFLD = T800S-SEARCHFLD.' TO CODETAB.
      APPEND 'ITAB_SETS-KOKRS = SPACE.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'MOVE T800S-SETNR TO ITAB_SETS-SETID.' TO CODETAB.
      APPEND 'MOVE T800S-SEARCHFLD TO ITAB_SETS-SEARCHFLD.' TO CODETAB.
      APPEND 'MOVE SPACE TO ITAB_SETS-KOKRS.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'MOVE SPACE TO ITAB_SETS-KTOPL.' TO CODETAB.
  ENDIF.
      APPEND 'APPEND ITAB_SETS.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ENDSELECT.' TO CODETAB.
      APPEND '*ENDIF.' TO CODETAB.
      APPEND 'MATCH_SETID = 0.' TO CODETAB.
      APPEND 'SORT ITAB_SETS BY SETID.' TO CODETAB.
      APPEND 'DELETE ADJACENT DUPLICATES FROM ITAB_SETS.' TO CODETAB.
      APPEND 'LOOP AT ITAB_SETS.' TO CODETAB.
      APPEND 'SETNR = ITAB_SETS-SETID.' TO CODETAB.
      APPEND 'MATCH_SETID = 1.' TO CODETAB.
      APPEND 'IF MATCH_SETID = 1.' TO CODETAB.
      APPEND 'CALL FUNCTION ''G_SET_GET_SUPERSETS''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND '*         CLIENT    = SY-MANDT' TO CODETAB.
      APPEND '*         SETCLASS  = '' ''' TO CODETAB.
      APPEND 'SETNAME   = SETNR' TO CODETAB.
      APPEND 'TABNAME   = $TABNAME' TO CODETAB.
      APPEND 'TABLES' TO CODETAB.
      APPEND 'SUPERSETS = ITAB_SUPERSETS' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS    = 1.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'NO_ROWS = 0.' TO CODETAB.
      APPEND 'LOOP AT ITAB_SUPERSETS.' TO CODETAB.
      APPEND '   CALL FUNCTION ''G_SET_DECRYPT_SETID''' TO CODETAB.
      APPEND '        EXPORTING' TO CODETAB.
      APPEND '        SETID    = ITAB_SUPERSETS-SETNAME' TO CODETAB.
      APPEND '        IMPORTING' TO CODETAB.
      APPEND '        SETCLASS = LOC_SETCLASS_FATHER.' TO CODETAB.
      APPEND '   IF LOC_SETCLASS = LOC_SETCLASS_FATHER.' TO CODETAB.
      APPEND '     NO_ROWS = 1.' TO CODETAB.
      APPEND '     EXIT.' TO CODETAB.
      APPEND '   ENDIF.' TO CODETAB.
      APPEND 'ENDLOOP.' TO CODETAB.
  ELSE.
      APPEND 'DESCRIBE TABLE ITAB_SUPERSETS LINES NO_ROWS.' TO CODETAB.
  ENDIF.
      APPEND '' TO CODETAB.
      APPEND 'IF NO_ROWS = 0 .' TO CODETAB.
      APPEND 'SETNR2 = SETNR.' TO CODETAB.
      APPEND '' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'CALL FUNCTION ''G_SET_TREE_IMPORT''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID          = SETNR2' TO CODETAB.
      APPEND 'TABNAME        = $TABNAME' TO CODETAB.
      APPEND 'NO_VARIABLE_REPLACEMENT   = ''X''' TO CODETAB.
      APPEND 'ROOT_HEADER_ONLY          = '' ''' TO CODETAB.
      APPEND 'TABLES' TO CODETAB.
      APPEND 'SET_HIERARCHY  = ITAB_HIERARCHY' TO CODETAB.
      APPEND 'SET_VALUES     = ITAB_VALUES' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'SET_NOT_FOUND  = 1' TO CODETAB.
      APPEND 'ILLEGAL_FIELD_REPLACEMENT = 2' TO CODETAB.
      APPEND 'ILLEGAL_TABLE_REPLACEMENT = 3' TO CODETAB.
      APPEND 'OTHERS         = 4.' TO CODETAB.
  ELSE.
      APPEND '    CALL FUNCTION ''G_SET_TREE_IMPORT_OLD''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID          = SETNR2' TO CODETAB.
      APPEND 'TABNAME        = $TABNAME' TO CODETAB.
      APPEND 'ROOT_HEADER_ONLY          = '' ''' TO CODETAB.
      APPEND 'TABLES' TO CODETAB.
      APPEND 'SET_HIERARCHY  = ITAB_HIERARCHY' TO CODETAB.
      APPEND 'SET_VALUES     = ITAB_VALUES' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'SET_NOT_FOUND  = 1' TO CODETAB.
      APPEND 'OTHERS         = 2.' TO CODETAB.
  ENDIF.
      APPEND '' TO CODETAB.
      APPEND 'NODE = 0.' TO CODETAB.
      APPEND 'IDX = 1.' TO CODETAB.
      APPEND 'LOOP AT ITAB_HIERARCHY.' TO CODETAB.
  IF LF_SAP_RELEASE > 4 OR
     ( LF_SAP_RELEASE = 4 AND LF_SAP_SUBVER > 4 ).
      APPEND 'CALL FUNCTION ''G_SET_DECRYPT_SETID''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID      = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'IMPORTING' TO CODETAB.
      APPEND 'SHORTNAME  = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS     = 1.' TO CODETAB.
  ENDIF.
      APPEND 'NODE = NODE + 1.' TO CODETAB.
      APPEND 'VCOUNT = ITAB_HIERARCHY-VCOUNT.' TO CODETAB.
      APPEND 'LEVEL = ITAB_HIERARCHY-LEVEL.' TO CODETAB.
      APPEND 'TYPE = ITAB_HIERARCHY-TYPE.' TO CODETAB.
      APPEND 'P_LEVEL = LEVEL - 1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'IF LEVEL = 0.' TO CODETAB.
      APPEND 'ITAB1-PARENT_ID = SPACE.' TO CODETAB.
      APPEND '' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'ITAB1-PARENT_DESC = SPACE.' TO CODETAB.
  ELSE.
      APPEND 'ITAB1-PARENT_DES = SPACE.' TO CODETAB.
  ENDIF.
      APPEND 'MOVE ITAB_HIERARCHY-SETID TO ITAB1-CHILD_ID.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-CHILD_DESC.'
        TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 1.' TO CODETAB.
      APPEND 'ITAB1-LEAF_FLAG = 0.' TO CODETAB.
      APPEND 'ITAB1-TREE_LEVEL = P_LEVEL.' TO CODETAB.
      APPEND 'CONDENSE ITAB1-TREE_LEVEL NO-GAPS.' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = SPACE.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = SPACE.' TO CODETAB.
      APPEND 'ITAB1-CONT_AREA = ITAB_SETS-KOKRS.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'ITAB1-CHART_OF_ACCT = ITAB_SETS-KTOPL.' TO CODETAB.
  ELSE.
  APPEND 'SELECT SINGLE * FROM TKA01 WHERE KOKRS = ITAB_SETS-SEARCHFLD.'
      TO CODETAB.
      APPEND 'IF SY-SUBRC = 0. ' TO CODETAB.
      APPEND 'ITAB1-CHART_OF_A = TKA01-KTOPL.' TO CODETAB.
      APPEND 'ELSE. ' TO CODETAB.
      APPEND 'ITAB1-CHART_OF_A = ITAB_SETS-SEARCHFLD.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
  ENDIF.
      APPEND 'ITAB1-SET_TABLE = $TABNAME.' TO CODETAB.
      APPEND 'IF VCOUNT = 0.' TO CODETAB.
      APPEND 'APPEND ITAB1.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-SETID TO ITAB1-CHILD_ID.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-CHILD_DESC.'
        TO CODETAB.
      APPEND 'IF LEVEL = 1.' TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 1.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 0.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ITAB1-LEAF_FLAG = 0.' TO CODETAB.
      APPEND 'ITAB1-TREE_LEVEL = P_LEVEL.' TO CODETAB.
      APPEND 'CONDENSE ITAB1-TREE_LEVEL NO-GAPS.' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = SPACE.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = SPACE.' TO CODETAB.
      APPEND 'ITAB1-CONT_AREA = ITAB_SETS-KOKRS.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'ITAB1-CHART_OF_ACCT = ITAB_SETS-KTOPL.' TO CODETAB.
  ELSE.
 APPEND 'SELECT SINGLE * FROM TKA01 WHERE KOKRS = ITAB_SETS-SEARCHFLD. '
       TO CODETAB.
      APPEND 'IF SY-SUBRC = 0. ' TO CODETAB.
      APPEND 'ITAB1-CHART_OF_A = TKA01-KTOPL.' TO CODETAB.
      APPEND 'ELSE. ' TO CODETAB.
      APPEND 'ITAB1-CHART_OF_A = ITAB_SETS-SEARCHFLD.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
  ENDIF.
      APPEND 'ITAB1-SET_TABLE = $TABNAME.' TO CODETAB.
      APPEND '* Pop the stack to find parent node.' TO CODETAB.
      APPEND 'INDEX = NODE.' TO CODETAB.
      APPEND 'WHILE INDEX > 0.' TO CODETAB.
      APPEND 'READ TABLE ITAB_HIERARCHY INDEX INDEX.' TO CODETAB.
      APPEND 'IF ITAB_HIERARCHY-LEVEL = P_LEVEL.' TO CODETAB.
  IF LF_SAP_RELEASE > 4 OR
     ( LF_SAP_RELEASE = 4 AND LF_SAP_SUBVER > 4 ).
      APPEND 'CALL FUNCTION ''G_SET_DECRYPT_SETID''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID      = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'IMPORTING' TO CODETAB.
      APPEND 'SHORTNAME  = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS     = 1.' TO CODETAB.
  ENDIF.
      APPEND 'MOVE ITAB_HIERARCHY-SETID TO ITAB1-PARENT_ID.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-PARENT_DESC.'
        TO CODETAB.
  ELSE.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-PARENT_DES.'
        TO CODETAB.
  ENDIF.
      APPEND 'IF VCOUNT = 0.' TO CODETAB.
      APPEND 'APPEND ITAB1.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'EXIT.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'INDEX = INDEX - 1.' TO CODETAB.
      APPEND 'ENDWHILE.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'WHILE VCOUNT > 0.' TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 0.' TO CODETAB.
      APPEND 'ITAB1-LEAF_FLAG = 1.' TO CODETAB.
      APPEND 'READ TABLE ITAB_VALUES INDEX IDX.' TO CODETAB.
      APPEND 'IF ( ITAB_VALUES-FROM(1) = Hex00 ) and' TO CODETAB.
      APPEND '( ITAB_VALUES-TO(1) = HexFF ).' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = ''*** All values ***''.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = SPACE.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = ITAB_VALUES-FROM.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = ITAB_VALUES-TO.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'APPEND ITAB1.' TO CODETAB.
      APPEND 'VCOUNT = VCOUNT - 1.' TO CODETAB.
      APPEND 'IDX = IDX + 1.' TO CODETAB.
      APPEND 'ENDWHILE.' TO CODETAB.
      APPEND 'ENDLOOP.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'MATCH_SETID = 0.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ENDLOOP.' TO CODETAB.
  IF LF_SAP_RELEASE > 3.
      APPEND '* Fetch 4.0 hierarchy' TO CODETAB.
      APPEND '* SELECT SINGLE * FROM TKA01 WHERE KOKRS LIKE $AREA.'
        TO CODETAB.
      APPEND '* IF SY-SUBRC = 0.' TO CODETAB.
      APPEND 'SELECT * FROM SETHEADER WHERE TABNAME = $TABNAME'
        TO CODETAB.
      APPEND 'AND FIELDNAME = $FELD' TO CODETAB.
      APPEND
        '* AND ( SUBCLASS = TKA01-KOKRS OR SUBCLASS = TKA01-KTOPL )'
        TO CODETAB.
      APPEND 'AND ( SETTYPE = ''S'' OR SETTYPE = ''B'' ).' TO CODETAB.
      APPEND 'IF ( SETHEADER-SUBCLASS <> SPACE ).' TO CODETAB.
      APPEND 'MOVE SETHEADER-SETNAME TO ITAB_SETS40-SETID.' TO CODETAB.
      APPEND 'MOVE SETHEADER-SUBCLASS TO ITAB_SETS40-SEARCHFLD.'
        TO CODETAB.
      APPEND 'MOVE TKA01-KOKRS TO ITAB_SETS40-KOKRS.' TO CODETAB.
      APPEND 'MOVE TKA01-KTOPL TO ITAB_SETS40-KTOPL.' TO CODETAB.
      APPEND 'MOVE SETHEADER-SETCLASS TO ITAB_SETS40-SETCLASS.'
        TO CODETAB.
      APPEND 'APPEND ITAB_SETS40.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'MOVE SETHEADER-SETNAME TO ITAB_SETS40-SETID.' TO CODETAB.
      APPEND 'MOVE SETHEADER-SUBCLASS TO ITAB_SETS40-SEARCHFLD.'
        TO CODETAB.
      APPEND 'MOVE SPACE TO ITAB_SETS40-KOKRS.' TO CODETAB.
      APPEND 'MOVE SPACE TO ITAB_SETS40-KTOPL.' TO CODETAB.
      APPEND 'MOVE SETHEADER-SETCLASS TO ITAB_SETS40-SETCLASS.'
        TO CODETAB.
      APPEND 'APPEND ITAB_SETS40.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ENDSELECT.' TO CODETAB.
      APPEND '*ENDIF.' TO CODETAB.
      APPEND 'MATCH_SETID = 0.' TO CODETAB.
      APPEND 'SORT ITAB_SETS40 BY SETID.' TO CODETAB.
      APPEND 'DELETE ADJACENT DUPLICATES FROM ITAB_SETS40.' TO CODETAB.
      APPEND 'LOOP AT ITAB_SETS40.' TO CODETAB.
      APPEND 'SETNR = ITAB_SETS40-SETID.' TO CODETAB.
      APPEND 'MATCH_SETID = 1.' TO CODETAB.
      APPEND 'IF MATCH_SETID = 1.' TO CODETAB.
      APPEND '* This line is needed to suppress screen pop-up from'
        TO CODETAB.
      APPEND '* G_SET_ENCRYPT_SETID' TO CODETAB.
      APPEND 'SY-BATCH = ''X''.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'CALL FUNCTION ''G_SET_ENCRYPT_SETID''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETCLASS  = ITAB_SETS40-SETCLASS' TO CODETAB.
      APPEND 'SHORTNAME = SETNR' TO CODETAB.
      APPEND 'KOKRS     = ITAB_SETS40-KOKRS' TO CODETAB.
      APPEND 'KTOPL     = ITAB_SETS40-KTOPL' TO CODETAB.
      APPEND 'IMPORTING' TO CODETAB.
      APPEND 'SETID     = SETNR2' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'NO_CO_AREA_SPECIFIED = 1' TO CODETAB.
      APPEND 'ILLEGAL_SETCLASS     = 2' TO CODETAB.
      APPEND 'OTHERS    = 3.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'CALL FUNCTION ''G_SET_GET_ALL_SUPERSETS''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETNAME   = SETNR2' TO CODETAB.
      APPEND 'TABLES' TO CODETAB.
      APPEND 'SUPERSETS = ITAB_SUPERSETS' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS    = 1.' TO CODETAB.
      APPEND 'NO_ROWS = 0.' TO CODETAB.
      APPEND 'LOOP AT ITAB_SUPERSETS.' TO CODETAB.
      APPEND '   CALL FUNCTION ''G_SET_DECRYPT_SETID''' TO CODETAB.
      APPEND '        EXPORTING' TO CODETAB.
      APPEND '        SETID    = ITAB_SUPERSETS-SETNAME' TO CODETAB.
      APPEND '        IMPORTING' TO CODETAB.
      APPEND '        SETCLASS = LOC_SETCLASS_FATHER.' TO CODETAB.
      APPEND '   IF LOC_SETCLASS = LOC_SETCLASS_FATHER.' TO CODETAB.
      APPEND '     NO_ROWS = 1.' TO CODETAB.
      APPEND '     EXIT.' TO CODETAB.
      APPEND '   ENDIF.' TO CODETAB.
      APPEND 'ENDLOOP.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'IF NO_ROWS = 0 .' TO CODETAB.
      APPEND 'CALL FUNCTION ''G_SET_TREE_IMPORT''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID          = SETNR2' TO CODETAB.
      APPEND 'TABNAME        = $TABNAME' TO CODETAB.
      APPEND 'NO_VARIABLE_REPLACEMENT   = ''X''' TO CODETAB.
      APPEND 'ROOT_HEADER_ONLY          = '' ''' TO CODETAB.
      APPEND 'TABLES' TO CODETAB.
      APPEND 'SET_HIERARCHY  = ITAB_HIERARCHY' TO CODETAB.
      APPEND 'SET_VALUES     = ITAB_VALUES' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'SET_NOT_FOUND  = 1' TO CODETAB.
      APPEND 'ILLEGAL_FIELD_REPLACEMENT = 2' TO CODETAB.
      APPEND 'ILLEGAL_TABLE_REPLACEMENT = 3' TO CODETAB.
      APPEND 'OTHERS         = 4.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'NODE = 0.' TO CODETAB.
      APPEND 'IDX = 1.' TO CODETAB.
      APPEND 'LOOP AT ITAB_HIERARCHY.' TO CODETAB.
      APPEND 'CALL FUNCTION ''G_SET_DECRYPT_SETID''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID      = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'IMPORTING' TO CODETAB.
      APPEND 'SHORTNAME  = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS     = 1.' TO CODETAB.
      APPEND 'NODE = NODE + 1.' TO CODETAB.
      APPEND 'VCOUNT = ITAB_HIERARCHY-VCOUNT.' TO CODETAB.
      APPEND 'LEVEL = ITAB_HIERARCHY-LEVEL.' TO CODETAB.
      APPEND 'TYPE = ITAB_HIERARCHY-TYPE.' TO CODETAB.
      APPEND 'P_LEVEL = LEVEL - 1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'IF LEVEL = 0.' TO CODETAB.
      APPEND 'ITAB1-PARENT_ID = SPACE.' TO CODETAB.
      APPEND 'ITAB1-PARENT_DESC = SPACE.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-SETID TO ITAB1-CHILD_ID.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-CHILD_DESC.'
        TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 1.' TO CODETAB.
      APPEND 'ITAB1-LEAF_FLAG = 0.' TO CODETAB.
      APPEND 'ITAB1-TREE_LEVEL = P_LEVEL.' TO CODETAB.
      APPEND 'CONDENSE ITAB1-TREE_LEVEL NO-GAPS.' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = SPACE.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = SPACE.' TO CODETAB.
      APPEND 'ITAB1-CONT_AREA = ITAB_SETS40-KOKRS.' TO CODETAB.
      APPEND 'ITAB1-CHART_OF_ACCT = ITAB_SETS40-KTOPL.' TO CODETAB.
      APPEND 'ITAB1-SET_TABLE = $TABNAME.' TO CODETAB.
      APPEND 'IF VCOUNT = 0.' TO CODETAB.
      APPEND 'APPEND ITAB1.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-SETID TO ITAB1-CHILD_ID.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-CHILD_DESC.'
        TO CODETAB.
      APPEND 'IF LEVEL = 1.' TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 1.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 0.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ITAB1-LEAF_FLAG = 0.' TO CODETAB.
      APPEND 'ITAB1-TREE_LEVEL = P_LEVEL.' TO CODETAB.
      APPEND 'CONDENSE ITAB1-TREE_LEVEL NO-GAPS.' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = SPACE.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = SPACE.' TO CODETAB.
      APPEND 'ITAB1-CONT_AREA = ITAB_SETS40-KOKRS.' TO CODETAB.
      APPEND 'ITAB1-CHART_OF_ACCT = ITAB_SETS40-KTOPL.' TO CODETAB.
      APPEND 'ITAB1-SET_TABLE = $TABNAME.' TO CODETAB.
      APPEND '* Pop the stack to find parent node.' TO CODETAB.
      APPEND 'INDEX = NODE.' TO CODETAB.
      APPEND 'WHILE INDEX > 0.' TO CODETAB.
      APPEND 'READ TABLE ITAB_HIERARCHY INDEX INDEX.' TO CODETAB.
      APPEND 'IF ITAB_HIERARCHY-LEVEL = P_LEVEL.' TO CODETAB.
      APPEND 'CALL FUNCTION ''G_SET_DECRYPT_SETID''' TO CODETAB.
      APPEND 'EXPORTING' TO CODETAB.
      APPEND 'SETID      = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'IMPORTING' TO CODETAB.
      APPEND 'SHORTNAME  = ITAB_HIERARCHY-SETID' TO CODETAB.
      APPEND 'EXCEPTIONS' TO CODETAB.
      APPEND 'OTHERS     = 1.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-SETID TO ITAB1-PARENT_ID.' TO CODETAB.
      APPEND 'MOVE ITAB_HIERARCHY-DESCRIPT TO ITAB1-PARENT_DESC.'
        TO CODETAB.
      APPEND 'IF VCOUNT = 0.' TO CODETAB.
      APPEND 'APPEND ITAB1.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'EXIT.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'INDEX = INDEX - 1.' TO CODETAB.
      APPEND 'ENDWHILE.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'WHILE VCOUNT > 0.' TO CODETAB.
      APPEND 'ITAB1-ROOT_FLAG = 0.' TO CODETAB.
      APPEND 'ITAB1-LEAF_FLAG = 1.' TO CODETAB.
      APPEND 'READ TABLE ITAB_VALUES INDEX IDX.' TO CODETAB.
      APPEND 'IF ( ITAB_VALUES-FROM(1) = Hex00 ) and' TO CODETAB.
      APPEND '( ITAB_VALUES-TO(1) = HexFF ).' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = ''*** All values ***''.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = SPACE.' TO CODETAB.
      APPEND 'ELSE.' TO CODETAB.
      APPEND 'ITAB1-VALUE_FROM = ITAB_VALUES-FROM.' TO CODETAB.
      APPEND 'ITAB1-VALUE_TO = ITAB_VALUES-TO.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'APPEND ITAB1.' TO CODETAB.
      APPEND 'VCOUNT = VCOUNT - 1.' TO CODETAB.
      APPEND 'IDX = IDX + 1.' TO CODETAB.
      APPEND 'ENDWHILE.' TO CODETAB.
      APPEND 'ENDLOOP.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'MATCH_SETID = 0.' TO CODETAB.
      APPEND 'ENDIF.' TO CODETAB.
      APPEND 'ENDLOOP.' TO CODETAB.
  ENDIF.
      APPEND '' TO CODETAB.
      APPEND 'ENDFORM.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'FORM FORM2 TABLES WRITES.' TO CODETAB.
      APPEND 'data: begin of ht,' TO CODETAB.
      APPEND '      x(1) type c value ''|'',' TO CODETAB.
      APPEND '      end of ht.' TO CODETAB.
      APPEND 'data  dlmtlen type i value ''1''.' TO CODETAB.
      APPEND ' perform write_delimited_file' TO CODETAB.
      APPEND '           tables   ITAB1' TO CODETAB.
      APPEND '                    WRITES' TO CODETAB.
      APPEND '           using    ht' TO CODETAB.
      APPEND '                    dlmtlen.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'ENDFORM.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'form write_delimited_file' TO CODETAB.
      APPEND '           tables   datatab' TO CODETAB.
      APPEND '                    writes' TO CODETAB.
      APPEND '           using    delimit' TO CODETAB.
      APPEND '                    dlength.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '  data: type1,' TO CODETAB.
      APPEND '        appd(1),' TO CODETAB.
      APPEND '        temp(32),' TO CODETAB.
      APPEND '        time1(8),' TO CODETAB.
      APPEND '        date1(10),' TO CODETAB.
      APPEND '        output(8192),' TO CODETAB.
      APPEND '        rcount type i,' TO CODETAB.
      APPEND '        offset type i,' TO CODETAB.
      APPEND '        tablen type i,' TO CODETAB.
      APPEND '        maxlen type i value ''8192''.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '  data: begin of clientab occurs 0,' TO CODETAB.
      APPEND '             output(8192),' TO CODETAB.
      APPEND '          end of clientab.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '  field-symbols: <f>.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '  describe table datatab lines tablen.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '  loop at datatab.' TO CODETAB.
      APPEND '    clear: tablen, offset, output.' TO CODETAB.
      APPEND '    do.' TO CODETAB.
      APPEND '      assign component sy-index of' TO CODETAB.
      APPEND '         structure datatab to <f>.' TO CODETAB.
      APPEND '      if sy-subrc <> 0. exit. endif.' TO CODETAB.
      APPEND '      if sy-index > 1.' TO CODETAB.
      APPEND '         write delimit to output+offset(dlength).'
        TO CODETAB.
      APPEND '         add dlength to offset.' TO CODETAB.
      APPEND '      endif.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '      describe field <f> type type1.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '      if type1 = ''I'' or type1 = ''N''.' TO CODETAB.
      APPEND '          type1 = ''P''.' TO CODETAB.
      APPEND '      endif.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '      case type1.' TO CODETAB.
      APPEND '        when ''D''.' TO CODETAB.
      APPEND '          if <f> = ''00000000''.' TO CODETAB.
      APPEND '             <f> = '' ''.' TO CODETAB.
      APPEND '          else.' TO CODETAB.
      APPEND '             move <f> to time1.' TO CODETAB.
      APPEND '             assign time1 to <f>.' TO CODETAB.
      APPEND '          endif.' TO CODETAB.
      APPEND '        when ''F''.' TO CODETAB.
      APPEND '          if <f> = ''0.0''.' TO CODETAB.
      APPEND '            temp = ''0.0''.' TO CODETAB.
      APPEND '          else.' TO CODETAB.
      APPEND '             write <f> to temp exponent 0.' TO CODETAB.
      APPEND '          endif.' TO CODETAB.
      APPEND '          condense temp no-gaps.' TO CODETAB.
      APPEND '          translate temp using '',.''.' TO CODETAB.
      APPEND '          assign temp to <f>.' TO CODETAB.
      APPEND '        when ''P''.' TO CODETAB.
      APPEND '          if <f> < 0.' TO CODETAB.
      APPEND '             write ''-'' to output+offset(1).' TO CODETAB.
      APPEND '             add 1 to offset.' TO CODETAB.
      APPEND '             <f> = <f> * ( -1 ).' TO CODETAB.
      APPEND '          endif.' TO CODETAB.
      APPEND '          move <f> to temp.' TO CODETAB.
      APPEND '          condense temp no-gaps.' TO CODETAB.
      APPEND '          translate temp using '',.''.' TO CODETAB.
      APPEND '          assign temp to <f>.' TO CODETAB.
      APPEND '      endcase.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '      sy-fdpos = strlen( <f> ).' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '      tablen = offset + sy-fdpos.' TO CODETAB.
      APPEND '      if tablen > maxlen.' TO CODETAB.
      APPEND '         exit.' TO CODETAB.
      APPEND '      endif.' TO CODETAB.
      APPEND '      write <f> to output+offset(sy-fdpos).' TO CODETAB.
      APPEND '      add sy-fdpos to offset.' TO CODETAB.
      APPEND '    enddo.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '    condense output.' TO CODETAB.
      APPEND '    append output to writes.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '  endloop.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'endform.' TO CODETAB.
      generate subroutine pool CODETAB name pn message msg line ln.
endif.

PERFORM Z_AW_TREE_PROF_FORM IN PROGRAM (pn)
       TABLES
             WRITES
       USING
             P_GROUP
             P_TABLE.

ENDFUNCTION.

FORM Z_AW_TREE_PROF_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
