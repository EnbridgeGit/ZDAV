FUNCTION Z_AW_IDOC_SEARCH.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_NAME) LIKE  EDIDOT-DESCRP
*"             VALUE(P_DESC) LIKE  EDIDOT-DESCRP
*"             VALUE(P_LANG) LIKE  EDIDOT-LANGUA
*"             VALUE(P_MAXROWS) LIKE  SY-TABIX
*"             VALUE(P_FLAG) LIKE  SY-TABIX
*"       TABLES
*"              P_RETURN STRUCTURE  LISTZEILE
*"----------------------------------------------------------------------
DATA: COUNT TYPE I.

statics pn like sy-cprog.

DATA: codetab(72) occurs 10 with header line,
      ln type i, msg(128),
      L_TABNAME LIKE DD02L-TABNAME,
      lf_sap_release type i.

COUNT = 0.
SELECT * FROM EDIDOT
        WHERE LANGUA = P_LANG.

   IF P_FLAG = 1.
      IF P_NAME <> SPACE.
         IF EDIDOT-DOCTYP CS P_NAME.
            IF P_DESC <> SPACE.
               IF EDIDOT-DESCRP CS P_DESC.
                  IF COUNT = P_MAXROWS.
                     EXIT.
                  ELSE.
                     CONCATENATE
                        EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
                        INTO P_RETURN-ZEILE.
                     APPEND P_RETURN.
                     COUNT = COUNT + 1.
                  ENDIF.
               ENDIF.
            ELSE.
               IF COUNT = P_MAXROWS.
                  EXIT.
               ELSE.
                  CONCATENATE
                     EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
                     INTO P_RETURN-ZEILE.
                  APPEND P_RETURN.
                  COUNT = COUNT + 1.
               ENDIF.
            ENDIF.
         ENDIF.
      ELSEIF P_DESC <> SPACE.
         IF EDIDOT-DESCRP CS P_DESC.
            IF COUNT = P_MAXROWS.
               EXIT.
            ELSE.
               CONCATENATE
                  EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
                  INTO P_RETURN-ZEILE.
               APPEND P_RETURN.
               COUNT = COUNT + 1.
            ENDIF.
         ENDIF.
      ELSE.
         IF COUNT = P_MAXROWS.
            EXIT.
         ELSE.
            CONCATENATE
               EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
               INTO P_RETURN-ZEILE.
            APPEND P_RETURN.
            COUNT = COUNT + 1.
         ENDIF.
      ENDIF.
   ELSE.
      IF P_NAME <> SPACE.
         IF EDIDOT-DOCTYP = P_NAME.
            IF P_DESC <> SPACE.
               IF EDIDOT-DESCRP = P_DESC.
                  IF COUNT = P_MAXROWS.
                     EXIT.
                  ELSE.
                     CONCATENATE
                        EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
                        INTO P_RETURN-ZEILE.
                     APPEND P_RETURN.
                     COUNT = COUNT + 1.
                  ENDIF.
               ENDIF.
            ELSE.
               IF COUNT = P_MAXROWS.
                  EXIT.
               ELSE.
                  CONCATENATE
                     EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
                     INTO P_RETURN-ZEILE.
                  APPEND P_RETURN.
                  COUNT = COUNT + 1.
               ENDIF.
            ENDIF.
         ENDIF.
      ELSEIF P_DESC <> SPACE.
         IF EDIDOT-DESCRP = P_DESC.
            IF COUNT = P_MAXROWS.
               EXIT.
            ELSE.
               CONCATENATE
                  EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
                  INTO P_RETURN-ZEILE.
               APPEND P_RETURN.
               COUNT = COUNT + 1.
            ENDIF.
         ENDIF.
      ELSE.
         IF COUNT = P_MAXROWS.
            EXIT.
         ELSE.
            CONCATENATE
               EDIDOT-DOCTYP '|' EDIDOT-DESCRP '|'
               INTO P_RETURN-ZEILE.
            APPEND P_RETURN.
            COUNT = COUNT + 1.
         ENDIF.
      ENDIF.
   ENDIF.
ENDSELECT.

IF COUNT = 0.
   move sy-saprl(1) to lf_sap_release.

   SELECT TABNAME INTO L_TABNAME FROM DD02L UP TO 1 ROWS
   WHERE TABNAME = 'EDBAST' AND AS4LOCAL = 'A'.
   ENDSELECT.

   IF SY-SUBRC = 0 AND lf_sap_release > 3.
      if PN(2) <> '%_'.
      APPEND 'REPORT ZIDOCS02 LINE-SIZE 255.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'FORM ZAW_ZIDOCSRC_FORM' TO CODETAB.
      APPEND '  TABLES' TO CODETAB.
      APPEND '      P_RETURN STRUCTURE  LISTZEILE' TO CODETAB.
      APPEND '  USING' TO CODETAB.
      APPEND '     COUNT TYPE I' TO CODETAB.
      APPEND '     P_NAME LIKE  EDIDOT-DESCRP' TO CODETAB.
      APPEND '     P_DESC LIKE  EDIDOT-DESCRP' TO CODETAB.
      APPEND '     P_LANG LIKE  EDIDOT-LANGUA' TO CODETAB.
      APPEND '     P_MAXROWS LIKE  SY-TABIX' TO CODETAB.
      APPEND '     P_FLAG LIKE  SY-TABIX.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'TABLES: EDBAST.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'SELECT * FROM EDBAST' TO CODETAB.
      APPEND '        WHERE LANGUA = P_LANG.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND '   IF P_FLAG = 1.' TO CODETAB.
      APPEND '      IF P_NAME <> SPACE.' TO CODETAB.
      APPEND '         IF EDBAST-IDOCTYP CS P_NAME.' TO CODETAB.
      APPEND '            IF P_DESC <> SPACE.' TO CODETAB.
      APPEND '               IF EDBAST-DESCRP CS P_DESC.' TO CODETAB.
      APPEND '                  IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '                     EXIT.' TO CODETAB.
      APPEND '                  ELSE.' TO CODETAB.
      APPEND '                     CONCATENATE' TO CODETAB.
      APPEND '              EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '                        INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '                     APPEND P_RETURN.' TO CODETAB.
      APPEND '                     COUNT = COUNT + 1.' TO CODETAB.
      APPEND '                  ENDIF.' TO CODETAB.
      APPEND '               ENDIF.' TO CODETAB.
      APPEND '            ELSE.' TO CODETAB.
      APPEND '               IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '                  EXIT.' TO CODETAB.
      APPEND '               ELSE.' TO CODETAB.
      APPEND '                  CONCATENATE' TO CODETAB.
      APPEND '              EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '                     INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '                  APPEND P_RETURN.' TO CODETAB.
      APPEND '                  COUNT = COUNT + 1.' TO CODETAB.
      APPEND '               ENDIF.' TO CODETAB.
      APPEND '            ENDIF.' TO CODETAB.
      APPEND '         ENDIF.' TO CODETAB.
      APPEND '      ELSEIF P_DESC <> SPACE.' TO CODETAB.
      APPEND '         IF EDBAST-DESCRP CS P_DESC.' TO CODETAB.
      APPEND '            IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '               EXIT.' TO CODETAB.
      APPEND '            ELSE.' TO CODETAB.
      APPEND '               CONCATENATE' TO CODETAB.
      APPEND '             EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '                  INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '               APPEND P_RETURN.' TO CODETAB.
      APPEND '               COUNT = COUNT + 1.' TO CODETAB.
      APPEND '            ENDIF.' TO CODETAB.
      APPEND '         ENDIF.' TO CODETAB.
      APPEND '      ELSE.' TO CODETAB.
      APPEND '         IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '            EXIT.' TO CODETAB.
      APPEND '         ELSE.' TO CODETAB.
      APPEND '            CONCATENATE' TO CODETAB.
      APPEND '               EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '               INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '            APPEND P_RETURN.' TO CODETAB.
      APPEND '            COUNT = COUNT + 1.' TO CODETAB.
      APPEND '         ENDIF.' TO CODETAB.
      APPEND '      ENDIF.' TO CODETAB.
      APPEND '   ELSE.' TO CODETAB.
      APPEND '      IF P_NAME <> SPACE.' TO CODETAB.
      APPEND '         IF EDBAST-IDOCTYP = P_NAME.' TO CODETAB.
      APPEND '            IF P_DESC <> SPACE.' TO CODETAB.
      APPEND '               IF EDBAST-DESCRP = P_DESC.' TO CODETAB.
      APPEND '                  IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '                     EXIT.' TO CODETAB.
      APPEND '                  ELSE.' TO CODETAB.
      APPEND '                     CONCATENATE' TO CODETAB.
      APPEND '             EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '                        INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '                     APPEND P_RETURN.' TO CODETAB.
      APPEND '                     COUNT = COUNT + 1.' TO CODETAB.
      APPEND '                  ENDIF.' TO CODETAB.
      APPEND '               ENDIF.' TO CODETAB.
      APPEND '            ELSE.' TO CODETAB.
      APPEND '               IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '                  EXIT.' TO CODETAB.
      APPEND '               ELSE.' TO CODETAB.
      APPEND '                  CONCATENATE' TO CODETAB.
      APPEND '              EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '                     INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '                  APPEND P_RETURN.' TO CODETAB.
      APPEND '                  COUNT = COUNT + 1.' TO CODETAB.
      APPEND '               ENDIF.' TO CODETAB.
      APPEND '            ENDIF.' TO CODETAB.
      APPEND '         ENDIF.' TO CODETAB.
      APPEND '      ELSEIF P_DESC <> SPACE.' TO CODETAB.
      APPEND '         IF EDBAST-DESCRP = P_DESC.' TO CODETAB.
      APPEND '            IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '               EXIT.' TO CODETAB.
      APPEND '            ELSE.' TO CODETAB.
      APPEND '               CONCATENATE' TO CODETAB.
      APPEND '            EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '                  INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '               APPEND P_RETURN.' TO CODETAB.
      APPEND '               COUNT = COUNT + 1.' TO CODETAB.
      APPEND '            ENDIF.' TO CODETAB.
      APPEND '         ENDIF.' TO CODETAB.
      APPEND '      ELSE.' TO CODETAB.
      APPEND '         IF COUNT = P_MAXROWS.' TO CODETAB.
      APPEND '            EXIT.' TO CODETAB.
      APPEND '         ELSE.' TO CODETAB.
      APPEND '            CONCATENATE' TO CODETAB.
      APPEND '          EDBAST-IDOCTYP ''|'' EDBAST-DESCRP ''|'''
        TO CODETAB.
      APPEND '               INTO P_RETURN-ZEILE.' TO CODETAB.
      APPEND '            APPEND P_RETURN.' TO CODETAB.
      APPEND '            COUNT = COUNT + 1.' TO CODETAB.
      APPEND '         ENDIF.' TO CODETAB.
      APPEND '      ENDIF.' TO CODETAB.
      APPEND '   ENDIF.' TO CODETAB.
      APPEND 'ENDSELECT.' TO CODETAB.
      APPEND '' TO CODETAB.
      APPEND 'ENDFORM.' TO CODETAB.
      clear msg.
      generate subroutine pool CODETAB name pn message msg line ln.
      else. clear msg. endif. " Already generated
      if msg is initial.
         PERFORM ZAW_ZIDOCSRC_FORM IN PROGRAM (pn)
           TABLES
               P_RETURN
           USING
              COUNT
              P_NAME
              P_DESC
              P_LANG
              P_MAXROWS
              P_FLAG.
      endif.
   ENDIF.
ENDIF.

ENDFUNCTION.

FORM Z_AW_IDOC_SEARCH_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
