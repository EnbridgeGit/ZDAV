FUNCTION Z_AW_TABLE_SEARCH.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_NAME) LIKE  DD02VV-DDTEXT
*"             VALUE(P_DESC) LIKE  DD02VV-DDTEXT
*"             VALUE(P_LANG) LIKE  DD02V-DDLANGUAGE
*"             VALUE(P_MAXROWS) LIKE  SY-TABIX
*"             VALUE(P_FLAG) LIKE  SY-TABIX
*"       TABLES
*"              P_RETURN STRUCTURE  LISTZEILE
*"----------------------------------------------------------------------
DATA: COUNT TYPE I.

COUNT = 0.
SELECT * FROM DD02VV
        WHERE AS4LOCAL = 'A'
        AND   DDLANGUAGE = P_LANG
        AND   ( TABCLASS = 'TRANSP'
        OR      TABCLASS = 'CLUSTER'
        OR      TABCLASS = 'POOL'
        OR      TABCLASS = 'VIEW' ).

   IF DD02VV-TABCLASS = 'VIEW'.
       SELECT SINGLE *
       FROM DD02L
         WHERE TABNAME = DD02VV-TABNAME
           AND AS4LOCAL = DD02VV-AS4LOCAL
           AND TABCLASS = DD02VV-TABCLASS
           AND ( VIEWCLASS = 'D' OR VIEWCLASS = 'P' ).
       IF SY-SUBRC <> 0.
         CONTINUE.
       ENDIF.
   ENDIF.

   IF P_FLAG = 1.
      IF P_NAME <> SPACE.
         IF DD02VV-TABNAME CS P_NAME.
            IF P_DESC <> SPACE.
               IF DD02VV-DDTEXT CS P_DESC.
                  IF COUNT = P_MAXROWS.
                     EXIT.
                  ELSE.
                     CONCATENATE
                        DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
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
                     DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
                     INTO P_RETURN-ZEILE.
                  APPEND P_RETURN.
                  COUNT = COUNT + 1.
               ENDIF.
            ENDIF.
         ENDIF.
      ELSEIF P_DESC <> SPACE.
         IF DD02VV-DDTEXT CS P_DESC.
            IF COUNT = P_MAXROWS.
               EXIT.
            ELSE.
               CONCATENATE
                  DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
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
               DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
               INTO P_RETURN-ZEILE.
            APPEND P_RETURN.
            COUNT = COUNT + 1.
         ENDIF.
      ENDIF.
   ELSE.
      IF P_NAME <> SPACE.
         IF DD02VV-TABNAME = P_NAME.
            IF P_DESC <> SPACE.
               IF DD02VV-DDTEXT = P_DESC.
                  IF COUNT = P_MAXROWS.
                     EXIT.
                  ELSE.
                     CONCATENATE
                        DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
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
                     DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
                     INTO P_RETURN-ZEILE.
                  APPEND P_RETURN.
                  COUNT = COUNT + 1.
               ENDIF.
            ENDIF.
         ENDIF.
      ELSEIF P_DESC <> SPACE.
         IF DD02VV-DDTEXT = P_DESC.
            IF COUNT = P_MAXROWS.
               EXIT.
            ELSE.
               CONCATENATE
                  DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
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
               DD02VV-TABNAME '|' DD02VV-DDTEXT '|'
               INTO P_RETURN-ZEILE.
            APPEND P_RETURN.
            COUNT = COUNT + 1.
         ENDIF.
      ENDIF.
   ENDIF.
ENDSELECT.

ENDFUNCTION.

FORM Z_AW_TABLE_SEARCH_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
