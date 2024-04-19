FUNCTION Z_AW_READ_TEXT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(CLIENT) LIKE  SY-MANDT DEFAULT SY-MANDT
*"             VALUE(ID) LIKE  THEAD-TDID
*"             VALUE(LANGUAGE) LIKE  THEAD-TDSPRAS
*"             VALUE(NAME) LIKE  THEAD-TDNAME
*"             VALUE(OBJECT) LIKE  THEAD-TDOBJECT
*"             VALUE(ARCHIVE_HANDLE) LIKE  SY-TABIX DEFAULT 0
*"       EXPORTING
*"             VALUE(HEADER) LIKE  THEAD STRUCTURE  THEAD
*"       TABLES
*"              LINES STRUCTURE  TLINE
*"       EXCEPTIONS
*"              ID
*"              LANGUAGE
*"              NAME
*"              NOT_FOUND
*"              OBJECT
*"              REFERENCE_CHECK
*"              WRONG_ACCESS_TO_ARCHIVE
*"----------------------------------------------------------------------

DATA BEGIN OF TEXTHEADER.
        INCLUDE STRUCTURE THEAD.
DATA END OF TEXTHEADER.

DATA BEGIN OF TEXTLINES OCCURS 10.
        INCLUDE STRUCTURE TLINE.
DATA END OF TEXTLINES.

CLEAR TEXTHEADER.

CALL FUNCTION 'READ_TEXT'
       EXPORTING
            OBJECT                  = OBJECT
            ID                      = ID
            LANGUAGE                = LANGUAGE
            NAME                    = NAME
       IMPORTING
            HEADER                  = TEXTHEADER
       TABLES
            LINES                   = TEXTLINES
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

CASE SY-SUBRC.
  WHEN 1.
    RAISE ID.
  WHEN 2.
    RAISE LANGUAGE.
  WHEN 3.
    RAISE NAME.
  WHEN 4.
    RAISE NOT_FOUND.
  WHEN 5.
    RAISE OBJECT.
  WHEN 6.
    RAISE REFERENCE_CHECK.
ENDCASE.
LOOP AT TEXTLINES.
   LINES = TEXTLINES.
   APPEND LINES.
ENDLOOP.

ENDFUNCTION.

FORM Z_AW_READ_TEXT_GETV CHANGING VERSION TYPE C.

VERSION = '6.5.1.0'.

ENDFORM.
