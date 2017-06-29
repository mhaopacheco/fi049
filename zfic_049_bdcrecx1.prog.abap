*&---------------------------------------------------------------------*
*&  Include           ZBDCRECX1
*&---------------------------------------------------------------------*
***INCLUDE BDCRECX1.
*  for programs doing a data transfer by creating a batch-input session
*  and
*  for programs doing a data transfer by CALL TRANSACTION USING

SELECTION-SCREEN BEGIN OF BLOCK para WITH FRAME TITLE TEXT-t02.
*  PARAMETERS: P_DATASE LIKE RLGRAP-FILENAME OBLIGATORY DEFAULT 'C:_MAT.TXT' .
PARAMETERS p_mode LIKE ctu_params-dismode DEFAULT 'A'.
"A: show all dynpros
"E: show dynpro on error only
"N: do not display dynpro
SELECTION-SCREEN END OF BLOCK para.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA: BEGIN OF bdcdata OCCURS 0 .       "Batch  input table
    INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened(' ' or 'X')
DATA:   e_group_opened ,
      opt LIKE ctu_params.
*       message texts
TABLES: t100 .

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*  (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
*  IF SESSION = 'X'.
**   close batchinput group
*    CALL FUNCTION 'BDC_CLOSE_GROUP'.
*    WRITE: /(30) 'BDC_CLOSE_GROUP'(I04),
*           (12) 'returncode:'(I05),
*                 SY-SUBRC.
*  ELSE.
*    IF E_GROUP_OPENED = 'X'.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  WRITE: / .
  WRITE: /(30) 'Fehlermappe wurde erzeugt'(i06).
  e_group_opened = ' '.
*    ENDIF.
*  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
** batch input session
*  IF SESSION = 'X'.
*    CALL FUNCTION 'BDC_INSERT'
*         EXPORTING TCODE     = TCODE
*         TABLES    DYNPROTAB = BDCDATA.
*    IF SMALLLOG <> 'X'.
*      WRITE: / 'BDC_INSERT'(I03),
*               TCODE,
*               'returncode:'(I05),
*               SY-SUBRC,
*               'RECORD:',
*               SY-INDEX.
*    ENDIF.
** call transaction using
*  ELSE.
  REFRESH messtab.

  CALL TRANSACTION tcode USING bdcdata
        MODE   p_mode
        UPDATE 'S'
        MESSAGES INTO messtab.
*    L_SUBRC = SY-SUBRC.
*    IF SMALLLOG <> 'X'.

  FORMAT COLOR COL_GROUP INTENSIFIED OFF.
  WRITE: / 'CALL_TRANSACTION',
          tcode,
        'returncode:'(i05),
        sy-subrc,
        'RECORD:',
        sy-index.

  "BEGINOF: MPACHECO: 10.04.2017 14:50:36
  "Obser: Eliminar Mensajes cuando se creo el documento
  IF line_exists( messtab[ msgtyp = 'S' msgnr = '312' ] ).
    DELETE messtab WHERE NOT ( msgtyp = 'S' AND msgnr = '312' ) .
  ENDIF.
  "ENDOF: MPACHECO: 10.04.2017 14:50:46

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  LOOP AT messtab.
    SELECT SINGLE * FROM t100 WHERE sprsl = messtab-msgspra
    AND   arbgb = messtab-msgid
    AND   msgnr = messtab-msgnr.
    IF sy-subrc = 0 .
      l_mstring = t100-text .
      IF l_mstring CS '&1' .
        REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
      ELSE .
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
      ENDIF .
      CONDENSE l_mstring.
      WRITE : / messtab-msgtyp, l_mstring(250).
    ELSE.
      WRITE : / messtab.
    ENDIF .
  ENDLOOP.

  ULINE.
  SKIP.
*    ENDIF.
** Erzeugen fehlermappe ************************************************
*    IF L_SUBRC <> 0 AND E_GROUP <> SPACE.
*      IF E_GROUP_OPENED = ' '.
*        CALL FUNCTION 'BDC_OPEN_GROUP'
*             EXPORTING  CLIENT   = SY-MANDT
*                        GROUP    = E_GROUP
*                        USER     = E_USER
*                        KEEP     = E_KEEP
*                        HOLDDATE = E_HDATE.
*         E_GROUP_OPENED = 'X'.
*      ENDIF.
*      CALL FUNCTION 'BDC_INSERT'
*           EXPORTING TCODE     = TCODE
*           TABLES    DYNPROTAB = BDCDATA.
*    ENDIF.
*  ENDIF.
  REFRESH bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro .
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
*  IF FVAL <> NODATA.
*  IF FVAL <> '/' .

  CLEAR bdcdata.
  bdcdata-fnam = fnam .
  bdcdata-fval = fval .
  APPEND bdcdata.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*     -->P_0370   text
*----------------------------------------------------------------------*
FORM bdc_transaction2 USING VALUE(p_0370).
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.

  REFRESH messtab.
  opt-dismode = p_mode.

  CALL TRANSACTION p_0370 USING bdcdata OPTIONS FROM opt MESSAGES INTO messtab.

  " *******************************************************************
  FORMAT COLOR COL_GROUP INTENSIFIED OFF.
  WRITE: / 'CALL_TRANSACTION',
  p_0370,
  'returncode:'(i05),
  sy-subrc,
  'RECORD:',
  sy-index.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  LOOP AT messtab.
    SELECT SINGLE * FROM t100 WHERE sprsl = messtab-msgspra
    AND   arbgb = messtab-msgid
    AND   msgnr = messtab-msgnr.
    IF sy-subrc = 0 .
      l_mstring = t100-text .
      IF l_mstring CS '&1' .
        REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
      ELSE .
        REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
      ENDIF .
      CONDENSE l_mstring.
      WRITE : / messtab-msgtyp, l_mstring(250).
    ELSE.
      WRITE : / messtab.
    ENDIF .
  ENDLOOP.

  ULINE.
  SKIP.
  REFRESH bdcdata.

ENDFORM.                    " BDC_TRANSACTION2

*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*     -->P_0370   text
*----------------------------------------------------------------------*
FORM bdc_transaction3 USING VALUE(p_0370) ud_kunnr.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.

  REFRESH messtab.
  opt-dismode = p_mode.

  CALL TRANSACTION p_0370 USING bdcdata OPTIONS FROM opt MESSAGES INTO messtab.


  READ TABLE messtab INTO DATA(ls_messtab) WITH KEY msgid = 'F5' msgnr = '312'  .
  IF sy-subrc = 0.
    PERFORM add_row_output USING ud_kunnr ls_messtab-msgv1 'Documento Creado' .
  ELSE.
    LOOP AT messtab .
      SELECT SINGLE * FROM t100 WHERE sprsl = messtab-msgspra
        AND   arbgb = messtab-msgid
        AND   msgnr = messtab-msgnr.

      IF sy-subrc = 0 .
        l_mstring = t100-text .
        IF l_mstring CS '&1' .
          REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
          REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
          REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
          REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
        ELSE .
          REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
          REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
          REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
          REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
        ENDIF .
        CONDENSE l_mstring.
        PERFORM add_row_output USING ud_kunnr space l_mstring(250) .
      ENDIF.
    ENDLOOP.
  ENDIF.

  REFRESH bdcdata.

ENDFORM.                    " BDC_TRANSACTION3
