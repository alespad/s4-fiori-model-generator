*&---------------------------------------------------------------------*
*& Report Z_FPM_ANALYZER_GENERATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fiori_model_generator.


"---------- Headings shown as comments (no text elements) ----------

" Row spacing helpers
CONSTANTS: c_w TYPE i VALUE 120.

" ------- Block 1: parameters (no frame title) -------
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECTION-SCREEN COMMENT /1(80) gvtinout.
  SELECTION-SCREEN SKIP.

  PARAMETERS:
    p_infile TYPE rlgrap-filename OBLIGATORY LOWER CASE,  " Input CSV (app server path)
    p_csvout TYPE rlgrap-filename LOWER CASE,             " Output CSV (app server path)
    p_jsnout TYPE rlgrap-filename LOWER CASE,             " Output JSON (app server path)
    p_ids    TYPE string LOWER CASE.                      " Optional: comma-separated IDs

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(80) gv_i1.
  SELECTION-SCREEN COMMENT /1(80) gv_i2.
  SELECTION-SCREEN COMMENT /1(80) gv_i3.
SELECTION-SCREEN END OF BLOCK b1.

" ------- Block 2: output format (no frame title) -------
SELECTION-SCREEN BEGIN OF BLOCK b2.
  SELECTION-SCREEN COMMENT /1(80) gvtform.
  SELECTION-SCREEN SKIP.

  PARAMETERS:
    p_outcsv AS CHECKBOX DEFAULT abap_true,  " produce CSV
    p_outjsn AS CHECKBOX DEFAULT abap_true.  " produce JSON
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  gvtinout = 'Input / Output files'.
  gvtform = 'Output formats'.
  gv_i1 = 'Input CSV header: FioriId,AppName,Link,PrimaryODataServiceName,BSPName'.
  gv_i2 = 'Example: F0842A,Manage Purchase Orders,...,MM_PUR_PO_MAINT_V2_SRV,MM_PU_PO_MAINT_V2'.
  gv_i3 = 'Optional P_IDS: F0842A,F148A'.

*----------------------------------------------------------------------
* Start of selection
*----------------------------------------------------------------------
START-OF-SELECTION.

  DATA(results) = zcl_fiori_model_analyzer=>run_collect(
                    iv_infile = p_infile
                    iv_ids    = p_ids ).

  " Se almeno un path è valorizzato, scrivo i file selezionati
  IF ( p_csvout IS NOT INITIAL OR p_jsnout IS NOT INITIAL ).

    IF p_outcsv = abap_true AND p_csvout IS NOT INITIAL.
      zcl_fiori_model_analyzer=>write_output_csv(
        it_res  = results
        iv_path = p_csvout ).
      WRITE: / 'CSV written to', p_csvout.
    ENDIF.

    IF p_outjsn = abap_true AND p_jsnout IS NOT INITIAL.
      zcl_fiori_model_analyzer=>write_output_json(
        it_res  = results
        iv_path = p_jsnout ).
      WRITE: / 'JSON written to', p_jsnout.
    ENDIF.

    IF results IS INITIAL.
      WRITE: / 'No results produced (check input CSV or filter P_IDS)'.
    ENDIF.

  ELSE.
    " Nessun path di output: stampa a video
    IF results IS INITIAL.
      WRITE: / 'No results produced (check input CSV or filter P_IDS)'.
      RETURN.
    ENDIF.

    " Header
    DATA lv_head  TYPE string.
    lv_head = |FioriId;AppName;Link;MainServiceName;ODataVersion;ProgrammingModel;BusinessEntity;FPM_Enhanced|.
    WRITE: / lv_head.

    " Righe
    LOOP AT results ASSIGNING FIELD-SYMBOL(<r>).
      DATA lv_line  TYPE string.
      lv_line = |{ <r>-fiori_id };{ <r>-app_name };{ <r>-library_link };{ <r>-main_service_name };{ <r>-odata_version };{ <r>-programming_model };{ <r>-business_entity };{ <r>-fpm_extended }|.
      WRITE: / lv_line.
    ENDLOOP.
  ENDIF.
