*&---------------------------------------------------------------------*
*& Report Z_FIORI_MODEL_GENERATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fiori_model_generator.

" ------- Block 1: Input/Output Location -------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-T01.
  PARAMETERS:
    p_server RADIOBUTTON GROUP loc DEFAULT 'X' USER-COMMAND loc,
    p_local  RADIOBUTTON GROUP loc.
SELECTION-SCREEN END OF BLOCK b1.

" ------- Block 2: Server File Paths -------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-T02.
  PARAMETERS:
    p_infile TYPE string MODIF ID srv LOWER CASE.
  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_csvout TYPE string MODIF ID srv LOWER CASE.
  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_jsnout TYPE string MODIF ID srv LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

" ------- Block 3: Local File Paths -------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-T03.
  PARAMETERS:
    p_inloc  TYPE string MODIF ID loc LOWER CASE.
  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_csvloc TYPE string MODIF ID loc LOWER CASE.
  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_jsnloc TYPE string MODIF ID loc LOWER CASE.
SELECTION-SCREEN END OF BLOCK b3.

" ------- Block 4: Filters -------
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-T04.
  PARAMETERS:
    p_ids TYPE string LOWER CASE.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(70) TEXT-T31.
  SELECTION-SCREEN COMMENT /1(70) TEXT-T32.
SELECTION-SCREEN END OF BLOCK b4.

" ------- Block 5: Output Format -------
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-T05.
  PARAMETERS:
    p_outcsv AS CHECKBOX DEFAULT 'X',
    p_outjsn AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Hide/show based on location selection
    IF screen-group1 = 'SRV'.
      IF p_server = abap_true.
        screen-active = 1.
        screen-invisible = 0.
      ELSE.
        screen-active = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.

    ELSEIF screen-group1 = 'LOC'.
      IF p_local = abap_true.
        screen-active = 1.
        screen-invisible = 0.
      ELSE.
        screen-active = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_inloc.
  PERFORM f4_browse_local_input CHANGING p_inloc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_csvloc.
  PERFORM f4_browse_local_save USING 'CSV' CHANGING p_csvloc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_jsnloc.
  PERFORM f4_browse_local_save USING 'JSON' CHANGING p_jsnloc.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lv_infile   TYPE string,
        lv_csvout   TYPE string,
        lv_jsnout   TYPE string,
        lv_is_local TYPE abap_bool.

  " Determine file paths based on location
  IF p_server = abap_true.
    lv_infile   = p_infile.
    lv_csvout   = p_csvout.
    lv_jsnout   = p_jsnout.
    lv_is_local = abap_false.
  ELSE.
    lv_infile   = p_inloc.
    lv_csvout   = p_csvloc.
    lv_jsnout   = p_jsnloc.
    lv_is_local = abap_true.
  ENDIF.

  " Validate input
  IF lv_infile IS INITIAL.
    MESSAGE 'Input file path is required' TYPE 'E'.
    RETURN.
  ENDIF.

  " Validate at least one output is specified
  IF ( p_outcsv = abap_false OR lv_csvout IS INITIAL ) AND
     ( p_outjsn = abap_false OR lv_jsnout IS INITIAL ).
    MESSAGE 'At least one output file path is required' TYPE 'E'.
    RETURN.
  ENDIF.

  " Read input and process
  DATA: results      TYPE zcl_fiori_model_analyzer=>ty_res_tt,
        lv_input_csv TYPE string.

  IF lv_is_local = abap_true.
    " Read from local PC
    PERFORM read_local_file USING lv_infile CHANGING lv_input_csv.
    IF lv_input_csv IS INITIAL.
      MESSAGE 'Failed to read local input file' TYPE 'E'.
      RETURN.
    ENDIF.

    " Write to temp server file for processing
    DATA lv_temp_path TYPE string.
    CONCATENATE '/tmp/fiori_input_' sy-uname '_'
                sy-datum sy-uzeit '.csv'
      INTO lv_temp_path.
    PERFORM write_server_file USING lv_temp_path lv_input_csv.

    " Process
    results = zcl_fiori_model_analyzer=>run_collect(
                iv_infile = lv_temp_path
                iv_ids    = p_ids ).

    " Clean up temp file
    DELETE DATASET lv_temp_path.

  ELSE.
    " Process directly from server
    results = zcl_fiori_model_analyzer=>run_collect(
                iv_infile = lv_infile
                iv_ids    = p_ids ).
  ENDIF.

  " Check results
  IF results IS INITIAL.
    MESSAGE 'No results produced. Check input CSV or filter.' TYPE 'W'.
    RETURN.
  ENDIF.

  " Write output files
  DATA lv_count TYPE i.

  " CSV Output
  IF p_outcsv = abap_true AND lv_csvout IS NOT INITIAL.
    IF lv_is_local = abap_true.
      PERFORM write_csv_local USING results lv_csvout.
      WRITE: / 'CSV written to local:', lv_csvout.
    ELSE.
      zcl_fiori_model_analyzer=>write_output_csv(
        it_res  = results
        iv_path = lv_csvout ).
      WRITE: / 'CSV written to server:', lv_csvout.
    ENDIF.
    lv_count = lv_count + 1.
  ENDIF.

  " JSON Output
  IF p_outjsn = abap_true AND lv_jsnout IS NOT INITIAL.
    IF lv_is_local = abap_true.
      PERFORM write_json_local USING results lv_jsnout.
      WRITE: / 'JSON written to local:', lv_jsnout.
    ELSE.
      zcl_fiori_model_analyzer=>write_output_json(
        it_res  = results
        iv_path = lv_jsnout ).
      WRITE: / 'JSON written to server:', lv_jsnout.
    ENDIF.
    lv_count = lv_count + 1.
  ENDIF.

  " Summary
  SKIP 1.
  WRITE: / '======================================'.
  WRITE: / 'Processing completed successfully'.
  WRITE: / '  Total apps processed:', lines( results ).
  WRITE: / '  Output files generated:', lv_count.
  WRITE: / '======================================'.

*----------------------------------------------------------------------*
* Forms
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f4_browse_local_input
*&---------------------------------------------------------------------*
FORM f4_browse_local_input CHANGING cv_file TYPE string.
  DATA: lt_files  TYPE filetable,
        lv_rc     TYPE i,
        lv_action TYPE i.

  TRY.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING
          window_title      = 'Select Input CSV File'
          file_filter       = 'CSV (*.csv)|*.csv|All (*.*)|*.*'
          default_extension = 'csv'
        CHANGING
          file_table        = lt_files
          rc                = lv_rc
          user_action       = lv_action
        EXCEPTIONS
          OTHERS            = 1 ).

      IF sy-subrc = 0 AND
         lv_action = cl_gui_frontend_services=>action_ok.
        READ TABLE lt_files INDEX 1 INTO DATA(ls_file).
        IF sy-subrc = 0.
          cv_file = ls_file-filename.
        ENDIF.
      ENDIF.
    CATCH cx_root.
      MESSAGE 'Error opening file dialog' TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f4_browse_local_save
*&---------------------------------------------------------------------*
FORM f4_browse_local_save USING    iv_type TYPE string
                          CHANGING cv_file TYPE string.
  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string,
        lv_action   TYPE i,
        lv_filter   TYPE string,
        lv_title    TYPE string,
        lv_ext      TYPE string.

  IF iv_type = 'CSV'.
    lv_filter   = 'CSV Files (*.csv)|*.csv'.
    lv_title    = 'Save CSV Output As'.
    lv_filename = 'fiori_apps_output.csv'.
    lv_ext      = 'csv'.
  ELSE.
    lv_filter   = 'JSON Files (*.json)|*.json'.
    lv_title    = 'Save JSON Output As'.
    lv_filename = 'fiori_apps_output.json'.
    lv_ext      = 'json'.
  ENDIF.

  TRY.
      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          window_title         = lv_title
          default_extension    = lv_ext
          default_file_name    = lv_filename
          file_filter          = lv_filter
        CHANGING
          filename             = lv_filename
          path                 = lv_path
          fullpath             = lv_fullpath
          user_action          = lv_action
        EXCEPTIONS
          OTHERS               = 1 ).

      IF sy-subrc = 0 AND
         lv_action = cl_gui_frontend_services=>action_ok.
        cv_file = lv_fullpath.
      ENDIF.
    CATCH cx_root.
      MESSAGE 'Error opening save dialog' TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_local_file
*&---------------------------------------------------------------------*
FORM read_local_file USING    iv_filename TYPE string
                     CHANGING cv_content TYPE string.
  DATA: lt_data     TYPE TABLE OF string,
        lv_filename TYPE string,
        lv_line     TYPE string.

  lv_filename = iv_filename.

  TRY.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = lv_filename
          filetype                = 'ASC'
          has_field_separator     = abap_false
        CHANGING
          data_tab                = lt_data
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          OTHERS                  = 17 ).

      IF sy-subrc <> 0.
        MESSAGE 'Error reading local file' TYPE 'E'.
        RETURN.
      ENDIF.

      " Concatenate lines
      LOOP AT lt_data INTO lv_line.
        IF cv_content IS INITIAL.
          cv_content = lv_line.
        ELSE.
          CONCATENATE cv_content cl_abap_char_utilities=>newline
                      lv_line INTO cv_content.
        ENDIF.
      ENDLOOP.

    CATCH cx_root.
      MESSAGE 'Error reading local file' TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form write_server_file
*&---------------------------------------------------------------------*
FORM write_server_file USING iv_path    TYPE string
                             iv_content TYPE string.
  OPEN DATASET iv_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
  IF sy-subrc <> 0.
    MESSAGE 'Cannot create temp file on server' TYPE 'E'.
    RETURN.
  ENDIF.

  TRANSFER iv_content TO iv_path.
  CLOSE DATASET iv_path.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form write_csv_local
*&---------------------------------------------------------------------*
FORM write_csv_local USING it_res  TYPE zcl_fiori_model_analyzer=>ty_res_tt
                           iv_path TYPE string.
  DATA: lt_output TYPE TABLE OF string,
        lv_line   TYPE string.

  " Header
  CONCATENATE
    'FioriId;AppName;Link;MainServiceName;ServiceUri;'
    'ODataVersion;SEGWProject;ProgrammingModel;'
    'BusinessEntity;FPM_Extended'
    INTO lv_line.
  APPEND lv_line TO lt_output.

  " Data rows
  LOOP AT it_res ASSIGNING FIELD-SYMBOL(<r>).
    lv_line = |{ <r>-fiori_id };{ <r>-app_name };| &&
              |{ <r>-library_link };| &&
              |{ <r>-main_service_name };{ <r>-service_uri };| &&
              |{ <r>-odata_version };{ <r>-segw_project };| &&
              |{ <r>-programming_model };{ <r>-business_entity };| &&
              |{ <r>-fpm_extended }|.
    APPEND lv_line TO lt_output.
  ENDLOOP.

  " Download
  TRY.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                = iv_path
          filetype                = 'ASC'
          write_field_separator   = abap_false
        CHANGING
          data_tab                = lt_output
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          OTHERS                  = 22 ).

      IF sy-subrc <> 0.
        MESSAGE 'Error writing CSV to local file' TYPE 'E'.
      ENDIF.
    CATCH cx_root.
      MESSAGE 'Error writing CSV to local file' TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form write_json_local
*&---------------------------------------------------------------------*
FORM write_json_local USING it_res  TYPE zcl_fiori_model_analyzer=>ty_res_tt
                            iv_path TYPE string.
  DATA: lv_json   TYPE string,
        lt_output TYPE TABLE OF string.

  " Serialize to JSON
  TRY.
      lv_json = /ui2/cl_json=>serialize(
        data        = it_res
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.

  " Append to output table
  APPEND lv_json TO lt_output.

  " Download
  TRY.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                = iv_path
          filetype                = 'ASC'
          write_field_separator   = abap_false
        CHANGING
          data_tab                = lt_output
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          OTHERS                  = 22 ).

      IF sy-subrc <> 0.
        MESSAGE 'Error writing JSON to local file' TYPE 'E'.
      ENDIF.
    CATCH cx_root.
      MESSAGE 'Error writing JSON to local file' TYPE 'E'.
  ENDTRY.
ENDFORM.
