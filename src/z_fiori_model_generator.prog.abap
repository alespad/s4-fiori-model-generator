*&---------------------------------------------------------------------*
*& Report Z_FIORI_MODEL_GENERATOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fiori_model_generator.

" ------- Block 1: Input/Output Location -------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS:
    p_server RADIOBUTTON GROUP loc DEFAULT 'X' USER-COMMAND loc,
    p_local  RADIOBUTTON GROUP loc.
SELECTION-SCREEN END OF BLOCK b1.

" ------- Block 2: Server File Paths -------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS p_infile TYPE string MODIF ID srv LOWER CASE.
SELECTION-SCREEN SKIP.
PARAMETERS p_csvout TYPE string MODIF ID srv LOWER CASE.
SELECTION-SCREEN SKIP.
PARAMETERS p_jsnout TYPE string MODIF ID srv LOWER CASE.
SELECTION-SCREEN END OF BLOCK b2.

" ------- Block 3: Local File Paths -------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.
PARAMETERS p_inloc  TYPE string MODIF ID loc LOWER CASE.
SELECTION-SCREEN SKIP.
PARAMETERS p_csvloc TYPE string MODIF ID loc LOWER CASE.
SELECTION-SCREEN SKIP.
PARAMETERS p_jsnloc TYPE string MODIF ID loc LOWER CASE.
SELECTION-SCREEN END OF BLOCK b3.

" ------- Block 4: Filters -------
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-t04.
PARAMETERS p_ids TYPE string LOWER CASE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(70) TEXT-t31.
SELECTION-SCREEN COMMENT /1(70) TEXT-t32.
SELECTION-SCREEN END OF BLOCK b4.

" ------- Block 5: Output Format -------
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-t05.
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
    CASE screen-group1.
      WHEN 'SRV'.
        IF p_server = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.

      WHEN 'LOC'.
        IF p_local = abap_true.
          screen-active = 1.
          screen-invisible = 0.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
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

  DATA: infile   TYPE string,
        csvout   TYPE string,
        jsnout   TYPE string,
        is_local TYPE abap_bool.

  " Determine file paths based on location
  IF p_server = abap_true.
    infile   = p_infile.
    csvout   = p_csvout.
    jsnout   = p_jsnout.
    is_local = abap_false.
  ELSE.
    infile   = p_inloc.
    csvout   = p_csvloc.
    jsnout   = p_jsnloc.
    is_local = abap_true.
  ENDIF.

  " Validate input
  IF infile IS INITIAL.
    MESSAGE 'Input file path is required' TYPE 'E'.
    RETURN.
  ENDIF.

  " Validate at least one output is specified
  IF ( p_outcsv = abap_false OR csvout IS INITIAL ) AND
      ( p_outjsn = abap_false OR jsnout IS INITIAL ).
    MESSAGE 'At least one output file path is required' TYPE 'E'.
    RETURN.
  ENDIF.

" Read input and process
  DATA: results   TYPE zcl_fiori_model_analyzer=>result_table,
        input_csv TYPE string.

  IF is_local = abap_true.
    " Read from local PC
    PERFORM read_local_file USING infile CHANGING input_csv.
    IF input_csv IS INITIAL.
      MESSAGE 'Failed to read local input file' TYPE 'E'.
      RETURN.
    ENDIF.

    " Process directly from memory
    results = zcl_fiori_model_analyzer=>run_collect_from_string(
                csv_content = input_csv
                ids         = p_ids ).

  ELSE.
    " Process directly from server
    results = zcl_fiori_model_analyzer=>run_collect(
                infile = infile
                ids    = p_ids ).
  ENDIF.

  " Check results
  IF results IS INITIAL.
    MESSAGE 'No results produced. Check input CSV or filter.' TYPE 'W'.
    RETURN.
  ENDIF.

  " Write output files
  DATA count TYPE i.

  " CSV Output
  IF p_outcsv = abap_true AND csvout IS NOT INITIAL.
    IF is_local = abap_true.
      PERFORM write_csv_local USING results csvout.
      WRITE: / 'CSV written to local:',csvout.
    ELSE.
      zcl_fiori_model_analyzer=>write_output_csv(
        res  = results
        path = csvout ).
      WRITE: / 'CSV written to server:', csvout.
    ENDIF.
    count = count + 1.
  ENDIF.

  " JSON Output
  IF p_outjsn = abap_true AND jsnout IS NOT INITIAL.
    IF is_local = abap_true.
      PERFORM write_json_local USING results jsnout.
      WRITE: / 'JSON written to local:', jsnout.
    ELSE.
      zcl_fiori_model_analyzer=>write_output_json(
        res  = results
        path = jsnout ).
      WRITE: / 'JSON written to server:', jsnout.
    ENDIF.
    count = count + 1.
  ENDIF.

  " Summary
  SKIP 1.
  WRITE / '======================================'.
  WRITE / 'Processing completed successfully'.
  WRITE: / '  Total apps processed:', lines( results ).
  WRITE: / '  Output files generated:', count.
  WRITE / '======================================'.

*----------------------------------------------------------------------*
* Forms
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f4_browse_local_input
*&---------------------------------------------------------------------*
FORM f4_browse_local_input CHANGING cv_file TYPE string.
  DATA: files  TYPE filetable,
        rc     TYPE i,
        action TYPE i.

  TRY.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING
          window_title      = 'Select Input CSV File'
          file_filter       = 'CSV (*.csv)|*.csv|All (*.*)|*.*'
          default_extension = 'csv'
        CHANGING
          file_table        = files
          rc                = rc
          user_action       = action
        EXCEPTIONS
          OTHERS            = 1 ).

      IF sy-subrc = 0 AND
          action = cl_gui_frontend_services=>action_ok.
        READ TABLE files INDEX 1 INTO DATA(file).
        IF sy-subrc = 0.
          cv_file = file-filename.
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
  DATA: filename TYPE string,
        path     TYPE string,
        fullpath TYPE string,
        action   TYPE i,
        filter   TYPE string,
        title    TYPE string,
        ext      TYPE string.

  IF iv_type = 'CSV'.
    filter   = 'CSV Files (*.csv)|*.csv'.
    title    = 'Save CSV Output As'.
    filename = 'fiori_apps_output.csv'.
    ext      = 'csv'.
  ELSE.
    filter   = 'JSON Files (*.json)|*.json'.
    title    = 'Save JSON Output As'.
    filename = 'fiori_apps_output.json'.
    ext      = 'json'.
  ENDIF.

  TRY.
      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          window_title         = title
          default_extension    = ext
          default_file_name    = filename
          file_filter          = filter
        CHANGING
          filename             = filename
          path                 = path
          fullpath             = fullpath
          user_action          = action
        EXCEPTIONS
          OTHERS               = 1 ).

      IF sy-subrc = 0 AND
          action = cl_gui_frontend_services=>action_ok.
        cv_file = fullpath.
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
  DATA: file_data     TYPE TABLE OF string,
        filename TYPE string.

  filename = iv_filename.

  TRY.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = filename
          filetype                = 'ASC'
          has_field_separator     = abap_false
        CHANGING
          data_tab                = file_data
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
      LOOP AT file_data INTO DATA(line).
        IF cv_content IS INITIAL.
          cv_content = line.
        ELSE.
          CONCATENATE cv_content cl_abap_char_utilities=>newline
                      line INTO cv_content.
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
FORM write_csv_local USING it_res  TYPE zcl_fiori_model_analyzer=>result_table
                           iv_path TYPE string.
  DATA: output TYPE TABLE OF string,
        line   TYPE string.

  " Header
  CONCATENATE
    'FioriId;AppName;Link;MainServiceName;ServiceUri;'
    'ODataVersion;SEGWProject;ProgrammingModel;'
    'BusinessEntity;FPM_Extended'
    INTO line.
  APPEND line TO output.

  " Data rows
  LOOP AT it_res ASSIGNING FIELD-SYMBOL(<r>).
    line = |{ <r>-fiori_id };{ <r>-app_name };| &&
              |{ <r>-library_link };| &&
              |{ <r>-main_service_name };{ <r>-service_uri };| &&
              |{ <r>-odata_version };{ <r>-segw_project };| &&
              |{ <r>-programming_model };{ <r>-business_entity };| &&
              |{ <r>-fpm_extended }|.
    APPEND line TO output.
  ENDLOOP.

  " Download
  TRY.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                = iv_path
          filetype                = 'ASC'
          write_field_separator   = abap_false
        CHANGING
          data_tab                = output
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
FORM write_json_local USING it_res  TYPE zcl_fiori_model_analyzer=>result_table
                            iv_path TYPE string.
  DATA: json   TYPE string,
        output TYPE TABLE OF string.

  " Serialize to JSON
  TRY.
      json = /ui2/cl_json=>serialize(
        data        = it_res
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    CATCH cx_root INTO DATA(error).
      MESSAGE error->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.

  " Append to output table
  APPEND json TO output.

  " Download
  TRY.
      cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename                = iv_path
          filetype                = 'ASC'
          write_field_separator   = abap_false
          codepage              = '4110'  " UTF-8 encoding
        CHANGING
          data_tab                = output
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
