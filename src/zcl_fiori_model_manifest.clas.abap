CLASS zcl_fiori_model_manifest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_main_ds_info,
        datasource_id TYPE string,
        uri           TYPE string,
        odata_version TYPE string,
        is_v4         TYPE abap_bool,
      END OF ty_main_ds_info .
    TYPES:
      ty_entityset_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY .

    METHODS constructor
      IMPORTING
        !iv_manifest_json TYPE string .
    METHODS get_main_datasource
      RETURNING
        VALUE(rs_info) TYPE ty_main_ds_info .
    METHODS get_primary_entityset
      RETURNING
        VALUE(rv_entityset) TYPE string .
    METHODS get_candidate_entitysets
      RETURNING
        VALUE(rt_entitysets) TYPE ty_entityset_tab .
  PRIVATE SECTION.
    DATA mv_manifest_json TYPE string.

    METHODS find_between
      IMPORTING iv_start         TYPE string
                iv_end           TYPE string
                iv_source        TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    METHODS find_value_after_key
      IMPORTING iv_key           TYPE string
                iv_source        TYPE string
      RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.



CLASS ZCL_FIORI_MODEL_MANIFEST IMPLEMENTATION.


  METHOD constructor.
    mv_manifest_json = iv_manifest_json.
  ENDMETHOD.


  METHOD find_between.
    DATA: lv_start_pos TYPE i,
          lv_end_pos   TYPE i,
          lv_length    TYPE i.

    " Find start delimiter position
    FIND FIRST OCCURRENCE OF iv_start IN iv_source MATCH OFFSET lv_start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move position after start delimiter
    lv_start_pos = lv_start_pos + strlen( iv_start ).

    " Find end delimiter position
    FIND FIRST OCCURRENCE OF iv_end IN SECTION OFFSET lv_start_pos OF iv_source
         MATCH OFFSET lv_end_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Extract substring between delimiters using substring method
    lv_length = lv_end_pos - lv_start_pos.
    rv_result = substring( val = iv_source off = lv_start_pos len = lv_length ).
  ENDMETHOD.


  METHOD find_value_after_key.
    DATA: lv_key_pattern TYPE string,
          lv_key_pos     TYPE i,
          lv_value_start TYPE i,
          lv_value_end   TYPE i,
          lv_quote_pos   TYPE i.

    " Build search pattern: "key"
    lv_key_pattern = '"' && iv_key && '"'.

    " Find the key in source
    FIND FIRST OCCURRENCE OF lv_key_pattern IN iv_source
         MATCH OFFSET lv_key_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move past the key
    lv_value_start = lv_key_pos + strlen( lv_key_pattern ).

    " Find the colon after the key
    FIND FIRST OCCURRENCE OF ':' IN SECTION OFFSET lv_value_start OF iv_source
         MATCH OFFSET lv_value_start.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move past the colon
    lv_value_start = lv_value_start + 1.

    " Find the opening quote of the value
    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET lv_value_start OF iv_source
         MATCH OFFSET lv_quote_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move past the opening quote
    lv_value_start = lv_quote_pos + 1.

    " Find the closing quote of the value
    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET lv_value_start OF iv_source
         MATCH OFFSET lv_value_end.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Extract the value between quotes
    DATA lv_length TYPE i.
    lv_length = lv_value_end - lv_value_start.
    IF lv_length > 0.
      rv_result = substring( val = iv_source off = lv_value_start len = lv_length ).
    ENDIF.
  ENDMETHOD.


  METHOD get_candidate_entitysets.
    DATA: lv_offset   TYPE i VALUE 0,
          lv_match    TYPE string,
          lv_rest     TYPE string,
          lv_json_len TYPE i.

    lv_json_len = strlen( mv_manifest_json ).

    " Find all entitySet occurrences in manifest
    DO.
      FIND FIRST OCCURRENCE OF '"entitySet"' IN SECTION OFFSET lv_offset OF mv_manifest_json
           MATCH OFFSET lv_offset.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " Extract remaining string from current offset
      lv_rest = substring( val = mv_manifest_json off = lv_offset len = lv_json_len - lv_offset ).

      " Extract value after each entitySet key
      lv_match = find_value_after_key(
        iv_key    = 'entitySet'
        iv_source = lv_rest ).

      IF lv_match IS NOT INITIAL.
        APPEND lv_match TO rt_entitysets.
      ENDIF.

      lv_offset = lv_offset + 1.
    ENDDO.

    " Remove duplicates
    DELETE ADJACENT DUPLICATES FROM rt_entitysets.
  ENDMETHOD.


  METHOD get_main_datasource.
    DATA: lv_datasource_id  TYPE string,
          lv_models_section TYPE string,
          lv_ds_section     TYPE string.

    " Step 1: Extract complete "models" section using brace counting
    DATA: lv_start_pos   TYPE i,
          lv_end_pos     TYPE i,
          lv_brace_count TYPE i,
          lv_char        TYPE c LENGTH 1,
          lv_in_string   TYPE abap_bool,
          lv_json_len    TYPE i.

    lv_json_len = strlen( mv_manifest_json ).

    " Find "models" key
    FIND FIRST OCCURRENCE OF '"models"' IN mv_manifest_json MATCH OFFSET lv_start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Find opening brace of models object
    FIND FIRST OCCURRENCE OF '{' IN SECTION OFFSET lv_start_pos OF mv_manifest_json
         MATCH OFFSET lv_start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Count braces to extract complete models object
    lv_brace_count = 1.
    lv_end_pos = lv_start_pos + 1.
    lv_in_string = abap_false.

    WHILE lv_end_pos < lv_json_len AND lv_brace_count > 0.
      lv_char = substring( val = mv_manifest_json off = lv_end_pos len = 1 ).

      IF lv_char = '"' AND lv_end_pos > 0.
        DATA lv_prev TYPE c LENGTH 1.
        lv_prev = substring( val = mv_manifest_json off = lv_end_pos - 1 len = 1 ).
        IF lv_prev <> '\'.
          IF lv_in_string = abap_true.
            lv_in_string = abap_false.
          ELSE.
            lv_in_string = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_in_string = abap_false.
        IF lv_char = '{'.
          lv_brace_count = lv_brace_count + 1.
        ELSEIF lv_char = '}'.
          lv_brace_count = lv_brace_count - 1.
        ENDIF.
      ENDIF.

      lv_end_pos = lv_end_pos + 1.
    ENDWHILE.

    DATA lv_length TYPE i.
    lv_length = lv_end_pos - lv_start_pos.
    lv_models_section = substring( val = mv_manifest_json off = lv_start_pos len = lv_length ).

    IF lv_models_section IS INITIAL.
      RETURN.
    ENDIF.

    " Step 2: Find "dataSource" value in default model "" section
    lv_datasource_id = find_value_after_key(
      iv_key    = 'dataSource'
      iv_source = lv_models_section ).

    IF lv_datasource_id IS INITIAL.
      RETURN.
    ENDIF.

    " Step 3: Extract datasource section by ID (reuse existing logic)
    DATA lv_search TYPE string.
    lv_search = '"' && lv_datasource_id && '"'.

    " Find datasource ID in manifest
    FIND FIRST OCCURRENCE OF lv_search IN mv_manifest_json MATCH OFFSET lv_start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Find opening brace of datasource object
    FIND FIRST OCCURRENCE OF '{' IN SECTION OFFSET lv_start_pos OF mv_manifest_json
         MATCH OFFSET lv_start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Count braces to find matching closing brace
    lv_brace_count = 1.
    lv_end_pos = lv_start_pos + 1.
    lv_in_string = abap_false.

    WHILE lv_end_pos < lv_json_len AND lv_brace_count > 0.
      lv_char = substring( val = mv_manifest_json off = lv_end_pos len = 1 ).

      IF lv_char = '"' AND lv_end_pos > 0.
        lv_prev = substring( val = mv_manifest_json off = lv_end_pos - 1 len = 1 ).
        IF lv_prev <> '\'.
          IF lv_in_string = abap_true.
            lv_in_string = abap_false.
          ELSE.
            lv_in_string = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_in_string = abap_false.
        IF lv_char = '{'.
          lv_brace_count = lv_brace_count + 1.
        ELSEIF lv_char = '}'.
          lv_brace_count = lv_brace_count - 1.
        ENDIF.
      ENDIF.

      lv_end_pos = lv_end_pos + 1.
    ENDWHILE.

    " Extract datasource object using substring
    lv_length = lv_end_pos - lv_start_pos.
    lv_ds_section = substring( val = mv_manifest_json off = lv_start_pos len = lv_length ).

    " Step 4: Extract URI and OData version from datasource object
    rs_info-datasource_id = lv_datasource_id.
    rs_info-uri = find_value_after_key(
      iv_key    = 'uri'
      iv_source = lv_ds_section ).
    rs_info-odata_version = find_value_after_key(
      iv_key    = 'odataVersion'
      iv_source = lv_ds_section ).
    rs_info-is_v4 = xsdbool( rs_info-odata_version = '4.0' ).
  ENDMETHOD.


  METHOD get_primary_entityset.
    DATA: lv_section TYPE string,
          lv_pos     TYPE i.

    " Try 1: FE V4 / Freestyle - routing -> targets
    FIND FIRST OCCURRENCE OF '"targets"' IN mv_manifest_json
         MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_section = substring( val = mv_manifest_json off = lv_pos ).
      rv_entityset = find_value_after_key(
        iv_key    = 'entitySet'
        iv_source = lv_section ).

      IF rv_entityset IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    " Try 2: FE V2 - look for "pages" section directly
    FIND FIRST OCCURRENCE OF '"pages"' IN mv_manifest_json
         MATCH OFFSET lv_pos.
    IF sy-subrc = 0.
      lv_section = substring( val = mv_manifest_json off = lv_pos ).
      rv_entityset = find_value_after_key(
        iv_key    = 'entitySet'
        iv_source = lv_section ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
