CLASS zcl_fiori_model_manifest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF main_datasource_info,
        datasource_id TYPE string,
        uri           TYPE string,
        odata_version TYPE string,
        is_v4         TYPE abap_bool,
      END OF main_datasource_info.
    TYPES:
      entityset_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS constructor
      IMPORTING
        manifest_json TYPE string.

    METHODS get_main_datasource
      RETURNING
        VALUE(result) TYPE main_datasource_info.

    METHODS get_primary_entityset
      RETURNING
        VALUE(entityset) TYPE string.

    METHODS get_candidate_entitysets
      RETURNING
        VALUE(entitysets) TYPE entityset_table.

  PRIVATE SECTION.
    DATA manifest_json TYPE string.

    METHODS find_between
      IMPORTING
        start         TYPE string
        end           TYPE string
        source        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS find_value_after_key
      IMPORTING
        key           TYPE string
        source        TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_fiori_model_manifest IMPLEMENTATION.


  METHOD constructor.
    manifest_json = manifest_json.
  ENDMETHOD.


  METHOD find_between.
    DATA: start_pos TYPE i,
          end_pos   TYPE i,
          length    TYPE i.

    " Find start delimiter position
    FIND FIRST OCCURRENCE OF start IN source MATCH OFFSET start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move position after start delimiter
    start_pos = start_pos + strlen( start ).

    " Find end delimiter position
    FIND FIRST OCCURRENCE OF end IN SECTION OFFSET start_pos OF source
         MATCH OFFSET end_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Extract substring between delimiters using substring method
    length = end_pos - start_pos.
    result = substring( val = source
                        off = start_pos
                        len = length ).
  ENDMETHOD.


  METHOD find_value_after_key.
    DATA: key_pattern  TYPE string,
          key_pos      TYPE i,
          value_start  TYPE i,
          value_end    TYPE i,
          quote_pos    TYPE i.

    " Build search pattern: "key"
    key_pattern = '"' && key && '"'.

    " Find the key in source
    FIND FIRST OCCURRENCE OF key_pattern IN source
         MATCH OFFSET key_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move past the key
    value_start = key_pos + strlen( key_pattern ).

    " Find the colon after the key
    FIND FIRST OCCURRENCE OF ':' IN SECTION OFFSET value_start OF source
         MATCH OFFSET value_start.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move past the colon
    value_start = value_start + 1.

    " Find the opening quote of the value
    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET value_start OF source
         MATCH OFFSET quote_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Move past the opening quote
    value_start = quote_pos + 1.

    " Find the closing quote of the value
    FIND FIRST OCCURRENCE OF '"' IN SECTION OFFSET value_start OF source
         MATCH OFFSET value_end.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Extract the value between quotes
    DATA length TYPE i.
    length = value_end - value_start.
    IF length > 0.
      result = substring( val = source
                          off = value_start
                          len = length ).
    ENDIF.
  ENDMETHOD.


  METHOD get_candidate_entitysets.
    DATA: offset   TYPE i VALUE 0,
          match    TYPE string,
          rest     TYPE string,
          json_len TYPE i.

    json_len = strlen( manifest_json ).

    " Find all entitySet occurrences in manifest
    DO.
      FIND FIRST OCCURRENCE OF '"entitySet"'
           IN SECTION OFFSET offset OF manifest_json
           MATCH OFFSET offset.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " Extract remaining string from current offset
      rest = substring( val = manifest_json
                        off = offset
                        len = json_len - offset ).

      " Extract value after each entitySet key
      match = find_value_after_key(
        key    = 'entitySet'
        source = rest ).

      IF match IS NOT INITIAL.
        APPEND match TO entitysets.
      ENDIF.

      offset = offset + 1.
    ENDDO.

    " Remove duplicates
    DELETE ADJACENT DUPLICATES FROM entitysets.
  ENDMETHOD.


  METHOD get_main_datasource.
    DATA: datasource_id  TYPE string,
          models_section TYPE string,
          ds_section     TYPE string.

    " Step 1: Extract complete "models" section using brace counting
    DATA: start_pos   TYPE i,
          end_pos     TYPE i,
          brace_count TYPE i,
          char        TYPE c LENGTH 1,
          in_string   TYPE abap_bool,
          json_len    TYPE i.

    json_len = strlen( manifest_json ).

    " Find "models" key
    FIND FIRST OCCURRENCE OF '"models"' IN manifest_json
         MATCH OFFSET start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Find opening brace of models object
    FIND FIRST OCCURRENCE OF '{' IN SECTION OFFSET start_pos OF manifest_json
         MATCH OFFSET start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Count braces to extract complete models object
    brace_count = 1.
    end_pos = start_pos + 1.
    in_string = abap_false.

    WHILE end_pos < json_len AND brace_count > 0.
      char = substring( val = manifest_json
                        off = end_pos
                        len = 1 ).

      IF char = '"' AND end_pos > 0.
        DATA prev TYPE c LENGTH 1.
        prev = substring( val = manifest_json
                          off = end_pos - 1
                          len = 1 ).
        IF prev <> '\'.
          IF in_string = abap_true.
            in_string = abap_false.
          ELSE.
            in_string = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

      IF in_string = abap_false.
        IF char = '{'.
          brace_count = brace_count + 1.
        ELSEIF char = '}'.
          brace_count = brace_count - 1.
        ENDIF.
      ENDIF.

      end_pos = end_pos + 1.
    ENDWHILE.

    DATA length TYPE i.
    length = end_pos - start_pos.
    models_section = substring( val = manifest_json
                                off = start_pos
                                len = length ).

    IF models_section IS INITIAL.
      RETURN.
    ENDIF.

    " Step 2: Find "dataSource" value in default model "" section
    datasource_id = find_value_after_key(
      key    = 'dataSource'
      source = models_section ).

    IF datasource_id IS INITIAL.
      RETURN.
    ENDIF.

    " Step 3: Extract datasource section by ID
    DATA search TYPE string.
    search = '"' && datasource_id && '"'.

    " Find datasource ID in manifest
    FIND FIRST OCCURRENCE OF search IN manifest_json
         MATCH OFFSET start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Find opening brace of datasource object
    FIND FIRST OCCURRENCE OF '{' IN SECTION OFFSET start_pos OF manifest_json
         MATCH OFFSET start_pos.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Count braces to find matching closing brace
    brace_count = 1.
    end_pos = start_pos + 1.
    in_string = abap_false.

    WHILE end_pos < json_len AND brace_count > 0.
      char = substring( val = manifest_json
                        off = end_pos
                        len = 1 ).

      IF char = '"' AND end_pos > 0.
        prev = substring( val = manifest_json
                          off = end_pos - 1
                          len = 1 ).
        IF prev <> '\'.
          IF in_string = abap_true.
            in_string = abap_false.
          ELSE.
            in_string = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

      IF in_string = abap_false.
        IF char = '{'.
          brace_count = brace_count + 1.
        ELSEIF char = '}'.
          brace_count = brace_count - 1.
        ENDIF.
      ENDIF.

      end_pos = end_pos + 1.
    ENDWHILE.

    " Extract datasource object using substring
    length = end_pos - start_pos.
    ds_section = substring( val = manifest_json
                            off = start_pos
                            len = length ).

    " Step 4: Extract URI and OData version from datasource object
    result-datasource_id = datasource_id.
    result-uri = find_value_after_key(
      key    = 'uri'
      source = ds_section ).
    result-odata_version = find_value_after_key(
      key    = 'odataVersion'
      source = ds_section ).
    result-is_v4 = xsdbool( result-odata_version = '4.0' ).
  ENDMETHOD.


  METHOD get_primary_entityset.
    DATA: section TYPE string,
          pos     TYPE i.

    " Try 1: FE V4 / Freestyle - routing -> targets
    FIND FIRST OCCURRENCE OF '"targets"' IN manifest_json
         MATCH OFFSET pos.
    IF sy-subrc = 0.
      section = substring( val = manifest_json
                           off = pos ).
      entityset = find_value_after_key(
        key    = 'entitySet'
        source = section ).

      IF entityset IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    " Try 2: FE V2 - look for "pages" section directly
    FIND FIRST OCCURRENCE OF '"pages"' IN manifest_json
         MATCH OFFSET pos.
    IF sy-subrc = 0.
      section = substring( val = manifest_json
                           off = pos ).
      entityset = find_value_after_key(
        key    = 'entitySet'
        source = section ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.