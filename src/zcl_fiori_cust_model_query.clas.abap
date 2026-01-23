CLASS zcl_fiori_cust_model_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_result,
        bspname          TYPE c LENGTH 30,
        devclass         TYPE c LENGTH 30,
        author           TYPE c LENGTH 12,
        programmingmodel TYPE c LENGTH 10,
        odataversion     TYPE c LENGTH 5,
        businessentity   TYPE c LENGTH 80,
        mainservicename  TYPE c LENGTH 80,
        serviceuri       TYPE c LENGTH 255,
        segwproject      TYPE c LENGTH 40,
        fpmextended      TYPE c LENGTH 5,
        appname          TYPE c LENGTH 255,
      END OF ty_result.

    TYPES ty_result_table TYPE STANDARD TABLE OF ty_result WITH EMPTY KEY.

    CLASS-METHODS get_bsp_list
      IMPORTING
        filter_bsp_name TYPE string OPTIONAL
        filter_author   TYPE string OPTIONAL
        filter_devclass TYPE string OPTIONAL
      RETURNING
        VALUE(result)   TYPE ty_result_table.

    CLASS-METHODS analyze_bsp
      IMPORTING
        bsp_name      TYPE clike
        devclass      TYPE clike
        author        TYPE clike
      RETURNING
        VALUE(result) TYPE ty_result.

ENDCLASS.



CLASS ZCL_FIORI_CUST_MODEL_QUERY IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA: lt_result      TYPE ty_result_table,
          filter_bsp     TYPE string,
          filter_author  TYPE string,
          filter_package TYPE string.

    " Get filter conditions
    TRY.
        DATA(lt_filter_cond) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        CLEAR lt_filter_cond.
    ENDTRY.

    " Extract filter values
    LOOP AT lt_filter_cond ASSIGNING FIELD-SYMBOL(<filter>).
      CASE <filter>-name.
        WHEN 'BSPNAME'.
          IF lines( <filter>-range ) > 0.
            filter_bsp = <filter>-range[ 1 ]-low.
          ENDIF.
        WHEN 'AUTHOR'.
          IF lines( <filter>-range ) > 0.
            filter_author = <filter>-range[ 1 ]-low.
          ENDIF.
        WHEN 'DEVCLASS'.
          IF lines( <filter>-range ) > 0.
            filter_package = <filter>-range[ 1 ]-low.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    " Get BSP list and analyze
    lt_result = get_bsp_list(
      filter_bsp_name = filter_bsp
      filter_author   = filter_author
      filter_devclass = filter_package ).

    " Handle paging
    DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
    DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).

    IF lv_page_size > 0.
      DATA(lv_max_index) = lv_offset + lv_page_size.
      IF lv_max_index > lines( lt_result ).
        lv_max_index = lines( lt_result ).
      ENDIF.
    ELSE.
      lv_max_index = lines( lt_result ).
    ENDIF.

    " Apply paging
    IF lv_offset > 0 OR lv_page_size > 0.
      DATA lt_paged TYPE ty_result_table.
      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<res>) FROM ( lv_offset + 1 ) TO lv_max_index.
        APPEND <res> TO lt_paged.
      ENDLOOP.
      lt_result = lt_paged.
    ENDIF.

    " Set total count if requested
    IF io_request->is_total_numb_of_rec_requested( ).
      io_response->set_total_number_of_records( lines( lt_result ) ).
    ENDIF.

    " Return data
    io_response->set_data( lt_result ).

  ENDMETHOD.


  METHOD get_bsp_list.

    DATA: lt_tadir TYPE STANDARD TABLE OF tadir,
          ls_res   TYPE ty_result.

    " Build dynamic WHERE clause
    DATA: lv_where TYPE string.

    lv_where = |PGMID = 'R3TR' AND OBJECT = 'WAPA'|.

    " If no filter specified, default to custom BSPs only (Z* and Y*)
    IF filter_bsp_name IS INITIAL AND filter_author IS INITIAL AND filter_devclass IS INITIAL.
      lv_where = lv_where && | AND ( OBJ_NAME LIKE 'Z%' OR OBJ_NAME LIKE 'Y%' )|.
    ENDIF.

    IF filter_bsp_name IS NOT INITIAL.
      DATA(lv_bsp_upper) = to_upper( filter_bsp_name ).
      IF lv_bsp_upper CS '*'.
        REPLACE ALL OCCURRENCES OF '*' IN lv_bsp_upper WITH '%'.
        lv_where = lv_where && | AND OBJ_NAME LIKE '{ lv_bsp_upper }'|.
      ELSE.
        lv_where = lv_where && | AND OBJ_NAME = '{ lv_bsp_upper }'|.
      ENDIF.
    ENDIF.

    IF filter_author IS NOT INITIAL.
      DATA(lv_author_upper) = to_upper( filter_author ).
      IF lv_author_upper CS '*'.
        REPLACE ALL OCCURRENCES OF '*' IN lv_author_upper WITH '%'.
        lv_where = lv_where && | AND AUTHOR LIKE '{ lv_author_upper }'|.
      ELSE.
        lv_where = lv_where && | AND AUTHOR = '{ lv_author_upper }'|.
      ENDIF.
    ENDIF.

    IF filter_devclass IS NOT INITIAL.
      DATA(lv_devclass_upper) = to_upper( filter_devclass ).
      IF lv_devclass_upper CS '*'.
        REPLACE ALL OCCURRENCES OF '*' IN lv_devclass_upper WITH '%'.
        lv_where = lv_where && | AND DEVCLASS LIKE '{ lv_devclass_upper }'|.
      ELSE.
        lv_where = lv_where && | AND DEVCLASS = '{ lv_devclass_upper }'|.
      ENDIF.
    ENDIF.

    " Select BSP applications from TADIR
    SELECT obj_name, devclass, author
      FROM tadir
      WHERE (lv_where)
      INTO TABLE @DATA(lt_bsp).

    " Analyze each BSP
    LOOP AT lt_bsp ASSIGNING FIELD-SYMBOL(<bsp>).
      ls_res = analyze_bsp(
        bsp_name = <bsp>-obj_name
        devclass = <bsp>-devclass
        author   = <bsp>-author ).

      " Only add if manifest was found (valid Fiori app)
      IF ls_res-bspname IS NOT INITIAL.
        TRANSLATE ls_res TO UPPER CASE.
        APPEND ls_res TO result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD analyze_bsp.

    DATA: ls_app          TYPE zcl_fiori_model_analyzer=>app,
          ls_analyzer_res TYPE zcl_fiori_model_analyzer=>result.

    " Clear result
    CLEAR result.

    " Set up input for analyzer
    ls_app-bsp_name = bsp_name.
    ls_app-fiori_id = bsp_name.  " Use BSP name as ID for custom apps
    ls_app-app_name = bsp_name.
    CLEAR ls_app-library_link.   " No library link for custom apps

    " Call existing analyzer
    ls_analyzer_res = zcl_fiori_model_analyzer=>analyze_app( ls_app ).

    " Check if analysis was successful (manifest found)
    IF ls_analyzer_res IS INITIAL.
      RETURN.
    ENDIF.

    " Map result
    result-bspname          = bsp_name.
    result-devclass         = devclass.
    result-author           = author.
    result-programmingmodel = ls_analyzer_res-programming_model.
    result-odataversion     = ls_analyzer_res-odata_version.
    result-businessentity   = ls_analyzer_res-business_entity.
    result-mainservicename  = ls_analyzer_res-main_service_name.
    result-serviceuri       = ls_analyzer_res-service_uri.
    result-segwproject      = ls_analyzer_res-segw_project.
    result-fpmextended      = ls_analyzer_res-fpm_extended.
    result-appname          = ls_analyzer_res-app_name.

  ENDMETHOD.
ENDCLASS.
