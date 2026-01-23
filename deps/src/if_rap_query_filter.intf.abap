INTERFACE if_rap_query_filter PUBLIC.
  TYPES:
    BEGIN OF filter_range,
      sign   TYPE c LENGTH 1,
      option TYPE c LENGTH 2,
      low    TYPE string,
      high   TYPE string,
    END OF filter_range,
    filter_ranges TYPE STANDARD TABLE OF filter_range WITH EMPTY KEY,
    BEGIN OF filter_condition,
      name  TYPE string,
      range TYPE filter_ranges,
    END OF filter_condition,
    filter_conditions TYPE STANDARD TABLE OF filter_condition WITH EMPTY KEY.

  METHODS get_as_ranges
    RETURNING
      VALUE(result) TYPE filter_conditions
    RAISING
      cx_rap_query_filter_no_range.
ENDINTERFACE.
