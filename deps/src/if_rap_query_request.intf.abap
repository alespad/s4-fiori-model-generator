INTERFACE if_rap_query_request PUBLIC.
  METHODS get_filter
    RETURNING
      VALUE(result) TYPE REF TO if_rap_query_filter.
  METHODS get_paging
    RETURNING
      VALUE(result) TYPE REF TO if_rap_query_paging.
  METHODS is_total_numb_of_rec_requested
    RETURNING
      VALUE(result) TYPE abap_bool.
ENDINTERFACE.
