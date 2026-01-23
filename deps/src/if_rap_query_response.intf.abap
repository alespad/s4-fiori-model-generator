INTERFACE if_rap_query_response PUBLIC.
  METHODS set_total_number_of_records
    IMPORTING
      total_number TYPE int8.
  METHODS set_data
    IMPORTING
      data TYPE any.
ENDINTERFACE.
