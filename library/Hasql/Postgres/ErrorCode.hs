-- |
-- Error codes.
-- See <http://www.postgresql.org/docs/current/interactive/errcodes-appendix.html>
module Hasql.Postgres.ErrorCode where

import qualified Data.ByteString

type ErrorCode = 
  Data.ByteString.ByteString

-- * Class 00 — Successful Completion
-------------------------

successful_completion                                :: ErrorCode = "00000"

-- * Class 01 — Warning
-------------------------

warning                                              :: ErrorCode = "01000"
dynamic_result_sets_returned                         :: ErrorCode = "0100C"
implicit_zero_bit_padding                            :: ErrorCode = "01008"
null_value_eliminated_in_set_function                :: ErrorCode = "01003"
privilege_not_granted                                :: ErrorCode = "01007"
privilege_not_revoked                                :: ErrorCode = "01006"
string_data_right_truncation                         :: ErrorCode = "01004"
deprecated_feature                                   :: ErrorCode = "01P01"

-- * Class 02 — No Data (this is also a warning class per the SQL standard)
-------------------------

no_data                                              :: ErrorCode = "02000"
no_additional_dynamic_result_sets_returned           :: ErrorCode = "02001"

-- * Class 03 — SQL Statement Not Yet Complete
-------------------------

sql_statement_not_yet_complete                       :: ErrorCode = "03000"

-- * Class 08 — Connection Exception
-------------------------

connection_exception                                 :: ErrorCode = "08000"
connection_does_not_exist                            :: ErrorCode = "08003"
connection_failure                                   :: ErrorCode = "08006"
sqlclient_unable_to_establish_sqlconnection          :: ErrorCode = "08001"
sqlserver_rejected_establishment_of_sqlconnection    :: ErrorCode = "08004"
transaction_resolution_unknown                       :: ErrorCode = "08007"
protocol_violation                                   :: ErrorCode = "08P01"

-- * Class 09 — Triggered Action Exception
-------------------------

triggered_action_exception                           :: ErrorCode = "09000"

-- * Class 0A — Feature Not Supported
-------------------------

feature_not_supported                                :: ErrorCode = "0A000"

-- * Class 0B — Invalid Transaction Initiation
-------------------------

invalid_transaction_initiation                       :: ErrorCode = "0B000"

-- * Class 0F — Locator Exception
-------------------------

locator_exception                                    :: ErrorCode = "0F000"
invalid_locator_specification                        :: ErrorCode = "0F001"

-- * Class 0L — Invalid Grantor
-------------------------

invalid_grantor                                      :: ErrorCode = "0L000"
invalid_grant_operation                              :: ErrorCode = "0LP01"

-- * Class 0P — Invalid Role Specification
-------------------------

invalid_role_specification                           :: ErrorCode = "0P000"

-- * Class 0Z — Diagnostics Exception
-------------------------

diagnostics_exception                                :: ErrorCode = "0Z000"
stacked_diagnostics_accessed_without_active_handler  :: ErrorCode = "0Z002"

-- * Class 20 — Case Not Found
-------------------------

case_not_found                                       :: ErrorCode = "20000"

-- * Class 21 — Cardinality Violation
-------------------------

cardinality_violation                                :: ErrorCode = "21000"

-- * Class 22 — Data Exception
-------------------------

data_exception                                       :: ErrorCode = "22000"
array_subscript_error                                :: ErrorCode = "2202E"
character_not_in_repertoire                          :: ErrorCode = "22021"
datetime_field_overflow                              :: ErrorCode = "22008"
division_by_zero                                     :: ErrorCode = "22012"
error_in_assignment                                  :: ErrorCode = "22005"
escape_character_conflict                            :: ErrorCode = "2200B"
indicator_overflow                                   :: ErrorCode = "22022"
interval_field_overflow                              :: ErrorCode = "22015"
invalid_argument_for_logarithm                       :: ErrorCode = "2201E"
invalid_argument_for_ntile_function                  :: ErrorCode = "22014"
invalid_argument_for_nth_value_function              :: ErrorCode = "22016"
invalid_argument_for_power_function                  :: ErrorCode = "2201F"
invalid_argument_for_width_bucket_function           :: ErrorCode = "2201G"
invalid_character_value_for_cast                     :: ErrorCode = "22018"
invalid_datetime_format                              :: ErrorCode = "22007"
invalid_escape_character                             :: ErrorCode = "22019"
invalid_escape_octet                                 :: ErrorCode = "2200D"
invalid_escape_sequence                              :: ErrorCode = "22025"
nonstandard_use_of_escape_character                  :: ErrorCode = "22P06"
invalid_indicator_parameter_value                    :: ErrorCode = "22010"
invalid_parameter_value                              :: ErrorCode = "22023"
invalid_regular_expression                           :: ErrorCode = "2201B"
invalid_row_count_in_limit_clause                    :: ErrorCode = "2201W"
invalid_row_count_in_result_offset_clause            :: ErrorCode = "2201X"
invalid_time_zone_displacement_value                 :: ErrorCode = "22009"
invalid_use_of_escape_character                      :: ErrorCode = "2200C"
most_specific_type_mismatch                          :: ErrorCode = "2200G"
null_value_not_allowed                               :: ErrorCode = "22004"
null_value_no_indicator_parameter                    :: ErrorCode = "22002"
numeric_value_out_of_range                           :: ErrorCode = "22003"
string_data_length_mismatch                          :: ErrorCode = "22026"
string_data_right_truncation'                        :: ErrorCode = "22001"
substring_error                                      :: ErrorCode = "22011"
trim_error                                           :: ErrorCode = "22027"
unterminated_c_string                                :: ErrorCode = "22024"
zero_length_character_string                         :: ErrorCode = "2200F"
floating_point_exception                             :: ErrorCode = "22P01"
invalid_text_representation                          :: ErrorCode = "22P02"
invalid_binary_representation                        :: ErrorCode = "22P03"
bad_copy_file_format                                 :: ErrorCode = "22P04"
untranslatable_character                             :: ErrorCode = "22P05"
not_an_xml_document                                  :: ErrorCode = "2200L"
invalid_xml_document                                 :: ErrorCode = "2200M"
invalid_xml_content                                  :: ErrorCode = "2200N"
invalid_xml_comment                                  :: ErrorCode = "2200S"
invalid_xml_processing_instruction                   :: ErrorCode = "2200T"

-- * Class 23 — Integrity Constraint Violation
-------------------------

integrity_constraint_violation                       :: ErrorCode = "23000"
restrict_violation                                   :: ErrorCode = "23001"
not_null_violation                                   :: ErrorCode = "23502"
foreign_key_violation                                :: ErrorCode = "23503"
unique_violation                                     :: ErrorCode = "23505"
check_violation                                      :: ErrorCode = "23514"
exclusion_violation                                  :: ErrorCode = "23P01"

-- * Class 24 — Invalid Cursor State
-------------------------

invalid_cursor_state                                 :: ErrorCode = "24000"

-- * Class 25 — Invalid Transaction State
-------------------------

invalid_transaction_state                            :: ErrorCode = "25000"
active_sql_transaction                               :: ErrorCode = "25001"
branch_transaction_already_active                    :: ErrorCode = "25002"
held_cursor_requires_same_isolation_level            :: ErrorCode = "25008"
inappropriate_access_mode_for_branch_transaction     :: ErrorCode = "25003"
inappropriate_isolation_level_for_branch_transaction :: ErrorCode = "25004"
no_active_sql_transaction_for_branch_transaction     :: ErrorCode = "25005"
read_only_sql_transaction                            :: ErrorCode = "25006"
schema_and_data_statement_mixing_not_supported       :: ErrorCode = "25007"
no_active_sql_transaction                            :: ErrorCode = "25P01"
in_failed_sql_transaction                            :: ErrorCode = "25P02"

-- * Class 26 — Invalid SQL Statement Name
-------------------------

invalid_sql_statement_name                           :: ErrorCode = "26000"

-- * Class 27 — Triggered Data Change Violation
-------------------------

triggered_data_change_violation                      :: ErrorCode = "27000"

-- * Class 28 — Invalid Authorization Specification
-------------------------

invalid_authorization_specification                  :: ErrorCode = "28000"
invalid_password                                     :: ErrorCode = "28P01"

-- * Class 2B — Dependent Privilege Descriptors Still Exist
-------------------------

dependent_privilege_descriptors_still_exist          :: ErrorCode = "2B000"
dependent_objects_still_exist                        :: ErrorCode = "2BP01"

-- * Class 2D — Invalid Transaction Termination
-------------------------

invalid_transaction_termination                      :: ErrorCode = "2D000"

-- * Class 2F — SQL Routine Exception
-------------------------

sql_routine_exception                                :: ErrorCode = "2F000"
function_executed_no_return_statement                :: ErrorCode = "2F005"
modifying_sql_data_not_permitted                     :: ErrorCode = "2F002"
prohibited_sql_statement_attempted                   :: ErrorCode = "2F003"
reading_sql_data_not_permitted                       :: ErrorCode = "2F004"

-- * Class 34 — Invalid Cursor Name
-------------------------

invalid_cursor_name                                  :: ErrorCode = "34000"

-- * Class 38 — External Routine Exception
-------------------------

external_routine_exception                           :: ErrorCode = "38000"
containing_sql_not_permitted                         :: ErrorCode = "38001"
modifying_sql_data_not_permitted'                    :: ErrorCode = "38002"
prohibited_sql_statement_attempted'                  :: ErrorCode = "38003"
reading_sql_data_not_permitted'                      :: ErrorCode = "38004"

-- * Class 39 — External Routine Invocation Exception
-------------------------

external_routine_invocation_exception                :: ErrorCode = "39000"
invalid_sqlstate_returned                            :: ErrorCode = "39001"
null_value_not_allowed'                              :: ErrorCode = "39004"
trigger_protocol_violated                            :: ErrorCode = "39P01"
srf_protocol_violated                                :: ErrorCode = "39P02"

-- * Class 3B — Savepoint Exception
-------------------------

savepoint_exception                                  :: ErrorCode = "3B000"
invalid_savepoint_specification                      :: ErrorCode = "3B001"

-- * Class 3D — Invalid Catalog Name
-------------------------

invalid_catalog_name                                 :: ErrorCode = "3D000"

-- * Class 3F — Invalid Schema Name
-------------------------

invalid_schema_name                                  :: ErrorCode = "3F000"

-- * Class 40 — Transaction Rollback
-------------------------

transaction_rollback                                 :: ErrorCode = "40000"
transaction_integrity_constraint_violation           :: ErrorCode = "40002"
serialization_failure                                :: ErrorCode = "40001"
statement_completion_unknown                         :: ErrorCode = "40003"
deadlock_detected                                    :: ErrorCode = "40P01"

-- * Class 42 — Syntax Error or Access Rule Violation
-------------------------

syntax_error_or_access_rule_violation                :: ErrorCode = "42000"
syntax_error                                         :: ErrorCode = "42601"
insufficient_privilege                               :: ErrorCode = "42501"
cannot_coerce                                        :: ErrorCode = "42846"
grouping_error                                       :: ErrorCode = "42803"
windowing_error                                      :: ErrorCode = "42P20"
invalid_recursion                                    :: ErrorCode = "42P19"
invalid_foreign_key                                  :: ErrorCode = "42830"
invalid_name                                         :: ErrorCode = "42602"
name_too_long                                        :: ErrorCode = "42622"
reserved_name                                        :: ErrorCode = "42939"
datatype_mismatch                                    :: ErrorCode = "42804"
indeterminate_datatype                               :: ErrorCode = "42P18"
collation_mismatch                                   :: ErrorCode = "42P21"
indeterminate_collation                              :: ErrorCode = "42P22"
wrong_object_type                                    :: ErrorCode = "42809"
undefined_column                                     :: ErrorCode = "42703"
undefined_function                                   :: ErrorCode = "42883"
undefined_table                                      :: ErrorCode = "42P01"
undefined_parameter                                  :: ErrorCode = "42P02"
undefined_object                                     :: ErrorCode = "42704"
duplicate_column                                     :: ErrorCode = "42701"
duplicate_cursor                                     :: ErrorCode = "42P03"
duplicate_database                                   :: ErrorCode = "42P04"
duplicate_function                                   :: ErrorCode = "42723"
duplicate_prepared_statement                         :: ErrorCode = "42P05"
duplicate_schema                                     :: ErrorCode = "42P06"
duplicate_table                                      :: ErrorCode = "42P07"
duplicate_alias                                      :: ErrorCode = "42712"
duplicate_object                                     :: ErrorCode = "42710"
ambiguous_column                                     :: ErrorCode = "42702"
ambiguous_function                                   :: ErrorCode = "42725"
ambiguous_parameter                                  :: ErrorCode = "42P08"
ambiguous_alias                                      :: ErrorCode = "42P09"
invalid_column_reference                             :: ErrorCode = "42P10"
invalid_column_definition                            :: ErrorCode = "42611"
invalid_cursor_definition                            :: ErrorCode = "42P11"
invalid_database_definition                          :: ErrorCode = "42P12"
invalid_function_definition                          :: ErrorCode = "42P13"
invalid_prepared_statement_definition                :: ErrorCode = "42P14"
invalid_schema_definition                            :: ErrorCode = "42P15"
invalid_table_definition                             :: ErrorCode = "42P16"
invalid_object_definition                            :: ErrorCode = "42P17"

-- * Class 44 — WITH CHECK OPTION Violation
-------------------------

with_check_option_violation                          :: ErrorCode = "44000"

-- * Class 53 — Insufficient Resources
-------------------------

insufficient_resources                               :: ErrorCode = "53000"
disk_full                                            :: ErrorCode = "53100"
out_of_memory                                        :: ErrorCode = "53200"
too_many_connections                                 :: ErrorCode = "53300"
configuration_limit_exceeded                         :: ErrorCode = "53400"

-- * Class 54 — Program Limit Exceeded
-------------------------

program_limit_exceeded                               :: ErrorCode = "54000"
statement_too_complex                                :: ErrorCode = "54001"
too_many_columns                                     :: ErrorCode = "54011"
too_many_arguments                                   :: ErrorCode = "54023"

-- * Class 55 — Object Not In Prerequisite State
-------------------------

object_not_in_prerequisite_state                     :: ErrorCode = "55000"
object_in_use                                        :: ErrorCode = "55006"
cant_change_runtime_param                            :: ErrorCode = "55P02"
lock_not_available                                   :: ErrorCode = "55P03"

-- * Class 57 — Operator Intervention
-------------------------

operator_intervention                                :: ErrorCode = "57000"
query_canceled                                       :: ErrorCode = "57014"
admin_shutdown                                       :: ErrorCode = "57P01"
crash_shutdown                                       :: ErrorCode = "57P02"
cannot_connect_now                                   :: ErrorCode = "57P03"
database_dropped                                     :: ErrorCode = "57P04"

-- * Class 58 — System Error (errors external to PostgreSQL itself)
-------------------------

system_error                                         :: ErrorCode = "58000"
io_error                                             :: ErrorCode = "58030"
undefined_file                                       :: ErrorCode = "58P01"
duplicate_file                                       :: ErrorCode = "58P02"

-- * Class F0 — Configuration File Error
-------------------------

config_file_error                                    :: ErrorCode = "F0000"
lock_file_exists                                     :: ErrorCode = "F0001"

-- * Class HV — Foreign Data Wrapper Error (SQL/MED)
-------------------------

fdw_error                                            :: ErrorCode = "HV000"
fdw_column_name_not_found                            :: ErrorCode = "HV005"
fdw_dynamic_parameter_value_needed                   :: ErrorCode = "HV002"
fdw_function_sequence_error                          :: ErrorCode = "HV010"
fdw_inconsistent_descriptor_information              :: ErrorCode = "HV021"
fdw_invalid_attribute_value                          :: ErrorCode = "HV024"
fdw_invalid_column_name                              :: ErrorCode = "HV007"
fdw_invalid_column_number                            :: ErrorCode = "HV008"
fdw_invalid_data_type                                :: ErrorCode = "HV004"
fdw_invalid_data_type_descriptors                    :: ErrorCode = "HV006"
fdw_invalid_descriptor_field_identifier              :: ErrorCode = "HV091"
fdw_invalid_handle                                   :: ErrorCode = "HV00B"
fdw_invalid_option_index                             :: ErrorCode = "HV00C"
fdw_invalid_option_name                              :: ErrorCode = "HV00D"
fdw_invalid_string_length_or_buffer_length           :: ErrorCode = "HV090"
fdw_invalid_string_format                            :: ErrorCode = "HV00A"
fdw_invalid_use_of_null_pointer                      :: ErrorCode = "HV009"
fdw_too_many_handles                                 :: ErrorCode = "HV014"
fdw_out_of_memory                                    :: ErrorCode = "HV001"
fdw_no_schemas                                       :: ErrorCode = "HV00P"
fdw_option_name_not_found                            :: ErrorCode = "HV00J"
fdw_reply_handle                                     :: ErrorCode = "HV00K"
fdw_schema_not_found                                 :: ErrorCode = "HV00Q"
fdw_table_not_found                                  :: ErrorCode = "HV00R"
fdw_unable_to_create_execution                       :: ErrorCode = "HV00L"
fdw_unable_to_create_reply                           :: ErrorCode = "HV00M"
fdw_unable_to_establish_connection                   :: ErrorCode = "HV00N"

-- * Class P0 — PL/pgSQL Error
-------------------------

plpgsql_error                                        :: ErrorCode = "P0000"
raise_exception                                      :: ErrorCode = "P0001"
no_data_found                                        :: ErrorCode = "P0002"
too_many_rows                                        :: ErrorCode = "P0003"

-- * Class XX — Internal Error
-------------------------

internal_error                                       :: ErrorCode = "XX000"
data_corrupted                                       :: ErrorCode = "XX001"
index_corrupted                                      :: ErrorCode = "XX002"
