-- |
-- Error codes.
-- See <http://www.postgresql.org/docs/current/interactive/errcodes-appendix.html>
module Hasql.Postgres.ErrorCode where

import qualified Data.ByteString

type ErrorCode = 
  Data.ByteString.ByteString

-- * Class 00 — Successful Completion
-------------------------

-- | Code \"00000\".
successful_completion                                :: ErrorCode = "00000"

-- * Class 01 — Warning
-------------------------

-- | Code \"01000\".
warning                                              :: ErrorCode = "01000"
-- | Code \"0100C\".
dynamic_result_sets_returned                         :: ErrorCode = "0100C"
-- | Code \"01008\".
implicit_zero_bit_padding                            :: ErrorCode = "01008"
-- | Code \"01003\".
null_value_eliminated_in_set_function                :: ErrorCode = "01003"
-- | Code \"01007\".
privilege_not_granted                                :: ErrorCode = "01007"
-- | Code \"01006\".
privilege_not_revoked                                :: ErrorCode = "01006"
-- | Code \"01004\".
string_data_right_truncation                         :: ErrorCode = "01004"
-- | Code \"01P01\".
deprecated_feature                                   :: ErrorCode = "01P01"

-- * Class 02 — No Data (this is also a warning class per the SQL standard)
-------------------------

-- | Code \"02000\".
no_data                                              :: ErrorCode = "02000"
-- | Code \"02001\".
no_additional_dynamic_result_sets_returned           :: ErrorCode = "02001"

-- * Class 03 — SQL Statement Not Yet Complete
-------------------------

-- | Code \"03000\".
sql_statement_not_yet_complete                       :: ErrorCode = "03000"

-- * Class 08 — Connection Exception
-------------------------

-- | Code \"08000\".
connection_exception                                 :: ErrorCode = "08000"
-- | Code \"08003\".
connection_does_not_exist                            :: ErrorCode = "08003"
-- | Code \"08006\".
connection_failure                                   :: ErrorCode = "08006"
-- | Code \"08001\".
sqlclient_unable_to_establish_sqlconnection          :: ErrorCode = "08001"
-- | Code \"08004\".
sqlserver_rejected_establishment_of_sqlconnection    :: ErrorCode = "08004"
-- | Code \"08007\".
transaction_resolution_unknown                       :: ErrorCode = "08007"
-- | Code \"08P01\".
protocol_violation                                   :: ErrorCode = "08P01"

-- * Class 09 — Triggered Action Exception
-------------------------

-- | Code \"09000\".
triggered_action_exception                           :: ErrorCode = "09000"

-- * Class 0A — Feature Not Supported
-------------------------

-- | Code \"0A000\".
feature_not_supported                                :: ErrorCode = "0A000"

-- * Class 0B — Invalid Transaction Initiation
-------------------------

-- | Code \"0B000\".
invalid_transaction_initiation                       :: ErrorCode = "0B000"

-- * Class 0F — Locator Exception
-------------------------

-- | Code \"0F000\".
locator_exception                                    :: ErrorCode = "0F000"
-- | Code \"0F001\".
invalid_locator_specification                        :: ErrorCode = "0F001"

-- * Class 0L — Invalid Grantor
-------------------------

-- | Code \"0L000\".
invalid_grantor                                      :: ErrorCode = "0L000"
-- | Code \"0LP01\".
invalid_grant_operation                              :: ErrorCode = "0LP01"

-- * Class 0P — Invalid Role Specification
-------------------------

-- | Code \"0P000\".
invalid_role_specification                           :: ErrorCode = "0P000"

-- * Class 0Z — Diagnostics Exception
-------------------------

-- | Code \"0Z000\".
diagnostics_exception                                :: ErrorCode = "0Z000"
-- | Code \"0Z002\".
stacked_diagnostics_accessed_without_active_handler  :: ErrorCode = "0Z002"

-- * Class 20 — Case Not Found
-------------------------

-- | Code \"20000\".
case_not_found                                       :: ErrorCode = "20000"

-- * Class 21 — Cardinality Violation
-------------------------

-- | Code \"21000\".
cardinality_violation                                :: ErrorCode = "21000"

-- * Class 22 — Data Exception
-------------------------

-- | Code \"22000\".
data_exception                                       :: ErrorCode = "22000"
-- | Code \"2202E\".
array_subscript_error                                :: ErrorCode = "2202E"
-- | Code \"22021\".
character_not_in_repertoire                          :: ErrorCode = "22021"
-- | Code \"22008\".
datetime_field_overflow                              :: ErrorCode = "22008"
-- | Code \"22012\".
division_by_zero                                     :: ErrorCode = "22012"
-- | Code \"22005\".
error_in_assignment                                  :: ErrorCode = "22005"
-- | Code \"2200B\".
escape_character_conflict                            :: ErrorCode = "2200B"
-- | Code \"22022\".
indicator_overflow                                   :: ErrorCode = "22022"
-- | Code \"22015\".
interval_field_overflow                              :: ErrorCode = "22015"
-- | Code \"2201E\".
invalid_argument_for_logarithm                       :: ErrorCode = "2201E"
-- | Code \"22014\".
invalid_argument_for_ntile_function                  :: ErrorCode = "22014"
-- | Code \"22016\".
invalid_argument_for_nth_value_function              :: ErrorCode = "22016"
-- | Code \"2201F\".
invalid_argument_for_power_function                  :: ErrorCode = "2201F"
-- | Code \"2201G\".
invalid_argument_for_width_bucket_function           :: ErrorCode = "2201G"
-- | Code \"22018\".
invalid_character_value_for_cast                     :: ErrorCode = "22018"
-- | Code \"22007\".
invalid_datetime_format                              :: ErrorCode = "22007"
-- | Code \"22019\".
invalid_escape_character                             :: ErrorCode = "22019"
-- | Code \"2200D\".
invalid_escape_octet                                 :: ErrorCode = "2200D"
-- | Code \"22025\".
invalid_escape_sequence                              :: ErrorCode = "22025"
-- | Code \"22P06\".
nonstandard_use_of_escape_character                  :: ErrorCode = "22P06"
-- | Code \"22010\".
invalid_indicator_parameter_value                    :: ErrorCode = "22010"
-- | Code \"22023\".
invalid_parameter_value                              :: ErrorCode = "22023"
-- | Code \"2201B\".
invalid_regular_expression                           :: ErrorCode = "2201B"
-- | Code \"2201W\".
invalid_row_count_in_limit_clause                    :: ErrorCode = "2201W"
-- | Code \"2201X\".
invalid_row_count_in_result_offset_clause            :: ErrorCode = "2201X"
-- | Code \"22009\".
invalid_time_zone_displacement_value                 :: ErrorCode = "22009"
-- | Code \"2200C\".
invalid_use_of_escape_character                      :: ErrorCode = "2200C"
-- | Code \"2200G\".
most_specific_type_mismatch                          :: ErrorCode = "2200G"
-- | Code \"22004\".
null_value_not_allowed                               :: ErrorCode = "22004"
-- | Code \"22002\".
null_value_no_indicator_parameter                    :: ErrorCode = "22002"
-- | Code \"22003\".
numeric_value_out_of_range                           :: ErrorCode = "22003"
-- | Code \"22026\".
string_data_length_mismatch                          :: ErrorCode = "22026"
-- | Code \"22001\".
string_data_right_truncation'                        :: ErrorCode = "22001"
-- | Code \"22011\".
substring_error                                      :: ErrorCode = "22011"
-- | Code \"22027\".
trim_error                                           :: ErrorCode = "22027"
-- | Code \"22024\".
unterminated_c_string                                :: ErrorCode = "22024"
-- | Code \"2200F\".
zero_length_character_string                         :: ErrorCode = "2200F"
-- | Code \"22P01\".
floating_point_exception                             :: ErrorCode = "22P01"
-- | Code \"22P02\".
invalid_text_representation                          :: ErrorCode = "22P02"
-- | Code \"22P03\".
invalid_binary_representation                        :: ErrorCode = "22P03"
-- | Code \"22P04\".
bad_copy_file_format                                 :: ErrorCode = "22P04"
-- | Code \"22P05\".
untranslatable_character                             :: ErrorCode = "22P05"
-- | Code \"2200L\".
not_an_xml_document                                  :: ErrorCode = "2200L"
-- | Code \"2200M\".
invalid_xml_document                                 :: ErrorCode = "2200M"
-- | Code \"2200N\".
invalid_xml_content                                  :: ErrorCode = "2200N"
-- | Code \"2200S\".
invalid_xml_comment                                  :: ErrorCode = "2200S"
-- | Code \"2200T\".
invalid_xml_processing_instruction                   :: ErrorCode = "2200T"

-- * Class 23 — Integrity Constraint Violation
-------------------------

-- | Code \"23000\".
integrity_constraint_violation                       :: ErrorCode = "23000"
-- | Code \"23001\".
restrict_violation                                   :: ErrorCode = "23001"
-- | Code \"23502\".
not_null_violation                                   :: ErrorCode = "23502"
-- | Code \"23503\".
foreign_key_violation                                :: ErrorCode = "23503"
-- | Code \"23505\".
unique_violation                                     :: ErrorCode = "23505"
-- | Code \"23514\".
check_violation                                      :: ErrorCode = "23514"
-- | Code \"23P01\".
exclusion_violation                                  :: ErrorCode = "23P01"

-- * Class 24 — Invalid Cursor State
-------------------------

-- | Code \"24000\".
invalid_cursor_state                                 :: ErrorCode = "24000"

-- * Class 25 — Invalid Transaction State
-------------------------

-- | Code \"25000\".
invalid_transaction_state                            :: ErrorCode = "25000"
-- | Code \"25001\".
active_sql_transaction                               :: ErrorCode = "25001"
-- | Code \"25002\".
branch_transaction_already_active                    :: ErrorCode = "25002"
-- | Code \"25008\".
held_cursor_requires_same_isolation_level            :: ErrorCode = "25008"
-- | Code \"25003\".
inappropriate_access_mode_for_branch_transaction     :: ErrorCode = "25003"
-- | Code \"25004\".
inappropriate_isolation_level_for_branch_transaction :: ErrorCode = "25004"
-- | Code \"25005\".
no_active_sql_transaction_for_branch_transaction     :: ErrorCode = "25005"
-- | Code \"25006\".
read_only_sql_transaction                            :: ErrorCode = "25006"
-- | Code \"25007\".
schema_and_data_statement_mixing_not_supported       :: ErrorCode = "25007"
-- | Code \"25P01\".
no_active_sql_transaction                            :: ErrorCode = "25P01"
-- | Code \"25P02\".
in_failed_sql_transaction                            :: ErrorCode = "25P02"

-- * Class 26 — Invalid SQL Statement Name
-------------------------

-- | Code \"26000\".
invalid_sql_statement_name                           :: ErrorCode = "26000"

-- * Class 27 — Triggered Data Change Violation
-------------------------

-- | Code \"27000\".
triggered_data_change_violation                      :: ErrorCode = "27000"

-- * Class 28 — Invalid Authorization Specification
-------------------------

-- | Code \"28000\".
invalid_authorization_specification                  :: ErrorCode = "28000"
-- | Code \"28P01\".
invalid_password                                     :: ErrorCode = "28P01"

-- * Class 2B — Dependent Privilege Descriptors Still Exist
-------------------------

-- | Code \"2B000\".
dependent_privilege_descriptors_still_exist          :: ErrorCode = "2B000"
-- | Code \"2BP01\".
dependent_objects_still_exist                        :: ErrorCode = "2BP01"

-- * Class 2D — Invalid Transaction Termination
-------------------------

-- | Code \"2D000\".
invalid_transaction_termination                      :: ErrorCode = "2D000"

-- * Class 2F — SQL Routine Exception
-------------------------

-- | Code \"2F000\".
sql_routine_exception                                :: ErrorCode = "2F000"
-- | Code \"2F005\".
function_executed_no_return_statement                :: ErrorCode = "2F005"
-- | Code \"2F002\".
modifying_sql_data_not_permitted                     :: ErrorCode = "2F002"
-- | Code \"2F003\".
prohibited_sql_statement_attempted                   :: ErrorCode = "2F003"
-- | Code \"2F004\".
reading_sql_data_not_permitted                       :: ErrorCode = "2F004"

-- * Class 34 — Invalid Cursor Name
-------------------------

-- | Code \"34000\".
invalid_cursor_name                                  :: ErrorCode = "34000"

-- * Class 38 — External Routine Exception
-------------------------

-- | Code \"38000\".
external_routine_exception                           :: ErrorCode = "38000"
-- | Code \"38001\".
containing_sql_not_permitted                         :: ErrorCode = "38001"
-- | Code \"38002\".
modifying_sql_data_not_permitted'                    :: ErrorCode = "38002"
-- | Code \"38003\".
prohibited_sql_statement_attempted'                  :: ErrorCode = "38003"
-- | Code \"38004\".
reading_sql_data_not_permitted'                      :: ErrorCode = "38004"

-- * Class 39 — External Routine Invocation Exception
-------------------------

-- | Code \"39000\".
external_routine_invocation_exception                :: ErrorCode = "39000"
-- | Code \"39001\".
invalid_sqlstate_returned                            :: ErrorCode = "39001"
-- | Code \"39004\".
null_value_not_allowed'                              :: ErrorCode = "39004"
-- | Code \"39P01\".
trigger_protocol_violated                            :: ErrorCode = "39P01"
-- | Code \"39P02\".
srf_protocol_violated                                :: ErrorCode = "39P02"

-- * Class 3B — Savepoint Exception
-------------------------

-- | Code \"3B000\".
savepoint_exception                                  :: ErrorCode = "3B000"
-- | Code \"3B001\".
invalid_savepoint_specification                      :: ErrorCode = "3B001"

-- * Class 3D — Invalid Catalog Name
-------------------------

-- | Code \"3D000\".
invalid_catalog_name                                 :: ErrorCode = "3D000"

-- * Class 3F — Invalid Schema Name
-------------------------

-- | Code \"3F000\".
invalid_schema_name                                  :: ErrorCode = "3F000"

-- * Class 40 — Transaction Rollback
-------------------------

-- | Code \"40000\".
transaction_rollback                                 :: ErrorCode = "40000"
-- | Code \"40002\".
transaction_integrity_constraint_violation           :: ErrorCode = "40002"
-- | Code \"40001\".
serialization_failure                                :: ErrorCode = "40001"
-- | Code \"40003\".
statement_completion_unknown                         :: ErrorCode = "40003"
-- | Code \"40P01\".
deadlock_detected                                    :: ErrorCode = "40P01"

-- * Class 42 — Syntax Error or Access Rule Violation
-------------------------

-- | Code \"42000\".
syntax_error_or_access_rule_violation                :: ErrorCode = "42000"
-- | Code \"42601\".
syntax_error                                         :: ErrorCode = "42601"
-- | Code \"42501\".
insufficient_privilege                               :: ErrorCode = "42501"
-- | Code \"42846\".
cannot_coerce                                        :: ErrorCode = "42846"
-- | Code \"42803\".
grouping_error                                       :: ErrorCode = "42803"
-- | Code \"42P20\".
windowing_error                                      :: ErrorCode = "42P20"
-- | Code \"42P19\".
invalid_recursion                                    :: ErrorCode = "42P19"
-- | Code \"42830\".
invalid_foreign_key                                  :: ErrorCode = "42830"
-- | Code \"42602\".
invalid_name                                         :: ErrorCode = "42602"
-- | Code \"42622\".
name_too_long                                        :: ErrorCode = "42622"
-- | Code \"42939\".
reserved_name                                        :: ErrorCode = "42939"
-- | Code \"42804\".
datatype_mismatch                                    :: ErrorCode = "42804"
-- | Code \"42P18\".
indeterminate_datatype                               :: ErrorCode = "42P18"
-- | Code \"42P21\".
collation_mismatch                                   :: ErrorCode = "42P21"
-- | Code \"42P22\".
indeterminate_collation                              :: ErrorCode = "42P22"
-- | Code \"42809\".
wrong_object_type                                    :: ErrorCode = "42809"
-- | Code \"42703\".
undefined_column                                     :: ErrorCode = "42703"
-- | Code \"42883\".
undefined_function                                   :: ErrorCode = "42883"
-- | Code \"42P01\".
undefined_table                                      :: ErrorCode = "42P01"
-- | Code \"42P02\".
undefined_parameter                                  :: ErrorCode = "42P02"
-- | Code \"42704\".
undefined_object                                     :: ErrorCode = "42704"
-- | Code \"42701\".
duplicate_column                                     :: ErrorCode = "42701"
-- | Code \"42P03\".
duplicate_cursor                                     :: ErrorCode = "42P03"
-- | Code \"42P04\".
duplicate_database                                   :: ErrorCode = "42P04"
-- | Code \"42723\".
duplicate_function                                   :: ErrorCode = "42723"
-- | Code \"42P05\".
duplicate_prepared_statement                         :: ErrorCode = "42P05"
-- | Code \"42P06\".
duplicate_schema                                     :: ErrorCode = "42P06"
-- | Code \"42P07\".
duplicate_table                                      :: ErrorCode = "42P07"
-- | Code \"42712\".
duplicate_alias                                      :: ErrorCode = "42712"
-- | Code \"42710\".
duplicate_object                                     :: ErrorCode = "42710"
-- | Code \"42702\".
ambiguous_column                                     :: ErrorCode = "42702"
-- | Code \"42725\".
ambiguous_function                                   :: ErrorCode = "42725"
-- | Code \"42P08\".
ambiguous_parameter                                  :: ErrorCode = "42P08"
-- | Code \"42P09\".
ambiguous_alias                                      :: ErrorCode = "42P09"
-- | Code \"42P10\".
invalid_column_reference                             :: ErrorCode = "42P10"
-- | Code \"42611\".
invalid_column_definition                            :: ErrorCode = "42611"
-- | Code \"42P11\".
invalid_cursor_definition                            :: ErrorCode = "42P11"
-- | Code \"42P12\".
invalid_database_definition                          :: ErrorCode = "42P12"
-- | Code \"42P13\".
invalid_function_definition                          :: ErrorCode = "42P13"
-- | Code \"42P14\".
invalid_prepared_statement_definition                :: ErrorCode = "42P14"
-- | Code \"42P15\".
invalid_schema_definition                            :: ErrorCode = "42P15"
-- | Code \"42P16\".
invalid_table_definition                             :: ErrorCode = "42P16"
-- | Code \"42P17\".
invalid_object_definition                            :: ErrorCode = "42P17"

-- * Class 44 — WITH CHECK OPTION Violation
-------------------------

-- | Code \"44000\".
with_check_option_violation                          :: ErrorCode = "44000"

-- * Class 53 — Insufficient Resources
-------------------------

-- | Code \"53000\".
insufficient_resources                               :: ErrorCode = "53000"
-- | Code \"53100\".
disk_full                                            :: ErrorCode = "53100"
-- | Code \"53200\".
out_of_memory                                        :: ErrorCode = "53200"
-- | Code \"53300\".
too_many_connections                                 :: ErrorCode = "53300"
-- | Code \"53400\".
configuration_limit_exceeded                         :: ErrorCode = "53400"

-- * Class 54 — Program Limit Exceeded
-------------------------

-- | Code \"54000\".
program_limit_exceeded                               :: ErrorCode = "54000"
-- | Code \"54001\".
statement_too_complex                                :: ErrorCode = "54001"
-- | Code \"54011\".
too_many_columns                                     :: ErrorCode = "54011"
-- | Code \"54023\".
too_many_arguments                                   :: ErrorCode = "54023"

-- * Class 55 — Object Not In Prerequisite State
-------------------------

-- | Code \"55000\".
object_not_in_prerequisite_state                     :: ErrorCode = "55000"
-- | Code \"55006\".
object_in_use                                        :: ErrorCode = "55006"
-- | Code \"55P02\".
cant_change_runtime_param                            :: ErrorCode = "55P02"
-- | Code \"55P03\".
lock_not_available                                   :: ErrorCode = "55P03"

-- * Class 57 — Operator Intervention
-------------------------

-- | Code \"57000\".
operator_intervention                                :: ErrorCode = "57000"
-- | Code \"57014\".
query_canceled                                       :: ErrorCode = "57014"
-- | Code \"57P01\".
admin_shutdown                                       :: ErrorCode = "57P01"
-- | Code \"57P02\".
crash_shutdown                                       :: ErrorCode = "57P02"
-- | Code \"57P03\".
cannot_connect_now                                   :: ErrorCode = "57P03"
-- | Code \"57P04\".
database_dropped                                     :: ErrorCode = "57P04"

-- * Class 58 — System Error (errors external to PostgreSQL itself)
-------------------------

-- | Code \"58000\".
system_error                                         :: ErrorCode = "58000"
-- | Code \"58030\".
io_error                                             :: ErrorCode = "58030"
-- | Code \"58P01\".
undefined_file                                       :: ErrorCode = "58P01"
-- | Code \"58P02\".
duplicate_file                                       :: ErrorCode = "58P02"

-- * Class F0 — Configuration File Error
-------------------------

-- | Code \"F0000\".
config_file_error                                    :: ErrorCode = "F0000"
-- | Code \"F0001\".
lock_file_exists                                     :: ErrorCode = "F0001"

-- * Class HV — Foreign Data Wrapper Error (SQL/MED)
-------------------------

-- | Code \"HV000\".
fdw_error                                            :: ErrorCode = "HV000"
-- | Code \"HV005\".
fdw_column_name_not_found                            :: ErrorCode = "HV005"
-- | Code \"HV002\".
fdw_dynamic_parameter_value_needed                   :: ErrorCode = "HV002"
-- | Code \"HV010\".
fdw_function_sequence_error                          :: ErrorCode = "HV010"
-- | Code \"HV021\".
fdw_inconsistent_descriptor_information              :: ErrorCode = "HV021"
-- | Code \"HV024\".
fdw_invalid_attribute_value                          :: ErrorCode = "HV024"
-- | Code \"HV007\".
fdw_invalid_column_name                              :: ErrorCode = "HV007"
-- | Code \"HV008\".
fdw_invalid_column_number                            :: ErrorCode = "HV008"
-- | Code \"HV004\".
fdw_invalid_data_type                                :: ErrorCode = "HV004"
-- | Code \"HV006\".
fdw_invalid_data_type_descriptors                    :: ErrorCode = "HV006"
-- | Code \"HV091\".
fdw_invalid_descriptor_field_identifier              :: ErrorCode = "HV091"
-- | Code \"HV00B\".
fdw_invalid_handle                                   :: ErrorCode = "HV00B"
-- | Code \"HV00C\".
fdw_invalid_option_index                             :: ErrorCode = "HV00C"
-- | Code \"HV00D\".
fdw_invalid_option_name                              :: ErrorCode = "HV00D"
-- | Code \"HV090\".
fdw_invalid_string_length_or_buffer_length           :: ErrorCode = "HV090"
-- | Code \"HV00A\".
fdw_invalid_string_format                            :: ErrorCode = "HV00A"
-- | Code \"HV009\".
fdw_invalid_use_of_null_pointer                      :: ErrorCode = "HV009"
-- | Code \"HV014\".
fdw_too_many_handles                                 :: ErrorCode = "HV014"
-- | Code \"HV001\".
fdw_out_of_memory                                    :: ErrorCode = "HV001"
-- | Code \"HV00P\".
fdw_no_schemas                                       :: ErrorCode = "HV00P"
-- | Code \"HV00J\".
fdw_option_name_not_found                            :: ErrorCode = "HV00J"
-- | Code \"HV00K\".
fdw_reply_handle                                     :: ErrorCode = "HV00K"
-- | Code \"HV00Q\".
fdw_schema_not_found                                 :: ErrorCode = "HV00Q"
-- | Code \"HV00R\".
fdw_table_not_found                                  :: ErrorCode = "HV00R"
-- | Code \"HV00L\".
fdw_unable_to_create_execution                       :: ErrorCode = "HV00L"
-- | Code \"HV00M\".
fdw_unable_to_create_reply                           :: ErrorCode = "HV00M"
-- | Code \"HV00N\".
fdw_unable_to_establish_connection                   :: ErrorCode = "HV00N"

-- * Class P0 — PL/pgSQL Error
-------------------------

-- | Code \"P0000\".
plpgsql_error                                        :: ErrorCode = "P0000"
-- | Code \"P0001\".
raise_exception                                      :: ErrorCode = "P0001"
-- | Code \"P0002\".
no_data_found                                        :: ErrorCode = "P0002"
-- | Code \"P0003\".
too_many_rows                                        :: ErrorCode = "P0003"

-- * Class XX — Internal Error
-------------------------

-- | Code \"XX000\".
internal_error                                       :: ErrorCode = "XX000"
-- | Code \"XX001\".
data_corrupted                                       :: ErrorCode = "XX001"
-- | Code \"XX002\".
index_corrupted                                      :: ErrorCode = "XX002"
