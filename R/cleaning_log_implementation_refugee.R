library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)
library(supporteR)

loc_data_refugee <- "inputs/UGA2402_aba_mbarara_refugee_data.xlsx"

# main data
data_nms_refugee <- names(readxl::read_excel(path = loc_data_refugee, n_max = 300))
c_types_refugee <- ifelse(str_detect(string = data_nms_refugee, pattern = "_other$"), "text", "guess")
df_tool_data_refugee <- readxl::read_excel(loc_data_refugee, col_types = c_types_refugee)

# loops
# roster
data_nms_r_roster_refugee <- names(readxl::read_excel(path = loc_data_refugee, n_max = 300, sheet = "hh_roster"))
c_types_r_roster_refugee <- ifelse(str_detect(string = data_nms_r_roster_refugee, pattern = "_other$"), "text", "guess")
df_loop_r_roster_refugee <- readxl::read_excel(loc_data_refugee, col_types = c_types_r_roster_refugee, sheet = "hh_roster")
# income
data_nms_r_income_refugee <- names(readxl::read_excel(path = loc_data_refugee, n_max = 300, sheet = "grp_income_received"))
c_types_r_income_refugee <- ifelse(str_detect(string = data_nms_r_income_refugee, pattern = "_other$"), "text", "guess")
df_loop_r_income_refugee <- readxl::read_excel(loc_data_refugee, col_types = c_types_r_income_refugee, sheet = "grp_income_received")

# tool
loc_tool_refugee <- "inputs/UGA2402_aba_mbarara_refugee_tool.xlsx"
df_survey_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "survey")
df_choices_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "choices")

df_filled_cl_refugee <- readxl::read_excel("inputs/combined_checks_aba_mbarara_refugee.xlsx", sheet = "cleaning_log") %>% 
    filter(!is.na(reviewed), !question %in% c("_index"), !uuid %in% c("all"))

df_remove_survey_cl_refugee <- df_filled_cl_refugee %>% 
    filter(change_type %in% c("remove_survey"))

# check pii ---------------------------------------------------------------
pii_from_data_refugee <- cleaningtools::check_pii(dataset = df_tool_data_refugee, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data_refugee$potential_PII

# then determine wich columns to remove from both the raw and clean data
cols_to_remove_refugee <- c("audit", "audit_URL", "pt_num_msg", "pt_num_validation_message",
                         "pt_sample_lat", "pt_sample_lon", "dist_btn_sample_collected", 
                         "threshold_msg_2_positive", "threshold_msg_2_negative",
                         "telephone", "contact", "name", "gps", 
                         "latitude", "longitude", "contact", "geopoint",
                         "instance_name", "_geopoint_latitude", "_geopoint_longitude",
                         "_geopoint_altitude", "_geopoint_precision",
                         "phone_consent", "fgd_phone_number")



# Main dataset ------------------------------------------------------------

# filtered log
df_filled_cl_refugee_main <- df_filled_cl_refugee %>% 
    filter(is.na(sheet))

# updating the main dataset with new columns

df_data_with_added_cols_refugee <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_tool_data_refugee,
                                                               input_df_filled_cl = df_filled_cl_refugee_main, 
                                                               input_df_survey = df_survey_refugee,
                                                               input_df_choices = df_choices_refugee)

# check the cleaning log
df_cl_review_refugee <- cleaningtools::review_cleaning_log(
    raw_dataset = df_data_with_added_cols_refugee,
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_filled_cl_refugee_main,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_refugee <- df_filled_cl_refugee_main %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent"), 
           !uuid %in% c("all")) %>% 
    filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_refugee <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols_refugee %>% select(-any_of(cols_to_remove_refugee)) %>% 
        rename(`shelter_damage_issues/lack_of_space_inside_the_shelter_min_35m2_per_household_member` = `shelter_damage_issues/lack_of_space_inside_the_shelter_min_35m²_per_household_member`),
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_final_cleaning_log_refugee,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# handle parent question columns
df_updating_sm_parents_refugee <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step_refugee, 
                                                         input_uuid_col = "_uuid",
                                                         input_point_id_col = "point_number",
                                                         input_collected_date_col = "today",
                                                         input_location_col = "interview_cell") %>% 
    filter(!`_uuid` %in% df_remove_survey_cl_refugee$uuid)

# tool data to support loops ----------------------------------------------

df_tool_support_data_for_loops_refugee <- df_updating_sm_parents_refugee$updated_sm_parents %>% 
    select(`_uuid`, interview_cell, today, enumerator_id, point_number)

# roster cleaning ---------------------------------------------------------

# then determine wich columns to remove from both the raw and clean data
cols_to_remove_refugee_roster <- c("name")
# filtered log
df_filled_cl_refugee_roster <- df_filled_cl_refugee %>% 
    filter(sheet %in% c("hh_roster", "grp_hh_roster"), !is.na(index))

# updating the main dataset with new columns

df_data_with_added_cols_refugee_roster <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_loop_r_roster_refugee %>% 
                                                                          left_join(df_tool_support_data_for_loops_refugee, by = c("_submission__uuid" = "_uuid")),
                                                                      input_df_filled_cl = df_filled_cl_refugee_roster, 
                                                                      input_df_survey = df_survey_refugee,
                                                                      input_df_choices = df_choices_refugee)

# check the cleaning log
df_cl_review_refugee <- cleaningtools::review_cleaning_log(
    raw_dataset = df_data_with_added_cols_refugee_roster,
    raw_data_uuid_column = "_submission__uuid",
    cleaning_log = df_filled_cl_refugee_roster,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_refugee_roster <- df_filled_cl_refugee_roster %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                            "_index", "_parent_index"), 
           !uuid %in% c("all")) %>% 
    filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_refugee_roster <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols_refugee_roster %>% select(-any_of(cols_to_remove_refugee_roster)),
    raw_data_uuid_column = "_submission__uuid",
    cleaning_log = df_final_cleaning_log_refugee_roster,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# handle parent question columns
df_updating_sm_parents_refugee_roster <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step_refugee_roster,
                                                                input_uuid_col = "_submission__uuid",
                                                                input_enumerator_id_col = "enumerator_id",
                                                                input_point_id_col = "point_number",
                                                                input_collected_date_col = "today",
                                                                input_location_col = "interview_cell", 
                                                                input_dataset_type = "loop", 
                                                                input_sheet_name = "hh_roster", 
                                                                input_index_col = "_index") %>% 
    filter(!`_submission__uuid` %in% df_remove_survey_cl_refugee$uuid)

# income_received ---------------------------------------------------------

# then determine wich columns to remove from both the raw and clean data
# cols_to_remove_refugee_income <- c("name")
# filtered log
df_filled_cl_refugee_income <- df_filled_cl_refugee %>% 
    filter(sheet %in% c("grp_income_received"), !is.na(index))


# check the cleaning log
df_cl_review_refugee <- cleaningtools::review_cleaning_log(
    raw_dataset = df_loop_r_income_refugee,
    raw_data_uuid_column = "_submission__uuid",
    cleaning_log = df_filled_cl_refugee_income,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_refugee_income <- df_filled_cl_refugee_income %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                            "_index", "_parent_index"), 
           !uuid %in% c("all")) %>% 
    filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_refugee_income <- cleaningtools::create_clean_data(
    raw_dataset = df_loop_r_income_refugee,
    raw_data_uuid_column = "_submission__uuid",
    cleaning_log = df_final_cleaning_log_refugee_income,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value") %>% 
    filter(!`_submission__uuid` %in% df_remove_survey_cl_refugee$uuid)


# export datasets ---------------------------------------------------------

list_of_datasets_refugee <- list("raw_data" = df_tool_data_refugee %>% select(-any_of(cols_to_remove_refugee)),
                              "raw_roster" = df_loop_r_roster_refugee %>% select(-any_of(cols_to_remove_refugee_roster)),
                              "raw_income_received" = df_loop_r_income_refugee,
                              "cleaned_data" = df_updating_sm_parents_refugee$updated_sm_parents,
                              "cleaned_roster" = df_updating_sm_parents_refugee_roster$updated_sm_parents,
                              "cleaned_income_received" = df_cleaning_step_refugee_income,
                              "extra_log_sm_parents" = df_updating_sm_parents_refugee$extra_log_sm_parents,
                              "extra_log_sm_parents_roster" = df_updating_sm_parents_refugee_roster$extra_log_sm_parents
                              )

openxlsx::write.xlsx(list_of_datasets_refugee,
                     paste0("outputs/", butteR::date_file_prefix(), "_UGA2402_aba_mbarara_refugee_cleaned_data.xlsx"),
                     overwrite = TRUE)
