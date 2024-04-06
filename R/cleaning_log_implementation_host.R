library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)
library(supporteR)

loc_data_host <- "inputs/UGA2402_aba_mbarara_host_data.xlsx"

# main data
data_nms_host <- names(readxl::read_excel(path = loc_data_host, n_max = 300))
c_types_host <- ifelse(str_detect(string = data_nms_host, pattern = "_other$"), "text", "guess")
df_tool_data_host <- readxl::read_excel(loc_data_host, col_types = c_types_host)

# loops
# roster
data_nms_r_roster <- names(readxl::read_excel(path = loc_data_host, n_max = 300, sheet = "hh_roster"))
c_types_r_roster <- ifelse(str_detect(string = data_nms_r_roster, pattern = "_other$"), "text", "guess")
df_loop_r_roster <- readxl::read_excel(loc_data_host, col_types = c_types_r_roster, sheet = "hh_roster")
# income
data_nms_r_income <- names(readxl::read_excel(path = loc_data_host, n_max = 300, sheet = "grp_income_received"))
c_types_r_income <- ifelse(str_detect(string = data_nms_r_income, pattern = "_other$"), "text", "guess")
df_loop_r_income <- readxl::read_excel(loc_data_host, col_types = c_types_r_income, sheet = "grp_income_received")

# tool
loc_tool_host <- "inputs/UGA2402_aba_mbarara_host_tool.xlsx"
df_survey_host <- readxl::read_excel(loc_tool_host, sheet = "survey")
df_choices_host <- readxl::read_excel(loc_tool_host, sheet = "choices")

df_filled_cl_host <- readxl::read_excel("inputs/combined_checks_aba_mbarara_host.xlsx", sheet = "cleaning_log") %>% 
    filter(!is.na(reviewed), !question %in% c("_index"), !uuid %in% c("all"))

# check pii ---------------------------------------------------------------
pii_from_data_host <- cleaningtools::check_pii(dataset = df_tool_data_host, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data_host$potential_PII

# then determine wich columns to remove from both the raw and clean data
cols_to_remove_host <- c("audit", "audit_URL", "pt_num_msg", "pt_num_validation_message",
                         "pt_sample_lat", "pt_sample_lon", "dist_btn_sample_collected", 
                         "threshold_msg_2_positive", "threshold_msg_2_negative",
                         "telephone", "contact", "name", "gps", 
                         "latitude", "longitude", "contact", "geopoint",
                         "instance_name", "_geopoint_latitude", "_geopoint_longitude",
                         "_geopoint_altitude", "_geopoint_precision",
                         "phone_consent", "fgd_phone_number")



# Main dataset ------------------------------------------------------------

# filtered log
df_filled_cl_host_main <- df_filled_cl_host %>% 
    filter(is.na(sheet))

# updating the main dataset with new columns

df_data_with_added_cols_host <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_tool_data_host,
                                                               input_df_filled_cl = df_filled_cl_host_main, 
                                                               input_df_survey = df_survey_host,
                                                               input_df_choices = df_choices_host)

# check the cleaning log
df_cl_review_host <- cleaningtools::review_cleaning_log(
    raw_dataset = df_data_with_added_cols_host,
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_filled_cl_host_main,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_host <- df_filled_cl_host_main %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent"), 
           !uuid %in% c("all")) %>% 
    filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_host <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols_host %>% select(-any_of(cols_to_remove_host)) %>% 
        rename(`shelter_damage_issues/lack_of_space_inside_the_shelter_min_35m2_per_household_member` = `shelter_damage_issues/lack_of_space_inside_the_shelter_min_35m²_per_household_member`),
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_final_cleaning_log_host,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# handle parent question columns
df_updating_sm_parents_host <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step_host, 
                                                         input_location_col = "interview_cell")

# roster cleaning ---------------------------------------------------------

# then determine wich columns to remove from both the raw and clean data
cols_to_remove_host_roster <- c("name")
# filtered log
df_filled_cl_host_roster <- df_filled_cl_host %>% 
    filter(sheet %in% c("hh_roster", "grp_hh_roster"), !is.na(index))

# updating the main dataset with new columns

df_data_with_added_cols_host_roster <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_loop_r_roster,
                                                                      input_df_filled_cl = df_filled_cl_host_roster, 
                                                                      input_df_survey = df_survey_host,
                                                                      input_df_choices = df_choices_host)

# check the cleaning log
df_cl_review_host <- cleaningtools::review_cleaning_log(
    raw_dataset = df_data_with_added_cols_host_roster,
    raw_data_uuid_column = "_submission__uuid",
    cleaning_log = df_filled_cl_host_roster,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_host_roster <- df_filled_cl_host_roster %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                            "_index", "_parent_index"), 
           !uuid %in% c("all")) %>% 
    filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_host_roster <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols_host_roster %>% select(-any_of(cols_to_remove_host_roster)),
    raw_data_uuid_column = "_submission__uuid",
    cleaning_log = df_final_cleaning_log_host_roster,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value")

# handle parent question columns
df_updating_sm_parents_host_roster <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step_host_roster,
                                                                input_uuid_col = "_uuid",
                                                                input_enumerator_id_col = "enumerator_id",
                                                                input_point_id_col = "point_number",
                                                                input_collected_date_col = "today",
                                                                input_location_col = "interview_cell")



# income_received ---------------------------------------------------------




# export datasets ---------------------------------------------------------

list_of_datasets_host <- list("raw_data" = df_tool_data_host %>% select(-any_of(cols_to_remove_host)),
                              "raw_roster" = df_loop_r_roster %>% select(-any_of(cols_to_remove_host_roster)),
                              "cleaned_data" = df_updating_sm_parents_host$updated_sm_parents,
                              "cleaned_roster" = df_updating_sm_parents_host_roster$updated_sm_parents,
                              "extra_log_sm_parents" = df_updating_sm_parents_host$extra_log_sm_parents,
                              "extra_log_sm_parents_roster" = df_updating_sm_parents_host_roster$extra_log_sm_parents,
                              )

openxlsx::write.xlsx(list_of_datasets_host,
                     paste0("outputs/", butteR::date_file_prefix(), "_UGA2402_aba_mbarara_host_cleaned_data.xlsx"),
                     overwrite = TRUE)
