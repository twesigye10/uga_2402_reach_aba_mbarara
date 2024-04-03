library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)

loc_data_refugee <- "inputs/UGA2402_aba_mbarara_refugee_data.xlsx"

# main data
data_nms_refugee <- names(readxl::read_excel(path = loc_data_refugee, n_max = 300))
c_types_refugee <- ifelse(str_detect(string = data_nms_refugee, pattern = "_other$"), "text", "guess")
df_tool_data_refugee <- readxl::read_excel(loc_data_refugee, col_types = c_types_refugee)

# loops
# roster
data_nms_r_roster <- names(readxl::read_excel(path = loc_data_refugee, n_max = 300, sheet = "hh_roster"))
c_types_r_roster <- ifelse(str_detect(string = data_nms_r_roster, pattern = "_other$"), "text", "guess")
df_loop_r_roster <- readxl::read_excel(loc_data_refugee, col_types = c_types_r_roster, sheet = "hh_roster")
# income
data_nms_r_income <- names(readxl::read_excel(path = loc_data_refugee, n_max = 300, sheet = "grp_income_received"))
c_types_r_income <- ifelse(str_detect(string = data_nms_r_income, pattern = "_other$"), "text", "guess")
df_loop_r_income <- readxl::read_excel(loc_data_refugee, col_types = c_types_r_income, sheet = "grp_income_received")

# tool
loc_tool_refugee <- "inputs/UGA2402_aba_mbarara_refugee_tool.xlsx"
df_survey_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "survey")
df_choices_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "choices")

df_filled_cl_refugee <- readxl::read_excel("inputs/combined_checks_aba_mbarara_refugee.xlsx", sheet = "cleaning_log") %>% 
    filter(!is.na(reviewed), !question %in% c("_index"), !uuid %in% c("all"))

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
                            "_geopoint_altitude", "_geopoint_precision")


# updating the dataset with new columns -----------------------------------

# gather choice options based on unique choices list
df_grouped_choices_refugee<- df_choices_refugee %>%
    group_by(list_name) %>%
    summarise(choice_options = paste(name, collapse = " : "))

# get new name and choice pairs to add to the choices sheet
new_vars_sm_refugee <- df_filled_cl_refugee %>%
    filter(str_detect(string = question, pattern = "\\w+\\/+\\w+")) %>%
    filter(!str_detect(string = question, pattern = "other$"), change_type %in% c("change_response")) %>%
    mutate(int.new_value = str_replace_all(string = question, pattern = "\\w+\\/", replacement = ""),
           int.question = str_replace_all(string = question, pattern = "\\/+\\w+", replacement = "")) %>% 
    left_join(df_survey_refugee, by = c("int.question" = "name")) %>%
    filter(str_detect(string = type, pattern = "select_one|select one|select_multiple|select multiple")) %>%
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>%
    left_join(df_grouped_choices_refugee, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = int.new_value)) %>%
    select(question) %>%
    # distinct() %>% # to make sure there are no duplicates
    # arrange(question) %>% 
    group_by(question) %>% 
    summarise(n = n())

# write_csv(new_vars_sm_refugee, "outputs/test_new_choices.csv")

# handle when a question had not been answered
df_add_columns_to_data_refugee <- df_tool_data_refugee %>% 
    butteR:::mutate_batch(nm = new_vars_sm_refugee$question, value = NA_character_ ) # %>% 

# parent questions for select multiple
col_changes_parent_vars_sm_refugee <- new_vars_sm_refugee %>% 
    mutate(question = str_replace_all(string = question, pattern = "/.+", replacement = "")) %>% 
    pull(question) %>% 
    unique()

df_handle_sm_data_refugee <- df_add_columns_to_data_refugee

for (cur_sm_col in col_changes_parent_vars_sm_refugee) {
    df_updated_data_refugee <- df_handle_sm_data_refugee %>% 
        mutate(
            across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , 0, .)),
            across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA_integer_, .))
        )
    df_handle_sm_data_refugee <- df_updated_data_refugee
}

df_data_with_added_cols_refugee <- df_handle_sm_data_refugee



# create a clean data -----------------------------------------------------

# check the cleaning log
df_cl_review_refugee <- cleaningtools::review_cleaning_log(
    raw_dataset = df_data_with_added_cols_refugee,
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_filled_cl_refugee,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value"
)

# filter log for cleaning
df_final_cleaning_log_refugee <- df_filled_cl_refugee %>% 
    filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes"), !uuid %in% c("all")) %>% 
filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_refugee <- cleaningtools::create_clean_data(
    raw_dataset = df_data_with_added_cols_refugee %>% select(-any_of(cols_to_remove_refugee)),
    raw_data_uuid_column = "_uuid",
    cleaning_log = df_final_cleaning_log_refugee,
    cleaning_log_change_type_column = "change_type",
    change_response_value = "change_response",
    NA_response_value = "blank_response",
    no_change_value = "no_action",
    remove_survey_value = "remove_survey",
    cleaning_log_question_column = "question",
    cleaning_log_uuid_column = "uuid",
    cleaning_log_new_value_column = "new_value"
)

# handle parent question columns ------------------------------------------

# parent column names
sm_parent_cols_refugee <- df_cleaning_step_refugee %>% 
    select(contains("/")) %>% 
    colnames() %>% 
    str_replace_all(pattern = "’", replacement = "") %>% 
    str_replace_all(pattern = "\\/+\\w+", replacement = "") %>% 
    unique()

df_handle_parent_qn_data_refugee <- df_cleaning_step_refugee

for (cur_parent_sm_col in sm_parent_cols_refugee) {
    # test
    print(cur_parent_sm_col)
    
    df_updated_parent_qn_data_refugee <- df_handle_parent_qn_data_refugee %>% 
        mutate(across(.cols = contains(paste0(cur_parent_sm_col, "/")), 
                      .fns = ~ifelse(!is.na(!!sym(cur_parent_sm_col)) & .x == 1, 
                                     str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), 
                                     NA_character_),
                      .names = "int.{.col}"),
               across(.cols = contains(paste0(cur_parent_sm_col, "/")), 
                      .fns = ~ifelse(.x == 1 & !str_detect(string = !!sym(cur_parent_sm_col), pattern = str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = "")), 
                                     str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), 
                                     NA_character_),
                      .names = "check.extra.{.col}"),
               across(.cols = contains(paste0(cur_parent_sm_col, "/")), 
                      .fns = ~ifelse(.x == 0 & str_detect(string = !!sym(cur_parent_sm_col), pattern = str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = "")), 
                                     str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), 
                                     NA_character_),
                      .names = "check.removed.{.col}")
        ) %>% 
        unite(!!paste0("int.", cur_parent_sm_col), starts_with(glue::glue("int.{cur_parent_sm_col}/")), remove = FALSE, na.rm = TRUE, sep = " ") %>%
        unite(!!paste0("check.extra.", cur_parent_sm_col), starts_with(glue::glue("check.extra.{cur_parent_sm_col}/")), remove = FALSE, na.rm = TRUE, sep = " ") %>%
        unite(!!paste0("check.removed.", cur_parent_sm_col), starts_with(glue::glue("check.removed.{cur_parent_sm_col}/")), remove = FALSE, na.rm = TRUE, sep = " ") %>%
        mutate(!!paste0("check.old.", cur_parent_sm_col) := !!sym(cur_parent_sm_col),
               # !!paste0("check.remaining.", cur_parent_sm_col) := paste(str_extract_all(string = !!sym(cur_parent_sm_col), pattern = str_replace_all(string = !!sym(paste0("int.", cur_parent_sm_col)), pattern = " ", replacement = "|"), simplify = TRUE), collapse = " "),
               # !!paste0("check.remaining.", cur_parent_sm_col) := paste(unlist(str_match_all(string = !!sym(cur_parent_sm_col), pattern = str_replace_all(string = !!sym(paste0("int.", cur_parent_sm_col)), pattern = " ", replacement = "|"))), collapse = " "),
               "check.rem" := str_replace_all(string = !!sym(paste0("check.removed.", cur_parent_sm_col)), pattern = " ", replacement = "|"),
               !!paste0("check.remaining.", cur_parent_sm_col) := str_replace_all(string = !!sym(cur_parent_sm_col), pattern = check.rem, replacement = ""),
               !!cur_parent_sm_col := ifelse(!is.na(!!sym(cur_parent_sm_col)), !!sym(paste0("int.", cur_parent_sm_col)), !!sym(cur_parent_sm_col))) %>% 
        unite(!!paste0("check.final.", cur_parent_sm_col), c(!!sym(paste0("check.remaining.", cur_parent_sm_col)), !!sym(paste0("check.extra.", cur_parent_sm_col))), remove = FALSE, na.rm = TRUE, sep = " ")
    
    df_handle_parent_qn_data_refugee <- df_updated_parent_qn_data_refugee
}

df_updated_parent_cols_refugee <- df_handle_parent_qn_data_refugee

# output datasets

list_of_datasets_refugee <- list("raw_data" = df_tool_data_refugee %>% select(-any_of(cols_to_remove_refugee)),
                         # "cleaned_data" = df_updated_parent_cols_refugee %>% select(-starts_with("int."), -starts_with("check.old.")))
                         "cleaned_data" = df_updated_parent_cols_refugee)

openxlsx::write.xlsx(list_of_datasets_refugee, 
                     paste0("outputs/", butteR::date_file_prefix(), "_UGA2402_aba_mbarara_refugee_cleaned_data.xlsx"),
                     overwrite = TRUE)


# extra log for recreated select multiple ---------------------------------

df_log_parent_sm_cols_changes_refugee <- purrr::map_dfr(.x = sm_parent_cols_refugee, 
                                                .f = ~ {df_updated_parent_cols_refugee %>% 
                                                        dplyr::filter(!!sym(paste0("check.old.",.x)) != !!sym(.x)) %>% 
                                                        dplyr::mutate(i.check.uuid = `_uuid`,
                                                                      i.check.enumerator_id = enumerator_id,
                                                                      i.check.point_number = point_number,
                                                                      i.check.today = today,
                                                                      i.check.meta_village_name = meta_village_name,
                                                                      i.check.change_type = "change_response",
                                                                      i.check.question = .x,
                                                                      i.check.old_value = as.character(!!sym(paste0("check.old.",.x))),
                                                                      i.check.new_value = as.character(!!sym(.x)),
                                                                      i.check.issue = "changed parent sm column",
                                                                      i.check.description = "Parent column changed to match children columns",
                                                                      i.check.other_text = "",
                                                                      i.check.comment = "",
                                                                      i.check.reviewed = "1",
                                                                      i.check.so_sm_choices = "") %>%
                                                        dplyr::select(starts_with("i.check."))}) %>% 
    supporteR::batch_select_rename()


openxlsx::write.xlsx(df_log_parent_sm_cols_changes_refugee, 
                     paste0("outputs/", butteR::date_file_prefix(), 
                              "_extra_sm_parent_changes_checks_aba_mbarara_refugee.xlsx"))

