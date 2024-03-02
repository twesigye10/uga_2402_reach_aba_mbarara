library(tidyverse)
library(cleaningtools)
library(httr)
library(supporteR)

source("R/support_functions.R")
source("support_files/credentials.R")

# read data ---------------------------------------------------------------

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


# download audit files
download_audit_files(df = df_tool_data_refugee, 
                     uuid_column = "_uuid", 
                     audit_dir = "inputs/audit_files_refugee", 
                     usr = user_acc, 
                     pass = user_pss)
# zip audit files folder
if (dir.exists("inputs/audit_files_refugee")) {
    zip::zip(zipfile = "inputs/audit_files_refugee.zip", 
             files = list.dirs(path = "inputs/audit_files_refugee/", full.names = TRUE, recursive = FALSE),
             mode = "cherry-pick")
}


# cleaningtools checks ----------------------------------------------------


# check pii

pii_cols <- c("telephone","contact","name","gps","latitude","logitude","contact","geopoint")

pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data_refugee, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII

# duration
# read audit file
audit_list_data <- cleaningtools::create_audit_list(audit_zip_path = "inputs/audit_files_refugee.zip")
# add duration from audit
df_tool_data_with_audit_time <- cleaningtools::add_duration_from_audit(df_tool_data_refugee, uuid_column = "_uuid", audit_list = audit_list_data)

# outliers columns not to check
outlier_cols_not_4_checking <- df_tool_data_refugee %>% 
    select(matches("geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
    colnames()

# logical checks data
df_list_logical_checks_refugee <- read_csv("inputs/logical_checks_aba_mbarara_refugee.csv")

# combine cleaningtools checks
list_log_refugee <- df_tool_data_with_audit_time %>%
    check_pii(uuid_column = "_uuid") %>%
    check_duration(column_to_check = "duration_audit_sum_all_minutes",
                   uuid_column = "_uuid",
                   log_name = "duration_log",
                   lower_bound = 20,
                   higher_bound = 120) %>% 
    check_outliers(uuid_column = "_uuid", sm_separator = "/",
                   strongness_factor = 3, columns_not_to_check = outlier_cols_not_4_checking) %>% 
    check_soft_duplicates(kobo_survey = df_survey_refugee,
                          uuid_column = "_uuid",
                          idnk_value = "dk",
                          sm_separator = "/",
                          log_name = "soft_duplicate_log",
                          threshold = 7,
                          return_all_results = FALSE) %>%
    check_value(uuid_column = "_uuid", values_to_look = c(99, 999, 9999)) %>% 
    check_logical_with_list(uuid_column = "_uuid",
                            list_of_check = df_list_logical_checks_refugee,
                            check_id_column = "check_id",
                            check_to_perform_column = "check_to_perform",
                            columns_to_clean_column = "columns_to_clean",
                            description_column = "description",
                            bind_checks = TRUE )



# other checks ------------------------------------------------------------

df_other_checks_refugee <- cts_other_specify(input_tool_data = df_tool_data_refugee, 
                                                    input_uuid_col = "_uuid", 
                                                    input_survey = df_survey_refugee, 
                                                    input_choices = df_choices_refugee)
list_log_refugee$other_log <- df_other_checks_refugee

# other checks roster
df_other_checks_refugee_roster <- cts_other_specify_repeats(input_repeat_data = df_loop_r_roster, 
                                                    input_uuid_col = "_submission__uuid", 
                                                    input_survey = df_survey_refugee, 
                                                    input_choices = df_choices_refugee,
                                                    input_sheet_name = "hh_roster",
                                                    input_index_col = "_index")
list_log_refugee$other_log_roster <- df_other_checks_refugee_roster


# check duplicate uuids ---------------------------------------------------

df_duplicate_uuids <- cts_checks_duplicate_uuids(input_tool_data = df_tool_data_refugee)
list_log_refugee$duplicate_uuid_log <- df_duplicate_uuids


# FCS ---------------------------------------------------------------------

# values entered is the same across food groups
df_fcs_same_values <- df_tool_data_refugee %>%  
    filter(if_all(c(fcs_cereals, fcs_pulses, fcs_vegetables, fcs_fruits, fcs_condiments, 
                    fcs_protein, fcs_dairy, fcs_sugar, fcs_oils), ~ fcs_cereals == .x))  %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "cereals",
           i.check.old_value = as.character(fcs_cereals),
           i.check.new_value = "NA",
           i.check.issue = "Same fcs values",
           i.check.comment = "") %>% 
    slice(rep(1:n(), each = 9)) %>%  
    group_by(i.check.uuid, i.check.change_type,  i.check.question,  i.check.old_value) %>%  
    mutate(rank = row_number(),
           i.check.question = case_when(rank == 1 ~ "fcs_cereals", 
                                        rank == 2 ~ "fcs_pulses",
                                        rank == 3 ~ "fcs_vegetables", 
                                        rank == 4 ~ "fcs_fruits", 
                                        rank == 5 ~ "fcs_condiments", 
                                        rank == 6 ~ "fcs_protein", 
                                        rank == 7 ~ "fcs_dairy", 
                                        rank == 8 ~ "fcs_sugar", 
                                        TRUE ~ "oils"),
           i.check.old_value = case_when(rank == 1 ~ as.character(fcs_cereals),
                                         rank == 2 ~ as.character(fcs_pulses),
                                         rank == 3 ~ as.character(fcs_vegetables), 
                                         rank == 4 ~ as.character(fcs_fruits), 
                                         rank == 5 ~ as.character(fcs_condiments), 
                                         rank == 6 ~ as.character(fcs_protein), 
                                         rank == 7 ~ as.character(fcs_dairy), 
                                         rank == 8 ~ as.character(fcs_sugar), 
                                         TRUE ~ as.character(fcs_oils))
    ) %>% 
    supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

# add other checks to the list
list_log_refugee$fcs_same_values <- df_fcs_same_values



# silhouette --------------------------------------------------------------

# NOTE: the column for "col_admin" is kept in the data

omit_cols_sil <- c("start", "end", "today", "duration", "duration_minutes",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey",
                   "geopoint", "_geopoint_latitude", "_geopoint_altitude", "_geopoint_precision", "_id" ,"_submission_time","_validation_status","_notes","_status","_submitted_by","_tags","_index")

data_similartiy_sil <- df_tool_data_refugee %>% 
    select(- any_of(omit_cols_sil), - matches("_note$|^note_"))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey_refugee, 
                                             col_enum = "enumerator_id",
                                             col_admin = "interview_cell") %>% 
    mutate(si2= abs(si))

df_sil_processed <- df_sil_data[order(df_sil_data$`si2`, decreasing = TRUE),!colnames(df_sil_data)%in%"si2"] %>%  
    filter(si > 0.6) %>% 
    mutate(i.check.uuid = "all",
           i.check.question = NA_character_,
           i.check.issue = paste("Potential similar responses for enumerator. si: ",si)) %>% 
    batch_select_rename()

# add other checks to the list
list_log_refugee$enum_similarity <- df_sil_processed

# combine the checks ------------------------------------------------------

df_combined_log_refugee <- create_combined_log_keep_change_type(dataset_name = "checked_dataset", list_of_log = list_log_refugee)

# # add_info_to_cleaning_log()
# add_with_info_refugee <- add_info_to_cleaning_log(list_of_log = df_combined_log_refugee,
#                                           dataset = "checked_dataset",
#                                           cleaning_log = "cleaning_log",
#                                           dataset_uuid_column = "_uuid",
#                                           cleaning_log_uuid_column = "uuid",
#                                           information_to_add = c("enumerator_id", "today", "interview_cell")
# )
# 
# 
# # create_xlsx_cleaning_log()
# add_with_info_refugee |>
#     create_xlsx_cleaning_log(
#         kobo_survey = df_survey_refugee,
#         kobo_choices = df_choices_refugee,
#         use_dropdown = TRUE,
#         output_path = paste0("outputs/", butteR::date_file_prefix(), 
#                              "_combined_checks_aba_mbarara_refugee.xlsx")
#     )


# create workbook ---------------------------------------------------------
# prep data
cols_to_add_to_log <- c("enumerator_id", "point_number", "today", "interview_cell")

tool_support <- df_combined_log_refugee$checked_dataset %>% 
    select(uuid = `_uuid`, any_of(cols_to_add_to_log))

df_prep_checked_data_refugee <- df_combined_log_refugee$checked_dataset
df_prep_cleaning_log_refugee <- df_combined_log_refugee$cleaning_log %>%
    left_join(tool_support, by = "uuid") %>% 
    relocate(any_of(cols_to_add_to_log), .after = uuid)

df_prep_readme_refugee <- tibble::tribble(
    ~change_type_validation,                       ~description,
    "change_response", "Change the response to new_value",
    "blank_response",       "Remove and NA the response",
    "remove_survey",                "Delete the survey",
    "no_action",               "No action to take."
)

wb_log_refugee <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = F)

modifyBaseFont(wb = wb_log_refugee, fontSize = 11, fontName = "Arial Narrow")

addWorksheet(wb_log_refugee, sheetName="checked_dataset")
setColWidths(wb = wb_log_refugee, sheet = "checked_dataset", cols = 1:ncol(df_prep_checked_data_refugee), widths = 24.89)
writeDataTable(wb = wb_log_refugee, sheet = "checked_dataset", 
               x = df_prep_checked_data_refugee , 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log_refugee, "checked_dataset", firstActiveRow = 2, firstActiveCol = 2)


addWorksheet(wb_log_refugee, sheetName="cleaning_log")
setColWidths(wb = wb_log_refugee, sheet = "cleaning_log", cols = 1:ncol(df_prep_cleaning_log_refugee), widths = 24.89)
writeDataTable(wb = wb_log_refugee, sheet = "cleaning_log", 
               x = df_prep_cleaning_log_refugee , 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log_refugee, "cleaning_log", firstActiveRow = 2, firstActiveCol = 2)

addWorksheet(wb_log_refugee, sheetName="readme")
setColWidths(wb = wb_log_refugee, sheet = "readme", cols = 1:ncol(df_prep_readme_refugee), widths = 24.89)
writeDataTable(wb = wb_log_refugee, sheet = "readme", 
               x = df_prep_readme_refugee , 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log_refugee, "readme", firstActiveRow = 2, firstActiveCol = 2)

# openXL(wb_log_refugee)

saveWorkbook(wb_log_refugee, paste0("outputs/", butteR::date_file_prefix(),"_combined_checks_aba_mbarara_refugee.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_combined_checks_aba_mbarara_refugee.xlsx"))