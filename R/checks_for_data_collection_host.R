library(tidyverse)
library(cleaningtools)
library(httr)
library(supporteR)

source("R/support_functions.R")
source("support_files/credentials.R")

# read data ---------------------------------------------------------------

loc_data_host <- "inputs/UGA2402_aba_mbarara_host_data.xlsx"
df_tool_data_host <- readxl::read_excel(loc_data_host)

# tool
loc_tool_host <- "inputs/UGA2402_aba_mbarara_host_tool.xlsx"
df_survey_host <- readxl::read_excel(loc_tool_host, sheet = "survey") 
df_choices_host <- readxl::read_excel(loc_tool_host, sheet = "choices")


# download audit files
download_audit_files(df = df_tool_data_host, 
                     uuid_column = "_uuid", 
                     audit_dir = "inputs/audit_files_host", 
                     usr = user_acc, 
                     pass = user_pss)
# zip audit files folder
if (dir.exists("inputs/audit_files_host")) {
    zip::zip(zipfile = "inputs/audit_files_host.zip", 
             files = list.dirs(path = "inputs/audit_files_host/", full.names = TRUE, recursive = FALSE),
             mode = "cherry-pick")
}

# check pii ---------------------------------------------------------------

pii_cols <- c("telephone","contact","name","gps","latitude","logitude","contact","geopoint")

pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data_host, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII

# duration ----------------------------------------------------------------
# read audit file
audit_list_data <- cleaningtools::create_audit_list(audit_zip_path = "inputs/audit_files_host.zip")
# add duration from audit
df_tool_data_with_audit_time <- cleaningtools::add_duration_from_audit(df_tool_data_host, uuid_column = "_uuid", audit_list = audit_list_data)


# Exporting the flags in excel --------------------------------------------

# outliers columns not to check
outlier_cols_not_4_checking <- df_tool_data_host %>% 
    select(matches("geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
    colnames()

# logical checks data
df_list_logical_checks_host <- read_csv("inputs/logical_checks_aba_mbarara_host.csv")

# create_combined_log()
list_log_host <- df_tool_data_with_audit_time %>%
    check_pii(uuid_column = "_uuid") %>%
    check_duration(column_to_check = "duration_audit_sum_all_minutes",
                   uuid_column = "_uuid",
                   log_name = "duration_log",
                   lower_bound = 20,
                   higher_bound = 120) %>% 
    check_outliers(uuid_column = "_uuid", sm_separator = "/",
                   strongness_factor = 3, columns_not_to_check = outlier_cols_not_4_checking) %>% 
    check_soft_duplicates(kobo_survey = df_survey_host,
                          uuid_column = "_uuid",
                          idnk_value = "dk",
                          sm_separator = "/",
                          log_name = "soft_duplicate_log",
                          threshold = 7,
                          return_all_results = FALSE) %>%
    check_value(uuid_column = "_uuid", values_to_look = c(666, 99, 999, 9999, 98, 88, 888, 8888)) %>% 
    check_logical_with_list(uuid_column = "_uuid",
                            list_of_check = df_list_logical_checks_host,
                            check_id_column = "check_id",
                            check_to_perform_column = "check_to_perform",
                            columns_to_clean_column = "columns_to_clean",
                            description_column = "description",
                            bind_checks = TRUE )


# other checks

df_other_checks_host <- cts_other_specify(input_tool_data = df_tool_data_host, 
                                                 input_uuid_col = "_uuid", 
                                                 input_survey = df_survey_host, 
                                                 input_choices = df_choices_host)
list_log_host$other_log <- df_other_checks_host

# check duplicate uuids

df_duplicate_uuids <- cts_checks_duplicate_uuids(input_tool_data = df_tool_data_host)
list_log_refugee$duplicate_uuid_log <- df_duplicate_uuids

# silhouette
# NOTE: the column for "col_admin" is kept in the data

omit_cols_sil <- c("start", "end", "today", "duration", "duration_minutes",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey",
                   "geopoint", "_geopoint_latitude", "_geopoint_altitude", "_geopoint_precision", "_id" ,"_submission_time","_validation_status","_notes","_status","_submitted_by","_tags","_index")

data_similartiy_sil <- df_tool_data_host %>% 
    select(- any_of(omit_cols_sil), - matches("_note$|^note_"))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey_host, 
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
list_log_host$enum_similarity <- df_sil_processed



# combine the checks ------------------------------------------------------

df_combined_log_host <- create_combined_log_keep_change_type(dataset_name = "checked_dataset", list_of_log = list_log_host)

# # add_info_to_cleaning_log()
# add_with_info_host <- add_info_to_cleaning_log(list_of_log = df_combined_log_host,
#                                                dataset = "checked_dataset",
#                                                cleaning_log = "cleaning_log",
#                                                dataset_uuid_column = "_uuid",
#                                                cleaning_log_uuid_column = "uuid",
#                                                information_to_add = c("enumerator_id", "today", "interview_cell")
# )
# 
# 
# # create_xlsx_cleaning_log()
# add_with_info_host |>
#     create_xlsx_cleaning_log(kobo_survey = df_survey_host,
#                              kobo_choices = df_choices_host,
#                              use_dropdown = TRUE,
#                              output_path = paste0("outputs/", butteR::date_file_prefix(), 
#                                                   "_combined_checks_aba_mbarara_host.xlsx")
#     )


# create workbook ---------------------------------------------------------
# prep data
cols_to_add_to_log <- c("enumerator_id", "point_number", "today", "interview_cell")

tool_support <- df_combined_log_host$checked_dataset %>% 
    select(uuid = `_uuid`, any_of(cols_to_add_to_log))

df_prep_checked_data_host <- df_combined_log_host$checked_dataset
df_prep_cleaning_log_host <- df_combined_log_host$cleaning_log %>%
    left_join(tool_support, by = "uuid") %>% 
    relocate(any_of(cols_to_add_to_log), .after = uuid)

df_prep_readme_host <- tibble::tribble(
    ~change_type_validation,                       ~description,
    "change_response", "Change the response to new_value",
    "blank_response",       "Remove and NA the response",
    "remove_survey",                "Delete the survey",
    "no_action",               "No action to take."
)


wb_log_host <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = F)

modifyBaseFont(wb = wb_log_host, fontSize = 11, fontName = "Arial Narrow")

addWorksheet(wb_log_host, sheetName="checked_dataset")
setColWidths(wb = wb_log_host, sheet = "checked_dataset", cols = 1:ncol(df_prep_checked_data_host), widths = 24.89)
writeDataTable(wb = wb_log_host, sheet = "checked_dataset", 
               x = df_prep_checked_data_host , 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log_host, "checked_dataset", firstActiveRow = 2, firstActiveCol = 2)


addWorksheet(wb_log_host, sheetName="cleaning_log")
setColWidths(wb = wb_log_host, sheet = "cleaning_log", cols = 1:ncol(df_prep_cleaning_log_host), widths = 24.89)
writeDataTable(wb = wb_log_host, sheet = "cleaning_log", 
               x = df_prep_cleaning_log_host , 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log_host, "cleaning_log", firstActiveRow = 2, firstActiveCol = 2)

addWorksheet(wb_log_host, sheetName="readme")
setColWidths(wb = wb_log_host, sheet = "readme", cols = 1:ncol(df_prep_readme_host), widths = 24.89)
writeDataTable(wb = wb_log_host, sheet = "readme", 
               x = df_prep_readme_host , 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)
# freeze pane
freezePane(wb = wb_log_host, "readme", firstActiveRow = 2, firstActiveCol = 2)

# openXL(wb_log_host)

saveWorkbook(wb_log_host, paste0("outputs/", butteR::date_file_prefix(),"_combined_checks_aba_mbarara_host.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_combined_checks_aba_mbarara_host.xlsx"))
