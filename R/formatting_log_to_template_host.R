library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)
library(openxlsx)

options("openxlsx.borderStyle" = "thin")

# Read data and checking log

# log
log_path_host <- "inputs/combined_checks_aba_mbarara_host.xlsx"
log_data_nms_host <- names(readxl::read_excel(path = log_path_host, n_max = 2000))
log_c_types_host <- case_when(str_detect(string = log_data_nms_host, pattern = "sheet|new_value|other_text|enumerator_id") ~ "text",
                         str_detect(string = log_data_nms_host, pattern = "index") ~ "numeric",
                         TRUE ~ "guess")
df_cleaning_log_data_host <- readxl::read_excel(log_path_host, col_types = log_c_types_host) %>%  
    filter(reviewed %in% c("1")) %>% 
    mutate(comment = case_when(issue %in% c("Less than 25 differents options") ~ glue("num_cols_not_NA: {num_cols_not_NA}, number_different_columns: {number_different_columns}, Final comment: {comment}"), 
                               issue %in% c("silhouette flag") ~ glue("Description: {description}, Final comment: {comment}"),
                               TRUE ~ comment))
log_path_sm_parents_host <- "inputs/extra_sm_parent_changes_checks_aba_mbarara_host.xlsx"
log_data_nms_sm_parents_host <- names(readxl::read_excel(path = log_path_sm_parents_host, n_max = 2000, sheet = "extra_log_sm_parents"))
log_c_types_sm_parents_host <- case_when(str_detect(string = log_data_nms_sm_parents_host, pattern = "sheet|new_value|other_text|enumerator_id") ~ "text",
                                    str_detect(string = log_data_nms_sm_parents_host, pattern = "index|reviewed") ~ "numeric",
                                    str_detect(string = log_data_nms_sm_parents_host, pattern = "today") ~ "date",
                                    TRUE ~ "guess")
df_cleaning_log_sm_parents_host <- readxl::read_excel(log_path_sm_parents_host, col_types = log_c_types_sm_parents_host, sheet = "extra_log_sm_parents") %>%  
    filter(reviewed %in% c("1"))

# df_cleaning_log_sm_parents_host_roster
log_data_nms_sm_parents_roster_host <- names(readxl::read_excel(path = log_path_sm_parents_host, n_max = 2000, sheet = "extra_log_sm_parents_roster"))
log_c_types_sm_parents_roster_host <- case_when(str_detect(string = log_data_nms_sm_parents_roster_host, pattern = "sheet|new_value|other_text|enumerator_id") ~ "text",
                                         str_detect(string = log_data_nms_sm_parents_roster_host, pattern = "index|reviewed") ~ "numeric",
                                         str_detect(string = log_data_nms_sm_parents_roster_host, pattern = "today") ~ "date",
                                         TRUE ~ "guess")
df_cleaning_log_sm_parents_roster_host <- readxl::read_excel(log_path_sm_parents_host, col_types = log_c_types_sm_parents_roster_host, sheet = "extra_log_sm_parents_roster") %>%  
    filter(reviewed %in% c("1"))

# prepare seperate logs for the different data sheets
df_cleaning_log_host <- bind_rows(df_cleaning_log_data_host %>% filter(is.na(sheet)), 
                                  df_cleaning_log_sm_parents_host)
df_cleaning_log_host_roster <- bind_rows(df_cleaning_log_data_host %>% filter(sheet %in% c("hh_roster", "grp_hh_roster"), !is.na(index)), 
                                         df_cleaning_log_sm_parents_roster_host)
df_cleaning_log_host_income <- df_cleaning_log_data_host %>% filter(sheet %in% c("grp_income_received"), !is.na(index))

# raw data
loc_data_host <- "inputs/UGA2402_aba_mbarara_host_data.xlsx"

# main data
data_nms_host <- names(readxl::read_excel(path = loc_data_host, n_max = 300))
c_types_host <- ifelse(str_detect(string = data_nms_host, pattern = "_other$"), "text", "guess")
df_tool_data_host <- readxl::read_excel(loc_data_host, col_types = c_types_host)

# loops
# roster
data_nms_r_roster_host <- names(readxl::read_excel(path = loc_data_host, n_max = 300, sheet = "hh_roster"))
c_types_r_roster_host <- ifelse(str_detect(string = data_nms_r_roster_host, pattern = "_other$"), "text", "guess")
df_loop_r_roster_host <- readxl::read_excel(loc_data_host, col_types = c_types_r_roster_host, sheet = "hh_roster")
# income
data_nms_r_income_host <- names(readxl::read_excel(path = loc_data_host, n_max = 300, sheet = "grp_income_received"))
c_types_r_income_host <- ifelse(str_detect(string = data_nms_r_income_host, pattern = "_other$"), "text", "guess")
df_loop_r_income_host <- readxl::read_excel(loc_data_host, col_types = c_types_r_income_host, sheet = "grp_income_received")

# tool
loc_tool_host <- "inputs/UGA2402_aba_mbarara_host_tool.xlsx"
df_survey_host <- readxl::read_excel(loc_tool_host, sheet = "survey")
df_choices_host <- readxl::read_excel(loc_tool_host, sheet = "choices")

# filter the log for deletion and remaining entries -----------------------

df_log_del_confirmed_host <- df_cleaning_log_host %>% 
    filter(change_type %in% c("remove_survey"))

df_cleaning_log_host_updated <- df_cleaning_log_host %>% 
    filter(!uuid %in% df_log_del_confirmed_host$uuid)

df_cleaning_log_host_updated_roster <- df_cleaning_log_host_roster %>% 
    filter(!uuid %in% df_log_del_confirmed_host$uuid)

df_cleaning_log_host_updated_income <- df_cleaning_log_host_income %>% 
    filter(!uuid %in% df_log_del_confirmed_host$uuid)

# create variable summary -------------------------------------------------
# also need to add composite indicators
# need to determine new choices added and how many entries were affected
df_variable_summary_host <- df_survey_host %>%  
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) %>%  
    mutate(variable = name, action = "checked", description = "", observations_affected = "") %>%  
    select(variable, action, description, observations_affected)

# extract data ------------------------------------------------------------
df_data_extract_host <- df_tool_data_host %>%  
    mutate(`enumerator ID` = enumerator_id) %>% 
    select(uuid = `_uuid`, `enumerator ID`)

# log ---------------------------------------------------------------------

df_formatted_log_host <- df_cleaning_log_host_updated %>%  
    mutate(int.adjust_log = ifelse(change_type %in% c("no_action"), "no", "yes"),
           `enumerator ID` = enumerator_id, 
           question.name = question, 
           Issue = issue, 
           `Type of Issue` = change_type, 
           feedback = comment, 
           changed = int.adjust_log, 
           old.value = old_value, 
           new.value = new_value,
           new.value = ifelse(changed %in% c("no"), old.value, new.value)) %>%  
    select(uuid, `enumerator ID`, question.name, Issue, `Type of Issue`, 
           feedback, changed, old.value, new.value)

df_formatted_log_host_roster <- df_cleaning_log_host_updated_roster %>%  
    mutate(int.adjust_log = ifelse(change_type %in% c("no_action"), "no", "yes"),
           `enumerator ID` = enumerator_id, 
           question.name = question, 
           Issue = issue, 
           `Type of Issue` = change_type, 
           feedback = comment, 
           changed = int.adjust_log, 
           old.value = old_value, 
           new.value = new_value,
           new.value = ifelse(changed %in% c("no"), old.value, new.value)) %>%  
    select(uuid, `enumerator ID`, question.name, Issue, `Type of Issue`, 
           feedback, changed, old.value, new.value, sheet, index)

df_formatted_log_host_income <- df_cleaning_log_host_updated_income %>%  
    mutate(int.adjust_log = ifelse(change_type %in% c("no_action"), "no", "yes"),
           `enumerator ID` = enumerator_id, 
           question.name = question, 
           Issue = issue, 
           `Type of Issue` = change_type, 
           feedback = comment, 
           changed = int.adjust_log, 
           old.value = old_value, 
           new.value = new_value,
           new.value = ifelse(changed %in% c("no"), old.value, new.value)) %>%  
    select(uuid, `enumerator ID`, question.name, Issue, `Type of Issue`, 
           feedback, changed, old.value, new.value, sheet, index)

# deletion log ------------------------------------------------------------

df_deletion_log_host <- df_log_del_confirmed_host %>%  
    group_by(uuid) %>%  
    filter(row_number() == 1) %>%  
    ungroup() %>%  
    select(uuid, `enumerator ID` = enumerator_id, Issue = issue, `Type of Issue (Select from dropdown list)` = change_type, 
           feedback = comment)

# enumerator performance --------------------------------------------------
# Number of surveys collected by enumerators
df_surveys_by_enum_host <- df_tool_data_host %>%  
    group_by(enumerator_id) %>%  
    summarise(Number = n())
# Number of changes by enumerators
df_changes_by_enum_host <- df_cleaning_log_host_updated %>%  
    filter(!change_type %in% c("no_action")) %>%  
    group_by(enumerator_id) %>%  
    summarise(Number = n())
# Number of changes by enumerators filtered by issues
df_changes_by_enum_host_issue <- df_cleaning_log_host_updated %>%  
    filter(!change_type %in% c("no_action")) %>%  
    group_by(enumerator_id, issue) %>%  
    summarise(Number = n())
# Number of deletions by enumerators
df_deletion_by_enum_host <- df_deletion_log_host %>%  
    group_by(uuid) %>%  
    filter(row_number() == 1) %>%  
    ungroup() %>%  
    mutate(enumerator_id = `enumerator ID`) %>% 
    group_by(enumerator_id) %>%  
    summarise(Number = n())
# Number of deletions due to time by enumerator
df_deletion_by_enum_host_time <- df_deletion_log_host %>%  
    filter(Issue %in% c("Duration is lower or higher than the thresholds")) %>%  
    group_by(uuid) %>%  
    filter(row_number() == 1) %>%  
    ungroup() %>% 
    mutate(enumerator_id = `enumerator ID`) %>%
    group_by(enumerator_id) %>%  
    summarise(Number = n())


# format the logbook and export -------------------------------------------
df_variable_tracker <- tibble::tribble(
    ~Variable,   ~Action,                                      ~Rationale,
    "deviceid", "Removed",    "Blanked columns related to the survey and PII",
    "audit", "Removed",    "Blanked columns related to the survey and PII",
    "audit_URL", "Removed",    "Blanked columns related to the survey and PII",
    "instance_name", "Removed",    "Blanked columns related to the survey and PII",
    "geopoint", "Removed",    "Blanked columns related to the survey and PII",
    "_geopoint_latitude", "Removed",    "Blanked columns related to the survey and PII",
    "_geopoint_longitude", "Removed",    "Blanked columns related to the survey and PII",
    "_geopoint_altitude", "Removed",    "Blanked columns related to the survey and PII",
    "_geopoint_precision", "Removed",    "Blanked columns related to the survey and PII",
    "phone_consent", "Removed",    "Blanked columns related to the survey and PII",
    "fgd_phone_number", "Removed",    "Blanked columns related to the survey and PII",
    "how_participates_in_decision_making", "Removed", "Can not analyse free text",
    )


# create workbook ---------------------------------------------------------

wb_log_host <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = T)
hs2 <- createStyle(fgFill = "#C4BD97", textDecoration = "Bold", fontName = "Roboto Condensed", fontColour = "white", fontSize = 11, wrapText = T)
hs3 <- createStyle(fgFill = "#D8E4BC", textDecoration = "Bold", fontName = "Roboto Condensed", fontSize = 11, wrapText = T)
hs4 <- createStyle(fgFill = "#D9D9D9", textDecoration = "Bold", fontName = "Roboto Condensed", fontSize = 11, wrapText = T)


# deletion_log ------------------------------------------------------------

addWorksheet(wb_log_host, sheetName="deletion_log")
writeData(wb_log_host, sheet = "deletion_log", df_deletion_log_host, startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "deletion_log", hs1, rows = 1, cols = 1:6, gridExpand = FALSE)
setColWidths(wb = wb_log_host, sheet = "deletion_log", cols = 1, widths = 36)
setColWidths(wb = wb_log_host, sheet = "deletion_log", cols = 2, widths = 20)
setColWidths(wb = wb_log_host, sheet = "deletion_log", cols = 3, widths = 50)
setColWidths(wb = wb_log_host, sheet = "deletion_log", cols = 4, widths = 40)
setColWidths(wb = wb_log_host, sheet = "deletion_log", cols = 5, widths = 40)

# log book ----------------------------------------------------------------

addWorksheet(wb_log_host, sheetName="Log book")
writeData(wb_log_host, sheet = "Log book", df_formatted_log_host, startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "Log book", hs1, rows = 1, cols = 1:9, gridExpand = FALSE)
setColWidths(wb = wb_log_host, sheet = "Log book", cols = 1, widths = 36)
setColWidths(wb = wb_log_host, sheet = "Log book", cols = 2:9, widths = 20)

addWorksheet(wb_log_host, sheetName="Log book - roster")
writeData(wb_log_host, sheet = "Log book - roster", df_formatted_log_host_roster, startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "Log book - roster", hs1, rows = 1, cols = 1:11, gridExpand = FALSE)
setColWidths(wb = wb_log_host, sheet = "Log book - roster", cols = 1, widths = 36)
setColWidths(wb = wb_log_host, sheet = "Log book - roster", cols = 2:11, widths = 20)

addWorksheet(wb_log_host, sheetName="Log book - income")
writeData(wb_log_host, sheet = "Log book - income", df_formatted_log_host_income, startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "Log book - income", hs1, rows = 1, cols = 1:11, gridExpand = FALSE)
setColWidths(wb = wb_log_host, sheet = "Log book - income", cols = 1, widths = 36)
setColWidths(wb = wb_log_host, sheet = "Log book - income", cols = 2:11, widths = 20)


# data_extract ------------------------------------------------------------

addWorksheet(wb_log_host, sheetName="data_extract")
writeData(wb_log_host, sheet = "data_extract", df_data_extract_host, startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "data_extract", hs1, rows = 1, cols = 1:2, gridExpand = FALSE)
setColWidths(wb = wb_log_host, sheet = "data_extract", cols = 1, widths = 36)
setColWidths(wb = wb_log_host, sheet = "data_extract", cols = 2, widths = 20)


# variable_tracker --------------------------------------------------------

addWorksheet(wb_log_host, sheetName="variable_tracker")
writeData(wb_log_host, sheet = "variable_tracker", df_variable_tracker, startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "variable_tracker", hs1, rows = 1, cols = 1:3, gridExpand = FALSE)
setColWidths(wb = wb_log_host, sheet = "variable_tracker", cols = 1:3, widths = 25)

# Enumerator - performance ------------------------------------------------

addWorksheet(wb_log_host, sheetName="Enumerator - performance")

setColWidths(wb = wb_log_host, sheet = "Enumerator - performance", cols = 1, widths = 14)
setColWidths(wb = wb_log_host, sheet = "Enumerator - performance", cols = 5, widths = 14)
setColWidths(wb = wb_log_host, sheet = "Enumerator - performance", cols = 9, widths = 14)
setColWidths(wb = wb_log_host, sheet = "Enumerator - performance", cols = 14, widths = 14)
setColWidths(wb = wb_log_host, sheet = "Enumerator - performance", cols = 18, widths = 14)
setColWidths(wb = wb_log_host, sheet = "Enumerator - performance", cols = 22, widths = 14)

# Dataset
writeData(wb_log_host, sheet = "Enumerator - performance", "Dataset", startRow = 1, startCol = 1)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs2, rows = 1, cols = 1:2, gridExpand = FALSE)

mergeCells(wb_log_host, sheet = "Enumerator - performance", rows = 3:4, cols = 1:2)
writeData(wb_log_host, sheet = "Enumerator - performance", "Number of surveys collected by enumerators", startRow = 3, startCol = 1)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 1:2, gridExpand = FALSE)

writeDataTable(wb = wb_log_host, sheet = "Enumerator - performance",
               x = df_surveys_by_enum_host ,
               startRow = 6, startCol = 1,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Cleaning log 1
writeData(wb_log_host, sheet = "Enumerator - performance", "Cleaning log", startRow = 1, startCol = 5)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs2, rows = 1, cols = 5:6, gridExpand = FALSE)

mergeCells(wb_log_host, sheet = "Enumerator - performance", rows = 3:4, cols = 5:6)
writeData(wb_log_host, sheet = "Enumerator - performance", "Number of changes by enumerators", startRow = 3, startCol = 5)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 5:6, gridExpand = FALSE)

writeDataTable(wb = wb_log_host, sheet = "Enumerator - performance",
               x = df_changes_by_enum_host ,
               startRow = 6, startCol = 5,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Cleaning log 2
writeData(wb_log_host, sheet = "Enumerator - performance", "Cleaning log", startRow = 1, startCol = 9)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs2, rows = 1, cols = 9:11, gridExpand = FALSE)

mergeCells(wb_log_host, sheet = "Enumerator - performance", rows = 3:4, cols = 9:11)
writeData(wb_log_host, sheet = "Enumerator - performance", "Number of changes by enumerators filtered by issues", startRow = 3, startCol = 9)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 9:10, gridExpand = FALSE)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 11, gridExpand = FALSE)

writeDataTable(wb = wb_log_host, sheet = "Enumerator - performance",
               x = df_changes_by_enum_host_issue ,
               startRow = 6, startCol = 9,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 1
writeData(wb_log_host, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 14)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs2, rows = 1, cols = 14:15, gridExpand = FALSE)

mergeCells(wb_log_host, sheet = "Enumerator - performance", rows = 3:4, cols = 14:15)
writeData(wb_log_host, sheet = "Enumerator - performance", "Number of deletions by enumerators", startRow = 3, startCol = 14)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 14:15, gridExpand = FALSE)

writeDataTable(wb = wb_log_host, sheet = "Enumerator - performance",
               x = df_deletion_by_enum_host ,
               startRow = 6, startCol = 14,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 2
writeData(wb_log_host, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 18)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs2, rows = 1, cols = 18:19, gridExpand = FALSE)

mergeCells(wb_log_host, sheet = "Enumerator - performance", rows = 3:4, cols = 18:19)
writeData(wb_log_host, sheet = "Enumerator - performance", "Number of deletions due to time by enumerator", startRow = 3, startCol = 18)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 18:19, gridExpand = FALSE)

writeDataTable(wb = wb_log_host, sheet = "Enumerator - performance",
               x = df_deletion_by_enum_host_time ,
               startRow = 6, startCol = 18,
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 3
writeData(wb_log_host, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 22)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs2, rows = 1, cols = 22:24, gridExpand = FALSE)

mergeCells(wb_log_host, sheet = "Enumerator - performance", rows = 3:4, cols = 22:24)
writeData(wb_log_host, sheet = "Enumerator - performance", "Needs to be reviewed by Research Manager / lead AO before submission to HQ", startRow = 3, startCol = 22)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 22:23, gridExpand = FALSE)
addStyle(wb_log_host, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 24, gridExpand = FALSE)

writeDataTable(wb = wb_log_host, sheet = "Enumerator - performance",
               x = df_deletion_by_enum_host %>%  filter(Number > 20) %>%
                   mutate(`issue(s) followed up in country y/n` = "Yes",
                          `further comments` = NA_character_) %>%  select(-Number),
               startRow = 6, startCol = 22,
               tableStyle = "TableStyleLight10", headerStyle = hs4)


saveWorkbook(wb_log_host, paste0("outputs/", butteR::date_file_prefix(),"_UGA2402_aba_mbarara_host_logbook.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_UGA2402_aba_mbarara_host_logbook.xlsx"))

# renamed file
saveWorkbook(wb_log_host, paste0("outputs/UGA2402_aba_mbarara_host_logbook.xlsx"), overwrite = TRUE)
