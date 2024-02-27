library(tidyverse)
library(cleaningtools)
library(httr)
library(supporteR)

source("R/support_functions.R")
source("support_files/credentials.R")

# read data ---------------------------------------------------------------

loc_data_refugee <- "inputs/UGA2305_land_and_energy_data.xlsx"
df_tool_data_refugee <- readxl::read_excel(loc_data_refugee)

# tool
loc_tool_refugee <- "inputs/land_and_energy_tool.xlsx"
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

# check pii ---------------------------------------------------------------

pii_cols <- c("telephone","contact","name","gps","latitude","logitude","contact","geopoint")

pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data_refugee, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII

# duration ----------------------------------------------------------------
# read audit file
audit_list_data <- cleaningtools::create_audit_list(audit_zip_path = "inputs/audit_files_refugee.zip")
# add duration from audit
df_tool_data_with_audit_time <- cleaningtools::add_duration_from_audit(df_tool_data_refugee, uuid_column = "_uuid", audit_list = audit_list_data)


# Exporting the flags in excel --------------------------------------------

# outliers columns not to check
outlier_cols_not_4_checking <- df_tool_data_refugee %>% 
    select(matches("geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
    colnames()

# create_combined_log()
list_log <- df_tool_data_with_audit_time %>%
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
    check_value(uuid_column = "_uuid", values_to_look = c(666, 99, 999, 9999, 98, 88, 888, 8888))


# other checks

df_other_checks_refugee <- cts_format_other_specify(input_tool_data = df_tool_data_refugee, 
                                                    input_uuid_col = "_uuid", 
                                                    input_survey = df_survey_refugee, 
                                                    input_choices = df_choices_refugee)
# add other checks to the list
list_log$other_log <- df_other_checks

# silhouette

# similarity


# combine the checks ------------------------------------------------------

df_combined_log <- create_combined_log(dataset_name = "checked_dataset", list_of_log = list_log)

# add_info_to_cleaning_log()
add_with_info <- add_info_to_cleaning_log(list_of_log = df_combined_log,
                                          dataset = "checked_dataset",
                                          cleaning_log = "cleaning_log",
                                          dataset_uuid_column = "_uuid",
                                          cleaning_log_uuid_column = "uuid",
                                          information_to_add = c("meta_enumerator_id", "today")
)


# create_xlsx_cleaning_log()
add_with_info |>
    create_xlsx_cleaning_log(
        kobo_survey = df_survey,
        kobo_choices = df_choices,
        use_dropdown = TRUE,
        output_path = paste0("outputs/", butteR::date_file_prefix(), 
                             "_combined_checks_aba_mbarara.xlsx")
    )
