library(tidyverse)
library(cleaningtools)
library(httr)
library(sf)
library(glue)
library(supporteR)
library(openxlsx)
library(cluster)

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

# joining roster loop to main shet
df_repeat_hh_roster_data <- df_tool_data_refugee %>% 
    left_join(df_loop_r_roster, by = c("_uuid" = "_submission__uuid"))


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

# GIS layer for samples
df_sample_data_refugee <- sf::st_read("inputs/UGA2402_aba_mbarara_refugee_host_samples.gpkg", quiet = TRUE) %>% 
    filter(status %in% c("refugee"))


# cleaningtools checks ----------------------------------------------------


# check pii

pii_cols <- c("telephone","contact","name","gps","latitude","logitude","contact","geopoint")

pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data_refugee, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII

# duration
# read audit file
audit_list_data <- cleaningtools::create_audit_list(audit_zip_path = "inputs/audit_files_refugee.zip")
# add duration from audit
df_tool_data_with_audit_time_refugee <- cleaningtools::add_duration_from_audit(df_tool_data_refugee, uuid_column = "_uuid", audit_list = audit_list_data)

# outliers columns not to check
outlier_cols_not_4_checking <- df_tool_data_refugee %>% 
    select(matches("geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
    colnames()

# logical checks data
df_list_logical_checks_refugee <- read_csv("inputs/logical_checks_aba_mbarara_refugee_overview.csv")%>% 
    filter(!is.na(check_id))

# combine cleaningtools checks
list_log_refugee <- df_tool_data_with_audit_time_refugee %>%
    # check_pii(uuid_column = "_uuid") %>%
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
                          threshold = 25,
                          return_all_results = FALSE) %>%
    check_value(uuid_column = "_uuid", values_to_look = c(99, 999, 9999))

# logical checks
df_main_plus_loop_logical_checks <- df_repeat_hh_roster_data %>%
    check_logical_with_list(uuid_column = "_uuid",
                            list_of_check = df_list_logical_checks_host,
                            check_id_column = "check_id",
                            check_to_perform_column = "check_to_perform",
                            columns_to_clean_column = "columns_to_clean",
                            description_column = "description",
                            bind_checks = TRUE )
list_log_refugee$logical_checks <- df_main_plus_loop_logical_checks$logical_all


# other logical checks ----------------------------------------------------
# respondent_data_check
df_respondent_data_check <- df_repeat_hh_roster_data %>% 
    group_by(`_uuid`) %>%
    mutate(int.hoh_bio = ifelse(respondent_gender == gender & respondent_age == age, "given", "not")) %>% 
    filter(!str_detect(string = paste(int.hoh_bio, collapse = ":"), pattern = "given")) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "respondent_age",
           i.check.old_value = as.character(respondent_age),
           i.check.new_value = "NA",
           i.check.issue = "respondent_age_not_in_hh_roster",
           i.check.description = glue("respondent_data : {respondent_age}, {respondent_gender}, details not given in the hh_roster"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "grp_hh_roster",
           i.check.index = `_index.y`) %>% 
    batch_select_rename()
list_log_refugee$respondent_data_inconsistencies <- df_respondent_data_check

# If they report one of their main source of income is Casual/seasonal labour/farming 
# but nobody in the HH is reported to have their employment status Self-employed OR 
# Income from own business these do not match.
df_main_income_source_crop_prodn <- df_repeat_hh_roster_data %>% 
    group_by(`_uuid`) %>%
    mutate(int.income_sources = paste(hh_main_income_sources, collapse = " , ")) %>% 
    filter(!is.na(occupation_status)) %>% 
    filter(!occupation_status %in% c("self_employed", "business_owner") & str_detect(string = int.income_sources, 
                                                                                     pattern = "crop_production|casual_or_seasonal_labour|livestock_farming")) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "occupation_status",
           i.check.old_value = as.character(occupation_status),
           i.check.new_value = "NA",
           i.check.issue = "main_income_source_crop_prodn_or_casual_labor",
           i.check.description = glue("occupation_status : {occupation_status}, but  hh_main_income_sources : {int.income_sources}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "grp_hh_roster",
           i.check.index = `_index.y`) %>% 
    batch_select_rename()
list_log_refugee$income_inconsistencies <- df_main_income_source_crop_prodn

# If they report one of their main source if income is Employment but nobody in the HH is reported to have their 
# employment status Self-employed (including casual labour) OR  Paid employee OR Student who also works these do not match.
df_main_income_source_employment <- df_repeat_hh_roster_data %>% 
    group_by(`_uuid`) %>%
    mutate(int.income_sources = paste(hh_main_income_sources, collapse = " , ")) %>% 
    filter(!is.na(occupation_status)) %>% 
    filter(!occupation_status %in% c("self_employed", "paid_employee", "unpaid_family_worker", "student_who_also_works")
           & str_detect(string = int.income_sources, pattern = "employment")) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "occupation_status",
           i.check.old_value = as.character(occupation_status),
           i.check.new_value = "NA",
           i.check.issue = "main_income_source_employment",
           i.check.description = glue("occupation_status : {occupation_status}, but  hh_main_income_sources: {int.income_sources}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "grp_hh_roster",
           i.check.index = `_index.y`) %>% 
    batch_select_rename()
list_log_refugee$income_source_employment <- df_main_income_source_employment

# If all HH members are unemployed but they state they don’t face any challenges finding employment.
df_livelihoods_barriers <- df_repeat_hh_roster_data %>% 
    group_by(`_uuid`) %>%
    mutate(int.livelihoods_barriers = paste(livelihoods_barriers_faced, collapse = " , ")) %>% 
    filter(!is.na(occupation_status)) %>% 
    filter(occupation_status %in% c("unpaid_family_worker ", "unemployed_has_worked_previously", "unemployed_never_worked_before")
           & str_detect(string = int.livelihoods_barriers, pattern = "no_particular_challenge_or_issue")) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "occupation_status",
           i.check.old_value = as.character(occupation_status),
           i.check.new_value = "NA",
           i.check.issue = "all_hh_members_unemployed",
           i.check.description = glue("occupation_status : {occupation_status}, but  hh_livelihood_barriers : {int.livelihoods_barriers}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "grp_hh_roster",
           i.check.index = `_index.y`) %>% 
    batch_select_rename()
list_log_refugee$livelihoods_barriers <- df_livelihoods_barriers

# If the HH has no children aged 0-23 months but they state it's their HH's priority need to have food for children aged 0-23 months.
df_no_child_under_24_months <- df_tool_data_refugee %>% 
    filter(!is.na(number_child_aged_0_23)) %>% 
    filter(number_child_aged_0_23 == 0 & str_detect(string = unmet_needs, 
                                                    pattern = "special_food_needs_of_your_children")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "unmet_needs_special_food_for_children",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  number_child_aged_0_23 : {number_child_aged_0_23}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$no_child_under_24_months <- df_no_child_under_24_months

#If the HH has no pregnant/lactating women but they state it's their HH's priority need to have food 
# specifically for pregnant/lactating women.
df_no_lactating_mother <- df_tool_data_refugee %>% 
    filter(!is.na(number_mother_lactating)) %>% 
    filter(number_mother_lactating == 0 & str_detect(string = unmet_needs, 
                                                     pattern = "special_food_needs_of_pregnant_and_lactating_women")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "unmet_needs_special_food_for_pregnant/lcatating_mother",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  number_mother_lactating : {number_mother_lactating}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$no_lactating_mother <- df_no_lactating_mother

#If they said they have no issues related to water access but then they report it as a HH priority need. 
df_access_water_problems <- df_tool_data_refugee %>% 
    filter(!is.na(access_water_problems)) %>% 
    filter(str_detect(string = unmet_needs, pattern = "water_needs") & str_detect(string = access_water_problems, 
                                                                                  pattern = "no_problem_related_to_access_to_water")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "no_problem_related_to_access_to_water",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  access_water_problems : {access_water_problems}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$access_water_problems <- df_access_water_problems

#If they said they have no issues related to sanitation access but then they report it as a HH priority need.
df_access_wash_latrine_problems <- df_tool_data_refugee %>% 
    filter(!is.na(wash_latrine_problems_faced)) %>% 
    filter(str_detect(string = unmet_needs, pattern = "sanitation_needs") & str_detect(string = wash_latrine_problems_faced, 
                                                                                       pattern = "no_problem")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "access_wash_latrine_problems",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  wash_latrine_problems_faced : {wash_latrine_problems_faced}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$access_wash_latrine_problems <- df_access_wash_latrine_problems

#If they said they have no unmet healthcare needs in the past 1 year but then they report it as a HH priority need.
df_access_health_needs <- df_tool_data_refugee %>% 
    filter(!is.na(unmet_health_need)) %>% 
    filter(str_detect(string = unmet_needs, pattern = "healthcare_needs") & str_detect(string = unmet_health_need,
                                                                                       pattern = "no_unmet_needs")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "unmet_health_needs",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  unmet_health_need: {unmet_health_need}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$access_health_needs <- df_access_health_needs

# If the HH has no children aged 0-23 months but they state it's their HH's priority need to have healthcare for children aged 0-23 months.
df_no_child_under_24_months_healthcare <- df_tool_data_refugee %>% 
    filter(!is.na(number_child_aged_0_23)) %>% 
    filter(number_child_aged_0_23 == 0 & str_detect(string = unmet_needs, 
                                                    pattern = "special_healthcare_needs_of_your_children")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "unmet_needs_special_healthcare_for_children",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  number_child_aged_0_23 : {number_child_aged_0_23}"),
           i.check.other_text = "",
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$no_child_under_24_months_healthcare <- df_no_child_under_24_months_healthcare

# If the HH has no pregnant/lactating women but they state it's their HH's priority need to have healthcare specifically for pregnant/lactating women.
df_no_lactating_mother_healthcare <- df_tool_data_refugee %>% 
    filter(!is.na(number_mother_lactating)) %>% 
    filter(number_mother_lactating == 0 & str_detect(string = unmet_needs, 
                                                     pattern = "special_healthcare_needs_of_pregnant_and_lactating_women")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "unmet_needs_special_healthcare_for_pregnat/lactating_mother",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  number_mother_lactating : {number_mother_lactating}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$no_lactating_mother_healthcare <- df_no_lactating_mother_healthcare

# If no HH member is a child but they report education for children as a HH priority need.
df_no_school_aged_child <- df_tool_data_refugee %>% 
    filter(!is.na(number_school_aged_child)) %>% 
    filter(number_school_aged_child == 0 & str_detect(string = unmet_needs, 
                                                      pattern = "education_needs_for_children")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "unmet_needs_education_for_children",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  number_school_aged_child : {number_school_aged_child}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$no_school_aged_child <- df_no_school_aged_child

# If they said they dont have valid documents to stay in Uganda but later they say all HH members have legal documents.
df_document_to_stay_in_uganda <- df_tool_data_refugee %>% 
    filter(!is.na(document_to_stay_in_uganda)) %>% 
    filter(document_to_stay_in_uganda == "no_legalvalid_document" & 
               hh_members_all_have_legal_docs == "yes_all_household_members") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "unmet_needs",
           i.check.old_value = as.character(unmet_needs),
           i.check.new_value = "NA",
           i.check.issue = "no_document_to_stay_in_uganda",
           i.check.description = glue("unmet_needs : {unmet_needs}, but  document_to_stay_in_uganda : {document_to_stay_in_uganda}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$document_to_stay_in_uganda <- df_document_to_stay_in_uganda

# If they werent in a settlement before Mbarara but they report one of their main food sources is food assistance is incorrect.
df_main_hh_source_of_food <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               main_hh_source_of_food == "food_assistance_from_ngos_wfp_unhcr") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "main_hh_source_of_food_is_assistance",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  main_hh_source_of_food : {main_hh_source_of_food}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$main_hh_source_of_food <- df_main_hh_source_of_food

# If they werent in a settlement before Mbarara but they report one of their main food sources is food assistance is incorrect.
df_second_hh_source_of_food <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               second_hh_source_of_food == "food_assistance_from_ngos_wfp_unhcr") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "second_hh_source_of_food_is_assistance",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  second_hh_source_of_food : {second_hh_source_of_food}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$second_hh_source_of_food <- df_second_hh_source_of_food

# If they werent in a settlement before Mbarara but they report one of their main food sources is food assistance is incorrect.
df_third_hh_source_of_food <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               third_hh_source_of_food == "food_assistance_from_ngos_wfp_unhcr") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "third_hh_source_of_food_is_assistance",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  third_hh_source_of_food : {third_hh_source_of_food}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$third_hh_source_of_food <- df_third_hh_source_of_food

# If their main income source is money from UN or NGOs but they report no aid was received is an issues.
df_hh_main_income_sources <- df_tool_data_refugee %>% 
    filter(hh_received_aid_past == "no" & str_detect(string = hh_main_income_sources, 
                                                     pattern = "un_agencies_or_ngos")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "hh_received_aid_past",
           i.check.old_value = as.character(hh_received_aid_past),
           i.check.new_value = "NA",
           i.check.issue = "hh_main_income_sources_is_aid",
           i.check.description = glue("hh_received_aid_past : {hh_received_aid_past}, but  hh_main_income_sources : {hh_main_income_sources}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$hh_main_income_sources <- df_hh_main_income_sources

# If they werent in a settlement before Mbarara but they report they received aid in a settlement flag it.
df_place_aid_was_received_nakivale <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               place_where_aid_was_received == "nakivale_settlement") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "place_where_aid_was_received_nakivale",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  place_where_aid_was_received : {place_where_aid_was_received}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$place_aid_was_received_nakivale <- df_place_aid_was_received_nakivale

# If they werent in a settlement before Mbarara but they report they received aid in a settlement flag it.
df_place_aid_was_received_oruchinga <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               place_where_aid_was_received == "oruchinga_settlement") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "place_where_aid_was_received_oruchinga",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  place_where_aid_was_received : {place_where_aid_was_received}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$place_aid_was_received_oruchinga <- df_place_aid_was_received_oruchinga

# If they werent in a settlement before Mbarara but they report they received aid in a settlement flag it.
df_place_aid_was_received_rwamwanja <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               place_where_aid_was_received == "rwamwanja_settlement") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "place_where_aid_was_received_rwamwanja",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  place_where_aid_was_received : {place_where_aid_was_received}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$place_aid_was_received_oruchinga <- df_place_aid_was_received_rwamwanja

# If they werent in a settlement before Mbarara but they report they received aid in a settlement flag it.
df_place_aid_was_received_kyaka <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               place_where_aid_was_received == "kyaka_ii_settlement") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "place_where_aid_was_received_kyaka",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  place_where_aid_was_received : {place_where_aid_was_received}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$place_aid_was_received_kyaka <- df_place_aid_was_received_kyaka

# If they werent in a settlement before Mbarara but they report they received aid in a settlement flag it.
df_place_aid_was_received_other_settlement <- df_tool_data_refugee %>% 
    filter(place_lived_before_mbarara == "in_my_home_country" & 
               place_where_aid_was_received == "other_settlement") %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "place_lived_before_mbarara",
           i.check.old_value = as.character(place_lived_before_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "place_where_aid_was_received_other_settlement",
           i.check.description = glue("place_lived_before_mbarara : {place_lived_before_mbarara}, but  place_where_aid_was_received : {place_where_aid_was_received}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$place_aid_was_received_other_settlement <- df_place_aid_was_received_other_settlement

# If they were displaced less then a year but they lived in Mbarara more then 1 year
df_hh_date_displaced_less_1year <- df_tool_data_refugee %>% 
    filter(hh_date_displaced > as_date("2023-03-01") &
           hh_time_stayed_in_mbarara %in% c("between_1_2_years", "between_3_5_years", "greater_than_5_years")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "hh_time_stayed_in_mbarara",
           i.check.old_value = as.character(hh_time_stayed_in_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "hh_date_displaced_less_1year",
           i.check.description = glue("hh_time_stayed_in_mbarara : {hh_time_stayed_in_mbarara}, but  hh_date_displaced : {hh_date_displaced}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$hh_date_displaced_less_1year <- df_hh_date_displaced_less_1year

# If they were displaced less then 2 years but they lived in Mbarara more then 3 years
df_hh_date_displaced_less_2year <- df_tool_data_refugee %>% 
    filter(hh_date_displaced > as_date("2022-03-01") &
               hh_time_stayed_in_mbarara %in% c("between_3_5_years", "greater_than_5_years")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "hh_time_stayed_in_mbarara",
           i.check.old_value = as.character(hh_time_stayed_in_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "hh_date_displaced_less_2year",
           i.check.description = glue("hh_time_stayed_in_mbarara : {hh_time_stayed_in_mbarara}, but  hh_date_displaced : {hh_date_displaced}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$hh_date_displaced_less_2year <- df_hh_date_displaced_less_2year

# If they were displaced less then 3 years but they lived in Mbarara more then 4 years
df_hh_date_displaced_less_3year <- df_tool_data_refugee %>% 
    filter(hh_date_displaced > as_date("2021-03-01") &
               hh_time_stayed_in_mbarara %in% c("greater_than_5_years")) %>% 
    mutate(i.check.uuid = `_uuid`,
           i.check.change_type = "change_response",
           i.check.question = "hh_time_stayed_in_mbarara",
           i.check.old_value = as.character(hh_time_stayed_in_mbarara),
           i.check.new_value = "NA",
           i.check.issue = "hh_date_displaced_less_2year",
           i.check.description = glue("hh_time_stayed_in_mbarara : {hh_time_stayed_in_mbarara}, but  hh_date_displaced : {hh_date_displaced}"),
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "",
           i.check.index = "") %>% 
    batch_select_rename()
list_log_refugee$hh_date_displaced_less_3year <- df_hh_date_displaced_less_3year


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

# loops outliers ----------------------------------------------------------

# roster
df_loop_outliers_roster_r <- cleaningtools::check_outliers(dataset = df_loop_r_roster  %>%  mutate(loop_uuid = paste0(`_submission__uuid`, " * ", `_index`)), 
                                                           uuid_column = "loop_uuid", strongness_factor = 3,
                                                           sm_separator = "/") 

df_potential_loop_outliers_roster_r <- df_loop_outliers_roster_r$potential_outliers %>% 
    separate_wider_delim(cols = uuid, delim = " * ", names = c("i.check.uuid", "index")) %>% 
    mutate(i.check.change_type = "change_response",
           i.check.question = question,
           i.check.old_value = as.character(old_value),
           i.check.new_value = "NA",
           i.check.issue = issue,
           i.check.description = "",
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "hh_roster",
           i.check.index = index) %>% 
    batch_select_rename()
list_log_refugee$outliers_roster_log_r <- df_potential_loop_outliers_roster_r

# income
df_loop_outliers_income_r <- cleaningtools::check_outliers(dataset = df_loop_r_income  %>%  mutate(loop_uuid = paste0(`_submission__uuid`, " * ", `_index`)), 
                                                           uuid_column = "loop_uuid", strongness_factor = 3,
                                                           sm_separator = "/") 

df_potential_loop_outliers_income_r <- df_loop_outliers_income_r$potential_outliers %>% 
    separate_wider_delim(cols = uuid, delim = " * ", names = c("i.check.uuid", "index")) %>% 
    mutate(i.check.change_type = "change_response",
           i.check.question = question,
           i.check.old_value = as.character(old_value),
           i.check.new_value = "NA",
           i.check.issue = issue,
           i.check.description = "",
           i.check.other_text = "",
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.so_sm_choices = "",
           i.check.sheet = "grp_income_received",
           i.check.index = index) %>% 
    batch_select_rename()
list_log_refugee$outliers_income_log_r <- df_potential_loop_outliers_income_r

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

list_log_refugee$fcs_same_values <- df_fcs_same_values


# spatial checks ----------------------------------------------------------

if("status" %in% colnames(df_sample_data_refugee) & "status" %in% colnames(df_tool_data_refugee)){
    sample_pt_nos_refugee <- df_sample_data_refugee %>%
        mutate(unique_pt_number = paste0(status, "_", Name)) %>%
        pull(unique_pt_number) %>%
        unique()
}else{
    sample_pt_nos_refugee <- df_sample_data_refugee %>%
        mutate(unique_pt_number = Name) %>%
        pull(unique_pt_number) %>%
        unique()
}

# duplicate point numbers
df_duplicate_pt_nos_refugee <- cts_check_duplicate_pt_numbers(input_tool_data = df_tool_data_refugee,
                                                              input_uuid_col  = "_uuid",
                                                              input_location_col = "interview_cell",
                                                              input_point_id_col = "point_number",
                                                              input_sample_pt_nos_list = sample_pt_nos_refugee)

list_log_refugee$duplicate_pt_nos <- df_duplicate_pt_nos_refugee

# point number does not exist in sample
df_pt_number_not_in_sample_refugee <- cts_check_pt_number_not_in_samples(input_tool_data = df_tool_data_refugee,
                                                                         input_uuid_col  = "_uuid",
                                                                         input_point_id_col = "point_number",
                                                                         input_sample_pt_nos_list = sample_pt_nos_refugee)
list_log_refugee$pt_number_not_in_sample <- df_pt_number_not_in_sample_refugee

# check for exceeded threshold distance
df_greater_thresh_distance_refugee <- cts_check_threshold_distance(input_sample_data = df_sample_data_refugee,
                                                                   input_tool_data = df_tool_data_refugee,
                                                                   input_uuid_col  = "_uuid",
                                                                   input_point_id_col = "point_number",
                                                                   input_threshold_dist = 150)
list_log_refugee$greater_thresh_distance <- df_greater_thresh_distance_refugee
    

# silhouette --------------------------------------------------------------

# NOTE: the column for "col_admin" is kept in the data

omit_cols_sil <- c("start", "end", "today", "duration", "duration_minutes",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey",
                   "geopoint", "_geopoint_latitude", "_geopoint_longitude","_geopoint_altitude", 
                   "_geopoint_precision", "_id" ,"_submission_time","_validation_status","_notes",
                   "_status","_submitted_by","_tags","_index", "__version__" )

data_similartiy_sil <- df_tool_data_refugee %>% 
    select(- any_of(omit_cols_sil), - matches("_note$|^note_"))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey_refugee, 
                                             col_enum = "enumerator_id",
                                             col_admin = "interview_cell") %>% 
    mutate(si2= abs(si))

df_sil_processed <- df_sil_data[order(df_sil_data$`si2`, decreasing = TRUE),!colnames(df_sil_data)%in%"si2"] %>%  
    # filter(si > 0.6) %>%
    mutate(i.check.uuid = "all",
           i.check.question = NA_character_,
           i.check.issue = "silhouette flag",
           i.check.description = glue::glue("Potential similar responses for enumerator:{enumerator_id}, interview_cell:{interview_cell}. si: {si}")) %>% 
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
    relocate(any_of(cols_to_add_to_log), .after = uuid) %>% 
    add_qn_label_to_cl(input_cl_name_col = "question",
                       input_tool = df_survey_refugee, 
                       input_tool_name_col = "name", 
                       input_tool_label_col = "label") %>% 
    mutate(enumerator_id = ifelse(issue %in% c("silhouette flag"), 
                                  str_replace(string = str_extract(string = description, pattern = "enumerator:[0-9]{1,3}"), pattern = "enumerator:", ""),
                                  enumerator_id),
           interview_cell = ifelse(issue %in% c("silhouette flag"), 
                                  str_replace(string = str_extract(string = description, pattern = "interview_cell:\\w+"), pattern = "interview_cell:", ""),
                                  interview_cell))

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

