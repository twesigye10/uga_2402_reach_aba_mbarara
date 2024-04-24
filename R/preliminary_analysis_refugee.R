library(tidyverse)
library(srvyr)
library(supporteR)
library(analysistools)

source("R/composite_indicators.R")

# clean data

clean_loc_data_refugee <- "inputs/UGA2402_aba_mbarara_refugee_cleaned_data.xlsx"

# main data
clean_data_nms_refugee <- names(readxl::read_excel(path = clean_loc_data_refugee, n_max = 300, sheet = "cleaned_data"))
clean_c_types_refugee <- ifelse(str_detect(string = clean_data_nms_refugee, pattern = "_other$"), "text", "guess")
df_main_clean_data_refugee <- readxl::read_excel(clean_loc_data_refugee, col_types = clean_c_types_refugee, sheet = "cleaned_data")

# loops
# roster
clean_data_nms_r_roster_refugee <- names(readxl::read_excel(path = clean_loc_data_refugee, n_max = 300, sheet = "cleaned_roster"))
clean_c_types_r_roster_refugee <- ifelse(str_detect(string = clean_data_nms_r_roster_refugee, pattern = "_other$"), "text", "guess")
df_clean_loop_r_roster_refugee <- readxl::read_excel(clean_loc_data_refugee, col_types = clean_c_types_r_roster_refugee, sheet = "cleaned_roster")
# income
clean_data_nms_r_income_refugee <- names(readxl::read_excel(path = clean_loc_data_refugee, n_max = 300, sheet = "cleaned_income_received"))
clean_c_types_r_income_refugee <- ifelse(str_detect(string = clean_data_nms_r_income_refugee, pattern = "_other$"), "text", "guess")
df_clean_loop_r_income_refugee <- readxl::read_excel(clean_loc_data_refugee, col_types = clean_c_types_r_income_refugee, sheet = "cleaned_income_received")

# tool
loc_tool_refugee <- "inputs/UGA2402_aba_mbarara_refugee_tool.xlsx"
df_survey_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "survey")
df_choices_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "choices")

# loa // list of analysis
all_loa_refugee <- read_csv("inputs/r_loa_aba_mbarara_refugee.csv")

# individual to hh level refugee ------------------------------------------

list_individual_to_hh_refugee <- list()

# dependency ratio

df_data_dependency_refugee <- df_clean_loop_r_roster_refugee %>% 
    mutate(int.dependency_cat = ifelse(age < 15 | age > 65, "dependant", "non_dependant"),
           int.dependant = ifelse(int.dependency_cat %in% c("dependant"), 1, 0),
           int.non_dependant = ifelse(int.dependency_cat %in% c("non_dependant"), 1, 0)) %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(i.n_dependant = sum(int.dependant, na.rm = TRUE),
              i.n_non_dependant = sum(int.non_dependant, na.rm = TRUE)) 
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_data_dependency_refugee")

# member_hoh_by_gender
df_member_hoh_by_gender_refugee <- df_clean_loop_r_roster_refugee %>% 
    filter(member_hoh %in% c("yes")) %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    select(`_submission__uuid`, i.member_hoh_by_gender = gender, i.member_hoh_age = age, i.hoh_education_level = hoh_education_level)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_member_hoh_by_gender_refugee")

# hh_with_disabled_member
df_hh_with_disabled_member_refugee <- df_clean_loop_r_roster_refugee %>% 
    create_composites_loop_roster_refugee() %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(int.hh_disability_status = paste(i.disability_prevalence, collapse = " : ")) %>% 
    mutate(i.hh_with_disabled_member = case_when(str_detect(string = int.hh_disability_status, pattern = "yes_disability") ~ "yes_disability",
                                                 !str_detect(string = int.hh_disability_status, pattern = "yes_disability") ~ "no_disability")) %>% 
    select(-int.hh_disability_status)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_hh_with_disabled_member_refugee")

# hoh_disability
df_hoh_disability <- df_clean_loop_r_roster_refugee %>% 
    filter(member_hoh %in% c("yes")) %>% 
    create_composites_loop_roster_refugee() %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    mutate(i.hoh_disability = case_when(i.disability_prevalence %in% c("yes_disability") ~ "yes_disability",
                                        i.disability_prevalence %in% c("no_disability") ~ "no_disability")) %>% 
    select(`_submission__uuid`, i.hoh_disability)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_hoh_disability")

# female_hoh_single_parent
df_female_hoh_single_parent_refugee <- df_clean_loop_r_roster_refugee %>% 
    filter(member_hoh %in% c("yes"), gender %in% c("female")) %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    mutate(i.female_hoh_single_parent = female_hoh_single_parent) %>% 
    select(`_submission__uuid`, i.female_hoh_single_parent)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_female_hoh_single_parent_refugee")

# lactating_mother
df_hh_with_lactating_mother_refugee <- df_clean_loop_r_roster_refugee %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(int.hh_lactating_mother = paste(lactating_mother, collapse = " : "),
              i.num_pregnant_lactating = sum(lactating_mother %in% c("yes"), na.rm = TRUE)) %>% 
    mutate(i.hh_with_lactating_mother = case_when(str_detect(string = int.hh_lactating_mother, pattern = "yes") ~ "yes",
                                                  !str_detect(string = int.hh_lactating_mother, pattern = "yes") ~ "no")) %>% 
    select(-int.hh_lactating_mother)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_hh_with_lactating_mother_refugee")

# unaccompanied_separated_or_orphan
df_hh_with_unaccompanied_separated_or_orphan_refugee <- df_clean_loop_r_roster_refugee %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(int.unaccompanied_separated_or_orphan = paste(unaccompanied_separated_or_orphan, collapse = " : ")) %>% 
    mutate(i.hh_with_unaccompanied_separated_or_orphan = case_when(str_detect(string = int.unaccompanied_separated_or_orphan, pattern = "yes") ~ "yes",
                                                                   !str_detect(string = int.unaccompanied_separated_or_orphan, pattern = "yes") ~ "no")) %>% 
    select(-int.unaccompanied_separated_or_orphan)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_hh_with_unaccompanied_separated_or_orphan_refugee")

# number of children less than 1
df_num_children_less_than_one <- df_clean_loop_r_roster_refugee %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(i.num_children_less_than_one = sum(age %in% c("0"), na.rm = TRUE))
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_num_children_less_than_one")



# combine the calculated indicators

df_combined_hh_indicators_from_roster_refugee <- list_individual_to_hh_refugee %>%
    reduce(.f = full_join, by = '_submission__uuid')


# data with composites ----------------------------------------------------

# main data with composites
df_data_with_composites_refugee <- df_main_clean_data_refugee %>% 
    left_join(df_combined_hh_indicators_from_roster_refugee, by = c("_uuid" = "_submission__uuid")) %>% 
    create_composites_main_refugee() %>%
    mutate(strata = paste0("refugee_", interview_cell)) %>% 
    filter(!interview_cell %in% c("kyamugolanyi"))
    
# roster
df_clean_loop_r_roster_with_composites_refugee <- df_clean_loop_r_roster_refugee %>% 
    create_composites_loop_roster_refugee()

# refugee analysis - main -------------------------------------------------

# main
df_main_ref <- df_data_with_composites_refugee
# survey object
main_ref_svy <- as_survey(.data = df_main_ref, strata = strata)

# loa
df_main_loa <- all_loa_refugee %>% 
    filter(dataset %in% c("main_data"))

# analysis
df_main_analysis_refugee <- analysistools::create_analysis(design = main_ref_svy, 
                                                      loa = df_main_loa,
                                                      sm_separator = "/")


# refugee analysis - roster -----------------------------------------------

# roster
df_roster_ref <- df_clean_loop_r_roster_with_composites_refugee

# survey object
roster_ref_svy <- as_survey(.data = df_roster_ref)

# loa roster
df_roster_loa <- all_loa_refugee %>% 
    filter(dataset %in% c("roster"))

# analysis
df_roster_analysis_refugee <- analysistools::create_analysis(design = roster_ref_svy, 
                                                             loa = df_roster_loa,
                                                             sm_separator = "/")

# refugee analysis - income -----------------------------------------------

# income received
df_income_ref <- df_clean_loop_r_income_refugee %>% 
    filter(!is.na(income_post))
    
# survey object - income received
income_ref_svy <- as_survey(.data = df_income_ref)

# loa income received
df_income_loa <- all_loa_refugee %>% 
    filter(dataset %in% c("income_received"))

# analysis
df_income_analysis_refugee <- analysistools::create_analysis(design = income_ref_svy, 
                                                             loa = df_income_loa,
                                                             sm_separator = "/")


# analysis tables ---------------------------------------------------------

# combine the tables

df_combined_tables <- bind_rows(df_main_analysis_refugee$results_table,
                                df_roster_analysis_refugee$results_table,
                                df_income_analysis_refugee$results_table
                                )
    
df_refugee_analysis_table <- presentresults::create_table_variable_x_group(results_table = df_combined_tables) %>% 
    filter(!(analysis_type %in% c("prop_select_one", "prop_select_multiple") & (is.na(analysis_var_value) | analysis_var_value %in% c("NA"))))

presentresults::create_xlsx_variable_x_group(table_group_x_variable = df_refugee_analysis_table,
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_refugee.xlsx"),
                                             table_sheet_name = "refugee", overwrite = TRUE
)
    
# presentresults::create_xlsx_variable_x_group(table_group_x_variable = presentresults::create_table_variable_x_group(results_table = df_main_analysis_refugee$results_table),
#                                              file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_refugee_main.xlsx"),
#                                              table_sheet_name = "refugee_main", overwrite = TRUE
# )
#     
# presentresults::create_xlsx_variable_x_group(table_group_x_variable = presentresults::create_table_variable_x_group(results_table = df_roster_analysis_refugee$results_table),
#                                              file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_refugee_roster.xlsx"),
#                                              table_sheet_name = "refugee_roster", overwrite = TRUE
# )
#     
# presentresults::create_xlsx_variable_x_group(table_group_x_variable = presentresults::create_table_variable_x_group(results_table = df_income_analysis_refugee$results_table),
#                                              file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_refugee_income.xlsx"),
#                                              table_sheet_name = "refugee_income", overwrite = TRUE
# )
