library(tidyverse)
library(srvyr)
library(supporteR)
library(analysistools)

source("R/composite_indicators.R")

# clean data

clean_loc_data_host <- "inputs/UGA2402_aba_mbarara_host_cleaned_data.xlsx"

# main data
clean_data_nms_host <- names(readxl::read_excel(path = clean_loc_data_host, n_max = 300, sheet = "cleaned_data"))
clean_c_types_host <- ifelse(str_detect(string = clean_data_nms_host, pattern = "_other$"), "text", "guess")
df_main_clean_data_host <- readxl::read_excel(clean_loc_data_host, col_types = clean_c_types_host, sheet = "cleaned_data")

# loops
# roster
clean_data_nms_r_roster_host <- names(readxl::read_excel(path = clean_loc_data_host, n_max = 300, sheet = "cleaned_roster"))
clean_c_types_r_roster_host <- ifelse(str_detect(string = clean_data_nms_r_roster_host, pattern = "_other$"), "text", "guess")
df_clean_loop_r_roster_host <- readxl::read_excel(clean_loc_data_host, col_types = clean_c_types_r_roster_host, sheet = "cleaned_roster")
# income
clean_data_nms_r_income_host <- names(readxl::read_excel(path = clean_loc_data_host, n_max = 300, sheet = "cleaned_income_received"))
clean_c_types_r_income_host <- ifelse(str_detect(string = clean_data_nms_r_income_host, pattern = "_other$"), "text", "guess")
df_clean_loop_r_income_host <- readxl::read_excel(clean_loc_data_host, col_types = clean_c_types_r_income_host, sheet = "cleaned_income_received")

# tool
loc_tool_host <- "inputs/UGA2402_aba_mbarara_host_tool.xlsx"
df_survey_host <- readxl::read_excel(loc_tool_host, sheet = "survey")
df_choices_host <- readxl::read_excel(loc_tool_host, sheet = "choices")

# loa // list of analysis
all_loa_host <- read_csv("inputs/r_loa_aba_mbarara_host.csv")

# individual to hh level host ------------------------------------------

list_individual_to_hh_host <- list()

# dependency ratio

df_data_dependency_host <- df_clean_loop_r_roster_host %>% 
    mutate(int.dependency_cat = ifelse(age < 15 | age > 65, "dependant", "non_dependant"),
           int.dependant = ifelse(int.dependency_cat %in% c("dependant"), 1, 0),
           int.non_dependant = ifelse(int.dependency_cat %in% c("non_dependant"), 1, 0)) %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(i.n_dependant = sum(int.dependant, na.rm = TRUE),
              i.n_non_dependant = sum(int.non_dependant, na.rm = TRUE)) 
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_data_dependency_host")

# member_hoh_by_gender
df_member_hoh_by_gender_host <- df_clean_loop_r_roster_host %>% 
    filter(member_hoh %in% c("yes")) %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    select(`_submission__uuid`, i.member_hoh_by_gender = gender, i.member_hoh_age = age, i.hoh_education_level = hoh_education_level)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_member_hoh_by_gender_host")

# hh_with_disabled_member
df_hh_with_disabled_member_host <- df_clean_loop_r_roster_host %>% 
    create_composites_loop_roster_host() %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(int.hh_disability_status = paste(i.disability_prevalence, collapse = " : ")) %>% 
    mutate(i.hh_with_disabled_member = case_when(str_detect(string = int.hh_disability_status, pattern = "yes_disability") ~ "yes_disability",
                                                 !str_detect(string = int.hh_disability_status, pattern = "yes_disability") ~ "no_disability")) %>% 
    select(-int.hh_disability_status)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_hh_with_disabled_member_host")

# hoh_disability
df_hh_with_disabled_member_host <- df_clean_loop_r_roster_host %>% 
    filter(member_hoh %in% c("yes")) %>% 
    create_composites_loop_roster_host() %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    mutate(i.hoh_disability = case_when(i.disability_prevalence %in% c("yes_disability") ~ "yes_disability",
                                        i.disability_prevalence %in% c("no_disability") ~ "no_disability")) %>% 
    select(`_submission__uuid`, i.hoh_disability)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_hh_with_disabled_member_host")

# female_hoh_single_parent
df_female_hoh_single_parent_host <- df_clean_loop_r_roster_host %>% 
    filter(member_hoh %in% c("yes"), gender %in% c("female")) %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    mutate(i.female_hoh_single_parent = female_hoh_single_parent) %>% 
    select(`_submission__uuid`, i.female_hoh_single_parent)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_female_hoh_single_parent_host")

# lactating_mother
df_hh_with_lactating_mother_host <- df_clean_loop_r_roster_host %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(int.hh_lactating_mother = paste(lactating_mother, collapse = " : "),
              i.num_pregnant_lactating = sum(lactating_mother %in% c("yes"), na.rm = TRUE)) %>% 
    mutate(i.hh_with_lactating_mother = case_when(str_detect(string = int.hh_lactating_mother, pattern = "yes") ~ "yes",
                                                  !str_detect(string = int.hh_lactating_mother, pattern = "yes") ~ "no")) %>% 
    select(-int.hh_lactating_mother)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_hh_with_lactating_mother_host")

# number of children less than 1
df_num_children_less_than_one <- df_clean_loop_r_roster_host %>% 
    group_by(`_submission__uuid`) %>% 
    summarise(i.num_children_less_than_one = sum(age %in% c("0"), na.rm = TRUE))
add_checks_data_to_list(input_list_name = "list_individual_to_hh_host", input_df_name = "df_num_children_less_than_one")




# combine the calculated indicators

df_combined_hh_indicators_from_roster_host <- list_individual_to_hh_host %>%
    reduce(.f = full_join, by = '_submission__uuid')


# data with composites ----------------------------------------------------

# main data with composites
df_data_with_composites_host <- df_main_clean_data_host %>% 
    left_join(df_combined_hh_indicators_from_roster_host, by = c("_uuid" = "_submission__uuid")) %>% 
    create_composites_main_host() %>%
    mutate(strata = paste0("host_", interview_cell)) #%>% 
    # filter(!interview_cell %in% c("kyamugolanyi"))

# roster
df_clean_loop_r_roster_with_composites_host <- df_clean_loop_r_roster_host %>% 
    create_composites_loop_roster_host()

# host analysis - main -------------------------------------------------

# main
df_main_host <- df_data_with_composites_host
# survey object
main_host_svy <- as_survey(.data = df_main_host)

# loa
df_main_loa <- all_loa_host %>% 
    filter(dataset %in% c("main_data"))

# analysis
df_main_analysis_host <- analysistools::create_analysis(design = main_host_svy, 
                                                        loa = df_main_loa,
                                                        sm_separator = "/")


# host analysis - roster -----------------------------------------------

# roster
df_roster_host <- df_clean_loop_r_roster_with_composites_host %>% 
    left_join(df_main_host %>% select(any_of(c("_uuid", "strata"))), by = c("_submission__uuid" = "_uuid"))

# survey object
roster_host_svy <- as_survey(.data = df_roster_host)

# loa roster
df_roster_loa <- all_loa_host %>% 
    filter(dataset %in% c("roster"))

# analysis
df_roster_analysis_host <- analysistools::create_analysis(design = roster_host_svy, 
                                                          loa = df_roster_loa,
                                                          sm_separator = "/")

# host analysis - income -----------------------------------------------

# income received
df_income_host <- df_clean_loop_r_income_host %>% 
    left_join(df_main_host %>% select(any_of(c("_uuid", "strata"))), by = c("_submission__uuid" = "_uuid"))

# survey object - income received
income_host_svy <- as_survey(.data = df_income_host)

# loa income received
df_income_loa <- all_loa_host %>% 
    filter(dataset %in% c("income_received"))

# analysis
df_income_analysis_host <- analysistools::create_analysis(design = income_host_svy, 
                                                          loa = df_income_loa,
                                                          sm_separator = "/")


# analysis tables ---------------------------------------------------------

# combine the tables

# df_combined_tables <- bind_rows(df_main_analysis_host$results_table,
#                                 df_roster_analysis_host$results_table,
#                                 df_income_analysis_host$results_table
# )
# 
# df_host_analysis_table <- presentresults::create_table_variable_x_group(results_table = df_combined_tables)
# 
# presentresults::create_xlsx_variable_x_group(table_group_x_variable = df_host_analysis_table,
#                                              file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_host.xlsx)",
#                                              table_sheet_name = "host"
#                                              
# )

presentresults::create_xlsx_variable_x_group(table_group_x_variable = presentresults::create_table_variable_x_group(results_table = df_main_analysis_host$results_table),
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_host_main.xlsx"),
                                             table_sheet_name = "host_main"
)

presentresults::create_xlsx_variable_x_group(table_group_x_variable = presentresults::create_table_variable_x_group(results_table = df_roster_analysis_host$results_table),
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_host_roster.xlsx"),
                                             table_sheet_name = "host_roster"
)

presentresults::create_xlsx_variable_x_group(table_group_x_variable = presentresults::create_table_variable_x_group(results_table = df_income_analysis_host$results_table),
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), "_analysis_tables_UGA2402_aba_mbarara_host_income.xlsx"),
                                             table_sheet_name = "host_income"
)
