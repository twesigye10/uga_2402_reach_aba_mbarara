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
    select(`_submission__uuid`, i.member_hoh_gender = gender, i.member_hoh_age = age, i.hoh_education_level = hoh_education_level)
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
df_hh_with_disabled_member_refugee <- df_clean_loop_r_roster_refugee %>% 
    filter(member_hoh %in% c("yes")) %>% 
    create_composites_loop_roster_refugee() %>% 
    group_by(`_submission__uuid`) %>% 
    filter(n() == 1) %>% 
    mutate(i.hoh_disability = case_when(i.disability_prevalence %in% c("yes_disability") ~ "yes_disability",
                                        i.disability_prevalence %in% c("no_disability") ~ "no_disability")) %>% 
    select(`_submission__uuid`, i.hoh_disability)
add_checks_data_to_list(input_list_name = "list_individual_to_hh_refugee", input_df_name = "df_hh_with_disabled_member_refugee")

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


# combine the calculated indicators ---------------------------------------

df_combined_hh_indicators_from_roster_refugee <- list_individual_to_hh_refugee %>%
    reduce(.f = full_join, by = '_submission__uuid')
