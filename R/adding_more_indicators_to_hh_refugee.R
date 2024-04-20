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


df_hh_refugee_loop_roster_indicator_data <- df_loop_r_roster %>%
     rename(uuid = "_submission__uuid") %>% 
    mutate(int.member_hoh_by_gender = ifelse(member_hoh %in% c("yes"), "yes_hoh", "not_hoh"),
           int.hh_with_disabled_member =  case_when(if_any(c(vulnerability_see, vulnerability_hear, vulnerability_walk, vulnerability_concentrate, 
                                                             vulnerability_self_care, vulnerability_communicate),  ~ .x %in% c("yes_a_lot_of_difficulty","cannot_do_at_all")) ~ "yes_disability",
                                                    if_any(c(vulnerability_see, vulnerability_hear, vulnerability_walk, vulnerability_concentrate,
                                                             vulnerability_self_care, vulnerability_communicate), ~ .x %in% c("no_difficulty" , "yes_some_difficulty")) ~ "no_disability", 
                                                    TRUE ~ NA_character_),
           int.hh_hoh_disability = case_when(member_hoh %in% c("yes") & int.hh_with_disabled_member %in% c("yes_disability")~ "yes_disability",
                                             member_hoh %in% c("yes") & int.hh_with_disabled_member %in% c("no_disability")~ "no_disability",
                                             TRUE ~ NA_character_),
           int.female_hoh_single_parent = ifelse(female_hoh_single_parent %in% c("yes"), "yes_hoh_f_single_parent", "hoh_f_not_single_parent"),
           int.hoh_education_level = ifelse(member_hoh %in% c("yes"), hoh_education_level, NA),
           int.lactating_mother = ifelse(lactating_mother %in% c("yes"), "yes_lactating", "not_lactating"),
           int.unaccompanied_children = ifelse(unaccompanied_separated_or_orphan %in% c("yes"), "yes_unaccompanied_children", "no_unaccompanied_children"),
           
    ) %>% 
    
    group_by(uuid) %>%
    summarise(int.hoh_by_gender = paste(int.member_hoh_by_gender, collapse = " : "),
              int.hh_disabled = paste(int.hh_with_disabled_member, collapse = " : "),
              int.hoh_disability = paste(int.hh_hoh_disability, collapse = " : "),
              int.f_hoh_single_parent = paste(int.female_hoh_single_parent, collapse = " : "),
              int.hoh_educ_level = paste(int.hoh_education_level, collapse = " : "),
              int.lactate_mother = paste(int.lactating_mother, collapse = " : "),
              int.unaccompanied_child = paste(int.unaccompanied_children, collapse = " : "),
              
    ) %>%  
    mutate(i.member_hoh_by_gender = case_when(str_detect(string = int.hoh_by_gender, pattern = "yes_hoh") ~ "yes_hoh",
                                              !str_detect(string = int.lactate_mother, pattern = "yes_hoh") & 
                                                  str_detect(string = int.lactate_mother, pattern = "not_hoh")  ~ "not_hoh"),
           i.hh_with_disabled_member =  case_when(str_detect(string = int.hh_disabled, pattern = "yes_disability") ~ "yes_disability",
                                                  !str_detect(string = int.hh_disabled, pattern = "yes_disability") & 
                                                      str_detect(string = int.hh_disabled, pattern = "no_disability")  ~ "no_disability"),
           i.hoh_disability =  case_when(str_detect(string = int.hoh_disability, pattern = "yes_disability") ~ "yes_disability",
                                         !str_detect(string = int.hoh_disability, pattern = "yes_disability") & 
                                             str_detect(string = int.hoh_disability, pattern = "no_disability")  ~ "no_disability"),
           i.female_hoh_single_parent = case_when(str_detect(string = int.f_hoh_single_parent, pattern = "yes_hoh_f_single_parent") ~ "yes_hoh_f_single_parent",
                                                  !str_detect(string = int.f_hoh_single_parent, pattern = "yes_hoh_f_single_parent") & 
                                                      str_detect(string = int.f_hoh_single_parent, pattern = "hoh_f_not_single_parent")  ~ "hoh_f_not_single_parent"),
           i.hoh_education_level = case_when(str_detect(string = int.hoh_educ_level, pattern = "no_formal_education") ~ "no_formal_education",
                                             str_detect(string = int.hoh_educ_level, pattern = "pre_primary") ~ "pre_primary",
                                             str_detect(string = int.hoh_educ_level, pattern = "primary") ~ "primary",
                                             str_detect(string = int.hoh_educ_level, pattern = "lower_secondary") ~ "lower_secondary",
                                             str_detect(string = int.hoh_educ_level, pattern = "upper_secondary") ~ "upper_secondary",
                                             str_detect(string = int.hoh_educ_level, pattern = "vocational_college") ~ "vocational_college",
                                             str_detect(string = int.hoh_educ_level, pattern = "tertiaryuniversity") ~ "tertiaryuniversity",
                                             str_detect(string = int.hoh_educ_level, pattern = "other") ~ "other",
                                             str_detect(string = int.hoh_educ_level, pattern = "dk") ~ "dk",
                                             str_detect(string = int.hoh_educ_level, pattern = "prefer_not_to_answer") ~ "prefer_not_to_answer"),
           i.lactating_mother = case_when(str_detect(string = int.lactate_mother, pattern = "yes_lactating") ~ "yes_lactating",
                                          !str_detect(string = int.lactate_mother, pattern = "yes_lactating") & 
                                              str_detect(string = int.lactate_mother, pattern = "not_lactating")  ~ "not_lactating"),
           i.unaccompanied_children = case_when(str_detect(string = int.unaccompanied_child, pattern = "yes_unaccompanied_children") ~ "yes_unaccompanied_children",
                                          !str_detect(string = int.unaccompanied_child, pattern = "yes_unaccompanied_children") & 
                                              str_detect(string = int.unaccompanied_child, pattern = "no_unaccompanied_children")  ~ "no_unaccompanied_children"),
           
    ) %>%
     select(-c(starts_with("int.")))
write_csv(x = df_hh_refugee_loop_roster_indicator_data, file = "outputs/hhhh.csv")   

    
