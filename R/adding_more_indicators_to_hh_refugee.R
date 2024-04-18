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
     mutate(int.member_hoh_by_gender = ifelse(member_hoh %in% c("yes"), member_hoh, NA),
            int.hh_with_disabled_member =  case_when(if_any(c(vulnerability_see, vulnerability_hear, vulnerability_walk, vulnerability_concentrate, 
                                                              vulnerability_self_care, vulnerability_communicate),  ~ .x %in% c("yes_a_lot_of_difficulty","cannot_do_at_all")) ~ "yes_disability",
                                                     if_any(c(vulnerability_see, vulnerability_hear, vulnerability_walk, vulnerability_concentrate,
                                                              vulnerability_self_care, vulnerability_communicate), ~ .x %in% c("no_difficulty" , "yes_some_difficulty")) ~ "no_disability", 
                                                     TRUE ~ NA_character_),
            int.hh_hoh_disability = case_when(member_hoh %in% c("yes") & int.hh_with_disabled_member %in% c("yes_disability")~ "yes_disability",
                                           member_hoh %in% c("yes") & int.hh_with_disabled_member %in% c("no_disability")~ "no_disability",
                                         TRUE ~ NA_character_),
            int.female_hoh_single_parent = ifelse(female_hoh_single_parent %in% c("yes"), female_hoh_single_parent, NA)
            
     ) %>% 
            
     group_by(uuid) %>%
     summarise(int.hoh_by_gender = paste(int.member_hoh_by_gender, collapse = " : "),
               int.hh_disabled = paste(int.hh_with_disabled_member, collapse = " : "),
               int.hoh_disability = paste(int.hh_hoh_disability, collapse = " : "),
               int.f_hoh_single_parent = paste(int.female_hoh_single_parent, collapse = " : ")
               
               ) %>%  
     mutate(i.member_hoh_by_gender = case_when(str_detect(string = int.hoh_by_gender, pattern = "yes") ~ "yes",
                                               TRUE ~ NA_character_),
            i.hh_with_disabled_member =  case_when(str_detect(string = int.hh_disabled, pattern = "yes_disability") ~ "yes_disability",
                                                   !str_detect(string = int.hh_disabled, pattern = "yes_disability") & 
                                                       str_detect(string = int.hh_disabled, pattern = "no_disability")  ~ "no_disability"),
            i.hoh_disability =  case_when(str_detect(string = int.hoh_disability, pattern = "yes_disability") ~ "yes_disability",
                                                   !str_detect(string = int.hoh_disability, pattern = "yes_disability") & 
                                                       str_detect(string = int.hoh_disability, pattern = "no_disability")  ~ "no_disability"),
            i.female_hoh_single_parent = case_when(str_detect(string = int.f_hoh_single_parent, pattern = "yes") ~ "yes",
                                               TRUE ~ NA_character_)
     ) %>%
     select(-c(starts_with("int.")))
   

    
