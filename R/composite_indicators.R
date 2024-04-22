# create_composites -------------------------------------------------------
create_composites_main_refugee <- function(input_df) {
    input_df %>% 
        mutate(i.months_displaced = case_when(hh_date_displaced <= as_date("2013-03-01") ~ "up_to_120_months_ago+",
                                              hh_date_displaced < as_date("2019-03-01") ~ "up_to_60_months_ago",
                                              hh_date_displaced < as_date("2021-03-01") ~ "up_to_36_months_ago",
                                              hh_date_displaced < as_date("2023-03-01") ~ "up_to_12_months_ago"),
               i.avg_family_size = rowSums(select(., any_of(c("number_hh_members_in_mbarara","number_hh_members_live_in_settlement", "number_hh_members_live_in_other_places"))), na.rm = TRUE),
               i.shelter_index = number_hh_members_in_mbarara/shelter_room_sleep_number  
        ) %>% 
        addindicators::add_fcs(cutoffs = "normal",
                               fsl_fcs_cereal = "fcs_cereals",
                               fsl_fcs_legumes = "fcs_pulses",
                               fsl_fcs_veg = "fcs_vegetables",
                               fsl_fcs_fruit = "fcs_fruits",
                               fsl_fcs_meat = "fcs_protein",
                               fsl_fcs_dairy = "fcs_dairy",
                               fsl_fcs_sugar = "fcs_sugar",
                               fsl_fcs_oil = "fcs_oils"
        ) %>% 
        select(-starts_with("fcs_weight"), -c(starts_with("int."))) %>% 
        rename(i.fcs_score = fsl_fcs_score) %>% 
        rename(i.fcs_cat = fsl_fcs_cat)
}

# loop_roster_refugee
create_composites_loop_roster_refugee <- function(input_df) {
    input_df %>%
        unite(col = "int.disability", starts_with("vulnerability_"), remove = FALSE, sep = " : ", na.rm = TRUE) %>% 
        mutate(i.hh_members_age_group = case_when(age <= 2 ~ "age_0_2",
                                                  age <= 5 ~ "age_3_5",
                                                  age <= 12 ~ "age_6_12",
                                                  age <= 18 ~ "age_13_18",
                                                  age <= 24 ~ "age_19_24",
                                                  age <= 59 ~ "age_25_59",
                                                  age > 59  ~ "age_greater_59"),
               i.disability_age_group = case_when(age > 4 & age < 13 ~ "age_5_12",
                                                  age < 19 ~ "age_13_18",
                                                  age < 25 ~ "age_19_24",
                                                  age <= 59 ~ "age_25_59",
                                                  age >= 60  ~ "age_greater_59"),
               i.disability_prevalence =  case_when(!is.na(i.disability_age_group) & str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all") ~ "yes_disability",
                                                       !is.na(i.disability_age_group) & !str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all") ~ "no_disability"),
               i.occupation_age_group = case_when((age >= 18 & age < 25) ~ "age_18_24",
                                                  (age >= 25 & age <= 59) ~ "age_25_59",
                                                  (age > 59)  ~ "age_greater_59"),
               
               i.child_labor_age_group = case_when((age >= 3 & age <= 5) ~ "age_3_5",
                                                   (age >= 6 & age <= 12) ~ "age_6_12",
                                                   (age >= 13 & age <= 17) ~ "age_13_17"),
               i.child_enrollment_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                        (age >=6 & age <=12) ~ "age_6_12",
                                                        (age >=13 & age <=16) ~ "age_13_17",
                                                        (age >=17 & age <=18) ~ "age_17_18"),
               i.regular_school_attendance_age_group = case_when((age >= 3 & age <= 5) ~ "age_3_5",
                                                                 (age >= 6 & age <= 12) ~ "age_6_12",
                                                                 (age >= 13 & age <= 16) ~ "age_13_17",
                                                                 (age >= 17 & age <= 18) ~ "age_17_18"),
               i.non_formal_educ_activities_age_group = case_when((age >= 3 & age <= 5) ~ "age_3_5",
                                                                  (age >= 6 & age <= 12) ~ "age_6_12",
                                                                  (age >= 13 & age <= 16) ~ "age_13_16",
                                                                  (age >= 17 & age <= 18) ~ "age_17_18",
                                                                  (age >= 19 & age <= 24) ~ "age_19_24",
                                                                  (age >= 25 & age <= 59) ~ "age_25_59",
                                                                  (age > 59)  ~ "age_greater_59"),
        ) %>%
        
        select(-c(starts_with("int.")))
}


# composites_main_host
create_composites_main_host <- function(input_df) {
    input_df %>% 
        mutate(i.shelter_index = number_hh_members_in_mbarara/shelter_room_sleep_number,
               
        ) %>%
        addindicators::add_fcs(cutoffs = "normal",
                               fsl_fcs_cereal = "fcs_cereals",
                               fsl_fcs_legumes = "fcs_pulses",
                               fsl_fcs_veg = "fcs_vegetables",
                               fsl_fcs_fruit = "fcs_fruits",
                               fsl_fcs_meat = "fcs_protein",
                               fsl_fcs_dairy = "fcs_dairy",
                               fsl_fcs_sugar = "fcs_sugar",
                               fsl_fcs_oil = "fcs_oils"
        ) %>% 
        select(-starts_with("fcs_weight"), -c(starts_with("int."))) %>% 
        rename(i.fcs_score = fsl_fcs_score) %>% 
        rename(i.fcs_cat = fsl_fcs_cat)
} 

# loop_roster host 
create_composites_loop_roster_host <- function(input_df) {
    input_df %>%
        unite(col = "int.disability", starts_with("vulnerability_"), remove = FALSE, sep = " : ", na.rm = TRUE) %>% 
        mutate(i.hh_members_age_group = case_when(age <= 2 ~ "age_0_2",
                                                  age <= 5 ~ "age_3_5",
                                                  age <= 12 ~ "age_6_12",
                                                  age <= 18 ~ "age_13_18",
                                                  age <= 24 ~ "age_19_24",
                                                  age <= 59 ~ "age_25_59",
                                                  age > 59  ~ "age_greater_59"),
               i.disability_age_group = case_when(age > 4 & age < 13 ~ "age_5_12",
                                                  age < 19 ~ "age_13_18",
                                                  age < 25 ~ "age_19_24",
                                                  age <= 59 ~ "age_25_59",
                                                  age >= 60  ~ "age_greater_59"),
               i.disability_prevalence =  case_when(!is.na(i.disability_age_group) & str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all") ~ "yes_disability",
                                                       !is.na(i.disability_age_group) & !str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all") ~ "no_disability"),
               i.occupation_age_group = case_when((age >=18 & age <25) ~ "age_18_24",
                                                  (age >=25 & age <=59) ~ "age_25_59",
                                                  (age > 59)  ~ "age_greater_59"),
               i.child_labor_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                   (age >=6 & age <=12 ) ~ "age_6_12",
                                                   (age >=13 & age <=17 ) ~ "age_13_17"),
               i.child_enrollment_age_group = case_when((age >= 3 & age <= 5) ~ "age_3_5",
                                                        (age >= 6 & age <= 12) ~ "age_6_12",
                                                        (age >= 13 & age <= 16) ~ "age_13_17",
                                                        (age >= 17 & age <= 18) ~ "age_17_18"),
               i.regular_school_attendance_age_group = case_when((age >= 3 & age <= 5) ~ "age_3_5",
                                                                 (age >= 6 & age <= 12) ~ "age_6_12",
                                                                 (age >= 13 & age <= 16) ~ "age_13_17",
                                                                 (age >= 17 & age <= 18) ~ "age_17_18"),
               i.non_formal_educ_activities_age_group = case_when((age >= 3 & age <= 5) ~ "age_3_5",
                                                                  (age >= 6 & age <= 12) ~ "age_6_12",
                                                                  (age >= 13 & age <= 16) ~ "age_13_16",
                                                                  (age >= 17 & age <= 18) ~ "age_17_18",
                                                                  (age >= 19 & age <= 24) ~ "age_19_24",
                                                                  (age >= 25 & age <= 59) ~ "age_25_59",
                                                                  (age > 59)  ~ "age_greater_59"),
        ) %>%
        
        select(-c(starts_with("int.")))
}

