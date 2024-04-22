# create_composites -------------------------------------------------------
create_composites_main_refugee <- function(input_df) {
    input_df %>% 
        mutate(#i.respondent_age = case_when(respondent_age <= 20 ~ "age_18_20",
                                            # respondent_age <= 24 ~ "age_21_24",
                                            # respondent_age <= 59 ~ "age_25_59",
                                            # respondent_age >= 60 ~ "age_greater_59"),
               i.months_displaced = case_when(hh_date_displaced <= as_date("2013-03-01") ~ "up_to_120_months_ago+",
                                              hh_date_displaced < as_date("2019-03-01") ~ "up_to_60_months_ago",
                                              hh_date_displaced < as_date("2021-03-01") ~ "up_to_36_months_ago",
                                              hh_date_displaced < as_date("2023-03-01") ~ "up_to_12_months_ago"),
               i.avg_family_size = rowSums(select(., any_of(c("number_hh_members_in_mbarara","number_hh_members_live_in_settlement", "number_hh_members_live_in_other_places"))), na.rm = TRUE),
               
               # i.rank_mbarara_decision_impact_on_livelihood_stay_mbarara = rank_mbarara_decision_impact_on_livelihood,
               # 
               # i.top_food_sources = paste(main_hh_source_of_food, second_hh_source_of_food, third_hh_source_of_food, sep = " "),
               
               i.shelter_index = number_hh_members_in_mbarara/shelter_room_sleep_number,
               # i.enough_money_for_food_single_f_hoh = enough_money_for_food,
               # i.enough_money_for_educ_and_health_single_f_hoh = enough_money_for_educ_and_health,
               # i.facility_type_hh_members_sought_treatment_by_reason = facility_type_hh_members_sought_treatment,
               # i.hh_received_aid_past_single_f_hoh = hh_received_aid_past,
               # i.unmet_needs_single_f_hoh = unmet_needs,
               # i.rank_refugee_host_relationship_by_stay_mbarara = rank_refugee_host_relationship,
               # i.feeling_part_of_decision_making_by_stay_mbarara = feeling_part_of_decision_making,
               
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
        mutate(i.hh_members_age_group = case_when(age <= 2 ~ "age_0_2",
                                                  age <=5 ~ "age_3_5",
                                                  age <=12 ~ "age_6_12",
                                                  age <=18 ~ "age_13_18",
                                                  age <=24 ~ "age_19_24",
                                                  age <=59 ~ "age_25_59",
                                                  age > 59  ~ "age_greater_59"),
               i.disability_age_group = case_when(age >4 & age <13 ~ "age_5_12",
                                                  age <19 ~ "age_13_18",
                                                  age <25 ~ "age_19_24",
                                                  age <=59 ~ "age_25_59",
                                                  age >= 60  ~ "age_greater_59"),
               int.disability = unite(col = starts_with("vulnerability_"), remove = FALSE, sep = " : ", na.rm = TRUE),
               i.hh_disability_prevalence =  case_when(!is.na(i.disability_age_group) & str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all") ~ "yes_disability",
                                                       !is.na(i.disability_age_group) & !str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all") ~ "no_disability"),
               # i.occupation_status_by_gender = occupation_status,
               # i.occupation_status_by_stay_mbarara = occupation_status,
               
               i.occupation_age_group = case_when((age >=18 & age <25) ~ "age_18_24",
                                                  (age >=25 & age <=59 ) ~ "age_25_59",
                                                  (age > 59)  ~ "age_greater_59"),
               
               i.child_labor_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                   (age >=6 & age <=12 ) ~ "age_6_12",
                                                   (age >=13 & age <=17 ) ~ "age_13_17"),
               i.child_engaged_in_hh_labor_by_age = child_engaged_in_hh_labor,
               i.child_engaged_in_hh_labor_by_gender = child_engaged_in_hh_labor,
               i.child_enrollment_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                        (age >=6 & age <=12 ) ~ "age_6_12",
                                                        (age >=13 & age <=16 ) ~ "age_13_17",
                                                        (age >=17 & age <=18 ) ~ "age_17_18"),
               i.child_enrollment_status_by_age = child_enrollment_status,
               i.child_enrollment_status_by_gender = child_enrollment_status,
               i.regular_school_attendance_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                                 (age >=6 & age <=12 ) ~ "age_6_12",
                                                                 (age >=13 & age <=16 ) ~ "age_13_17",
                                                                 (age >=17 & age <=18 ) ~ "age_17_18"),
               i.regular_school_attendance_by_age = regular_school_attendance,
               i.regular_school_attendance_by_gender = regular_school_attendance,
               i.reason_child_not_attending_school_by_gender = reason_child_not_attending_school,
               i.educ_facility_ownership_by_reason = current_school_location,
               i.non_formal_educ_activities_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                                  (age >=6 & age <=12) ~ "age_6_12",
                                                                  (age >=13 & age <=16) ~ "age_13_16",
                                                                  (age >=17 & age <=18) ~ "age_17_18",
                                                                  (age >=19 & age <=24) ~ "age_19_24",
                                                                  (age >=25 & age <=59) ~ "age_25_59",
                                                                  (age > 59)  ~ "age_greater_59"),
               i.non_formal_educ_activities_by_age = non_formal_educ_activities,
               i.non_formal_educ_activities_by_gender = non_formal_educ_activities
        ) %>%
        
        select(-c(starts_with("int.")))
}

# convert individual to hh for refugee
create_composite_ind_hh_ref <- function(input_df) {
    input_df %>% 
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
}

# composites_main_host
create_composites_main_host <- function(input_df) {
    input_df %>% 
        mutate(i.respondent_age = case_when(respondent_age <= 20 ~ "age_18_20",
                                            respondent_age <= 24 ~ "age_21_24",
                                            respondent_age <= 59 ~ "age_25_59",
                                            respondent_age >= 60 ~ "age_greater_59"),
               i.top_food_sources = paste(main_hh_source_of_food, second_hh_source_of_food, third_hh_source_of_food, sep = " "),
               
               i.fcs = (fcs_cereals*2 + fcs_pulses*3 + fcs_vegetables*1 + fcs_fruits*1 + fcs_protein*4 + fcs_dairy*4 +
                            fcs_sugar*0.5 + fcs_oils*0.5),
               i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                     i.fcs <= 35 ~ "Borderline",
                                     i.fcs <= 112 ~ "Acceptable"),
               i.shelter_index = number_hh_members_in_mbarara/shelter_room_sleep_number,
               i.enough_money_for_food_single_f_hoh = enough_money_for_food,
               i.enough_money_for_educ_and_health_single_f_hoh = enough_money_for_educ_and_health,
               i.facility_type_hh_members_sought_treatment_by_reason = facility_type_hh_members_sought_treatment,
               i.hh_received_aid_past_single_f_hoh = hh_received_aid_past,
               i.unmet_needs_single_f_hoh = unmet_needs,
               i.rank_refugee_host_relationship_by_stay_mbarara = rank_refugee_host_relationship,
               i.feeling_part_of_decision_making_by_stay_mbarara = feeling_part_of_decision_making,
               
        ) %>%
        
        select(-c(starts_with("int.")))
} 

# loop_roster host 
create_composites_loop_roster_host <- function(input_df) {
    input_df %>%
        mutate(
            i.hh_members_age_group = case_when((age >= 0 & age <= 2) ~ "age_0_2",
                                               (age >= 3 & age <=5) ~ "age_3_5",
                                               (age >=6 & age <=12) ~ "age_6_12",
                                               (age >=13 & age <=18) ~ "age_13_18",
                                               (age >=19 & age <=24) ~ "age_19_24",
                                               (age >=25 & age <=59) ~ "age_25_59",
                                               (age > 59)  ~ "age_greater_59"),
            i.disability_age_group = case_when((age >=5 & age <13) ~ "age_5_12",
                                               (age >12 & age <19) ~ "age_13_18",
                                               (age >18 & age <25) ~ "age_19_24",
                                               (age >=25 & age <60) ~ "age_25_59",
                                               (age > 59)  ~ "age_greater_59"),
            i.hh_disability_prevalence =  case_when(vulnerability_see %in% c("yes_a_lot_of_difficulty","cannot_do_at_all")|vulnerability_hear %in% c("yes_a_lot_of_difficulty" , "cannot_do_at_all")|
                                                        vulnerability_walk %in% c("yes_a_lot_of_difficulty" , "cannot_do_at_all")|vulnerability_concentrate %in% c("yes_a_lot_of_difficulty" , "cannot_do_at_all")|
                                                        vulnerability_self_care %in% c("yes_a_lot_of_difficulty" , "cannot_do_at_all")|vulnerability_communicate %in% c("yes_a_lot_of_difficulty" , "cannot_do_at_all")~ "yes_disability",
                                                    vulnerability_see %in% c("no_difficulty" , "yes_some_difficulty")|vulnerability_hear %in% c("no_difficulty" , "yes_some_difficulty")|
                                                        vulnerability_walk %in% c("no_difficulty" , "yes_some_difficulty")|vulnerability_concentrate %in% c("no_difficulty" , "yes_some_difficulty")|
                                                        vulnerability_self_care %in% c("no_difficulty" , "yes_some_difficulty")|vulnerability_communicate %in% c("no_difficulty" , "yes_some_difficulty")~
                                                        "no_disability", TRUE ~ NA_character_),
            i.occupation_status_by_gender = occupation_status,
            i.occupation_status_by_stay_mbarara = occupation_status,
            
            i.occupation_age_group = case_when((age >=18 & age <25) ~ "age_18_24",
                                               (age >=25 & age <=59 ) ~ "age_25_59",
                                               (age > 59)  ~ "age_greater_59"),
            
            i.child_labor_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                (age >=6 & age <=12 ) ~ "age_6_12",
                                                (age >=13 & age <=17 ) ~ "age_13_17"),
            i.child_engaged_in_hh_labor_by_age = child_engaged_in_hh_labor,
            i.child_engaged_in_hh_labor_by_gender = child_engaged_in_hh_labor,
            i.child_enrollment_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                     (age >=6 & age <=12 ) ~ "age_6_12",
                                                     (age >=13 & age <=16 ) ~ "age_13_17",
                                                     (age >=17 & age <=18 ) ~ "age_17_18"),
            i.child_enrollment_status_by_age = child_enrollment_status,
            i.child_enrollment_status_by_gender = child_enrollment_status,
            i.regular_school_attendance_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                              (age >=6 & age <=12 ) ~ "age_6_12",
                                                              (age >=13 & age <=16 ) ~ "age_13_17",
                                                              (age >=17 & age <=18 ) ~ "age_17_18"),
            i.regular_school_attendance_by_age = regular_school_attendance,
            i.regular_school_attendance_by_gender = regular_school_attendance,
            i.reason_child_not_attending_school_by_gender = reason_child_not_attending_school,
            i.educ_facility_ownership_by_reason = current_school_location,
            i.non_formal_educ_activities_age_group = case_when((age >= 3 & age <=5) ~ "age_3_5",
                                                               (age >=6 & age <=12) ~ "age_6_12",
                                                               (age >=13 & age <=16) ~ "age_13_16",
                                                               (age >=17 & age <=18) ~ "age_17_18",
                                                               (age >=19 & age <=24) ~ "age_19_24",
                                                               (age >=25 & age <=59) ~ "age_25_59",
                                                               (age > 59)  ~ "age_greater_59"),
            i.non_formal_educ_activities_by_age = non_formal_educ_activities,
            i.non_formal_educ_activities_by_gender = non_formal_educ_activities
        ) %>%
        
        select(-c(starts_with("int.")))
    }


# convert individual to hh for host
create_composite_ind_hh_host <- function(input_df) {
    input_df %>% 
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
               
        ) %>% 
        
        group_by(uuid) %>%
        summarise(int.hoh_by_gender = paste(int.member_hoh_by_gender, collapse = " : "),
                  int.hh_disabled = paste(int.hh_with_disabled_member, collapse = " : "),
                  int.hoh_disability = paste(int.hh_hoh_disability, collapse = " : "),
                  int.f_hoh_single_parent = paste(int.female_hoh_single_parent, collapse = " : "),
                  int.hoh_educ_level = paste(int.hoh_education_level, collapse = " : "),
                  int.lactate_mother = paste(int.lactating_mother, collapse = " : "),
                  
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
        ) %>%
        
        select(-c(starts_with("int.")))
}
