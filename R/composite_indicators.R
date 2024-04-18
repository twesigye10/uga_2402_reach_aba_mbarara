# creating composite indicators -------------------------------------------
# main data
create_composites_main <- function(input_df) {
    input_df %>% 
        mutate(i.respondent_age = case_when(respondent_age <= 20 ~ "age_18_20",
                                            respondent_age <= 24 ~ "age_21_24",
                                            respondent_age <= 59 ~ "age_25_59",
                                            respondent_age >= 60 ~ "age_greater_59"),
               i.months_displaced = case_when(hh_date_displaced > as_date("2023-03-01") ~ "up_to_12_months_ago",
                                              hh_date_displaced > as_date("2021-03-01") ~ "up_to_36_months_ago",
                                              hh_date_displaced > as_date("2019-03-01") ~ "up_to_60_months_ago",
                                              hh_date_displaced > as_date("2013-03-01") ~ "up_to_120_months_ago",
                                              TRUE ~ "more_than_10_years_ago"),
               i.avg_hh_size_mbarara = case_when(number_hh_members_in_mbarara <= 3 ~ "between_1_and_3_members",
                                                 number_hh_members_in_mbarara <= 6 ~ "between_4_and_6_members",
                                                 number_hh_members_in_mbarara <= 9 ~ "between_7_and_9_members",
                                                 number_hh_members_in_mbarara >= 10 ~ "10_or_more_members"),
               i.avg_hh_size_settlement = case_when(number_hh_members_live_in_settlement == 0 ~ NA,
                                                    number_hh_members_live_in_settlement <= 3 ~ "between_1_and_3_members",
                                                    number_hh_members_live_in_settlement <= 6 ~ "between_4_and_6_members",
                                                    number_hh_members_live_in_settlement <= 9 ~ "between_7_and_9_members",
                                                    number_hh_members_live_in_settlement >= 10 ~ "10_or_more_members"),
               i.avg_hh_size_other = case_when(number_hh_members_live_in_other_places == 0 ~ NA,
                                               number_hh_members_live_in_other_places <= 3 ~ "between_1_and_3_members",
                                               number_hh_members_live_in_other_places <= 6 ~ "between_4_and_6_members",
                                               number_hh_members_live_in_other_places <= 9 ~ "between_7_and_9_members",
                                               number_hh_members_live_in_other_places >= 10 ~ "10_or_more_members"),
               i.avg_hh_size = case_when((number_hh_members_in_mbarara + number_hh_members_live_in_settlement + number_hh_members_live_in_other_places)
                                         <= 3 ~ "between_1_and_3_members",
                                         (number_hh_members_in_mbarara + number_hh_members_live_in_settlement + number_hh_members_live_in_other_places)
                                         <= 6 ~ "between_4_and_6_members",
                                         (number_hh_members_in_mbarara + number_hh_members_live_in_settlement + number_hh_members_live_in_other_places)
                                         <= 9 ~ "between_7_and_9_members",
                                         (number_hh_members_in_mbarara + number_hh_members_live_in_settlement + number_hh_members_live_in_other_places)
                                         >= 10 ~ "10_or_more_members"),
               i.rank_mbarara_decision_impact_on_livelihood_stay_mbarara = rank_mbarara_decision_impact_on_livelihood,
               
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
# hh_roster 
create_composites_loop_roster <- function(input_df) {
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
