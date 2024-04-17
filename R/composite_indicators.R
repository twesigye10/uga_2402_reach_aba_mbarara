# creating composite indicators -------------------------------------------
# main data
create_composites <- function(input_df) {
    input_df %>% 
        mutate(i.respondent_age = case_when(respondent_age <= 20 ~ "age_18_20",
                                            respondent_age <= 24 ~ "age_21_24",
                                            respondent_age <= 59 ~ "age_25_59",
                                            respondent_age >= 60 ~ "age_greater_60"),
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
    
              # hh_roster 
               i.hh_members_age_group = case_when((age >= 0 & age <= 2) ~ "age_0_2",
                                                  (age >= 3 & age <=5) ~ "age_3_5",
                                                  (age >=6 & age <=12) ~ "age_6_12",
                                                  (age >=13 & age <=18) ~ "age_13_18",
                                                  (age >=19 & age <=24) ~ "age_19_24",
                                                  (age >=25 & age <=59) ~ "age_25_59",
                                                  (age >= 59)  ~ "age_greater_59"),
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
              i.occupation_age_group = case_when(age <= 24 ~ "age_18_24",
                                           age <= 59 ~ "age_25_59",
                                           age >= 60 ~ "age_greater_60"),
    
               )
}
