# function for creating composite indicators
create_composite_indicators_main <- function(input_df) {
    input_df %>% 
        mutate(i.avg_family_size = rowSums(select(., any_of(c("number_hh_members_in_mbarara","number_hh_members_live_in_settlement", "number_hh_members_live_in_other_places"))), na.rm = TRUE),
               i.shelter_index = number_hh_members_in_mbarara/shelter_room_sleep_number
        )
}

