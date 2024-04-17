# function for creating composite indicators
create_composite_indicators_main <- function(input_df) {
    input_df %>% 
        mutate(i.avg_family_size = rowSums(select(., any_of(c("number_hh_members_in_mbarara","number_hh_members_live_in_settlement", "number_hh_members_live_in_other_places"))), na.rm = TRUE),
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
        select(-starts_with("fcs_weight")) %>% 
        rename(i.fcs_score = fsl_fcs_score) %>% 
        rename(i.fcs_cat = fsl_fcs_cat) 
}

