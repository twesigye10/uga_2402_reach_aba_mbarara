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

# tool
loc_tool_refugee <- "inputs/UGA2402_aba_mbarara_refugee_tool.xlsx"
df_survey_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "survey")
df_choices_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "choices")

# loa // list of analysis
all_loa_refugee <- read_csv("inputs/r_loa_aba_mbarara_refugee.csv")

# pop
df_ref_pop <- read_csv("inputs/refugee_population_aba_mbarara.csv")

# data with composites
df_data_with_composites_refugee <- df_main_clean_data_refugee %>% 
    create_composite_indicators() %>%
    mutate(strata = paste0("refugee_", interview_cell))
    


# refugee analysis - main -------------------------------------------------

# weights
df_main_ref_with_weights <- analysistools::add_weights(dataset = df_data_with_composites_refugee %>% 
                                                      filter(status %in% c("refugee")),
                                                  sample_data = df_ref_pop,
                                                  strata_column_dataset = "strata",
                                                  strata_column_sample = "strata",
                                                  population_column =  "population")
# survey object
main_ref_svy <- as_survey(.data = df_main_ref_with_weights, strata = strata, weights = weights)

# loa
df_main_loa <- all_loa_refugee %>% 
    filter(dataset %in% c("main"))

# analysis
df_main_analysis_refugee <- analysistools::create_analysis(design = main_ref_svy, 
                                                      loa = df_main_loa,
                                                      sm_separator = "/")


# refugee analysis - roster -----------------------------------------------

# weights
df_roster_ref_with_weights <- analysistools::add_weights(dataset = df_data_with_composites_refugee %>% 
                                                             filter(status %in% c("refugee")),
                                                         sample_data = df_ref_pop,
                                                         strata_column_dataset = "strata",
                                                         strata_column_sample = "strata",
                                                         population_column =  "population")
# survey object
roster_ref_svy <- as_survey(.data = df_roster_ref_with_weights, strata = strata, weights = weights)

# loa roster
df_roster_loa <- all_loa_refugee %>% 
    filter(dataset %in% c("roster"))

# analysis
df_roster_analysis_refugee <- analysistools::create_analysis(design = roster_ref_svy, 
                                                             loa = df_roster_loa,
                                                             sm_separator = "/")

# refugee analysis - income -----------------------------------------------

# weights
df_income_ref_with_weights <- analysistools::add_weights(dataset = df_data_with_composites_refugee %>% 
                                                             filter(status %in% c("refugee")),
                                                         sample_data = df_ref_pop,
                                                         strata_column_dataset = "strata",
                                                         strata_column_sample = "strata",
                                                         population_column =  "population")
# survey object - income received
income_ref_svy <- as_survey(.data = df_income_ref_with_weights, strata = strata, weights = weights)

# loa income received
df_income_loa <- all_loa_refugee %>% 
    filter(dataset %in% c("income_received"))

# analysis
df_income_analysis_refugee <- analysistools::create_analysis(design = income_ref_svy, 
                                                             loa = df_income_loa,
                                                             sm_separator = "/")


# analysis tables ---------------------------------------------------------

df_main_refugee_analysis_table <- presentresults::create_table_variable_x_group(results_table = df_main_analysis_refugee$results_table)

presentresults::create_xlsx_variable_x_group(table_group_x_variable = df_main_refugee_analysis_table,
                                             file_path = "outputs/analysis_tables_UGA2402_aba_mbarara_refugee_main.xlsx",
                                             table_sheet_name = "main"
                                             
)