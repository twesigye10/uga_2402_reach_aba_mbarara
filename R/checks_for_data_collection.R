library(tidyverse)
library(cleaningtools)
library(httr)
library(supporteR)

source("R/support_functions.R")
source("support_files/credentials.R")

# read data ---------------------------------------------------------------

loc_data_refugee <- "inputs/UGA2305_land_and_energy_data.xlsx"
df_tool_data_refugee <- readxl::read_excel(loc_data_refugee)

# tool
loc_tool_refugee <- "inputs/land_and_energy_tool.xlsx"
df_survey_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "survey") 
df_choices_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "choices")


# download audit files
download_audit_files(df = df_tool_data_refugee, 
                     uuid_column = "_uuid", 
                     audit_dir = "inputs/audit_files_refugee", 
                     usr = user_acc, 
                     pass = user_pss)
# zip audit files folder
if (dir.exists("inputs/audit_files_refugee")) {
    zip::zip(zipfile = "inputs/audit_files_refugee.zip", 
             files = list.dirs(path = "inputs/audit_files_refugee/", full.names = TRUE, recursive = FALSE),
             mode = "cherry-pick")
}


