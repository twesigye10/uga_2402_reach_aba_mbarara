library(tidyverse)
library(cleaningtools)
library(httr)
library(supporteR)

source("R/support_functions.R")
source("support_files/credentials.R")

# read data ---------------------------------------------------------------

loc_data <- "inputs/UGA2305_land_and_energy_data.xlsx"

df_tool_data <- readxl::read_excel(loc_data)

# tool
loc_tool <- "inputs/land_and_energy_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey") 
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")


# download audit files
download_audit_files(df = df_tool_data, 
                     uuid_column = "_uuid", 
                     audit_dir = "inputs/audit_files", 
                     usr = user_acc, 
                     pass = user_pss)
# zip audit files folder
if (dir.exists("inputs/audit_files")) {
    zip::zip(zipfile = "inputs/audit_files.zip", 
             # files = "inputs/audit_files/",
             files = list.dirs(path = "inputs/audit_files/", full.names = TRUE, recursive = FALSE),
             mode = "cherry-pick")
}
