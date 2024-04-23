library(openxlsx)


# host -----------------------------------------------------------------

# run after running the analysis script
df_analysis_wide_host <- df_roster_analysis_host$results_table %>% 
    select(-c(group_var, stat_low, stat_upp, 
              n_total, n_w, n_w_total, analysis_key)) %>% 
    mutate(group_var_value = ifelse(is.na(group_var_value), "total", group_var_value)) %>% 
    pivot_wider(names_from = c(group_var_value), values_from = c(stat, n)) %>% 
    mutate(row_id = row_number())
    

# openxlsx::write.xlsx(df_analysis_wide_host, "outputs/test_wide_format.xlsx")

df_cols_for_ordering <- readxl::read_excel("outputs/column_ordering.xlsx", sheet = "combined")

reordered_columns <- df_cols_for_ordering %>%
    pivot_longer(cols = c(result_col, n_unweighted), names_to = "entries", values_to = "columns") %>%
    pull(columns)

# reorder

df_analysis_wide_host_reodered <- df_analysis_wide_host %>%
    relocate(any_of(reordered_columns), .after = "analysis_var_value") %>% 
    relocate(analysis_type, .after = "analysis_var_value")

cols_for_num_pct_formatting <- df_analysis_wide_host_reodered %>% 
    select(stat_total:row_id) %>% 
    select(!matches("^n_"), -row_id) %>% 
    colnames()

# extract header data

df_to_extract_header = df_analysis_wide_host_reodered %>% 
    select(-any_of(c("analysis_var", "analysis_var_value", 
              "row_id"))) %>% 
    colnames()

df_extracted_header_data <- tibble("old_cols" = df_to_extract_header) %>% 
    mutate("new_cols" = paste0("x", row_number())) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "Results\\(mean\\/percentage\\)_|_host_community$|_refugee$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "_host_community_NA$|_refugee_NA$", replacement = "")) %>% 
    mutate(old_cols = str_replace(string = old_cols, pattern = "^n_.+", replacement = "n")) %>% 
    pivot_wider(names_from = new_cols, values_from = old_cols)

df_extracted_header <- bind_rows(df_extracted_header_data) %>% 
    mutate(x1 = "Analysis Type")

# create workbook ---------------------------------------------------------

wb_host <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T, 
                   border = "TopBottomLeftRight", borderStyle = "medium", borderColour = "#000000")
hs2 <- createStyle(fgFill = "grey", halign = "LEFT", textDecoration = "Bold", fontColour = "white", wrapText = F)
hs2_no_bold <- createStyle(fgFill = "grey", halign = "LEFT", textDecoration = "", fontColour = "white", wrapText = F)
hs2_relevant <- createStyle(fgFill = "grey", halign = "LEFT", textDecoration = "", fontColour = "#808080", wrapText = F)
hs3 <- createStyle(fgFill = "grey", halign = "CENTER", fontColour = "white", textDecoration = "Bold", 
                   border = "TopBottomLeftRight", borderStyle = "medium", borderColour = "#000000")

# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")
# percent
pct = createStyle(numFmt="0.0%") # not working

addWorksheet(wb_host, sheetName="Analysis")


# addStyle(wb_host, sheet = "Analysis", hs1, rows = 1, cols = 3:34, gridExpand = TRUE)

# header showing results headings
writeData(wb_host, sheet = "Analysis", df_extracted_header %>% head(1), startCol = 2, 
          startRow = 3, headerStyle = hs2, colNames = FALSE, 
          borders = "all", borderColour = "#000000", borderStyle = "thin")

setColWidths(wb = wb_host, sheet = "Analysis", cols = 1, widths = 70)
setColWidths(wb = wb_host, sheet = "Analysis", cols = 2, widths = 10)
setColWidths(wb = wb_host, sheet = "Analysis", cols = 3:90, widths = 8)

# split variables to be written in different tables with in a sheet
sheet_variables_data <- split(df_analysis_wide_host_reodered, factor(df_analysis_wide_host_reodered$analysis_var, levels = unique(df_analysis_wide_host_reodered$analysis_var)))

previous_row_end <- 3

for (i in 1:length(sheet_variables_data)) {
    
    current_variable_data <- sheet_variables_data[[i]]
    
    get_question <- current_variable_data %>% select(analysis_var) %>% unique() %>% pull()
    get_qn_type <- current_variable_data %>% select(analysis_type) %>% unique() %>% pull()

    if(get_qn_type %in% c("prop_select_one", "prop_select_multiple")){
        for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "percentage"}
    }else{
        for(n in cols_for_num_pct_formatting){class(current_variable_data[[n]])= "numeric"}
    }
    
    # this controls rows between questions
    current_row_start <- previous_row_end + 2
    
    print(current_row_start)
    
    # add header for variable
    writeData(wb_host, sheet = "Analysis", get_question, startCol = 1, startRow = previous_row_end + 1)
    writeData(wb_host, sheet = "Analysis", get_qn_type, startCol = 2, startRow = previous_row_end + 1)
    addStyle(wb_host, sheet = "Analysis", hs2, rows = previous_row_end + 1, cols = 1:2, gridExpand = TRUE)
    
    current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)
    
    writeData(wb = wb_host, 
              sheet = "Analysis", 
              x = current_variable_data %>% 
                  select(-c(analysis_var, 
                            row_id, analysis_var)
                  ) %>% mutate(analysis_type = NA_character_), 
              startRow = current_row_start, 
              startCol = 1, 
              colNames = FALSE)
    
    previous_row_end <- current_row_start + current_data_length
}
# hide grid lines
# showGridLines(wb_host,  "Analysis", showGridLines = FALSE)  

# freeze pane
freezePane(wb_host, "Analysis", firstActiveRow = 4, firstActiveCol = 3)

# openXL(wb_host)

saveWorkbook(wb_host, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_aba_mbarara_roster_host.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_aba_mbarara_roster_host.xlsx"))
