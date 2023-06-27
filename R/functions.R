library(readxl)
library(tidyverse)

extract_df <- function(path) {
  
  raw_sample_info = read_excel(path = path, sheet = "Sample Setup", skip = 0,range = ("A1:B46"), col_names = c("Field", "Value"))
  raw_sample_setup = read_excel(path = path, sheet = "Sample Setup", skip = 47)
  raw_Ct_results = read_excel(path = path, sheet = "Results", skip = 47)
  raw_Tm_results = read_excel(path = path, sheet = "Melt Curve Result", skip = 47)
  
  raw_results <- raw_sample_setup %>%
    left_join(raw_Ct_results, by = c("Well", "Well Position", "Target Name", "Sample Name", "Task", "Reporter", "Quencher", "Quantity", "Comments")) %>%
    left_join(raw_Tm_results, by = c("Well", "Well Position", "Target Name", "Sample Name" = "Sample"))
  
  list(
    raw_sample_info = raw_sample_info,
    raw_results = raw_results,
    raw_Ct = read_excel(path = path, sheet = "Amplification Data", skip = 47),
    raw_Tm = read_excel(path = path, sheet = "Melt Curve Raw Data", skip = 47)
  )
}

idenitfy_sample_type <- function(df) {
  
  # Note that we removed the condition where the "Sample" was in the column since we have merged it with 'Sample Name'
  if ("Sample Name" %in% colnames(df)) {
    results_after_id <- df %>% 
      filter(!is.na(`Sample Name`)) %>%
      separate(col = `Sample Name`, into = c('name', 'type'), sep = '_', remove = FALSE, fill = 'right') %>%
      mutate(type = case_when(name == "Positive" ~ "POS", name == "Negative" ~ "NEG", is.na(type) & !is.na(name) ~ "UNK", .default = type))
  } else {
    warning("There is no 'Sample Name' or 'Sample' column in the dataframe.")
    stop("Identification process terminated.")
  }
  
  return(results_after_id)
  
}


# ------- Perform the QC checks -----------------

## A. Positive controls

POS_QC <- function(df) {
  
  ### 1. Filter for only type == POS
  ### 2. POS_01: Check for 23.5000 <= Ct <= 27.5000
  ### 3. POS_02: Check for 83.0 < Tm < 86.0
  ### 4. POS_03: Check for DV >= 30000
  
  df %>% 
    filter(type == "POS") %>% 
    mutate(POS_01 = ((CT >= 23.5000) & (CT <= 27.5000))) %>%
    mutate(POS_02 = ((Tm > 83.0) & (Tm <= 86.0))) %>%
    mutate(POS_03 = `Melt Peak Height` >= 30000) %>%
    mutate(outcome = case_when(POS_01 & POS_02 & POS_03 ~ "POS_outcome_01", 
                               !POS_01 ~ "POS_outcome_02", 
                               POS_01 & !POS_02 ~ "POS_outcome_03",
                               POS_01 & POS_02 & !POS_03 ~ "POS_outcome_04",
                               .default = NA))
}

# B. Check NTC conditions

NEG_QC <- function(df) {
  
  ### 1. Filter for only type == NTC
  ### 2. NTC_01: Is Ct < 36.2300?
  ### 3. NTC_02: Is 75.50 <= Tm <= 83.00?
  ### 4. NTC_03: Is Tm < 75.50 or Tm > 86.00?
  ### 5. NTC_04: Is DV > 30000?
  df %>% 
    filter(type == "NEG") %>% 
    mutate(NEG_01 = CT < 36.2300) %>%
    mutate(NEG_02 = (Tm > 75.50) & (Tm < 83.00)) %>%
    mutate(NEG_03 = (Tm < 75.50) | (Tm > 86.00)) %>%
    mutate(NEG_04 = (`Melt Peak Height` >= 30000)) %>%
    mutate(outcome = case_when(!NEG_01 ~ "NEG_outcome_01", 
                               NEG_01 & !NEG_02 & NEG_03 ~ "NEG_outcome_02",
                               NEG_01 & NEG_02 & !NEG_04 ~ "NEG_outcome_03",
                               NEG_01 & NEG_02 & NEG_04 ~ "NEG_outcome_04",
                               .default = NA))

}


# C. Check IHC conditions

IHC_QC <- function(df) {
    
  ### 1. Filter for only type == IHC
  ### 2. IHC_01: Does it passed POS Control?
  ### 3. IHC_02: Is Ct >= 36.2300?
  ### 4. IHC_03: Is 75.50 <= Tm <= 83.00?
  ### 5. IHC_04: Is 83.00 <= Tm <= 86.00?
  ### 2. IHC_05: Is DV >= 30000?
  ### 3. IHC_06: Is Tm < 75.50?
  ### 4. IHC_07: Is Tm > 86.00?
  ### 5. IHC_08: Is 15000 <= DV <= 30000?
  
  df %>% 
    filter(type == "IHC") %>% 
    mutate(POS_01 = ((CT >= 23.5000) & (CT <= 27.5000))) %>%
    mutate(POS_02 = ((Tm > 83.0) & (Tm <= 86.0))) %>%
    mutate(POS_03 = `Melt Peak Height` >= 30000) %>%
    mutate(IHC_01 = case_when(POS_01 & POS_02 & POS_03 ~ TRUE, 
                              !POS_01 ~ FALSE, 
                              POS_01 & !POS_02 ~ FALSE,
                              POS_01 & POS_02 & !POS_03 ~ FALSE,
                              .default = NA)) %>%
    mutate(IHC_02 = CT >= 36.23000) %>%
    mutate(IHC_03 = Tm >= 75.50 & Tm <= 83.00) %>%
    mutate(IHC_04 = Tm >= 83.00 & Tm <= 86.00) %>%
    mutate(IHC_05 = `Melt Peak Height` >= 30000) %>%
    mutate(IHC_06 = Tm > 86.00) %>%
    mutate(IHC_07 = Tm < 75.50) %>%
    mutate(IHC_08 = `Melt Peak Height` < 30000 & `Melt Peak Height` >= 30000) %>%
    mutate(outcome = case_when(!POS_01 ~ "IHC_outcome_01.1",
                               POS_01 & !POS_02 ~ "IHC_outcome_01.2",
                               POS_01 & POS_02 & !POS_03 ~ "IHC_outcome_01.3",
                               IHC_01 & IHC_02 ~ "IHC_outcome_02",
                               IHC_01 & !IHC_02 & !IHC_03 & IHC_04 & IHC_05 ~ "IHC_outcome_03",
                               IHC_01 & !IHC_02 & !IHC_03 & IHC_04 & !IHC_05 ~ "IHC_outcome_04",
                               IHC_01 & !IHC_02 & !IHC_03 & !IHC_04 & IHC_06 ~ "IHC_outcome_05",
                               IHC_01 & !IHC_02 & !IHC_03 & !IHC_04 & IHC_07 ~ "IHC_outcome_06",
                               IHC_01 & !IHC_02 & IHC_03 & !IHC_05 & !IHC_08 ~ "IHC_outcome_07",
                               IHC_01 & !IHC_02 & IHC_03 & !IHC_05 & IHC_08 ~ "IHC_outcome_08",
                               IHC_01 & !IHC_02 & IHC_03 & IHC_05 ~ "IHC_outcome_09", 
                               .default = NA))

}


append_results <- function (setup_for_NEG, setup_for_POS, setup_for_IHC, result_lookup_table) {
  
  outcome_after_test_long <- (setup_for_NEG %>% pivot_longer(cols = starts_with("NEG_"), names_to = "Test", values_to = "Test Results")) %>%
    bind_rows(setup_for_POS %>% pivot_longer(cols = starts_with("POS_"), names_to = "Test", values_to = "Test Results")) %>% 
    bind_rows(setup_for_IHC %>% pivot_longer(cols = starts_with("IHC_") | starts_with("POS_"), names_to = "Test", values_to = "Test Results")) 
  
  results_after_test_long <- outcome_after_test_long %>% select(-c("Test", "Test Results")) %>% distinct() %>% 
    left_join(result_lookup_table, by = c("type", "outcome")) %>% 
    mutate(across(where(is.numeric), round, digits = 2))
  
  return(list(
    outcome_after_test_long = outcome_after_test_long, 
    results_after_test_long = results_after_test_long
  ))
  
}
