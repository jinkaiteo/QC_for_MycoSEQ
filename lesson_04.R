# 1. Load excel into R
# 2. Extract the relevant data/information
# 3. Perform the QC checks
# 4. Create a report
# 5. Save/export the report


library(readxl)
library(tidyverse)

source_file <- choose.files()
raw_sample_setup <- read_excel(path = source_file, sheet = "Sample Setup", skip = 47)
raw_Ct_results <- read_excel(path = source_file, sheet = "Results", skip = 47)
raw_Tm_results <- read_excel(path = source_file, sheet = "Melt Curve Result", skip = 47)

## I hope you had the chance to play with `left_join`. With that knowledge in mind, lets do some minor changes:
## Recall previously, we tried to filter 3 dataframes (setup_after_id, Ct_after_id and Tm_after_id) individually.
## After which we tested if the Ct, Tm or DV meets the QC criteria.
## Finally, we joined those 3 dataframes at the last step.
## Note in this algorithm, we did 3 filters -> n QC steps -> 1 left_join, a total of 4 + n steps.
## We can do one better! Lets instead, do the left_join at the start:
## 1 left_join -> 1 filter -> n QC steps, a total of 1 + n steps, therefore more efficient!
## Lets make that change!

# Here we perform the initial left_join. Do note that we use the `by` argument (arg) to explicitly indicate the columns to match.
raw_results <- raw_sample_setup %>%
  left_join(raw_Ct_results, by = c("Well", "Well Position", "Target Name", "Sample Name", "Task", "Reporter", "Quencher", "Quantity", "Comments")) %>%
  left_join(raw_Tm_results, by = c("Well", "Well Position", "Target Name", "Sample Name" = "Sample")) # note in here that in the name of the column in raw_sample_setup and raw_Ct_results df is `Sample Name` while the corresponding name in raw_Tm_results is `Sample`. 

# ----- Extracting Relevant Data ------------

# 1. Identify UNK, IHC, Pos, Neg
## when name == "Positive", type = "POS"
## when name == "Negative", type = "NEG"
## when type == "IHC", type = "IHC"
## when is.na(type) and !is.na(name), type = "UNK"
## when is.na(type) and is.na(name), type = NA

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


setup_after_id <- idenitfy_sample_type(df = raw_results)


# ------- Perform the QC checks -----------------

## A. Positive controls

### 1. Filter for only type == POS
### 2. POS_01: Check for 23.5000 <= Ct <= 27.5000
### 3. POS_02: Check for 83.0 < Tm < 86.0
### 4. POS_03: Check for DV >= 30000
setup_for_POS <- setup_after_id %>% 
  filter(type == "POS") %>% 
  mutate(POS_01 = ((CT >= 23.5000) & (CT <= 27.5000))) %>%
  mutate(POS_02 = ((Tm > 83.0) & (Tm <= 86.0))) %>%
  mutate(POS_03 = `Melt Peak Height` >= 30000) %>%
  mutate(outcome = case_when(POS_01 & POS_02 & POS_03 ~ "POS_outcome_01", 
                            !POS_01 ~ "POS_outcome_02", 
                            POS_01 & !POS_02 ~ "POS_outcome_03",
                            POS_01 & POS_02 & !POS_03 ~ "POS_outcome_04",
                            .default = NA))

# Truth table for OR (x | y)
# x   y   results
# T   T   T
# T   F   T
# F   T   T
# F   F   F

# Truth table for AND (x & y)
# x   y   results
# T   T   T
# T   F   F
# F   T   F
# F   F   F

# Truth table for NOT (!x)
# x   results
# T   F
# F   T


# B. Check NTC conditions

### 1. Filter for only type == NTC
### 2. NTC_01: Is Ct < 36.2300?
### 3. NTC_02: Is 75.50 <= Tm <= 83.00?
### 4. NTC_03: Is Tm < 75.50 or Tm > 86.00?
### 5. NTC_04: Is DV > 30000?
setup_for_NEG <- setup_after_id %>% 
  filter(type == "NEG") %>% 
  mutate(NEG_01 = CT < 36.2300) %>%
  mutate(NEG_02 = (Tm > 75.50) & (Tm < 83.00)) %>%
  mutate(NEG_03 = (Tm < 75.50) | (Tm > 86.00)) %>%
  mutate(NEG_04 = (`Melt Peak Height` >= 30000)) %>%
  mutate(outcome = case_when(!NEG_01 ~ "NEG_outcome_01", 
                            NEG_01 & !NEG_02 & NEG_03 ~ "NEG_outcome_02",
                            NEG_01 & NEG_02 & !NEG_04 ~ "NEG_outcome_03",
                            NEG_01 & NEG_02 & NEG_04 ~ "NEG_outcome_04"))


# C. Check IHC conditions

### 1. Filter for only type == IHC
### 2. IHC_01: Does it passed POS Control?
### 3. IHC_02: Is Ct >= 36.2300?
### 4. IHC_03: Is 75.50 <= Tm <= 83.00?
### 5. IHC_04: Is 83.00 <= Tm <= 86.00?
### 2. IHC_05: Is DV >= 30000?
### 3. IHC_06: Is Tm < 75.50?
### 4. IHC_07: Is Tm > 86.00?
### 5. IHC_08: Is 15000 <= DV <= 30000?

setup_for_IHC <- setup_after_id %>% 
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

# I hope you could follow this session! To recap, we learned:
# 1. how to use AND (&), OR (|) and NOT (!)
# 2. how to review our code and make our code neater

# Food for thought:
# The last `mutate` for each of the QC check essentially explores the different conditions for each QC tests to produce a result.
# Using the `mutate` function, we create one column showing the results. 
# Imagine, if we would like to instead add another column specifying the colors used to display for each result
# (ie. yellow for Review, red for Fail or Present, blue for Pass or Absent), how do we add that?
# One solution is to copy the whole `mutate` block and change the results - that is possible, but is there a more elegant method?
# YOU TRY: using what you had learnt so far, can you write a function to allow addition of n columns when checking the conditions? 

# d. look-up result conditions

lookup_table <- read.csv(file = "config/result_lookup_table.csv")

outcome_after_test_long <- (setup_for_NEG %>% pivot_longer(cols = starts_with("NEG_"), names_to = "Test", values_to = "Test Results")) %>%
  bind_rows(setup_for_POS %>% pivot_longer(cols = starts_with("POS_"), names_to = "Test", values_to = "Test Results")) %>% 
  bind_rows(setup_for_IHC %>% pivot_longer(cols = starts_with("IHC_") | starts_with("POS_"), names_to = "Test", values_to = "Test Results")) %>% 
  select(c("Well Position", "name", "type", "Sample Name", "CT", "Tm", "Melt Peak Height", "outcome", "Test", "Test Results"))

results_after_test_long <- outcome_after_test_long %>% select(-c("Test", "Test Results")) %>% distinct() %>% 
  left_join(lookup_table, by = c("type", "outcome")) %>% 
  mutate(across(where(is.numeric), round, digits = 2))


## to dos for lesson 06
## 1. Load the other worksheets into R - Amplification Data & Raw TM data
## 2. Plot CT (fluro vs cycle)
## 3. Plot Tm (fluro/DV vs temp)
## 4. Put everything in Rmarkdown and complete the report
## 5. Can you make all the QC checks in to a function?