# 1. Load excel into R
# 2. extract the relevant data/information
# 3. Perform the QC checks
# 4. Create a report
# 5. Save/export the report

library(readxl)
source_file <- choose.files()
raw_sample_setup <- read_excel(source_file, sheet = "Sample Setup", skip = 47)
raw_Ct_results <- read_excel(source_file, sheet = "Results", skip = 47)
raw_Tm_results <- read_excel(source_file, sheet = "Melt Curve Result", skip = 47)

# ----- Extracting Relevant Data ------------

# 1. Identify UNK, IHC, Pos, Neg
install.packages("tidyverse")
library(tidyverse)

## when name == "Positive", type = "POS"
## when name == "Negative", type = "NEG"
## when type == "IHC", type = "IHC"
## when is.na(type) and !is.na(name), type = "UNK"
## when is.na(type) and is.na(name), type = NA

idenitfy_sample_type <- function(df) {
  
  if ("Sample Name" %in% colnames(df)) {
    results_after_id <- df %>% 
      filter(!is.na(`Sample Name`)) %>%
      separate(col = `Sample Name`, into = c('name', 'type'), sep = '_', remove = FALSE, fill = 'right') %>%
      mutate(type = case_when(name == "Positive" ~ "POS", name == "Negative" ~ "NEG", is.na(type) & !is.na(name) ~ "UNK", .default = type))
  } else if ("Sample" %in% colnames(df)) {
    results_after_id <- df %>% 
      filter(!is.na(`Sample`)) %>%
      separate(col = `Sample`, into = c('name', 'type'), sep = '_', remove = FALSE, fill = 'right') %>%
      mutate(type = case_when(name == "Positive" ~ "POS", name == "Negative" ~ "NEG", is.na(type) & !is.na(name) ~ "UNK", .default = type))
  } else {
    warning("There is no 'Sample Name' or 'Sample' column in the dataframe.")
    stop("Identification process terminated.")
  }
  
  return(results_after_id)
  
}

setup_after_id <- idenitfy_sample_type(raw_sample_setup)
Ct_after_id <- idenitfy_sample_type(raw_Ct_results)
Tm_after_id <- idenitfy_sample_type(raw_Tm_results)


# ------- Perform the QC checks -----------------

## A. Positive controls

### 1. Filter for only type == POS
setup_for_POS <- setup_after_id %>% 
  filter(type == "POS")

Ct_for_POS <- Ct_after_id %>%
  filter(type == "POS")

Tm_for_POS <- Tm_after_id %>%
  filter(type == "POS")
  
### 2. POS_01: Check for 23.5000 <= Ct <= 27.5000
Ct_for_POS <- Ct_for_POS %>% 
  mutate(POS_01 = ((CT >= 23.5000) & (CT <= 27.5000)))

### 3. Check for 83.0 < Tm < 86.0
### 4. Check for DV >= 30000
Tm_for_POS <- Tm_for_POS %>%
  mutate(POS_02 = ((Tm > 83.0) & (Tm <= 86.0))) %>%
  mutate(POS_03 = `Melt Peak Height` >= 30000)

results_for_POS <- setup_for_POS %>%
  left_join(Ct_for_POS) %>% 
  left_join(Tm_for_POS) %>%
  mutate(result = POS_01 & POS_02 & POS_03)


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
