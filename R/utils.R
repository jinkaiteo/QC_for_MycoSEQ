create_truth_table <- function(types = c("POS", "NEG", "IHC"), number_of_outcomes = c(4, 4, 9), df = data.frame(NULL)) {
  
  if(length(types) != length(number_of_outcomes)) {
    stop("length of `types` and `number_of_outcomes` must be the same!")
  } 
  
  if(nrow(df) != 0 & sum(number_of_outcomes) != nrow(df)) {
    stop("length of `df` and the sum of outcomes must be the same!")
  }
  
  seed <- data.frame(types, number_of_outcomes)
  
  perm <- seed |> 
    split(seed$types) |> 
    map(\(A) data.frame(outcome = paste(A$types, "outcome", str_pad(seq(A$number_of_outcomes), pad = 0, width = 2), sep = "_"))) |> 
    list_rbind(names_to = "type")
  
  if(nrow(df) == 0) {
    warning("no attributes were appended.")
    outcomes <- data.frame(perm)
  } else {
    outcomes <- data.frame(perm, df)
  }
  
  return(outcomes)
  
}

truth_table <- create_truth_table(types = c("POS", "NEG", "IHC"), number_of_outcomes = c(4, 4, 9), df = data.frame(Results = c(
  "REVIEW: Failed IHC prevented call",
  "ABSENT: Negative",
  "REVIEW: DPC Detected",
  "ABSENT: Negative",
  "ABSENT: Negative",
  "REIVEW: Non-specific Amplification",
  "ABSENT: Negative",
  "REVIEW: Review DV",
  "PRESENT: Positive",
  "Negative Control Pass", 
  "Negative Control Pass",
  "Negative Control Pass",
  "Negative Control Fail",
  "Positive Control Pass", 
  "Positive Control Fail", 
  "Positive Control Fail",
  "Positive Control Fail"
), color = c(
  "red",
  "blue",
  "yellow",
  "blue",
  "blue",
  "yellow",
  "blue",
  "yellow",
  "red",
  "blue",
  "blue",
  "blue",
  "red",
  "blue",
  "red",
  "red",
  "red"
), check = c(
  "",
  "",
  "Ct, Tm, DV",
  "",
  "",
  "Ct, Tm",
  "",
  "Ct, Tm, DV",
  "Ct, Tm, DV",
  "",
  "",
  "",
  "DV",
  "",
  "Ct",
  "Tm",
  "DV"
)
))


write.csv(truth_table, file = "test.csv")
testA <- read.csv("test.csv")
testA
