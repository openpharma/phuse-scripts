library(dplyr)
library(rtables)
library(stringr)

s_summary <- function(x) {
  if (is.numeric(x)) {
    in_rows(
      "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)), format = "xx.xx (xx.xx)"),
      "Median" = rcell(c(median(x, na.rm = TRUE)), format = "xx.xx"),
      "Q1 - Q3" = rcell(quantile(x, probs=c(.25, .75), na.rm = TRUE), format = "xx.xx - xx.xx"),
      "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
    )
  } else if (is.factor(x)) {
    
    vs <- table(x)
    sumall <- sum(vs, na.rm = TRUE)
    do.call(in_rows, lapply(as.list(vs), function(y) rcell(c(y, y/sumall), format = "xx (xx.x%)")))
    
  } else (
    stop("type not supported")
  )
}


layout <- basic_table(title = "Demographic Baseline Characteristics: Overview",
                      subtitles = c("\nNDA: 12345", "Study: CDISCPILOT01", paste("Analysis run:", format(Sys.Date(), "%Y-%m-%d")))) %>% 
  split_cols_by(var = "ARM") %>%
  add_overall_col("Overall") %>% 
  add_colcounts() %>%
  analyze(c("AGE", "AGEGR1", "SEX", "RACE", "ETHNIC"), var_labels=c("Age", "Age Group", "Sex", "Race", "Ethnicity"), afun=s_summary)
build_table(layout, adsl) 
