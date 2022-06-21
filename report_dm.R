s_summary <- function(x) {
  if (is.numeric(x)) {
    in_rows(
 #     "n" = rcell(sum(!is.na(x)), format = "xx"),
      "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)), format = "xx.xx (xx.xx)"),
      "IQR" = rcell(IQR(x, na.rm = TRUE), format = "xx.xx"),
      "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
    )
  } else if (is.character(x)) {
    
    vs <- as.list(table(x))
    do.call(in_rows, lapply(vs, rcell, format = "xx"))
    
  } else (
    stop("type not supported")
  )
}

myfun <- function(x) {
  vs <- table(x)
  sumall <- sum(vs)
  do.call(in_rows, lapply(as.list(vs), function(y) rcell(c(y, y/sumall), format = "xx (xx.xx%)")))
}

layout <- basic_table(title = "Demographic Baseline Characteristics: Overview",
                      subtitles = c("\nNDA: 12345", "Study: CDISCPILOT01", paste("Analysis run:", format(Sys.Date(), "%Y-%m-%d")))) %>% 
  split_cols_by(var = "ARM") %>%
  add_overall_col("Overall") %>% 
  add_colcounts() %>%
  analyze(c("AGE"), s_summary) %>% 
  analyze("SEX", myfun)
  # split_rows_by("SEX") %>% 
  # summarize_row_groups(cfun = function(df, labelstr, .N_col, ...){
  #   in_rows( rcell(nrow(df) * c(1, 1/.N_col), format = "xx (xx.xx%)"),
  #            .labels = paste0(labelstr, ": count (perc.)"))
  # })
build_table(layout, adsl) 