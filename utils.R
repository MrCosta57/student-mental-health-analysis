# Helper functions for data cleaning and visualization

print_summary_custom <- function(df) {
  print(
    dfSummary(
      df,
      max.distinct.values = 20,
      na.col = FALSE,
      valid.col = FALSE,
      headings = FALSE
    ),
    method = "render",
    footnote = NA
  )
}


reduce_df_categories <- function(df_src, col_name, thresholds) {
  res <- factor(ifelse(
    df_src[[col_name]] < thresholds[1],
    "Low",
    ifelse(df_src[[col_name]] < thresholds[2], "Medium", "High")
  ),
  levels = c("Low", "Medium", "High"))
  return(res)
}


plot_interaction_exploration <- function(df_numeric,
                                         response_name,
                                         predictor_name,
                                         df_categ,
                                         interaction_name,
                                         colors,
                                         jitter_ammount,
                                         xlab,
                                         ylab) {
  i <- 1
  for (l in levels(df_categ[[interaction_name]])) {
    if (i == 1) {
      plot(
        jitter(df_numeric[[response_name]][df_categ[[interaction_name]] == l], jitter_ammount) ~ 
          jitter(df_numeric[[predictor_name]][df_categ[[interaction_name]] == l], jitter_ammount),
        col = colors[i],
        xlab = xlab,
        ylab = ylab
      )
    }
    else{
      points(
        jitter(df_numeric[[response_name]][df_categ[[interaction_name]] == l], jitter_ammount) ~ 
          jitter(df_numeric[[predictor_name]][df_categ[[interaction_name]] == l], jitter_ammount),
        col = colors[i]
      )
      
    }
    i <- i + 1
  }
}