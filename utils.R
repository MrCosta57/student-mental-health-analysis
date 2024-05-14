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


plot_counts <- function(df,
                        col_name,
                        filter_col_name,
                        filter_val,
                        xlab,
                        ylab,
                        color) {
  counts <- table(df[[col_name]][df[[filter_col_name]] == filter_val])
  
  # Convert the counts to a dataframe
  counts_df <- as.data.frame(counts)
  
  # Rename columns for clarity
  names(counts_df) <- c("Factor", "Count")
  
  counts_df$Factor <- factor(
    counts_df$Factor,
    levels = levels(counts_df$Factor),
    labels = to_vec(for (l in levels(counts_df$Factor))
      split_camel_case(l))
  )
  # Create the bar plot
  plot(
    ggplot(counts_df, aes(x = Factor, y = Count)) +
      geom_bar(stat = "identity", fill = color) +
      scale_x_discrete(labels = label_wrap(15)) +
      labs(x = xlab, y = ylab)
  )
}


var_name_to_label <- function(var_name, end_text = "score") {
  return(paste(str_to_title(str_replace_all(var_name, "_", " ")), end_text, sep =
                 if (end_text != "")
                   " "
               else
                 ""))
}


split_camel_case <- function(input_string) {
  # Use regular expression to split camel case
  result <- gsub("(?<=[a-z])(?=[A-Z])", " ", input_string, perl = TRUE)
  return(result)
}
