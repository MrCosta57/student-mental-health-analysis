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

plot_freq_by_category <- function(data, freq_var, binary_category) {
  pred_label <- NULL
  if (is.factor(data[[freq_var]])) {
    data[[freq_var]] <- factor(data[[freq_var]],
      levels = levels(data[[freq_var]]),
      labels = to_vec(for (l in levels(data[[freq_var]])) {
        split_camel_case(l)
      })
    )
    pred_label <- var_name_to_label(freq_var, "")
  } else {
    pred_label <- var_name_to_label(freq_var)
  }

  resp_label <- var_name_to_label(binary_category, "")

  # Create the plot
  p <- ggplot(
    data %>% group_by(.data[[freq_var]], .data[[binary_category]]) %>% summarise(freq = n()),
    aes(x = .data[[freq_var]], y = freq, fill = .data[[binary_category]])
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = paste(
        "Frequency of",
        var_name_to_intitle(freq_var),
        "by",
        var_name_to_intitle(binary_category),
        "values",
        sep = " "
      ),
      x = pred_label,
      y = "Frequency",
      fill = resp_label
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(
      aes(label = freq),
      color = "black",
      position = position_dodge(0.9),
      vjust = -0.5,
      # Adjust vertical position for better readability
      size = 3
    ) +
    scale_fill_brewer(palette = "Paired")

  if (is.factor(data[[freq_var]])) {
    p <- p + scale_x_discrete(labels = label_wrap(15))
  }
  print(p)
}


plot_proportion_by_category <- function(data, freq_var, binary_category) {
  pred_label <- NULL
  if (is.factor(data[[freq_var]])) {
    data[[freq_var]] <- factor(data[[freq_var]],
      levels = levels(data[[freq_var]]),
      labels = to_vec(for (l in levels(data[[freq_var]])) {
        split_camel_case(l)
      })
    )
    pred_label <- var_name_to_label(freq_var, "")
  } else {
    pred_label <- var_name_to_label(freq_var)
  }

  resp_label <- var_name_to_label(binary_category, "")

  p <- ggplot(
    data %>% group_by(.data[[freq_var]], .data[[binary_category]]) %>% summarise(freq = n()) %>% mutate(ratio = freq / sum(freq)),
    aes(x = .data[[freq_var]], y = ratio, fill = .data[[binary_category]])
  ) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(aes(label = scales::percent(ratio, accuracy = 0.1)),
      position =
        position_fill(vjust = 0.5)
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = paste(
        "Percentage of",
        var_name_to_intitle(freq_var),
        "by",
        var_name_to_intitle(binary_category),
        "values",
        sep = " "
      ),
      x = pred_label,
      y = "Percentage",
      fill = resp_label
    ) +
    scale_fill_brewer(palette = "Paired")

  if (is.factor(data[[freq_var]])) {
    p <- p + scale_x_discrete(labels = label_wrap(15))
  }

  print(p)
}