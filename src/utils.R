# Helper functions for data cleaning and visualization

var_name_to_label <- function(var_name, end_text = "score") {
  return(paste(str_to_sentence(str_replace_all(var_name, "_", " ")), end_text,
    sep =
      if (end_text != "") {
        " "
      } else {
        ""
      }
  ))
}

var_name_to_intitle <- function(var_name) {
  return(paste(str_replace_all(var_name, "_", " "), sep = " "))
}

split_camel_case <- function(input_string) {
  # Use regular expression to split camel case
  result <- gsub("(?<=[a-z])(?=[A-Z])", " ", input_string, perl = TRUE)
  return(result)
}

reduce_df_categories_to3 <- function(df_src, col_name, thresholds, levels) {
  res <- factor(
    ifelse(
      df_src[[col_name]] < thresholds[1],
      levels[1],
      ifelse(df_src[[col_name]] < thresholds[2], levels[2], levels[3])
    ),
    levels = levels
  )
  return(res)
}

# Function to extract predictors from a formula that are in a given vector
extract_predictors_in_vec <- function(formula, input_vector) {
  # Extract the terms object from the formula
  terms_obj <- terms(formula)

  # Extract the variable names from the terms object
  vars <- attr(terms_obj, "variables")

  # Convert to character and exclude the response variable
  predictors <- as.character(vars[-1])

  # Return the intersection of predictors and input_vector
  common_predictors <- intersect(predictors, input_vector)

  return(common_predictors)
}

jitter_subset <- function(data, cols, factor = 0.0001) {
  return(data %>%
    mutate(across(all_of(cols), ~ jitter(.x, factor = factor))))
}

# Define the k-fold cross-validation function
k_fold_cv <- function(
    model_formula, model_func, data, response_col_name = "suicide",
    is_generative = FALSE, is_knn = FALSE, is_nb = FALSE, is_glmnet = FALSE,
    symptoms_cols = NULL, n_fold = 5, ...) {
  # Ensure data is a dataframe
  if (!is.data.frame(data)) {
    stop("The data argument must be a dataframe.")
  }

  # Number of observations
  n <- nrow(data)
  folds <- sample(rep(1:n_fold, length = n))

  # Initialize variables to store results
  sensitivities <- c()
  specificity <- c()
  accuracy <- c()
  thresholds <- c()
  auc_vals <- c()

  # Perform k-fold cross-validation
  for (i in 1:n_fold) {
    # Create training and testing sets
    val_indices <- which(folds == i)
    train_data <- data[-val_indices, ]
    val_data <- data[val_indices, ]
    gt <- val_data[[response_col_name]]
    # Train the model
    if (is_knn) {
      # Add a small noise to solve "too many ties in knn()" error
      jitter_train <- jitter_subset(train_data, symptoms_cols)
      jitter_test <- jitter_subset(val_data, symptoms_cols)
      x.train <- model.matrix(model_formula, data = jitter_train[, !(names(jitter_train) %in% response_col_name)])[, -1]
      x.test <- model.matrix(model_formula, data = jitter_test[, !(names(jitter_test) %in% response_col_name)])[, -1]
      pred <- model_func(train = x.train, test = x.test, cl = train_data[[response_col_name]], ...)
      conf_matrix_knn <- table(gt, pred)
      TP_knn <- conf_matrix_knn[2, 2]
      TN_knn <- conf_matrix_knn[1, 1]
      FP_knn <- conf_matrix_knn[1, 2]
      FN_knn <- conf_matrix_knn[2, 1]
      sensitivities <- c(sensitivities, TN_knn / (TN_knn + FP_knn))
      specificity <- c(specificity, TP_knn / (TP_knn + FN_knn))
      accuracy <- c(accuracy, (TP_knn + TN_knn) / (TP_knn + TN_knn + FP_knn + FN_knn))
    } else {
      model <- model_func(formula = model_formula, data = data, ...)

      if (is_generative) {
        pred <- predict(model, newdata = val_data, type = "response")$posterior[, 2]
      } else if (is_nb) {
        pred <- predict(model, newdata = val_data, type = "raw")[, 2]
      } else if (is_glmnet) {
        x <- model.matrix(model_formula, data = train_data)[, -1]
        y <- train_data[[response_col_name]]
      } else {
        pred <- predict(model, newdata = val_data, type = "response")
      }
      roc_obj <- roc(gt ~ as.vector(pred))
      auc_val <- auc(roc_obj)
      metrics <- coords(roc_obj, x = "best", ret = c("threshold", "sensitivity", "specificity", "accuracy"))

      thresholds <- c(thresholds, metrics[1][[1]])
      auc_vals <- c(auc_vals, auc_val)
      sensitivities <- c(sensitivities, metrics[2][[1]])
      specificity <- c(specificity, metrics[3][[1]])
      accuracy <- c(accuracy, metrics[4][[1]])
    }
  }

  # Return average sensitivity
  mean_threshold <- if (length(thresholds) == 0) c(-1) else mean(thresholds)
  mean_sensitivity <- mean(sensitivities)
  mean_specificity <- mean(specificity)
  mean_accuracy <- mean(accuracy)
  mean_auc_val <- if (length(auc_vals) == 0) c(-1) else mean(auc_vals)

  result <- list(
    "mean_threshold" = mean_threshold,
    "mean_sensitivity" = mean_sensitivity,
    "mean_auc_val" = mean_auc_val,
    "mean_specificity" = mean_specificity,
    "mean_accuracy" = mean_accuracy
  )
  return(result)
}

evaluate_test_set <- function(model, df_test, threshold, response_col_name = "suicide", is_knn = FALSE, is_generative = FALSE, is_nb = FALSE) {
  if (is_knn) {
    pred <- predict(model, newdata = df_test)
  } else {
    if (is_generative) {
      pred <- as.vector(predict(model, newdata = df_test, type = "response")$posterior[, 2]) > threshold
    } else if (is_nb) {
      pred <- as.vector(predict(model, newdata = df_test, type = "raw")[, 2]) > threshold
    } else {
      pred <- as.vector(predict(model, newdata = df_test, type = "response")) > threshold
    }
  }

  conf_matrix <- table(df_test[[response_col_name]], pred)
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[1, 2]
  FN <- conf_matrix[2, 1]
  sensitivity <- TN / (TN + FP)
  specificity <- TP / (TP + FN)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  result <- list(
    "sensitivity" = sensitivity,
    "specificity" = specificity,
    "accuracy" = accuracy
  )
  return(result)
}
# Load the dataset
# data <- read.csv("your_dataset.csv")  # Load your data here

# Run k-fold cross-validation
# mean_sensitivity <- k_fold_cv(glm_model_func, data, response_col = "target_column", k = 5)
# print(mean_sensitivity)


# plot_interaction_exploration <- function(df_numeric,
#                                          response_name,
#                                          predictor_name,
#                                          df_categ,
#                                          interaction_name,
#                                          colors,
#                                          jitter_ammount,
#                                          xlab,
#                                          ylab) {
#   i <- 1
#   for (l in levels(df_categ[[interaction_name]])) {
#     if (i == 1) {
#       plot(
#         jitter(df_numeric[[response_name]][df_categ[[interaction_name]] == l], jitter_ammount) ~
#           jitter(df_numeric[[predictor_name]][df_categ[[interaction_name]] == l], jitter_ammount),
#         col = colors[i],
#         xlab = xlab,
#         ylab = ylab
#       )
#     }
#     else{
#       points(
#         jitter(df_numeric[[response_name]][df_categ[[interaction_name]] == l], jitter_ammount) ~
#           jitter(df_numeric[[predictor_name]][df_categ[[interaction_name]] == l], jitter_ammount),
#         col = colors[i]
#       )
#
#     }
#     i <- i + 1
#   }
# }
