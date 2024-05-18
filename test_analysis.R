set.seed(123) # set pseudorandom generator for reprocucibility

# Load all the packages and install them if they are not present
default_colors <- palette.colors()[2:length(palette.colors())]  #exclude black color
requirements <- c("summarytools", "MASS", "car", "effects", "pROC", "glmnetUtils", "mgcv", 
                  "e1071", "class",
                  "stringr", "ggplot2", "scales",  "comprehenr", "randomForest", "dplyr")

for (library_name in requirements){
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name, repos = "https://cloud.r-project.org")
    library(library_name, character.only = TRUE)
  }
}


# Read the dataset
df <- read.csv("data/mental_health_data.csv")
# Remove the column representing the row index
df <- df[, -1]

# Rename existing columns
colnames(df)[colnames(df) == "whether.only.child"] <- "whether_only_child"
colnames(df)[colnames(df) == "birth.place"] <- "birth_place"
colnames(df)[colnames(df) == "family.economic.status"] <- "family_economic_status"
colnames(df)[colnames(df) == "psychiatric.symptoms"] <- "psychiatric_symptoms"
colnames(df)[colnames(df) == "sleeping.disturbance"] <- "sleeping_disturbance"
colnames(df)[colnames(df) == "internet.addiction"] <- "internet_addiction"
colnames(df)[colnames(df) == "hostile.aggression"] <- "hostile_aggression"
colnames(df)[colnames(df) == "self.injury.behaviors"] <- "self_injury_behaviors"
colnames(df)[colnames(df) == "eating.problems"] <- "eating_problems"


# Transforming variables into factors
df$gender <- factor(df$gender,
                    levels = c(0, 1),
                    labels = c("Female", "Male")
)
df$whether_only_child <- factor(df$whether_only_child,
                                levels = c(0, 1),
                                labels = c("No", "Yes")
)
df$birth_place <- factor(
  df$birth_place,
  levels = 0:3,
  labels = c("Countryside", "Town", "SmallCity", "MediumToLargeCities")
)
df$family_economic_status <- factor(
  df$family_economic_status,
  levels = 0:4,
  labels = c("ExtremelyPoor", "Poor", "Average", "Good", "Rich")
)
df$major <- factor(df$major,
                   levels = 0:2,
                   labels = c("Liberal", "Science", "Art")
)
df$grade <- factor(
  df$grade,
  levels = 0:5,
  labels = c(
    "Postgraduate",
    "UndergraduateGradeFive",
    "Junior",
    "Sophomore",
    "Freshman",
    "Senior"
  )
)

general_cols <- c("gender", "whether_only_child", "birth_place", "family_economic_status", "major", "grade")
symptoms_cols <- c("psychiatric_symptoms", "dependence", "impulsivity", "compulsion", "sleeping_disturbance", "internet_addiction", "hostile_aggression", "self_injury_behaviors", "eating_problems")

scaled_suicide <- df$suicide - min(df$suicide)
# Transforming the variables into factors
df$suicide <- as.factor(df$suicide>8)




# mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions,
#         highlighting = "Admit", highlighting_fill = c("lightblue", "pink"),
#         direction = c("v","h","v"))


plot_freq_by_category <- function(data, freq_var, binary_category) {
  
  pred_label <- NULL
  if (is.factor(data[[freq_var]])) {
  df[[freq_var]] <- factor(df[[freq_var]],
                                levels = levels(df[[freq_var]]),
                                labels = to_vec(for (l in levels(df[[freq_var]]))
                                  split_camel_case(l)))
  pred_label <- var_name_to_label(freq_var, "")
  
  }else{
    pred_label <- var_name_to_label(freq_var)
  }
  
  resp_label <- var_name_to_label(binary_category, "")
  # Create the plot
  p <- ggplot(data, aes(x = .data[[freq_var]], fill = .data[[binary_category]])) +
    geom_bar(position = "dodge") +
    labs(title=paste("Frequency of", pred_label, sep = " "), x = pred_label, y = "Frequency", fill = resp_label) +
    theme_minimal()
  
  if (is.factor(data[[freq_var]])) {
    p <- p + scale_x_discrete(labels = label_wrap(15))
  }
  print(p)
}
plot_freq_by_category(df, "gender", "suicide")

