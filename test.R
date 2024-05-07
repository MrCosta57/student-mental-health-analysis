df <- read.csv("data/mental_health_data.csv", )
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
df$gender <- factor(df$gender, levels = c(0, 1), labels = c("female", "male"))
df$whether_only_child <- factor(df$whether_only_child, levels = c(0, 1), labels = c("no", "yes"))
df$birth_place <- factor(df$birth_place, levels = 0:3, labels = c("countryside", "town", "small_city", "medium_to_large_cities"))
df$family_economic_status <- factor(df$family_economic_status, levels = 0:4, labels = c("extremely_poor", "poor", "average", "good", "rich"))
df$major <- factor(df$major, levels = 0:2, labels = c("liberal", "science", "art"))
df$grade <- factor(df$grade, levels = 0:5, labels = c("postgraduate", "undergraduate_grade_five", "junior", "sophomore", "freshman", "senior"))
df$psychiatric_symptoms <- factor(df$psychiatric_symptoms)
df$suicide <- factor(df$suicide)
df$dependence <- factor(df$dependence)
df$impulsivity <- factor(df$impulsivity)
df$compulsion <- factor(df$compulsion)
df$sleeping_disturbance <- factor(df$sleeping_disturbance)
df$internet_addiction <- factor(df$internet_addiction)
df$hostile_aggression <- factor(df$hostile_aggression)
df$self_injury_behaviors <- factor(df$self_injury_behaviors)
df$eating_problems <- factor(df$eating_problems)



model<-glm(suicide ~ gender +family_economic_status, data=df, family="binomial")

