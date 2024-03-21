#Explorative analysis 
# Data upload
dataset <- read.csv("dataset.csv")

# Dataset exploration
head(dataset)

# dataset summary
summary(dataset)

# Install dplyr package to explore data 
install.packages("dplyr")
library(dplyr)

# Categorical variable distribution 
# Movement type 
table(dataset$Movement.type)

# Light conditions
table(dataset$Light.conditions)

# Sensitivity setting
table(dataset$Sensitivity.setting.)

# relations 
# Use dplyr to count occurence of variables
count_movimento_light <- dataset %>% count(Movement.type., Light.conditions)
print(count_movimento_light)

count_movimento_sensitivity <- dataset %>% count(Movement.type., Sensitivity.setting.)
print(count_movimento_sensitivity)

#relationships between variables 
#ggplot2 to create plots 
install.packages("ggplot2")
library(ggplot2)

# Bar Chart to describe the Distribution of Movement Type by Light Condition
ggplot(data = count_movimento_light, aes(x = Movement.type., y = n, fill = Light.conditions)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Movement Type by Light Condition", x = "Movement Type", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("darkgoldenrod1", "blue"))

# Bar Chart to describe Distribution of Movement Type by Camera Sensitivity
ggplot(data = count_movimento_sensitivity, aes(x = Movement.type., y = n, fill = Sensitivity.setting.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Movement Type by Camera Sensitivity", x = "Movement Type", y = "Frequency") +
  theme_minimal()

# plot automatic classification vs movement type and sensitivity and light
# Aggregate the data
data_summary <- dataset %>%
  group_by(Movement.type., Light.conditions, Sensitivity.setting., Automatic.class..) %>%
  summarise(count = n())

# Plot automatic annotations by movement type, light condition, and sensitivity setting
sensitivity_order <- c("High", "Medium", "Low")

# Convert SensitivitySetting to factor with the desired order
dataset$Sensitivity.setting. <- factor(dataset$Sensitivity.setting., levels = sensitivity_order)

# Aggregate the data
data_summary <- dataset %>%
  group_by(Movement.type., Light.conditions, Sensitivity.setting., Automatic.class..) %>%
  summarise(count = n())

# Plot automatic annotations by movement type, light condition, and sensitivity setting
ggplot(data_summary, aes(x = Movement.type., y = count, fill = Automatic.class.., color = Automatic.class..)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Light.conditions * Sensitivity.setting., scales = "free") +
  labs(title = "Automatic Classification by Movement Type, Light Condition, and Sensitivity Setting",
       x = "Movement Type", y = "Count",
       fill = "Automatic Classification", color = "Automatic Classification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


table_auto_manual <- table(dataset$Automatic.class.., dataset$Manual.class.)
print(table_auto_manual)

#Packages needed to create crosstables 
install.packages("gmodels")
library(gmodels)

# chi-square test to evaluate association between automatic and manual classification system
chisq.test(table_auto_manuale)

# Cross table
CrossTable(dataset$Automatic.class.., dataset$Manual.class., prop.chisq = FALSE, chisq = TRUE)

table_auto_manuale <- table(dataset$Automatic.class.., dataset$Manual.class.)

# Conversion of table to dataframe to use ggplot2
df_table <- as.data.frame.table(table_auto_manuale)

# Plot
ggplot(df_table, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cross-tabulation Table between Automatic and Manual Classification", x = "Automatic Classification", y = "Frequency") +
  scale_fill_manual(name = "Manual Classification", values = c("purple")) +  # Specifica i colori desiderati
  theme_minimal()

#Performance metrics 
# Accuracy Computation 
accuracy <- sum(dataset$Automatic.class.. == dataset$Manual.class.) / nrow(dataset)
print(paste("Accuracy:", accuracy))

# Precision computation
precision <- function(pred, true, class_label) {
  tp <- sum(pred == class_label & true == class_label)
  fp <- sum(pred == class_label & true != class_label)
  if (tp + fp == 0) {
    return(0)
  } else {
    return(tp / (tp + fp))
  }
}
precision_class2 <- precision(dataset$Automatic.class.., dataset$Manual.class., "Human")

# recall calculation 
recall <- function(pred, true, class_label) {
  tp <- sum(pred == class_label & true == class_label)
  fn <- sum(pred != class_label & true == class_label)
  if (tp + fn == 0) {
    return(0)
  } else {
    return(tp / (tp + fn))
  }
}
recall_class2 <- recall(dataset$Automatic.class.., dataset$Manual.class., "Human")


# F1-score calculation
f1_score <- function(precision, recall) {
  if (precision + recall == 0) {
    return(0)
  } else {
    return(2 * precision * recall / (precision + recall))
  }
}

f1_score_class2 <- f1_score(precision_class2, recall_class2)


performance_metrics <- data.frame(
  Metric = c("Accuracy", "Recall (Human)", "F1-score (Human)"),
  Value = c(accuracy,recall_class2,f1_score_class2)
)

# Plot performance metrics
ggplot(performance_metrics, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_text(aes(label = format(Value, nsmall = 4)), vjust = -0.5, size = 4) +
  labs(title = "Performance Metrics Summary",
       x = "Metric",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

