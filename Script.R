library(dplyr)
library(readr)



data<- read_csv("/Users/macbook/Downloads/Maternal/Maternal Health Risk Data Set - Maternal Health Risk Data Set.csv")
View(data)
numeric_vars <- data %>% select(Age, SystolicBP, DiastolicBP, BS, BodyTemp, HeartRate)
View (numeric_vars)
numeric_vars %>% 
  summarise(across(everything(), list(
    mean = ~mean(. , na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )))
library(tidyr)
summary_table <- numeric_vars %>%
  summarise(across(everything(), list(
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE)
  ))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)

# View the summary table
print(summary_table)



library(ggplot2)
library(dplyr)

cat_summary <- data %>%
  group_by(Level = RiskLevel) %>%
  summarise(
    Count = n(),
    Percentage = round((n() / nrow(data)) * 100, 2)
  )

ggplot(cat_summary, aes(x = Level, y = Count, fill = Level)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +  # <- This adds data labels
  labs(
    title = "Risk Levels",
    x = "Level",
    y = "Count"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


library(ggplot2)
library(reshape2)
library(RColorBrewer)

# Create correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Melt the correlation matrix
cor_melted <- melt(cor_matrix)

# Plot heatmap with labels
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3, color = "black") +  # Add labels here
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "", y = "")




# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Select numeric variables
numeric_vars <- data %>%
  select(Age, SystolicBP, DiastolicBP, BS, BodyTemp, HeartRate)

# Convert to long format for faceted plotting
long_data <- pivot_longer(numeric_vars, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap
ggplot(long_data, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Numeric Variables", x = "", y = "Frequency")





### MACHINE LEARNING IMPLEMENTATION


# Load necessary libraries
library(caret)
library(rpart)
library(randomForest)
library(dplyr)

# Convert RiskLevel to factor (categorical)
data$RiskLevel <- as.factor(data$RiskLevel)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$RiskLevel, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Create a data frame for visualization
train_test_sizes <- data.frame(
  Set = c("Train", "Test"),
  Size = c(nrow(train_data), nrow(test_data))
)

# Plot the train and test sizes
ggplot(train_test_sizes, aes(x = Set, y = Size, fill = Set)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = Size), vjust = -0.3, size = 5) +
  labs(title = "Train vs Test Size", x = "Dataset", y = "Number of Samples") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


# Select numeric variables for the model
numeric_vars <- c("Age", "SystolicBP", "DiastolicBP", "BS", "BodyTemp", "HeartRate")

# Standardize the numeric variables (if needed)
preProcess <- preProcess(train_data[, numeric_vars], method = c("center", "scale"))
train_data[, numeric_vars] <- predict(preProcess, train_data[, numeric_vars])
test_data[, numeric_vars] <- predict(preProcess, test_data[, numeric_vars])

# Check the pre processing
head(train_data)






# Train Decision Tree model
dt_model <- rpart(RiskLevel ~ Age + SystolicBP + DiastolicBP + BS + BodyTemp + HeartRate,
                  data = train_data, method = "class")

# Predict on test data
dt_predictions <- predict(dt_model, test_data, type = "class")

# Evaluate model performance (Confusion Matrix)
dt_conf_matrix <- confusionMatrix(dt_predictions, test_data$RiskLevel)
print(dt_conf_matrix)

# Visualize Decision Tree
library(rpart.plot)
rpart.plot(dt_model, extra = 104)








# Train Random Forest model
rf_model <- randomForest(RiskLevel ~ Age + SystolicBP + DiastolicBP + BS + BodyTemp + HeartRate,
                         data = train_data, ntree = 500)

# Predict on test data
rf_predictions <- predict(rf_model, test_data)

# Evaluate model performance (Confusion Matrix)
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$RiskLevel)
print(rf_conf_matrix)

# Feature importance from Random Forest
rf_importance <- importance(rf_model)
print(rf_importance)

# Plot the feature importance
library(ggplot2)
varImpPlot(rf_model)






# Load necessary library
library(caret)
library(ggplot2)

# Confusion Matrix for Decision Tree
dt_conf_matrix <- confusionMatrix(dt_predictions, test_data$RiskLevel)

# Convert the confusion matrix to a dataframe for ggplot
dt_cm_df <- as.data.frame(as.table(dt_conf_matrix))
names(dt_cm_df) <- c("Predicted", "Actual", "Frequency")

# Plot the Confusion Matrix for Decision Tree
ggplot(dt_cm_df, aes(x = Predicted, y = Actual, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), vjust = 1, color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Decision Tree", x = "Predicted", y = "Actual") +
  theme_minimal()




# Confusion Matrix for Random Forest
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$RiskLevel)

# Convert the confusion matrix to a data frame for ggplot
rf_cm_df <- as.data.frame(as.table(rf_conf_matrix))
names(rf_cm_df) <- c("Predicted", "Actual", "Frequency")

# Plot the Confusion Matrix for Random Forest
ggplot(rf_cm_df, aes(x = Predicted, y = Actual, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), vjust = 1, color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix for Random Forest", x = "Predicted", y = "Actual") +
  theme_minimal()
