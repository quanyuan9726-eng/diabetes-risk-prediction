library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaps)
library(MASS)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
# Loading dataset from NHANES
A1C <- read_xpt("C:/Users/79146/Downloads/P_GHB.XPT")
Demo <- read_xpt("C:/Users/79146/Downloads/P_DEMO.XPT")
chol <- read_xpt("C:/Users/79146/Downloads/P_TCHOL.XPT")
BP<- read_xpt("C:/Users/79146/Downloads/P_BPQ.XPT")
Alcohol <- read_xpt("C:/Users/79146/Downloads/P_ALQ.XPT")
Sleep <- read_xpt("C:/Users/79146/Downloads/P_SLQ.XPT")
PA <- read_xpt("C:/Users/79146/Downloads/P_PAQ.XPT")
Smoking <- read_xpt("C:/Users/79146/Downloads/P_SMQ.XPT")
Insurance <- read_xpt("C:/Users/79146/Downloads/P_HIQ.XPT")
BMI <- read_xpt("C:/Users/79146/Downloads/P_BMx.XPT")

# Extract variables from each dataset
Demo <- Demo %>% dplyr::select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, DMDMARTZ)
# Gender, age, race/ethnicity, education, marital status
chol <- chol %>% dplyr::select(SEQN, LBXTC) # Cholesterol
BP <- BP %>% dplyr::select(SEQN, BPQ020) # Ever told you had high blood pressure
Alcohol <- Alcohol %>% dplyr::select(SEQN, ALQ111)# Ever had a drink of any kind of alcohol
Sleep <- Sleep %>% dplyr::select(SEQN, SLD012) # Sleep hours - weekdays or workdays
PA <- PA %>% dplyr::select(SEQN, PAQ665)# Moderate recreational activities
Smoking <- Smoking %>% dplyr::select(SEQN, SMQ040) # Do you now smoke cigarettes
Insurance <- Insurance %>% dplyr::select(SEQN, HIQ011)# Covered by health insurance
BMI <- BMI %>% dplyr::select(SEQN, BMXBMI) # BMI


# Delete participant who answered "Refused", "Don't know" and missing values in 
# all variables
A1C[A1C == "." ]<-NA
A1C <- A1C %>% filter(if_any(-SEQN, ~ !is.na(.)))
Demo[Demo == "." | Demo$DMDEDUC2 == "7" | Demo == "9" | Demo == "77" | Demo == "99"]<-NA
Demo <- Demo %>% filter(!is.na(DMDEDUC2) & !is.na(DMDMARTZ))
chol[chol == "."] <- NA
chol <- chol %>% filter(if_any(-SEQN, ~ !is.na(.)))
BP[BP == 9] <- NA
BP <- BP %>% filter(if_any(-SEQN, ~ !is.na(.)))
Alcohol[Alcohol == "."] <- NA
Alcohol <- Alcohol %>% filter(if_any(-SEQN, ~ !is.na(.)))
Sleep[Sleep == "."] <- NA
Sleep <- Sleep %>% filter(if_any(-SEQN, ~ !is.na(.)))
PA[PA == 9] <- NA
PA <- PA %>% filter(if_any(-SEQN, ~ !is.na(.)))
Smoking[Smoking == "."] <- NA
Smoking <- Smoking %>% filter(if_any(-SEQN, ~ !is.na(.)))
Insurance[Insurance == 7 | Insurance == 9] <- NA
Insurance <- Insurance %>% filter(if_any(-SEQN, ~ !is.na(.)))
BMI[BMI == "."] <- NA
BMI <- BMI %>% filter(if_any(-SEQN, ~ !is.na(.)))

# Merging these dataset into one 
dt <- A1C %>% 
  left_join(Demo, by = "SEQN") %>%
  left_join(chol, by = "SEQN") %>%
  left_join(BP, by = "SEQN") %>%
  left_join(Alcohol, by = "SEQN") %>%
  left_join(Sleep, by = "SEQN") %>%
  left_join(PA, by = "SEQN") %>%
  left_join(Smoking, by = "SEQN") %>%
  left_join(Insurance, by = "SEQN") %>%
  left_join(BMI, by = "SEQN")

sum(!complete.cases(dt))

dt <- dt %>% drop_na()
dt <- dt %>% dplyr:: select(-SEQN)
# rename the variable
dt <- dt %>%
  rename(a1c = LBXGH, Gender = RIAGENDR, age = RIDAGEYR, race = RIDRETH3, cholesterol = LBXTC, 
         blood_pres = BPQ020, alcohol = ALQ111, sleep = SLD012, phy_act = PAQ665,
         smoke = SMQ040, health_ins = HIQ011, bmi = BMXBMI, marital = DMDMARTZ,
         education = DMDEDUC2)
# Combine smoke into binary variable (yes and no)
dt$smoke <- ifelse(dt$smoke %in% c(1, 2), 1, 
                   ifelse(dt$smoke == 3, 2, dt$smoke))
dt$race <- ifelse(dt$race %in% c(1, 2), 1, dt$race)
dt$education <- ifelse(dt$education %in% c(1, 2), 1, dt$education)

# General statistics with plots
# A1C level
ggplot(dt, aes(x = a1c)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of a1c Levels", x = "a1c Level", y = "Count")
# Gender, 1 - men, 2 - women
ggplot(dt, aes(x = a1c)) + 
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of a1c Levels by Gender", x = "a1c Level", y = "Count") +
  facet_wrap(~ Gender)
# Race/ethnicity
# 1 -	Mexican American	
# 2	- Other Hispanic
# 3	- Non-Hispanic White
# 4	- Non-Hispanic Black	
# 6	- Non-Hispanic Asian	
# 7	- Other Race - Including Multi-Racial
dt$race <- as.factor(dt$race)
ggplot(dt, aes(x = race, fill = race)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Count of A1C Levels by Race", x = "Race", y = "Count") +
  scale_fill_manual(values = c("1" = "red", "2" = "green", "3" = "orange", "4" = "yellow",
                               "6" = "blue", "7" = "purple")) 

# Exclude the A1C level between 5.7 and 6.5(prediabetes)
dt_new <- dt %>%
  filter(!(a1c >= 5.7 & a1c < 6.5))
# Convert A1C into binary variable
dt_new <- dt_new %>%
  mutate(diabetes = case_when(
    a1c >= 6.5  ~ "1", # diabetes
    a1c < 5.7  ~ "0" # Normal
  ))

dt_new <- dt_new[, !names(dt_new) %in% "a1c"]
dt_new$diabetes <- as.factor(dt_new$diabetes)

# Univariate analysis
summary(dt$a1c)
sd(dt$a1c)
summary(dt_new)
summary(dt_new$age)
sd(dt_new$age)
sum(dt_new$Gender == 1)
sum(dt_new$Gender == 2)
sum(dt_new$race == 1)
sum(dt_new$race == 3)
sum(dt_new$race == 4)
sum(dt_new$race == 6)
sum(dt_new$race == 7)
sum(dt_new$education == 1) # Less than 11th grade (Includes 12th grade with no diploma)
sum(dt_new$education == 3) # High school graduate/GED or equivalent
sum(dt_new$education == 4) # Some college or AA degree
sum(dt_new$education == 5) # College graduate or above
sum(dt_new$marital == 1) # Married/Living with Partner
sum(dt_new$marital == 2) # Widowed/Divorced/Separated
sum(dt_new$marital == 3) # Never married
sum(dt_new$health_ins == 1)
sum(dt_new$health_ins == 2)
sum(dt_new$cholesterol >= 200)
sum(dt_new$cholesterol < 200)
sum(dt_new$blood_pres == 1)
sum(dt_new$blood_pres == 2)
sum(dt_new$alcohol == 1)
sum(dt_new$alcohol == 2)
summary(dt_new$sleep)
sd(dt_new$sleep)
sum(dt_new$phy_act == 1)
sum(dt_new$phy_act == 2)
sum(dt_new$smoke == 1)
sum(dt_new$smoke == 2)
summary(dt_new$bmi)
sd(dt_new$bmi)
sum(dt_new$diabetes == 1)
sum(dt_new$diabetes == 0)


predictors <- setdiff(names(dt_new), "diabetes")
results <- data.frame(
  Predictor = character(),  # Variable name
  P_value = numeric(),      # P-value from the test
  Test = character(),       # Type of test used
  stringsAsFactors = FALSE
)

factor_vars <- c("Gender", "race", "education", "marital", "blood_pres", "alcohol",
                "phy_act", "smoke", "health_ins")
dt_new[factor_vars] <- lapply(dt_new[factor_vars], factor)

for (var in predictors) {
  # Check if the variable is numeric
  if (is.numeric(dt_new[[var]])) {
    # Use t-test for continuous/numeric variables
    test <- t.test(dt_new[[var]] ~ dt_new$diabetes)
    p <- test$p.value
    test_type <- "t-test"
    } else if (is.factor(dt_new[[var]]) || is.character(dt_new[[var]])) {
    # Use chi-squared test for categorical variables
    tbl <- table(dt_new[[var]], dt_new$diabetes)
    test <- chisq.test(tbl)
    p <- test$p.value
    test_type <- "chi-squared"
    
  } else {
    # Skip variables that are not numeric or categorical
    next
  }
  results <- rbind(
    results,
    data.frame(Predictor = var, P_value = p, Test = test_type)
  )
}

results[order(results$P_value), ]

diabetes_yes <- subset(dt_new, diabetes == 1)
diabetes_no <- subset(dt_new, diabetes == 0)

# Univariate analysis continued
summary(diabetes_yes$age)
sd(diabetes_yes$age)
sum(diabetes_yes$Gender == 1)
sum(diabetes_yes$Gender == 2)
sum(diabetes_yes$race == 1)
sum(diabetes_yes$race == 3)
sum(diabetes_yes$race == 4)
sum(diabetes_yes$race == 6)
sum(diabetes_yes$race == 7)
sum(diabetes_yes$education == 1) # Less than 11th grade (Includes 12th grade with no diploma)
sum(diabetes_yes$education == 3) # High school graduate/GED or equivalent
sum(diabetes_yes$education == 4) # Some college or AA degree
sum(diabetes_yes$education == 5) # College graduate or above
sum(diabetes_yes$marital == 1) # Married/Living with Partner
sum(diabetes_yes$marital == 2) # Widowed/Divorced/Separated
sum(diabetes_yes$marital == 3) # Never married
sum(diabetes_yes$health_ins == 1)
sum(diabetes_yes$health_ins == 2)
sum(diabetes_yes$cholesterol >= 200)
sum(diabetes_yes$cholesterol < 200)
sum(diabetes_yes$blood_pres == 1)
sum(diabetes_yes$blood_pres == 2)
sum(diabetes_yes$alcohol == 1)
sum(diabetes_yes$alcohol == 2)
summary(diabetes_yes$sleep)
sd(diabetes_yes$sleep)
sum(diabetes_yes$phy_act == 1)
sum(diabetes_yes$phy_act == 2)
sum(diabetes_yes$smoke == 1)
sum(diabetes_yes$smoke == 2)
summary(diabetes_yes$bmi)
sd(diabetes_yes$bmi)

# Univariate analysis continued
summary(diabetes_no$age)
sd(diabetes_no$age)
sum(diabetes_no$Gender == 1)
sum(diabetes_no$Gender == 2)
sum(diabetes_no$race == 1)
sum(diabetes_no$race == 3)
sum(diabetes_no$race == 4)
sum(diabetes_no$race == 6)
sum(diabetes_no$race == 7)
sum(diabetes_no$education == 1) # Less than 11th grade (Includes 12th grade with no diploma)
sum(diabetes_no$education == 3) # High school graduate/GED or equivalent
sum(diabetes_no$education == 4) # Some college or AA degree
sum(diabetes_no$education == 5) # College graduate or above
sum(diabetes_no$marital == 1) # Married/Living with Partner
sum(diabetes_no$marital == 2) # Widowed/Divorced/Separated
sum(diabetes_no$marital == 3) # Never married
sum(diabetes_no$health_ins == 1)
sum(diabetes_no$health_ins == 2)
sum(diabetes_no$cholesterol >= 200)
sum(diabetes_no$cholesterol < 200)
sum(diabetes_no$blood_pres == 1)
sum(diabetes_no$blood_pres == 2)
sum(diabetes_no$alcohol == 1)
sum(diabetes_no$alcohol == 2)
summary(diabetes_no$sleep)
sd(diabetes_no$sleep)
sum(diabetes_no$phy_act == 1)
sum(diabetes_no$phy_act == 2)
sum(diabetes_no$smoke == 1)
sum(diabetes_no$smoke == 2)
summary(diabetes_no$bmi)
sd(diabetes_no$bmi)

# Bivariate analysis(Binary outcome)
# Random Forest
rf_model <- randomForest(diabetes ~ ., data = dt_new, importance = TRUE, ntree = 500)
importance(rf_model)
varImpPlot(rf_model)

# Decision trees
reg_trees <- rpart(diabetes ~ ., data = dt_new, method = "class")
rpart.plot(reg_trees, type = 2, extra = 101, tweak = 1.2)
reg_trees$variable.importance

# Gradient Boosting
dt_new$diabetes <- as.numeric(as.character(dt_new$diabetes))
boost_model <- gbm(diabetes ~ ., data = dt_new, distribution = "bernoulli",
                   n.trees = 5000, interaction.depth = 3, shrinkage = 0.01)
summary(boost_model)

# Logistic regression
logit_model <- glm(diabetes ~ ., data = dt_new, family = binomial)
summary(logit_model)

# Multivariate analysis
# Split data into 7:3 training data and test data
set.seed(123) 
acc_results <- c()
for (i in 1:80) {
  train_indices <- sample(1:nrow(dt_new), size = 0.7 * nrow(dt_new))
  train_data <- dt_new[train_indices, ]
  test_data <- dt_new[-train_indices, ]
}
dt_new$diabetes <- as.factor(dt_new$diabetes)
train_data$diabetes <- as.factor(train_data$diabetes)
test_data$diabetes <- as.factor(test_data$diabetes)

# define 5-fold CV
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)

# Random forest
rf <- train(diabetes ~ age + bmi + cholesterol + race + blood_pres + sleep + 
         education + marital, data = train_data, method = "rf", trControl = train_control)
predictions1 <- predict(rf, test_data)
accurancy1 <- mean(predictions1 == test_data$diabetes)
acc_results1 <- c(acc_results, accurancy1)
mean(acc_results1)

# Decision trees
dt_model <- train(diabetes ~ age + bmi + cholesterol + race + blood_pres + sleep + 
        education + marital, data = train_data, method = "rpart", trControl = train_control)
predictions2 <- predict(dt_model, test_data)
accurancy2 <- mean(predictions2 == test_data$diabetes)
acc_results2 <- c(acc_results, accurancy2)
mean(acc_results2)

# K Nearest Neighbors
knn_model <- train(diabetes ~ age + bmi + cholesterol + race + blood_pres + sleep + 
                     education + marital, 
                   data = train_data, 
                   method = "knn", 
                   preProcess = c("center", "scale"), 
                   trControl = train_control,
                   tuneLength = 10)
predictions3 <- predict(knn_model, test_data)
accurancy3 <- mean(predictions3 == test_data$diabetes)
acc_results3 <- c(acc_results, accurancy3)
mean(acc_results3)

# Logistic Regression
logit_model <- train(diabetes ~ age + bmi + cholesterol + race + blood_pres + sleep + 
                       education + marital, 
                     data = train_data, 
                     method = "glm", 
                     trControl = train_control)
predictions4 <- predict(logit_model, test_data)
accurancy4 <- mean(predictions4 == test_data$diabetes)
acc_results4 <- c(acc_results, accurancy4)
mean(acc_results4)

# Gradient Boosting
set.seed(123)
GBM_model <- train(diabetes ~ age + bmi + cholesterol + race + blood_pres + sleep + 
                     education + marital, data = train_data, 
                   method = "gbm", trControl = train_control, verbose = FALSE)
predictions5 <- predict(GBM_model, test_data)
accurancy5 <- mean(predictions5 == test_data$diabetes)
acc_results5 <- c(acc_results, accurancy5)
mean(acc_results5)

# Compute the AUC value
# Extract Probabilities with positive class
rf_probs <- predict(rf, test_data, type = "prob")[, 2]
dt_probs <- predict(dt_model, test_data, type = "prob")[, 2]
GBM_probs <- predict(GBM_model, test_data, type = "prob")[, 2]
knn_probs <- predict(knn_model, test_data, type = "prob")[, 2]
logit_probs <- predict(logit_model, test_data, type = "prob")[, 2]

# Compute ROC curve
rf_roc <- roc(test_data$diabetes, rf_probs)
dt_roc <- roc(test_data$diabetes, dt_probs)
GBM_roc <- roc(test_data$diabetes, GBM_probs)
knn_roc <- roc(test_data$diabetes, knn_probs)
logit_roc <- roc(test_data$diabetes, logit_probs)

# Plot ROC curves
plot(rf_roc, col = "red", main = "ROC Curves for ML Models")
plot(dt_roc, col = "blue", add = TRUE)
plot(GBM_roc, col = "green", add = TRUE)
plot(knn_roc, col = "purple", add = TRUE)
plot(logit_roc, col = "orange", add = TRUE)

# Adding AUC values
legend("bottomright", 
       legend = c(paste("RF AUC =", round(auc(rf_roc), 3)),
                  paste("DT AUC =", round(auc(dt_roc), 3)),
                  paste("GBM AUC =", round(auc(GBM_roc), 3)),
                  paste("KNN AUC =", round(auc(knn_roc), 3)),
                  paste("Logit AUC =", round(auc(logit_roc), 3))),
       col = c("red", "blue", "green", "purple", "orange"),
       lwd = 2)

