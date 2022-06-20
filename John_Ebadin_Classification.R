# TASK 1: CLASSIFICATION
# Importing the dataset 'frs_housing_data'
# The data is imported with the correct data types using tidyverse as follows:

# DATA IMPORT AND PREP

library(tidyverse)
library(Boruta)
library(rsample)
library(party)
library(caret)
library(randomForest)

datatype <- c('iiiiiiiiiiiiiiiiiffffffffffffffiff')
frs_housing_data <- read_csv('D:/ASDM_Coursework/Datasets/frs_housing_data.csv', col_types = datatype)

frs_housing_data <- frs_housing_data %>% select(!c(PTENTYP2, SERNUM))

# PTENTYP2 which was the dependent variable from which RENTYP was created 
# and serial number (SERNUM) were dropped because they were irrelevant to the
# classification tasks.

colnames(frs_housing_data)
head(frs_housing_data)
dim(frs_housing_data)
glimpse(frs_housing_data)

# TASK 1A: CLASSIFICATION OF UK TENANTS ACCORDING TO TYPE OF RENTED ACCOMMODATION (RENTYP)

# Feature selection using 'Boruta'
feat_select_RENTYP <- Boruta(RENTYP~., data = frs_housing_data, doTrace = 2)
feat_select_RENTYP

# multiple household (MULTI) and household status (HHSTAT) were found to be tentative attributes
# but on application of the TentativeRoughFix algorithm they were found to also be important.
# Therefore all 31 variables were found to be important in predicting rent type (RENTYP) by Boruta.

feat_select_RENTYP_TRF <- TentativeRoughFix(feat_select_RENTYP)
feat_select_RENTYP_TRF
plot(feat_select_RENTYP_TRF, las= 1, cex=0.5)

formula_RENTYP <- getConfirmedFormula(feat_select_RENTYP_TRF)
formula_RENTYP
# Thus, all 33 variables were found to be important in the classification of RENTYP

# Stratified random sampling and splitting into training and test sets.
task1_RENTYP_split <- initial_split(frs_housing_data, prop = 0.8, strata = RENTYP, set.seed(222))
task1_RENTYP_split

task1_RENTYP_train <- training(task1_RENTYP_split)
task1_RENTYP_test <- testing(task1_RENTYP_split)

head(task1_RENTYP_train)
dim(task1_RENTYP_train)
head(task1_RENTYP_test)
dim(task1_RENTYP_test)
class(frs_housing_data$RENTYP)

# METHOD I: DECISION TREE

# Model training using train data (task1_RENTYP_train) and 'ctree' algorithm.
RENTYP_dt <- ctree(formula_RENTYP, data = task1_RENTYP_train, set.seed(333))
RENTYP_dt
plot(RENTYP_dt, type = 'simple')

# Predictions based on the test data (task1_RENTYP_test) using 'predict'.
RENTYP_dt_pred <- predict(RENTYP_dt, newdata = task1_RENTYP_test)
RENTYP_dt_pred

# Predicted vs actual class confusion matrix
RENTYP_dt_confumax <- confusionMatrix(RENTYP_dt_pred, task1_RENTYP_test$RENTYP)
RENTYP_dt_confumax

# METHOD 2: RANDOM FOREST

RENTYP_rf <- randomForest(formula_RENTYP, data = task1_RENTYP_train, set.seed(444))
RENTYP_rf

plot(RENTYP_rf)

hist(treesize(RENTYP_rf), 
     main = 'Distribution of Nodes in RENTYP Random Forest (RENTYP_rf)',
     xlab = 'Nodes', col = (c=13))

# Predictions based on the test data (task1_RENTYP_test) using 'predict'
RENTYP_rf_pred <- predict(RENTYP_rf, newdata = task1_RENTYP_test)
RENTYP_rf_pred

# Predicted vs actual confusion matrix
RENTYP_rf_confumax <- confusionMatrix(RENTYP_rf_pred, task1_RENTYP_test$RENTYP)
RENTYP_rf_confumax


varImpPlot(RENTYP_rf, n.var=10, main = 'Top 10 Predictor Variables for Rent Type "RENTYP"')

# TASK 1B: CLASSIFICATION OF UK TENANTS ACCORDING TO DURATION OF TENANCY (YEARBND)

# DATA PREP
colnames(frs_housing_data)
dim(frs_housing_data)

# Number of year lived in house (YEARNO) and year that occupancy began are dropped
# because they are the variables with which duration of tenancy (YEARBND) was created.
# The goal of this task is to be able to predict the how long a new tenant will 
# live in a property using other attributes of the tenant. Hence YEARNO and YEARWHC
# have got to be removed from the model.

frs_housing_data <- frs_housing_data %>% select(!c(YEARNO, YEARWHC))
dim(frs_housing_data)

# Feature selection using 'Boruta'
feat_select_YEARBND <- Boruta(YEARBND~., data = frs_housing_data, doTrace = 2)
feat_select_YEARBND

feat_select_YEARBND_TRF <- TentativeRoughFix(feat_select_YEARBND)
feat_select_YEARBND_TRF
# DISCHHC1, HHINV, MULTI, SHELTER were dropped as they were found to be unimportant by Boruta.

plot(feat_select_YEARBND_TRF, las= 2, cex=0.5)
formula_YEARBND <- getConfirmedFormula(feat_select_YEARBND)
formula_YEARBND

# Boruta confirmed 25 independent variables as important for predicting YEARBND.

# Stratified random sampling and splitting into training and test sets.
task1_YEARBND_split <- initial_split(frs_housing_data, prop = 0.8, strata = YEARBND, set.seed(555))
task1_YEARBND_split

task1_YEARBND_train <- training(task1_YEARBND_split)
task1_YEARBND_test <- testing(task1_YEARBND_split)

head(task1_YEARBND_train)
dim(task1_YEARBND_train)
head(task1_YEARBND_test)
dim(task1_YEARBND_test)
class(frs_housing_data$YEARBND)

# CORRECTION OF CLASS IMBALANCE USING 

table(task1_YEARBND_train$YEARBND)
task1_YEARBND_train<- downSample(task1_YEARBND_train[,-30], task1_YEARBND_train$YEARBND, yname = 'YEARBND')
table(task1_YEARBND_train$YEARBND)
colnames(task1_YEARBND_train)
# METHOD I: DECISION TREE

# Model training using train data (task1_YEARBND_train) and 'ctree' algorithm.
YEARBND_dt <- ctree(formula_YEARBND, data = task1_YEARBND_train, set.seed(6661))
YEARBND_dt
plot(YEARBND_dt, type = 'simple')

# Predictions based on the test data (task1_RENTYP_test) using 'predict'.
YEARBND_dt_pred <- predict(YEARBND_dt, newdata = task1_YEARBND_test)
YEARBND_dt_pred

# Predicted vs actual class confusion matrix
YEARBND_dt_confumax <- confusionMatrix(YEARBND_dt_pred, task1_YEARBND_test$YEARBND)
YEARBND_dt_confumax

# METHOD 2: RANDOM FOREST

YEARBND_rf <- randomForest(formula_YEARBND, data = task1_YEARBND_train, set.seed(777))
YEARBND_rf
plot(YEARBND_rf)

hist(treesize(YEARBND_rf), 
     main = 'Distribution of Nodes in RENTYP Random Forest (RENTYP_rf)',
     xlab = 'Nodes', col = (c=13))

# Predictions based on the test data (task1_RENTYP_test) using 'predict'
YEARBND_rf_pred <- predict(YEARBND_rf, newdata = task1_YEARBND_test)
YEARBND_rf_pred

# Predicted vs actual confusion matrix
YEARBND_rf_confumax <- confusionMatrix(YEARBND_rf_pred, task1_YEARBND_test$YEARBND)
YEARBND_rf_confumax

varImpPlot(YEARBND_rf, n.va =10, main = 'Top 10 Predictor Variables for Duration of Tenancy "YEARBND"')
