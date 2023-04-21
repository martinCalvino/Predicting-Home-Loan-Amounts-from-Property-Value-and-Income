# Predicting-Home-Loan-Amounts-from-Property-Value-and-Income
Linear Regression in R

# Script Name:      Linear_Regression_in_R
# Created On:       April_17_2023
# Author:           Martin_Calvino
# Author's web:     https://medium.com/@martincalvino
# Purpose:          Predict loan amounts from property value and income using linear regression
# Version:          V1_April17_2023

# OBJECTIVE
# Predict loan_amount associated to Bank of America home loan applications in New Jersey during 2021 from
# a linear regression model trained with data associated with Bank of America home loan applications in New Jersey during 2020

library(tidyverse)
library(patchwork)

# datasets were downloaded from:
# https://ffiec.cfpb.gov/data-browser/data/2020?category=states&items=NJ&leis=B4TYDEB6GKMZO031MB27

# dataset: home loans from Bank of America (boa) in New Jersey for 2020
boa20 <- read.csv(file.choose()) # 18,855 observations x 99 variables
# dataset: home loans from Bank of America (boa) in New Jersey for 2021
boa21 <- read.csv(file.choose()) # 17,316 observations x 99 variables

# description/documentation of variables can be found at:
# https://ffiec.cfpb.gov/documentation/2018/lar-data-fields/

# take accepted loans only (action_taken == 1)
boa20 <- filter(boa20, action_taken == 1) # 9,348 observations x 99 variables
boa21 <- filter(boa21, action_taken == 1) # 8,297 observations x 99 variables 

# select numeric columns
colnames(boa20)
View(boa20)
# numeric.boa.accepted.2020 as nuba20
nuba20 <- select(boa20, loan_amount, income, property_value, loan_to_value_ratio, interest_rate)
# numeric.boa.accepted.2021 as nuba21
nuba21 <- select(boa21, loan_amount, income, property_value, loan_to_value_ratio, interest_rate)

# identify and remove NAs
sum(is.na(nuba20)) # 29 missing values from boa20
nuba20 <- na.omit(nuba20)
sum(is.na(nuba21)) #  16 missing values from boa2021
nuba21 <- na.omit(nuba21)

# multiply income*1000
nuba20$income <- nuba20$income*1000
nuba21$income <- nuba21$income*1000

# let's explore some statistics for our variables
summary(nuba20)
summary(nuba21)

# select observations displaying income values bigger than 0
nuba20 <- filter(nuba20, income > 0)
nuba21 <- filter(nuba21, income > 0)

################################################################################
# remove outliers one variable at a time for nuba20
# loan amount
outliers.la.nuba20 <- boxplot(nuba20[, 1])$out
nuba20 <- nuba20[-which(nuba20[, 1] %in% outliers.la.nuba20), ] 
boxplot(nuba20[, c(1:3)])$out
# income
outliers.inc.nuba20 <- boxplot(nuba20[, 2])$out
nuba20 <- nuba20[-which(nuba20[, 2] %in% outliers.inc.nuba20), ]
boxplot(nuba20[, c(1:3)])$out
# property value
outliers.pv.nuba20 <- boxplot(nuba20[, 3])$out
nuba20 <- nuba20[-which(nuba20[, 3] %in% outliers.pv.nuba20), ]
boxplot(nuba20[, c(1:3)])$out
# loan to value ratio
outliers.ltvr.nuba20 <- boxplot(nuba20[, 4])$out
nuba20 <- nuba20[-which(nuba20[, 4] %in% outliers.ltvr.nuba20), ]
boxplot(nuba20[, 4])$out
# interest rate
outliers.ir.nuba20 <- boxplot(nuba20[, 5])$out
nuba20 <- nuba20[-which(nuba20[, 5] %in% outliers.ir.nuba20), ]
boxplot(nuba20[, 5])$out


# let's only consider values from 0 to 100 for loan_to_value_ratio
nuba20 <- filter(nuba20, loan_to_value_ratio <= 100)

################################################################################
# remove outliers one variable at a time for nuba21
# loan amount
outliers.la.nuba21 <- boxplot(nuba21[, 1])$out
nuba21 <- nuba21[-which(nuba21[, 1] %in% outliers.la.nuba21), ] 
boxplot(nuba21[, c(1:3)])$out
# income
outliers.inc.nuba21 <- boxplot(nuba21[, 2])$out
nuba21 <- nuba21[-which(nuba21[, 2] %in% outliers.inc.nuba21), ]
boxplot(nuba21[, c(1:3)])$out
# property value
outliers.pv.nuba21 <- boxplot(nuba21[, 3])$out
nuba21 <- nuba21[-which(nuba21[, 3] %in% outliers.pv.nuba21), ]
boxplot(nuba21[, c(1:3)])$out
# loan to value ratio
outliers.ltvr.nuba21 <- boxplot(nuba21[, 4])$out
nuba21 <- nuba21[-which(nuba21[, 4] %in% outliers.ltvr.nuba21), ]
boxplot(nuba21[, 4])$out
# interest rate
outliers.ir.nuba21 <- boxplot(nuba21[, 5])$out
nuba21 <- nuba21[-which(nuba21[, 5] %in% outliers.ir.nuba21), ]
boxplot(nuba21[, 5])$out


# let's only consider values from 0 to 100 for loan_to_value_ratio
nuba21 <- filter(nuba21, loan_to_value_ratio <= 100)

################################################################################

# let's explore again the summary statistics for our variables after outliers were removed
summary(nuba20)
summary(nuba21)

# let's filter nuba21 to have same ranges as nuba20 for loan_amount, property_value, and income
# as to potentially improve model performance later on (model can predict based on what it learned)
nuba21 <- filter(nuba21, loan_amount >= 25000 & loan_amount <=955000)
nuba21 <- filter(nuba21, income >= 13000 & income <= 451000)
nuba21 <- filter(nuba21, property_value >= 45000 & property_value <= 1205000)

summary(nuba20)
summary(nuba21)

# let's check the correlation among variables
cor(nuba20)
cor(nuba21)

# use the gather() function on nuba20 to plot each variable against loan_amount to get an idea of the relationship in the data
# all.variables.against.loan.amount as avala20
avala20 <- gather(nuba20, key = "Variable", value = "Value", -loan_amount)
View(avala20)

avala21 <- gather(nuba21, key = "Variable", value = "Value", -loan_amount)
View(avala21)

# plot predictor variables against the outcome variable (loan_amount)
# it seems income and property_value have a linear relationship with loan_amount

# avala20
ggplot(data20 = avala, mapping = aes(x = Value, y = loan_amount)) +
  facet_wrap(~ Variable, scale = "free_x") +
  geom_point(alpha = 0.15) +
  geom_smooth() +
  geom_smooth(method = "lm", col = "red") +
  theme_bw() +
  ggtitle("Relationship among variables to loan_amount", "6583 accepted home loan applications: Bank of America - New Jersey - 2020")

# avala21
ggplot(data = avala21, mapping = aes(x = Value, y = loan_amount)) +
  facet_wrap(~ Variable, scale = "free_x") +
  geom_point(alpha = 0.15) +
  geom_smooth() +
  geom_smooth(method = "lm", col = "red") +
  theme_bw() +
  ggtitle("Relationship among variables to loan_amount", "6617 accepted home loan applications: Bank of America - New Jersey - 2021")


# SIMPLE LINEAR REGRESSION >> lm(response_variable ~ predictor_variable, data = dataFrame)
# response/outcome variable: loan_amount
# predictor/explanatory variable: property_value

la.pv.fit <- lm(loan_amount ~ property_value, data = nuba20)
summary(la.pv.fit) # adjusted R-squared: 0.53
# the prediction equation is loan_amount = 17600 + 0,5609 * property_value

# plot residuals model 1
plot.residuals.m1 <- ggplot(data = la.pv.fit) +
  geom_histogram(mapping = aes(x = la.pv.fit$residuals)) +
  ggtitle("model 1")


# let's predict loan_amounts from 2021
predicted_loan.model.1 <- predict(la.pv.fit, data.frame(property_value = nuba21$property_value))
is.vector(predicted_loan.model.1)
predicted_loan.model.1[1:10]

# let's add predicted loan_amounts as a new column on nuba21 dataframe
nuba21$pred.la.m1 <- predicted_loan.model.1
View(nuba21)
# place the newly created column (named pre.la.m1) next to loan_amount column
nuba21 <- select(nuba21, loan_amount, pred.la.m1, everything())
View(nuba21)

################################################################################

# MULTIPLE LINEAR REGRESSION (additive) >>> lm(response_variable ~ predictor_variable1 + predictor_variable2, data = dataFrame)
# add 'income' as a second predictor variable and see if linear model performs better
la.pv.plus.inc.fit <- lm(loan_amount ~ property_value + income, data = nuba20)
summary(la.pv.plus.inc.fit) # adjusted R-squared 0.54

# predict a single loan_amount from property_value and income
predict(la.pv.plus.inc.fit, data.frame(property_value = 369000, income = 80000))

# plot residuals model 2
plot.residuals.m2 <- ggplot(data = la.pv.plus.inc.fit) +
  geom_histogram(mapping = aes(x = la.pv.plus.inc.fit$residuals)) +
  ggtitle("model 2")

# let's make predictions on loan amounts for 2021
predicted_loan.model.2 <- predict(la.pv.plus.inc.fit, data.frame(property_value = nuba21$property_value, income = nuba21$income))
is.vector(predicted_loan.model.2)
predicted_loan.model.2[1:10]

# let's add a second new column with predicted loan amounts from model2 to nuba21
nuba21$pred.la.m2 <- predicted_loan.model.2
nuba21 <- select(nuba21, loan_amount, pred.la.m1, pred.la.m2, everything())
View(nuba21)

# MULTIPLE LINEAR REGRESSION (with interaction among predictor variables)
# lm(response_variable ~ predictor_variable1 + predictor_variable2 + predictor_variable1:predictor_variable2, data = dataFrame)
interaction.pv.inc.fit <- lm(loan_amount ~ property_value + income + property_value:income, data = nuba20)
summary(interaction.pv.inc.fit) # adjusted R-squared is 0.55

# predict a single loan_amount from property_value and income
predict(interaction.pv.inc.fit, data.frame(property_value = 369000, income = 80000))

# let's predict loan_amounts in 2021 with model3
predicted_loan.model.3 <- predict(interaction.pv.inc.fit, data.frame(property_value = nuba21$property_value, income = nuba21$income))

# add a 3rd new column with predicted loan_amounts from model3 to nuba21
nuba21$pred.la.m3 <- predicted_loan.model.3
nuba21 <- select(nuba21, loan_amount, pred.la.m1, pred.la.m2, pred.la.m3, everything())


# plot residuals model 3
plot.residuals.m3 <- ggplot(data = interaction.pv.inc.fit) +
  geom_histogram(mapping = aes(x = interaction.pv.inc.fit$residuals)) +
  ggtitle("model 3")


#####################################################################################################
# EVALUATE MODELS

# plot histograms with residuals from the 3 models
plot.residuals.m1 + plot.residuals.m2 + plot.residuals.m3

# confidence interval for model 3
confint(interaction.pv.inc.fit)

# assess regression diagnostics
par(mfrow = c(2, 2))
plot(interaction.pv.inc.fit)
par(mfrow = c(1, 1))


# inspect nuba21 with columns containing predicted values for loan amounts
View(nuba21)

# let's plot real loan_amounts from home loan applications in 2021 versus loan_amount predictions from the 3 models we built
plot1 <- ggplot(data = nuba21) +
  geom_point(mapping = aes(x = loan_amount, y = pred.la.m1), alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(breaks = seq(0, 1000000, 100000)) +
  ggtitle("Loan amounts \npredicted by model 1")


plot2 <- ggplot(data = nuba21) +
  geom_point(mapping = aes(x = loan_amount, y = pred.la.m2), alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(breaks = seq(0, 1000000, 100000)) +
  ggtitle("Loan amounts \npredicted by model 2")

plot3 <- ggplot(data = nuba21) +
  geom_point(mapping = aes(x = loan_amount, y = pred.la.m3), alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(breaks = seq(0, 1000000, 100000)) +
  ggtitle("Loan amounts \npredicted by model 3")

# visualize the 3 plots together for easy comparison
plot1 + plot2 + plot3
