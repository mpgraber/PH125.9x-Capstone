if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

if(!require(ggpubr)) {
  install.packages("ggpubr")
  library(ggpubr)
}

if(!require(kernlab)) {
  install.packages("kernlab")
  library(kernlab)
}

if(!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(caret)) {
  install.packages("caret")
  library(caret)
}

# Introduction

# As part of the Data Science: Capstone course in the Data Science Professional
# Certificate EdX program, students are encouraged to find a data set of their 
# own or from an online source to build a machine learning model.  The intent 
# of the project is to demonstrate the ability to analyze a data set as well as
# clearly communicate the process and insights gained from the analysis.

# This project intends to develop a model to predict the compressive strength 
# of a given concrete mixture after a certain number of days.  At first, I 
# considered this dataset mundane and flat.  Upon reflection, given how useful 
# concrete is in both developed and developing nations, the ability to use 
# local materials and know that the concrete will be strong seems to add value 
# to the data and the utility of a potential model.

# Objectives & Approach

# The objective of this analysis is to define a reusable model to predict the 
# compressive strength of a concrete mixture comprised of up to 7 ingredients 
# and allowed to cure for a certain number of days before strength measurement.  
# I will use several approaches to identify models and determine which model 
# has the highest accuracy for predicting results.  

# The data set has 9 variables, 8 dependent and one independent.  All variables
# are numeric.  For purposes of brevity, I will establish shortened attribute
# names for the original variable names

Concrete_Data <- read_excel("Concrete_Data.xls")

# Clean up attribute names to improve readability
new_names <- c("Cement", "Slag", "Ash", "Water", "Plasticizer", "Coarse_Agg", 
               "Fine_Agg", "Days", "Strength")
colnames(Concrete_Data) <- new_names

Concrete_Data %>% filter(Cement == 500)

summary(Concrete_Data)

# Check to see if Strength is normally distributed
ggplot(data = Concrete_Data, aes(x=Strength)) + geom_histogram(bins = 30)

# compare distribution of age vs. strength
# the data seems pre-stratified by virtue of the ages at which the strength is
# measured:
Concrete_Data %>% summarize(r = cor(Days, Strength)) %>% pull(r)
ggplot(data = Concrete_Data, aes(Days, Strength)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# Based on the graph above, we can see in general that the longer a given 
# concrete mixture is permitted to age, the stronger it gets.

# Beyond curing time, other aspects of the concrete mixture affect the 
# compressive strength. A multiple linear regression model might provide a good
# place to start to build a predictive solution:

linear_model <- train(Strength ~ ., data = Concrete_Data, method = "lm")
summary(linear_model)

# The results of this first linear model show most coefficients have a low 
# P-value, with two attributes on the threshold (Coarse_Agg and Fine_Agg are 
# both approximately 0.06), and a relatively high P-value for the Y-intercept 
# (0.38).  Overall, this model only explains about 61% of the variability of 
# the dependent value, compressive Strength.  This linear regression model 
# results in a predictive formula of:

# Strength = 0.12 * Cement + 0.104 * Slag + 0.088 * Ash - 0.15 * Water + 
#            0.29 * Plasticizer + 0.018 * Coarse_Agg + 0.02 * Fine_Agg + 
#            0.114 * Days - 23.163

# I think we can do better by looking further at the attributes, their 
# correlation to the strength of the concrete, and their interaction related to
# the actual chemical reaction that occurs during the curing phase.

# CORRELATION COEFFICIENTS
# Interaction considerations

# Since some of the mixtures include certain components and not others, we have
# to consider whether or not the components are part of the chemical reaction 
# (hydration), part of the final structural (compressive) strength, or 
# neither.  According to industry publications, cement, ash, and slag are 
# considered "cementitious" components and contribute to the hydration reaction
# of the curing concrete.  Plasticizer, likewise, allows the production of 
# concrete with less water, and slow the curing of concrete (as would adding 
# more water and prolonging the drying/curing process.)  Other ingredients, 
# such as coarse and fine aggregate, are necessary for the ultimate strength of
# the cured mixture, but do not play a part in the chemistry.

# As part of this data set review and model, we have to consider whether to 
# include in the correlation analysis the zero values for cementitious 
# components:  If they are not present, they cannot influence the chemical 
# reaction.  In one sense, we are treating these attributes as both logistic 
# and continuous.  For the high level perspective of determine a correlation 
# coeffecient between these attributes and the Strength attribute, I have 
# filtered out the zero values.

# As an example, we can see that with or without the Ash=0 values, the 
# correlation of Ash content to Strength is negative, but more pronounced if 
# the zero values are excluded.  This indicates that stronger concrete uses 
# less Ash, and the strongest (regardless of curing time) uses none.

# Correlation Coefficient of Ash:Strength with all values
Concrete_Data %>% 
  summarize(r = cor(Ash, Strength)) %>% 
  pull(r)

# Correlation Coefficient of Ash:Strength, ignoring mixtures with zero Ash
Concrete_Data %>% 
  filter(Ash > 0) %>% 
  summarize(r = cor(Ash, Strength)) %>% 
  pull(r)

# For visual review and for the purposes of considering variable interactions,
# I will exclude the zero values.

colorGray = "#666666"

Concrete_Data %>% 
  summarize(r = cor(Cement, Strength)) %>% 
  pull(r)
Concrete_Data %>% 
  filter(Slag > 0) %>% 
  summarize(r = cor(Slag, Strength)) %>% 
  pull(r)
Concrete_Data %>% 
  filter(Plasticizer > 0) %>% 
  summarize(r = cor(Plasticizer, Strength)) %>% 
  pull(r)
Concrete_Data %>% 
  summarize(r = cor(Water, Strength)) %>% 
  pull(r)
Concrete_Data %>% 
  summarize(r = cor(Days, Strength)) %>% 
  pull(r)
Concrete_Data %>% 
  summarize(r = cor(Coarse_Agg, Strength)) %>% 
  pull(r)
Concrete_Data %>% 
  summarize(r = cor(Fine_Agg, Strength)) %>% 
  pull(r)

gpCorCement <- ggplot(data = Concrete_Data, 
                      aes(Cement, Strength)) + 
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorAsh <- ggplot(data = (Concrete_Data %>% filter(Ash > 0)), 
                   aes(Ash, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorSlag <- ggplot(data = (Concrete_Data %>% filter(Slag > 0)), 
                    aes(Slag, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorPlasticizer <- ggplot(data = (Concrete_Data %>% filter(Plasticizer > 0)), 
                           aes(Plasticizer, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorWater <- ggplot(data = Concrete_Data, 
                     aes(Water, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorDays <- ggplot(data = Concrete_Data, 
                    aes(Days, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorCoarse <- ggplot(data = Concrete_Data, 
                      aes(Coarse_Agg, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

gpCorFine <- ggplot(data = Concrete_Data, 
                    aes(Fine_Agg, Strength)) +
  geom_point(colour = colorGray) + 
  geom_smooth(method = 'lm')

# Draw some plots for discussion
ggarrange(gpCorCement, gpCorWater, gpCorDays, gpCorPlasticizer,
          labels = c("Cement", "Water", "Days", "Plasticizer"), 
          ncol = 2, nrow = 2)

ggarrange(gpCorAsh, gpCorSlag, gpCorCoarse, gpCorFine,
          labels = c("Ash", "Slag", "Coarse", "Fine"),
          ncol = 2, nrow = 2)

# As the plots above show, the solids that eventually comprise the concrete
# after curing all have similar slopes and likely have no interaction among
# them.  However, there are other interactions to consider: The correlation 
# coefficient for water is negative, while those for days of curing and 
# plasticizer are positive.  There may be interactions worth evaluting.
# Likewise, the coefficient for cement is positive, while those for all other
# solids are negative. We will explore these interactions as ratios in 
# further regression models.

# Water / Days

# One interaction for consideration - based on both intuition and on the 
# respective coefficients of correlation is that of Water and Days.  That is, 
# does the interaction of these two attributes taken together improve the 
# model as compared to the two attributes considered separately?

Conc_Interact2 <- Concrete_Data %>% 
  mutate(WaterDays = Water/Days)
linear_model2 <- train(Strength ~ Cement + Slag + Ash + Plasticizer + Coarse_Agg + Fine_Agg + 
                         WaterDays, data = Conc_Interact2, method = "lm")
summary(linear_model2)

# In this updated model, we can see that the Standard Error for the Intercept
# and the overall Residual Standard Error for the model have both improved.  
# Also, the multiple R-squared value for the model as a whole now demonstrates 
# that this model explains about 73% of the variation in the dependent variable
# - an improvement over the original.

# Cement + Ash

# Another consideration might be the combination of Portland Cement and Fly 
# Ash.  Both contribute to the chemical hardening of concrete during the curing
# process.  We can adjust the model to combine them into one attribute; we can
# add an interaction to the model:

Conc_Interact3 <- Conc_Interact2 %>% mutate(Silicates = Cement+Ash)
linear_model3 <- train(Strength ~ Silicates + Slag + Plasticizer + Coarse_Agg +
                         Fine_Agg + WaterDays, 
                       data = Conc_Interact3, 
                       method = "lm")
summary(linear_model3)

# The results show that this interaction consideration does not improve the 
# model, so we will not include this interaction moving forward.

# Total Aggregate

Conc_Interact4 <- Conc_Interact2 %>% mutate(Aggregate = Coarse_Agg + Fine_Agg)
linear_model4 <- train(Strength ~ Cement + Slag + Ash + Plasticizer + 
                         Aggregate + WaterDays, 
                       data = Conc_Interact4, 
                       method = "lm")
summary(linear_model4)

# This did not improve the model beyond the results above when we included the
# interaction of water and curing days, but it does produce almost identical 
# summary statistics, so it does make the model simpler.  This matches 
# intuition, because while the aggregate component of the concrete does 
# eventually contribute to the strength of the mixture, it plays no part
# in the chemical process, purely adding the structural stability of stone to
# the cohesive components produced.

# One more complex interaction worth considering is the interaction of water 
# and plasticizer with the days of curing.  In concrete, plasticizer is used 
# to reduce the amount of water used in the mixture while increasing strength 
# and maintaining workability during the pouring (placement) of the mix.

Conc_Interact5 <- Conc_Interact4 %>% 
  mutate(WaterPlasticDays = (Water + Plasticizer)/Days)
linear_model5 <- train(Strength ~ Cement + Slag + Ash +  Aggregate + 
                         WaterPlasticDays, 
                       data = Conc_Interact5, 
                       method = "lm")
summary(linear_model5)

# This combination makes trivial changes to the model, so we can choose to keep
# it or drop it.  Given a larger or different data set, this may change, but 
# with the samples we have, the interaction does not seem to make a difference.

# Linear Regression Equation

# Taking the result of the linear model refinement and using the results of 
# linear model 4, we end up with a formula to predict the compressive strength 
# of a concrete mix:

# Strength = 0.157 * Cement + 0.132 * Slag + 0.117 * Ash + 
#            0.052 * (Coarse_Agg + Fine_Agg) - 
#            0.444 * (Water + Plasticizer) / Days - .001

# This results in a prediction value with a standard error of +/- 7.7 MPa


# Another Potential Application

# Beyond predicting what a given mixture's strength will be after a certain 
# number of days, it may be useful to determine if a certain combination of 
# components will achieve the construction standards expected of concrete.  
# Since application requirements differ, we can choose one to develop the model, 
# and adjust if and when the model is utilized.

# Where I live in Massachussets, USA, the compressive strength requirement for
# residential concrete use is 2500PSI at 28 days for most applications.  Using 
# 1 pound per square inch = 6.895kPa, this converts to approximately 17.24 MPa
# after 28 days.  By adding a qualifying attribute to the data, we can also 
# build a logistic model to try to predict if a given concrete mixture will 
# meet these requirements.

# To consider the condition explored at the beginning of this analysis - that 
# there may be multiple strength test measurements for the same mixture at 
# different times - we will only consider those measurements taken when the 
# curing period is 28 days.  That is, we will consider:

# 1) days = 28 and strength >= 17.24 evaluates to meeting requirements
# 2) days = 28 and strength < 17.24 evaluates to failing requirements

# Other considerations are inconclusive since during the time up to the 28th
# day, the concrete may not meet requirements, but is not yet expected to do 
# so.  Likewise, for this model usage, if a concrete mix will eventually reach
# the required strength but do so after the residential construction
# inspection deadline, it will not meet expectations.

# First, we limit the dataset to just those records which are measured at 28 
# days and convert it to a matrix.
Conc_Quality <- Concrete_Data %>% filter(Days == 28) %>% 
  mutate(MeetSpec = ifelse(Strength >= 17.24, 1, 0))
SVM_Data <- as.matrix(Conc_Quality)

# build two data frames to store results
conf_matrix <- data.frame(Label=character(), Predict_0=numeric(), 
                          Predict_1=numeric(), Total=numeric(), 
                          stringsAsFactors = FALSE)
fit_test <- data.frame(C=numeric(), fit=numeric(), error=numeric()) 

# Support Vector Machine Model

# I will build a Support Vector Machine model and try out a range of C values
# (used to score the Constraint Violation Penalty) to find the value that 
# results in the best prediction fit to actual values.  This helps the 
# R function build the model coefficients by softening constraints in the 
# objective function being optimized.  The default is 1, so we'll try 0.0001 to 
# 10,000 by increasing order of magnitude.

lambda <- c(10^(-4:5))
for(l in lambda) {
  model <- ksvm(SVM_Data[,1:7], SVM_Data[,10], type = 'C-svc', 
                kernel = 'vanilladot', C = l, scaled = TRUE)

  pred <- predict(model,SVM_Data[,1:7])
  #evaluate prediction against real values and store for review
  fit_ratio <- sum(pred == SVM_Data[,10]) / nrow(SVM_Data)
  fit_test[nrow(fit_test) + 1, ] = c(l, fit_ratio, model@error)
  
}

# review fit test results for range of C values in lambda
fit_test

# The best value for C (Cost of Constraints Violation) is 1, so let's proceed
# with the model definition using that value:

model <- ksvm(SVM_Data[,1:7], SVM_Data[,10], type = 'C-svc', 
              kernel = 'vanilladot', C = 1, scaled = TRUE)

A0P0 = sum(pred == SVM_Data[,10] & SVM_Data[,10] == 0)
A0P1 = sum(pred != SVM_Data[,10] & SVM_Data[,10] == 0)
A1P1 = sum(pred == SVM_Data[,10] & SVM_Data[,10] == 1)
A1P0 = sum(pred != SVM_Data[,10] & SVM_Data[,10] == 1)

TotalA0 = A0P0 + A0P1
TotalA1 = A1P0 + A1P1
TotalP0 = A0P0 + A1P0
TotalP1 = A0P1 + A1P1

Total = A0P0 + A1P1 + A1P0 + A0P1

conf_matrix[1, ] = c("Actual_0", A0P0, A0P1, TotalA0)
conf_matrix[2, ] = c("Actual_1", A1P0, A1P1, TotalA1)
conf_matrix[3, ] = c("Total", TotalP0, TotalP1, Total)

conf_matrix

# This confusion matrix shows a model accuracy of (25+393)/425 = 98.35%, 
# which is a measure of the true matches compared to the total records. It has
# a precision (accurate TRUEs) rate of 393/396 = 99%.  Overall this model looks
# very useful in determining if a given concrete mix would meet residential 
# use specifications.

# Results

# The multiple linear regression model went through four iterations to find a
# predictive equation that explains about 73% of the variability in the 
# compressive strength of a concrete mixture after a give number of days.  
# That formula is:

# Strength = 0.157 * Cement + 0.132 * Slag + 0.117 * Ash + 
#            0.052 * (Coarse_Agg + Fine_Agg) - 
#            0.444 * (Water + Plasticizer) / Days - .001

# This model is simplified for use as it aggregates two values (pun intended) 
# since they have effectively combined identical effect on the dependent 
# value (Strength), and is also further simplified by using an interaction 
# effect between the amount of water in the mixture and the days the concrete
# is allowed to cure.

# The support vector machine model was applied for a slightly different purpose
# as a tool to determine whether or not a given concrete mix would meet 
# selected concrete performance requirements.  As a sample, the residential 
# construction specifications for Massachusetts, USA was selected.  The dataset
# that was used for this model analysis was filtered based on the construction
# specification timing: 28 days.

# Several values for constraint violation penalty were tested to find an 
# optimum setting.  The resulting model can then be used to predict whether a
# given mixture meets (1) or does not meet (0) the requirements.  By using a 
# confusion matrix, we can evaluate the accuracy (98.35%) and precision (99%) 
# of the support vector model that was created.

# Summary

# I built two models based on this Concrete Data dataset, one multiple linear 
# regression model using the entire data set, and another support vector 
# machine model using the set of mixtures which were measured for compressive 
# strength at 28 days.

# Linear regression models have a tendency to overfit, we have to consider
# that when using the Strength formula that was developed.  The number of 
# attributes further increases the possibility of overfiting.  The final 
# attribut set, which reduced the number by two, may help alleviate that risk.
# The multiple linear regression model that we produced had an adjusted 
# R-squaredvalue of about 73%, which explains most of the variability of the 
# dependent value.  One additional consideration that may have reduced accuracy
# was the presence of multiple records with the same concrete mixture but 
# measured for strength after different curing durations.  Future 
# considerations for analysis might be adding a time study element to those
# batches of records.  Not many mixtures had the same number of measurements, 
# so wrangling the data would be difficult.

# The practical application of the support vector machine is also very useful
# since, given a variety of materials available in differing geographies, it is
# helpful to be able to predict if a certain mixture will meet building
# requirements.  Our SVM had a very high fit to actual values (98.35%) and the
# confusion matrix produced from the prediction/actual comparison also verified
# a high precision (99%).

# References

# Since my subject matter knowledge about concrete was limited, and I neither wanted to make 
# assumptions nor trust incorrect intuition, I found these article useful.  They may be useful 
# to reviewers and readers.

# Overview of concrete and common components
# https://www.ccagc.org/resources/whats-the-difference-between-cement-and-concrete/

# Use of plasticizer in concrete mixtures
# https://en.wikipedia.org/wiki/Plasticizer#Concrete

# Mass Residential Concrete Requirements
# https://up.codes/viewer/massachusetts/irc-2015/chapter/4/foundations#R402.2

# Data Set Source
# https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength