# Import the dataset
creditCard <- read.csv('Dataset/creditcard.csv')

# Look at the structure of the dataset
str(creditCard)

# Convert Class (fraud or not) to factor variable
creditCard$Class <- factor(creditCard$Class, levels = c(0, 1))

# Summarize the data
summary(creditCard)

# Count/check for missing values
sum(is.na(creditCard))

# Get distribution of legit vs. fraud transactions in the dataset
table(creditCard$Class)

# Get percentage of legit and fraud transactions
prop.table(table(creditCard$Class))

# Create pie chart representing the transaction dataset
labels <- c("legit", "fraud")
labels <- paste(labels, round(100 * prop.table(table(creditCard$Class)), 2))
labels <- paste0(labels, "%")

# orange -> legit, red -> fraud
pie(table(creditCard$Class), labels, col = c("orange", "red"),
    main = "Credit Card Transactions Pie Chart")

#-------------------------------------------------------------------------------

# No model predictions (baseline to compare against later)

predictions <- rep.int(0, nrow(creditCard))
predictions <- factor(predictions, levels = c(0, 1))

#install.packages('caret')
library(caret)
confusionMatrix(data = predictions, reference = creditCard$Class)

#-------------------------------------------------------------------------------
library(dplyr)

set.seed(1) # ensures same sample every time
creditCard <- creditCard %>% sample_frac(0.1) # Extracts 10% of rows from dataset

table(creditCard$Class)

# plot scatter plot
library(ggplot2)

ggplot(data = creditCard, aes(x = V1, y = V2, col = Class)) + 
  geom_point() + 
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2', 'red'))
# not a lot of fraud data points, model will be unable to learn a lot unless balanced

#-------------------------------------------------------------------------------
# Create training and test sets for model, only balancing the training set

install.packages('caTools')
library(caTools)

set.seed(123)

# 80% will be training set, 20% for test set
dataSample = sample.split(creditCard$Class, SplitRatio = 0.80) 
trainData = subset(creditCard, dataSample == TRUE)
testData = subset(creditCard, dataSample == FALSE)

# Check dimensions of different subsets
dim(trainData)
dim(testData)

#-------------------------------------------------------------------------------
# Random over-sampling (ROS)

table(trainData$Class)

nLegit = 22750
newFracLegit <- 0.50
new_n_total <- nLegit/newFracLegit

install.packages('ROSE')
library(ROSE)
oversamplingResult <- ovun.sample(Class ~ .,
                                  data = trainData, 
                                  method = "over", 
                                  N = new_n_total, 
                                  seed = 2019)

oversampledCredit <- oversamplingResult$data
table(oversampledCredit$Class) # now balanced dataset, equal legit and fraud points

ggplot(data = oversampledCredit, aes(x = V1, y = V2, col = Class)) + 
  geom_point(position = position_jitter(width = 0.1)) + 
  theme_bw() +
  scale_color_manual(values = c('dodgerblue2', 'red')) # lots of overlapping points

#-------------------------------------------------------------------------------
# Random under-sampling (RUS)

table(trainData$Class)

nFraud = 35
newFracFraud <- 0.50
new_n_total <- nFraud/newFracFraud

undersamplingResult <- ovun.sample(Class ~ .,
                                  data = trainData, 
                                  method = "under", 
                                  N = new_n_total, 
                                  seed = 2019)

undersampledCredit <- undersamplingResult$data
table(undersampledCredit$Class)

# reveals drawback that is loss of data
ggplot(data = undersampledCredit, aes(x = V1, y = V2, col = Class)) + 
  geom_point() + 
  theme_bw() +
  scale_color_manual(values = c('dodgerblue2', 'red'))

#-------------------------------------------------------------------------------
<<<<<<< HEAD
# ROS & RUS
# Oversample the fraud cases and undersample the legit cases

nNew <- nrow(trainData) # 22785
fractionFraudNew <- 0.50

samplingResult <- ovun.sample(Class ~ ., 
                              data = trainData, 
                              method = "both", 
                              N = nNew, 
                              p = fractionFraudNew,
                              seed = 2019)

sampledCredit <- samplingResult$data
table(sampledCredit$Class)
prop.table(table(sampledCredit$Class)) # percentage legit and fraud

# plot over and undersampled data
ggplot(data = sampledCredit, aes(x = V1, y = V2, col = Class)) + 
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw() + 
  scale_color_manual(values = c('dodgerblue2', 'red')) # fraud cases still overlapped

#-------------------------------------------------------------------------------
#Using SMOTE to balance dataset 
# generates synthetic instances for the minority class using existing instance and
# its k nearest neighbors

install.packages("smotefamily")
library(smotefamily)

table(trainData$Class)

# set amount of legit and fraud cases, + desired ratio of legit to fraud
n0 = 22750 # legit
n1 = 35 # fraud
r0 = 0.6 # ratio (60% legit, 40% fraud)

# calculate value for dup_size paramater of SMOTE function
nTimes <- ((1 - r0) / r0) * (n0 / n1) - 1

smoteOutput = SMOTE(X = trainData[ , -c(1, 31)], # features only, drop ID and class
                     target = trainData$Class, # labels (Class: 0 or 1)
                     K = 5, # K nearest neighbors
                     dup_size = nTimes)

creditSmote <- smoteOutput$data
colnames(creditSmote)[30] <- "Class" # just uppercases column name
prop.table(table(creditSmote$Class)) # see ratio of legit to fraud is now ~ 60/40

# Class distribution for original training dataset (w/o SMOTE)
ggplot(trainData, aes(x = V1, y = V2, color = Class)) + 
  geom_point() + 
  scale_color_manual(values = c('dodgerblue2', 'red'))

# Class distribution for over-sampled dataset using SMOTE
ggplot(creditSmote, aes(x = V1, y = V2, color = Class)) + 
  geom_point() + 
  scale_color_manual(values = c('dodgerblue2', 'red'))

#-------------------------------------------------------------------------------
# Build decision tree to predict whether a transaction is fraudulent or not

install.packages('rpart')
install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

CART_model <- rpart(Class ~ . , creditSmote)
