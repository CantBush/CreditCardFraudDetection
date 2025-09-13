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
# ROS & RUS