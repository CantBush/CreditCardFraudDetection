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

pie(table(creditCard$Class), labels, col = c("orange", "red"),
    main = "Credit Card Transactions Pie Chart")
