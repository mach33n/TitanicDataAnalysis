#Load Raw Data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Swap first two columns to match ordering in "Train" dataframe.
test.survived <- test.survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]

#Combine data sets
data.combined <- rbind(train, test.survived)

#A bit about R data types
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
str(data.combined)

#Take a look at gross survival rate
table(data.combined$Survived)

#Take a look at class
table(data.combined$Pclass)

#Load up ggplot2 package to use for visualizations
library(ggplot2)

#Hypothesis - Rick folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")
