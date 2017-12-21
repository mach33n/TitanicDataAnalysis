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

#Examine the first few names in the training data set
head(as.character(train$Name))

#How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

#Two Duplicate names, take a closer look
#First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

#What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

#Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#Any correlation with other variables (e.g., sibsp)?
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#Any correlation with other variables (e.g., sibsp)?
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

#Expand upon the relationship between 'Survived' and 'Pclass' by adding the new 'Title' variable to the 
# data set and then explore a potential 3-dimensional relationship.

#Create a utility function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0){
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0){
    return("Mrs.")
  }else if (length(grep("Mr.", Name)) > 0){
    return("Mr.")
  } else {
    return("Other")
  }
  
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

#Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~ Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Distribution between female and male
table(data.combined$Sex)

#Visualize relationship between sex, pclass and survival.
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~ Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#We understand the relatiosnhip between sex,pclass and survival rates 
#Now lets analyze age
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#Graph with respect to sex and class
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~ Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

#Validate that Master is a good proxy for male children.
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~ Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

#OK, appears female children may have different survival rate, 
#could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

#Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)

#Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

#We believe title is predictive. Visualize survival rates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

#Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
#Total family size
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

#Visualize this to see if it is predictive.
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")