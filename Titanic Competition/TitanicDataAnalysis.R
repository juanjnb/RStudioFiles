#defining local variables
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Create a sub-data frame to add to a "Survived" value to "test"
temp.initializeSurviveData <- rep("None",nrow(test))
temp.initializeSurviveData <- as.factor(rep("None",nrow(test)))

#Adding a "Survived" variable to the "test" set to allow for combining data set
test.survived <- data.frame(Survived = temp.initializeSurviveData, test[,])

#Combining the data of "train" with "test.survived" - appending it
data.combined <- rbind(train, test.survived)

#str displays the structure of the data.frame and values
str(temp.initializeSurviveData)
str(train)
str(test)
str(test.survived)
str(data.combined)

#Re-defining data type of the columns
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

# Distribution based on sex
table(data.combined$Sex)

# Take a look at gross survival rates
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

table(train$Age<=10)

table(train$Pclass=="3")

# Load up ggplot2 package to use for visualization
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = train$Pclass, fill = factor(train$Survived)))+
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(train, aes(x = train$Pclass, fill = factor(train$Survived)))+
  geom_bar(stat = "count")+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Hypothesis - Male in 3rd class has a higher rate of survival

# Get the names of the columns in train
names(train)

# Create a new data frame using a subset and logical instruction
folksSurvive3rd <- subset(train,Pclass=="3")

# Examine the new data frame
str(folksSurvive3rd)

table(folksSurvive3rd$Survived)
table(folksSurvive3rd$Sex)

folksSurvive3rd$Sex <- as.factor(folksSurvive3rd$Sex)
ggplot(folksSurvive3rd, aes(x = folksSurvive3rd$Sex, fill = factor(folksSurvive3rd$Survived)))+
  geom_bar(stat = "count") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Conclusion: males in the 3rd class has less chance of survival than the females

# Examine the first few names in the training data set
str(train)
train$Name <- as.factor(train$Name)
head(as.factor(train$Name))

# How many unique names are there across both train and test data
length(unique(as.character(data.combined$Name))) #outputs 1307, but we have 1309 observations, there might be some duplicates

# Creating a vector with all the duplicated names in it
dup.vector <- duplicated(data.combined$Name)

str(dup.vector)

# Get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"name"])

# Investigate the title
library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses [1:5, ]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses [1:5, ]

males <- data.combined[which(data.combined$Sex == "male"),]
males [1:5, ]

