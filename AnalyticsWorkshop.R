path <- "C:/Users/Kira/Desktop/RData/Data Science workshop"
setwd(path)
train <- read.csv("train.csv")
str(train)
train_count <- subset(train, select = c(ID, Age, Hours.Per.Week)) #select continous vars
train_cat <- subset(train, select = -c(ID, Age, Hours.Per.Week)) # the rest are categorical 
#Univariate analysis
summary(train_count) #generate stat. analysis

#hensem look for the stat. results 
#install.packages("pastecs")
library("pastecs")
options(scipen = 100)
options(digits = 2)
stat.desc(train_count)

apply(train_cat, 2, function(x){length(unique(x))}) # to generate unqiue values
head(sort(table(train_cat$Race), decreasing = TRUE, 10))
as.matrix((prop.table(table(train_cat$Race))))

head(sort(table(train_cat$Native.Country), decreasing = TRUE), 20)

head(round(sort(prop.table(table(train_cat$Native.Country)), decreasing = TRUE), 6), 20)

IQR(train$Age) #interquartile range 

### multivariate analysis
#install.packages(c("gmodels"))
library(gmodels)
CrossTable(train$Sex, train$Income.Group)

# continous - categorical variables
#install.packages(c("ggplot2"))
library(ggplot2)
ggplot(train, aes(Sex, fill = Income.Group)) + geom_bar()+labs(title = "Stacked Bar Chart", x = "Sex", y = "Count" + theme_bw())

#continous - continous variables 
scatter.smooth(train$Age, train$Hours.Per.Week)

# Categorical - Continous variables 
ggplot(train, aes(Sex, Hours.Per.Week)) + geom_boxplot() + labs(title="Boxplot")

# missing values treatment  ===> NOT WORKING
#apply fun for handling missing values ..
table(is.na(train))
colSums(is.na(train))
# imputation for missing values ..
install.packages("mlr", repos = 'http://cran.us.r-project.org')
library(mlr)

# all vals here that contains missing are categorical .. imputed by Mode
impute_data <- impute(train, classes = list(factor = imputeMode()))
train <- impute_data$data
colSums((is.na(train)))

###### Outlier treatment
library(ggplot2)
ggplot(train, aes(ID, Age)) + geom_jitter() # age variable

ggplot(train, aes(ID, Hours.Per.Week)) + geom_jitter() # hours of week variable 
# no outliers detected !

####### variable transformation .. feature engineering 
sapply(train, class)

as.matrix(sort(prop.table(table(train$Workclass))))
#install.packages(c("car"))
library(car) #built in func. we gonna combine the least 5% ratios 
train$WorkClass <- recode(train$WorkClass, "c('State-gov', 'Self-emp-inc', 'Federal-gov', 'Without-pay', 'Never-worked') = 'Others'")

as.matrix(prop.table(table(train$Workclass)))

# Predictive Modeling ..
# the last preprocessing step .. convert predictor into 0,1 
table(train$Income.Group)
train$Income.Group <- ifelse(train$Income.Group == "<=50K", 0,1)
table(train$Income.Group)

#wait a minute !! .. ID has to be ignored !! 
train <- subset(train, select = -c(ID))

# Building the DT

#install.packages(c("rpart"))
set.seed(333)
library(rpart)
train.tree <- rpart(Income.Group ~ ., data = train, method = "class", control = rpart.control(minsplit = 20, minbucket = 100, maxdepth = 10, xval = 5))
#xval is the cross validation
summary(train.tree)
library(rpart.plot)
rpart.plot(train.tree)

# prediction after building the tree ..
prediction_train <- predict(train.tree, newdata = train, type = "class")
prediction_test <- predict(train.tree, newdata = test, type = "class")

# claculate classifiction accuracy ..
install.packages(c("caret"))
library(caret)
confusionMatrix(prediction_train, train$Income.Group)

# FINALLY the solution file to be uploaded on "Analytics Vidhya" website ..
solution_frame <- data.frame((ID = test$ID, Income.Group = prediction_test))
write.csv(solution_frame, file = "Final_Solution.csv")