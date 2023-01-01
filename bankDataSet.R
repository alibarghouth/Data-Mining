#Reading Data From CSV File
bank <- read.csv("C:\\Users\\user\\Desktop\\bank-full.csv")

#delete
bank = bank[-c(4,9,10,11,13,14,15,16)]

#To view data set in another window[Excel way :)]
View(bank)

bank$job[bank$job == "unknown"] <- NA


#Checking for missing values
sapply(bank, function(x) sum(is.na(x)))


#delete missing values 
bank <- na.omit(bank)


#Checking for missing values
colSums(is.na(bank))

#Writing Data to CSV File
write.csv(bank,"C:\\Users\\user\\Desktop\\bank-withoutMissingValues.csv", row.names = FALSE)

install.packages("e1071")
library(e1071)

#Factoring default,housing,loan,marital,job variable
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$marital <- as.factor(bank$marital)
bank$job <- as.factor(bank$job)

#Removing Outliers
##Calculating IQR
###Summary gives 6 values : min, q1, median, mean, q3, max
summary = summary(bank$duration)

####Save q1 and q3 as numeric values
q1 = as.numeric(summary(bank$duration)[2])
q3 = as.numeric(summary(bank$duration)[5])

###Find IQR
iqr <- q3 - q1

##Removing values higher than q3 + 1.5*iqr
bank <- subset(bank, bank$duration <= q3 + iqr*1.5)

##Removing values lower than q1 - 1.5*iqr
bank <- subset(bank, bank$duration >= q1 - (iqr*1.5))

#Writing Data to CSV File
write.csv(bank,"C:\\Users\\user\\Desktop\\bankwithoutOutLiers-full.csv", row.names = FALSE)

#Factoring duration,balance and age variables
#0 - 200 = Short, 200-400 = Normal, <400 = Long
##duration
bank$duration2[bank$duration < 200] <- "Short"
bank$duration2[bank$duration >=200 & bank$duration < 400] <- "Normal"
bank$duration2[bank$duration >= 400] <- "Long"
bank$duration <- bank$duration2
bank$duration <- as.factor(bank$duration)


#>10000 = Low, 10000-60000 = Normal, <60000 = High
##balance
bank$balance2[bank$balance < 10000] <- "Low"
bank$balance2[bank$balance >=10000 & bank$balance < 60000] <- "Normal"
bank$balance2[bank$balance >= 60000] <- "High"
bank$balance <- bank$balance2
bank$balance <- as.factor(bank$balance)

#>25 = Young, 25-60 = Mature, <60 = Old
##age
bank$age2[bank$age < 25] <- "Young"
bank$age2[bank$age >=25 & bank$age < 60] <- "Mature"
bank$age2[bank$age >= 60] <- "Old"
bank$age <- bank$age2
bank$age <- as.factor(bank$age)


##delete the temporary age2,duratin2,balance2
#delete
bank = bank[-c(10,11,12)]


#Random sampling data -> 70% for training and rest for testing
s <- sample(nrow(bank), nrow(bank)*0.7)
trainingData <- bank[s,]
testingData <- bank[-s,]


#Building Naive Bayes Model
nb <- naiveBayes(y~., bank, laplace = 1)
#Prediction
predictionTable <- table(predict(nb, testingData),testingData$y)
##Confusion Matrix
misclassification = 1 - sum(diag(predictionTable)) / sum(predictionTable)
##Accuracy
accuracy = (1-misclassification)*100
##Accuracy  is  nearly 90%.

