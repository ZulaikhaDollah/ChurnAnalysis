
ChurnDF <- read.csv("https://raw.githubusercontent.com/ZulaikhaDollah/ChurnAnalysis/master/new-churn-dataset.xls", header = T, sep = ",")

#Split data into two set (Training & Test Set)

set.seed(123)
sample<- sample.split(ChurnDF,SplitRatio =0.75)
ChurnTrain<- subset(ChurnDF,sample==TRUE)
ChurnTest<- subset(ChurnDF,sample==FALSE)

dim(ChurnTrain); dim(ChurnTest)


#To check data structure
str(ChurnTrain)

#to check NA value
sapply(ChurnTrain, function(x) sum(is.na(x)))

#To check summary data
summary(ChurnTrain)

#Exploratory Data Analysis

##Churn ratio by categorical analysis
exp1 <- ggplot(ChurnTrain, aes(area.code, fill = churn)) + geom_bar(position = "fill") + labs(x = "Area code", y = "") + theme(legend.position = "bottom")
exp2 <- ggplot(ChurnTrain, aes(international.plan, fill = churn)) + geom_bar(position = "fill") + labs(x = "International?", y = "") + theme(legend.position = "bottom")
exp3 <- ggplot(ChurnTrain, aes(voice.mail.plan, fill = churn)) + geom_bar(position = "fill") + labs(x = "Voicemail", y = "") + theme(legend.position = "bottom") 
exp4 <- ggplot(ChurnTrain, aes(customer.service.calls, fill = churn)) + geom_bar(position = "fill") + labs(x = "Customer calls", y = "") + theme(legend.position = "bottom") 
grid.arrange(exp1, exp2, exp3, exp4, ncol = 4, nrow = 1, top = "Churn/Non-churn Proportion")

##Explore distributions by continuous predictors
daymin <- ggplot(ChurnTrain, aes(churn, total.day.minutes, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
evemin <- ggplot(ChurnTrain, aes(churn, total.eve.minutes, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
nitmin <- ggplot(ChurnTrain, aes(churn, total.night.minutes, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
intmin <- ggplot(ChurnTrain, aes(churn, total.intl.minutes, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
daycal <- ggplot(ChurnTrain, aes(churn, total.day.calls, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
evecal <- ggplot(ChurnTrain, aes(churn, total.eve.calls, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
nitcal <- ggplot(ChurnTrain, aes(churn, total.night.calls, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
intcal <- ggplot(ChurnTrain, aes(churn, total.intl.calls, fill = churn)) + geom_boxplot(alpha = 0.8) + theme(legend.position = "null")
grid.arrange(daymin, evemin, nitmin, intmin, 
             daycal, evecal, nitcal, intcal, 
             ncol = 4, nrow = 2)

exp6 <- ggplot(ChurnTrain, aes(account.length, fill = churn)) + geom_density(alpha = 0.7) + theme(legend.position = "null")
exp7 <- ggplot(ChurnTrain, aes(number.vmail.messages, fill = churn)) + geom_density(alpha = 0.7) + theme(legend.position = "null")
grid.arrange(exp6, exp7, ncol = 2, nrow = 1)


#Check for collinearity
corrplot(cor(ChurnTrain[sapply(ChurnTrain, is.numeric)]))

ChurnTrain$total.day.charge <- NULL
ChurnTrain$total.eve.charge <- NULL
ChurnTrain$total.night.charge <- NULL
ChurnTrain$total.intl.charge <- NULL
ChurnTrain$account.length <- NULL
ChurnTrain$area.code <- NULL
ChurnTrain$phone.number <- NULL
ChurnTrain$state <-NULL
ChurnTest$state <-NULL
ChurnTest$total.day.charge <- NULL
ChurnTest$total.eve.charge <- NULL
ChurnTest$total.night.charge <- NULL
ChurnTest$total.intl.charge <- NULL
ChurnTest$account.length <- NULL
ChurnTest$area.code <- NULL
ChurnTest$phone.number <- NULL

#Recheck data structure
str(ChurnTrain)

str(ChurnTest)

#Fitting the Logistic Regression Model

LogModel <- glm(formula = churn ~ ., family = "binomial", data = ChurnTrain)
print(summary(LogModel))

#glm variable importance
varImp(LogModel)




