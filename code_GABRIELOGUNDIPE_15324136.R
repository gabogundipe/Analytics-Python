install.packages('caret', dependencies = TRUE)
library(caret)
install.packages("dplyr")
library(magrittr)

set.seed(123)
training.sample <- creditdata$Outcome %>% createDataPartition(p = 0.7, list = F)
training_data  <- creditdata[training.sample, ]
testing_data <- creditdata[-training.sample, ]

#checking if data was split correctly
length(which(training_data$Outcome==1))

#read data in
creditdata <- read.table(file="CreditData.txt", sep = " ", col.names = colNames) #na.strings
creditdata$Outcome <- as.factor(creditData$Outcome)

#assigning column names to data

colNames <- c("AccountStatus", "Duration", "CreditHistory", "Purpose",
              "CreditAmount", "Savings", "Employement", "InstallmentRate",
              "Gender&MaritalStatus", "OtherDebtors", "ResidenceDuration", 
              "Property", "Age", "OtherInstallment", "Housing", "ExistingCredits",
              "Job", "NoOfDependants", "Telephone", "ForeignWorker", "Outcome")

ncol(creditdata)


creditdata$Outcome = ifelse(creditdata$Outcome == 1,0,1)
#for loop table frequencies

for (i in 1:ncol(creditdata)) {
  
  print(colnames(creditdata[i]))
  print(table(creditdata[i]))
  myTable <-table(creditdata[i])
  print(prop.table(myTable))
}

#1. account stats - negative, positive, none
levels(creditdata$AccountStatus) <- c("A11", "A12", "A12", "A14")
levels(creditdata$AccountStatus)
counts.AccountStatus <- table(creditdata$Outcome, creditdata$AccountStatus)
barplot(counts.AccountStatus, beside=TRUE, col=c("Coral","dodgerblue"), main="Account Status and Outcome",
        xlab="Account Status", ylab="Count", legend.text = c("Good","Bad"))

#2.Credit duration
boxplot(creditdata$Duration ~ creditdata$Outcome  ,main="Outcome vs Loan Duration",names = c("Good Customers", "Bad Customers"),ylab="Loan Duration",col=c("Coral","dodgerblue"))

#3. Credit History - grouping to get credits paid back, delayed, critical amount
levels(creditdata$CreditHistory) <- c("A30", "A30", "A30", "A33", "A34")
levels(creditdata$CreditHistory)

counts.CreditHistory <- table(creditdata$Outcome, creditdata$CreditHistory)
barplot(counts.CreditHistory, beside=TRUE, col=c("Coral","dodgerblue"), main="Credit History and Outcome",
        xlab="Credit History", ylab="Count", legend.text = c("Good","Bad"))

#4. Purpose - grouping radio/tv, domestic, repairs, others together. Seem like household expenses, repairs
#to me seems like unexpected costs, small frequency number. Felt others could also be put into this group as it represented small frequency
levels(creditdata$Purpose) <- c("A40", "A41", "A43", "A43", "A43", "A43","A43", "A46", "A46", "A49")
levels(creditdata$Purpose)

counts.Purpose <- table(creditdata$Outcome, creditdata$Purpose)
barplot(counts.Purpose, beside=TRUE, col=c("Coral","dodgerblue"), main="Purpose and Outcome",
        xlab="Purpose", ylab="Count", legend.text = c("Good","Bad"))

#5. Credit Amount - leave as it is as numerical
boxplot(creditdata$CreditAmount ~ creditdata$Outcome  ,main="Outcome vs Amount",names = c("Good Customers", "Bad Customers"),ylab="Amount",col=c("Coral","dodgerblue"))

#6. Savings - group as under 100, 100-500, and 500+ ... don't think it matters greatly whether you have 500 or just over 1000, many
#factors can come into play like income etc. 
levels(creditdata$Savings) <- c("A61", "A62", "A63", "A63", "A65")
counts.Savings <- table(creditdata$Outcome, creditdata$Savings)
barplot(counts.Savings, beside=TRUE, col=c("Coral","dodgerblue"), main="Savings and Outcome",
        xlab="Savings", ylab="Count", legend.text = c("Good","Bad"))

#7. Present employment - grouping as unemployed, <1, 1-4 and 4+
levels(creditdata$Employement) <- c("A71", "A72", "A73", "A74", "A74")
counts.Employment <- table(creditdata$Outcome, creditdata$Employement)
barplot(counts.Employment, beside=TRUE, col=c("Coral","dodgerblue"), main="Employment and Outcome",
        xlab="Present employment", ylab="Count", legend.text = c("Good","Bad"))

#8. Installment rate - leave as is
counts.InstallmentRate <- table(creditdata$Outcome, creditdata$InstallmentRate)
barplot(counts.InstallmentRate, beside=TRUE, col=c("Coral","dodgerblue"), main="Installment and Outcome",
        xlab="Installment rate", ylab="Count", legend.text = c("Good","Bad"))

#9. Personal status and sex - Merging all males except single as single represented a great amount of frequency
levels(creditdata$Gender.MaritalStatus) <- c("A91", "A92", "A93", "A91")
counts.GenderMarital <- table(creditdata$Outcome, creditdata$Gender.MaritalStatus)
barplot(counts.GenderMarital, beside=TRUE, col=c("Coral","dodgerblue"), main="Personal Status, Gender and Outcome",
        xlab="Personal status and sex", ylab="Count", legend.text = c("Good","Bad"))

#10. Debtors - joining co - applicant and guarantor as they are both equally weighted in a bank loan application
levels(creditdata$OtherDebtors) <- c("A101", "A102", "A102")
counts.OtherDebtors <- table(creditdata$Outcome, creditdata$OtherDebtors)
barplot(counts.OtherDebtors, beside=TRUE, col=c("Coral","dodgerblue"), main="Guarantors and Outcome",
        xlab="Debtors", ylab="Count", legend.text = c("Good","Bad"))

#11. Present residence - leave as is
counts.OtherDebtors <- table(creditdata$Outcome, creditdata$ResidenceDuration)
barplot(counts.OtherDebtors, beside=TRUE, col=c("Coral","dodgerblue"), main="Present Residence and Outcome",
        xlab="Present residence", ylab="Count", legend.text = c("Good","Bad"))

#12. Property - leave as is as i think each category represent distinctly different levels
counts.Property <- table(creditdata$Outcome, creditdata$Property)
barplot(counts.Property, beside=TRUE, col=c("Coral","dodgerblue"), main="Property and Outcome",
        xlab="Property", ylab="Count", legend.text = c("Good","Bad"))

#13. Age - leave as is ... look at min and max
counts.Age <- table(creditdata$Outcome, creditdata$Age)
barplot(counts.Age, beside=TRUE, col=c("Coral","dodgerblue"), main="Age and Outcome",
        xlab="Age", ylab="Count", legend.text = c("Good","Bad"))

boxplot(creditdata$Age ~ data$Outcome  ,main="Age vs Outcome", xlab = "response",names = c("Good Customers", "Bad Customers"),ylab="Age",col=c("Coral","dodgerblue"))

#14. Installment plans - not quite sure what stores means so will leave as is 
counts.InstallmentPlan<- table(creditdata$Outcome, creditdata$OtherInstallment)

barplot(counts.InstallmentPlan, beside=TRUE, col=c("Coral","dodgerblue"), main="Installment Plan and Outcome",
        xlab="Installment Plan", ylab="Count", legend.text = c("Good","Bad"))

#15. Housing - leave as is for now? "Free" can mean living with parents, or a friend 
counts.Housing <- table(creditdata$Outcome, creditdata$Housing)
barplot(counts.Housing, beside=TRUE, col=c("Coral","dodgerblue"), main="Housing and Outcome",
        xlab="Housing", ylab="Count", legend.text = c("Good","Bad"))

#16 existing credits - leave as is
levels(creditdata$ExistingCredits)
counts.existing <- table(creditdata$Outcome, creditdata$ExistingCredits)
barplot(counts.existing, beside=TRUE, col=c("Coral","dodgerblue"), main="Existing credits and Outcome",
        xlab="Existing credits", ylab="Count", legend.text = c("Good","Bad"))

#17 Job
levels(creditdata$Job) <- c("A171", "A171", "A173", "A174")
counts.Job <- table(creditdata$Outcome, creditdata$Job)
barplot(counts.Job, beside=TRUE, col=c("Coral","dodgerblue"), main="Jobs and Outcome",
        xlab="Job", ylab="Count", legend.text = c("Good","Bad"))

#18 number of people to pay maintenance for
counts.maintenance <- table(creditdata$Outcome, creditdata$NoOfDependants)
barplot(counts.maintenance, beside=TRUE, col=c("Coral","dodgerblue"), main="Dependants and Outcome",
        xlab="Dependants", ylab="Count", legend.text = c("Good","Bad"))

#19 telephone - delete.

#20 - foreign worker
counts.foreign <- table(creditdata$Outcome, creditdata$ForeignWorker)
barplot(counts.foreign, beside=TRUE, col=c("Coral","dodgerblue"), main="Dependants and Outcome",
        xlab="Foreign worker", ylab="Count", legend.text = c("Good","Bad"))


#AIC 685.64
model1 <- glm(formula = Outcome ~.,family=binomial(logit),data=training_data) 
summary(model1)
#increase threshold to 0.25 as there are some variables noted as insignificant that intuition tells me they may still be significant
#ForeignWorker, Housing, OtherInstallment, Age, Property, Gender.MaritalStatus, InstallmentRate, Savings, CreditAmount, Purpose, CreditHistory, Duration, AccountStatus

subset2 <- subset(training_data, select=c("ForeignWorker", "Housing", "ExistingCredits", "OtherInstallment",  "Property", 
                                            "Job", "InstallmentRate", "Employement", "Savings", "CreditAmount",
                                          "Purpose",  "CreditHistory", "Duration", "AccountStatus", "Outcome"))
                                        
#AIC 678.19 - removed telephone, Dependants, Job, existing credits, present residence, debtors, employement duration,
         
model2 <- glm(formula = Outcome ~., family = binomial(logit), data=subset2)
summary(model2)

subset3 <- subset(training_data, select=c("ForeignWorker", "Housing", "ExistingCredits", "OtherInstallment",
                                           "InstallmentRate", "Employement", "Savings", "CreditAmount",
                                          "Purpose",  "CreditHistory", "Duration", "AccountStatus", "Outcome"))
#AIC 672.34 - removed property & job
model3 <- glm(formula = Outcome ~., family = binomial(logit), data=subset3)
summary(model3)


#step model

step_model <- step(model1)

subset_step <- subset(training_data, select=c("ForeignWorker", "OtherInstallment", "ExistingCredits",  "Housing", "Telephone",
                                                                              "InstallmentRate","Employement", "Savings", "CreditAmount", 
                                                                   "Purpose",  "CreditHistory", "Duration", "AccountStatus", "Outcome"))

#AIC 672.11
model4 <- glm(formula = Outcome~., family = binomial(logit), data = subset_step)

BIC(model1)
BIC(model2)
BIC(model3)
BIC(model4)

#pick model 3 as lowest pick
summary(model4)


#AUC curve
fit <- predict(model3, type="response", newdata = testing_data)


library(ROCR)
testprediction = prediction( fit, testing_data$Outcome)
testperformance <- performance(testprediction, "tpr", "fpr")
plot(testperformance)
abline(0,1)


#AUC 0.73
AUCLog=performance(testprediction, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog,"\n")

#Accuracy

mod3pred <- predict(model3, newdata = testing_data, type="response")
mod3pred
#confusion matrix
cm <- as.matrix((table(testing_data$Outcome, mod3pred > 0.5)))
mod3accuracy <- (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1] ) 

mod3accuracy #0.73.227


