a <- read.csv("loans.csv")
print(a)
str(a)
summary(a)
table(a$not.fully.paid)
missing <- subset(a, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
str(missing)
nrow(missing)
table(missing$not.fully.paid)
library(mice)
set.seed(144)
vars.for.imputation <- setdiff(names(a), "not.fully.paid")
imputed <- complete(mice(a[vars.for.imputation]))
a[vars.for.imputation] <- imputed
library(caTools)
set.seed(144)
#Spliting 70% of dataset
spl <- sample.split(a$not.fully.paid, 0.7)
train <- subset(a, spl == TRUE)
test <- subset(a,spl==FALSE)

mod <- glm(not.fully.paid ~.,data=train,family = "binomial")
summary(mod)
test$predicted.risk <- predict(mod, newdata=test, type="response") # adding predicted.risk to test
table(test$not.fully.paid, test$predicted.risk > 0.5)
library(ROCR)
pred = prediction (test$predicted.risk, test$not.fully.paid) # prediction of risk
as.numeric(performance(pred, "auc")@y.values) # converting to AUC value
# creating bivariate logistic regression model, on int.rate only
bivariate <- glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(bivariate)
cor(train[c("int.rate","installment","log.annual.inc","dti","fico","days.with.cr.line","revol.bal","revol.util","inq.last.6mths","delinq.2yrs","pub.rec")])
pred.bivariate <- predict(bivariate,newdata=test,type = "response")
max (pred.bivariate)
prediction.bivariate <- prediction(pred.bivariate, test$not.fully.paid)
print("AUC of bivariate model is ")
as.numeric(performance(prediction.bivariate, "auc")@y.values)
# Calculating profit on investment on loans for 3 years at CI , set C=1
test$profit = exp(test$int.rate*3) - 1   # profit on an investment of $1
test$profit[test$not.fully.paid == 1] = -1 # profit on an investment of $1, not fully paid
summary(test$profit)   # checking average profit
highInterest <- subset(test,int.rate>=0.15) # subset of high interset loans
summary(highInterest$profit) 
print("Average return on high interest investment is ")
mean(highInterest$profit)
table(highInterest$not.fully.paid)
cutoff <-  sort(highInterest$predicted.risk, decreasing=FALSE)[100]
print("Cut-off risk for our investment is")
cutoff

selectedLoans=subset(highInterest,predicted.risk <= cutoff) # Making subset with predicted risk less than cutoff
print(" Total number of investemnt is")
nrow(selectedLoans)  # total numebr of investments
print("Total profit on these loans for $1 invested in each loan")
sum(selectedLoans$profit)  # total profit on these loans for $1 invested in each loan. 
print('Average profit on these loans is')
mean(selectedLoans$profit)   # average profit made on these loans
print("Breakdown of laons being paid or not")
table(selectedLoans$not.fully.paid) # breakdown of laons being paid or not
