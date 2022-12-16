
library(readr)
library(readxl)

Application_Data = read_excel("C:/Users/zhuru/Downloads/Homework - Business Analysis.xlsx", 
                               sheet = "Application Data")
Loan_Performance = read_excel("C:/Users/zhuru/Downloads/Homework - Business Analysis.xlsx", 
                               sheet = "Loan Performance")

US <- read_table2("C:/Users/zhuru/Downloads/US/US.txt")

names(US) = c("CountryCode", "zip", "PlaceName", 
                    "State", "AdminCode1", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")

ZipCodes = subset(US, select = c("zip", "State"))

#-------Data Prep--------

library(dplyr)

# There is no duplicate in Application Data, while there are some second applications in Loan Performance data.
# Only DFB2F1EC63 & B3F9914DBB have different results in first and second applications.
# Some rows in Application Data cannot find a match in Loan Performance Data.

Loan_Performance$time = substr(Loan_Performance$idLoan, 13, 13)
Loan_Performance$idLoan = substr(Loan_Performance$idLoan, 1, 10)
Loan_Performance = Loan_Performance[Loan_Performance$time == 1 & Loan_Performance$idLoan != "DFB2F1EC63" & Loan_Performance$idLoan != "B3F9914DBB",] # Only include first application result

Loan_Performance$idLoan = tolower(Loan_Performance$idLoan)
data = inner_join(Application_Data, Loan_Performance, by = c("customer_id" = "idLoan")) # Match loan performance with application

ZipCodes$zip = as.numeric(ZipCodes$zip)
data = inner_join(data, ZipCodes, by = c("address_zip" = "zip")) # Match zip with location

data$email_domain = sub(".*@", "", data$email) # Extract email domain

data$age = as.numeric(substr(data$application_when, 1, 4)) - as.numeric(substr(data$birth_date, 1, 4)) # Calculate applicant age

data$amount_gap = data$amount_requested - data$amount_approved # Calculate difference between requested amount & approved amount

data$duration_gap = data$loan_duration - data$duration_approved # Calculate difference between requested duration & approved duration

data$money_left = data$monthly_income_amount - data$monthly_rent_amount # Calculate how much money left after paying rent

data$interest = (data$payment_amount * data$num_payments - data$amount_approved)/data$amount_approved/data$duration_approved

data$weekday = weekdays(data$application_when)

data$landline_provided = NA

# Convert NAs

for (i in 1:638){
  if(is.na(data$other_phone_type[i])){
    data$other_phone_type[i] = "NA"
  }}

for (i in 1:638){
  if(is.na(data$payment_amount_approved[i])){
    data$payment_amount_approved[i] = 0
  }}

for (i in 1:638){
  if(is.na(data$bank_account_duration[i])){
    data$bank_account_duration[i] = "NA"
  }}

for (i in 1:638){
  if(is.na(data$how_use_money[i])){
    data$how_use_money[i] = "NA"
  }}

for (i in 1:638){
  if (data$home_phone_type[i] == "Home" | data$other_phone_type[i] == "Home"){
    data$landline_provided[i] = 1
  }else{
    data$landline_provided[i] = 0
  }
} # Identify whether landline contact is provided

data$landline_provided = as.factor(data$landline_provided)

# Convert binay variables

for (i in 1:638){
  if (data$residence_rent_or_own[i] == TRUE){
    data$residence_rent_or_own[i] = 1
  }else{
    data$residence_rent_or_own[i] = 0
  }
}

data$residence_rent_or_own = as.factor(data$residence_rent_or_own)

for (i in 1:638){
  if (data$bank_account_direct_deposit[i] == TRUE){
    data$bank_account_direct_deposit[i] = 1
  }else{
    data$bank_account_direct_deposit[i] = 0
  }
}

data$bank_account_direct_deposit = as.factor(data$bank_account_direct_deposit)

for (i in 1:638){
  if (data$payment_ach[i] == TRUE){
    data$payment_ach[i] = 1
  }else{
    data$payment_ach[i] = 0
  }
}

data$payment_ach = as.factor(data$payment_ach)

for (i in 1:638){
  if (data$flgGood[i] == "Good"){
    data$flgGood[i] = 1
  }else{
    data$flgGood[i] = 0
  }
}

# Change variable type

data$application_when = as.factor(substr(data$application_when, 1, 7))

data$email_duration = as.factor(data$email_duration)

data$email_domain = as.factor(data$email_domain)

data$residence_duration = as.factor(data$residence_duration)

data$bank_account_duration = as.factor(data$bank_account_duration)

data$payment_frequency = as.factor(data$payment_frequency)

data$home_phone_type = as.factor(data$home_phone_type)

data$how_use_money = as.factor(data$how_use_money)

data$State = as.factor(data$State)

data$other_phone_type = as.factor(data$other_phone_type)

data$weekday = as.factor(data$weekday)

data_clean = subset(data, select = -c(customer_id, birth_date, status, email, bank_routing_number, time, 
                                      payment_ach, address_zip)) # pre-clean columns

data_clean$flgGood = as.numeric(data_clean$flgGood)

summary(data_clean)


#--------Visualize-----------

library(ggplot2)
library(reshape2)

# Plot numeric variables
numeric_cols <- sapply(data_clean, is.numeric)

df.lng <- melt(data_clean[,numeric_cols], id = "flgGood")
head(df.lng)

p = ggplot(aes(x=value, group=flgGood, colour=factor(flgGood)), data=df.lng)

p + geom_density() +
  facet_wrap(~variable, scales="free")

# Plot factor variables
data_clean0 = data_clean
data_clean0$flgGood = as.factor(data_clean0$flgGood)

factor_cols <- sapply(data_clean0, is.factor)

df.lng <- melt(data_clean0[,factor_cols], id = "flgGood")
head(df.lng)

p = ggplot(aes(x=value, group=flgGood, colour=factor(flgGood)), data=df.lng)

p + geom_density() +
  facet_wrap(~variable, scales="free")

#------Split Trainig & Testing data -------

library(caret)

set.seed(12)   # for reproducible results
train = sample(1:nrow(data_clean),0.8*nrow(data_clean)) # Split test & train datasets
data.train = data_clean[train,]
data.test = data_clean[-train,]

prop.table(table(data.train$flgGood))
prop.table(table(data.test$flgGood))


#--------Grow Decision Tree---------

library(rpart)

data.train$flgGood = as.factor(data.train$flgGood)
data.test$flgGood = as.factor(data.test$flgGood)

tree = rpart(flgGood~amount_requested + num_payments + email_domain+  monthly_income_amount+raw_l2c_score+raw_FICO_telecom+
               payment_amount + amount_approved+raw_FICO_retail+raw_FICO_bank_card
             +age+interest+application_when+residence_duration+bank_account_duration+payment_frequency+weekday, data=data.train,
             control=rpart.control(xval=20, minsplit=2, cp=0)) #Grow a full tree

nrow(tree$frame)

plot(tree, 
     uniform=T, 
     branch=0.5, 
     compress=T, 
     main="Full Tree",
     margin=0.0) 

text(tree,  
     splits=T, 
     all=T,
     use.n=T, 
     pretty=F, 
     cex=0.6) #Plot full tree

predict_d = predict(tree, data.test, type = "class")

accuracy_d = confusionMatrix(table(pred = predict_d,
                                   actual = data.test$flgGood))
accuracy_d

plotcp(tree,
       upper="size") #Plot CP
bestcp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp

tree.small = rpart(flgGood ~ amount_requested + num_payments + email_domain + monthly_income_amount+raw_l2c_score+raw_FICO_telecom+
                     payment_amount + amount_approved+raw_FICO_retail+raw_FICO_bank_card
                   +age+interest+application_when+residence_duration+bank_account_duration+payment_frequency+weekday, 
                   data=data.train,
                   control=rpart.control(xval=10, cp=bestcp)) # Prune the tree
nrow(tree.small$frame)
plot(tree.small, uniform=T, branch=0.5, compress=T,
     main="Tree with best cp", margin=0.05)
text(tree.small,  splits=T, all=T, use.n=T, 
     pretty=T, fancy=F, cex=1.2) #Plot tree

predict_s = predict(tree.small, data.test, type = "class")

accuracy_s = confusionMatrix(table(pred=predict_s,
                                   actual = data.test$flgGood))
accuracy_s

#--------Build Random Forest--------

set.seed(12)   # for reproducible results
train = sample(1:nrow(data_clean),0.8*nrow(data_clean)) # Split test & train datasets
data.train = data_clean[train,]
data.test = data_clean[-train,]

library('randomForest')
library(ROCR)

fit = randomForest(flgGood ~ amount_requested + num_payments + email_domain+  monthly_income_amount+raw_l2c_score+raw_FICO_telecom+
                     payment_amount + amount_approved+raw_FICO_retail+raw_FICO_bank_card
                   +age+interest+application_when+residence_duration+bank_account_duration+payment_frequency+weekday, data = data.train) # Build Random Forest

fit

predict = predict(fit, data.test)

prediction = prediction(predict, 
                        data.test$flgGood)

predict.pf = performance(prediction, "tpr", "fpr")

performance = performance(prediction, "auc")@y.values

predict$flgGood = ifelse(predict > performance, 1, 0) # Take optimal cutoff

accuracy = confusionMatrix(table(pred=predict$flgGood,
                                 actual = data.test$flgGood))
accuracy

ntree = which.min(fit$mse)# Select optimize num of trees

fit0 = randomForest(flgGood~amount_requested + num_payments + email_domain+  monthly_income_amount+raw_l2c_score+raw_FICO_telecom+
                      payment_amount + amount_approved+raw_FICO_retail+raw_FICO_bank_card
                    +age+interest+application_when+residence_duration+bank_account_duration+payment_frequency+weekday, data = data.train, ntree = ntree) # Rebuild forest

predict0 = predict(fit0, data.test)

prediction0 = prediction(predict0, 
                        data.test$flgGood)

predict0.pf = performance(prediction0, "tpr", "fpr")

plot(predict.pf, 
     colorize=T,
     lwd=4) 
abline(0,1)
abline(h=1) 
abline(v=0) # Plot ROC curve

performance0 = performance(prediction0, "auc")@y.values

performance0

predict0$flgGood = ifelse(predict0 > performance0, 1, 0) # Take optimal cutoff

accuracy0 = confusionMatrix(table(pred=predict0$flgGood,
                                 actual = data.test$flgGood))
accuracy0

importance = as.data.frame(importance(fit0))

varImpPlot(fit,type=2) # View feature importance


#----------------Correlation Test------------------

cor.test(data_clean$raw_FICO_bank_card, data_clean$raw_FICO_money) # ###

cor.test(data_clean$raw_FICO_bank_card, data_clean$raw_FICO_telecom) # ###

cor.test(data_clean$raw_FICO_bank_card, data_clean$raw_FICO_retail) # ###

cor.test(data_clean$raw_FICO_bank_card, data_clean$raw_l2c_score) # -

cor.test(data_clean$raw_l2c_score, data_clean$raw_FICO_retail) # -

cor.test(data_clean$raw_l2c_score, data_clean$raw_FICO_telecom) # -

cor.test(data_clean$raw_l2c_score, data_clean$age) # -

cor.test(data_clean$raw_l2c_score, data_clean$monthly_income_amount) # -

cor.test(data_clean$age, data_clean$monthly_income_amount) # -

cor.test(data_clean$age, data_clean$monthly_rent_amount) # -

cor.test(data_clean$raw_FICO_bank_card, data_clean$age) # ##

cor.test(data_clean$raw_l2c_score, data_clean$age) # -

cor.test(data_clean$monthly_income_amount, data_clean$monthly_rent_amount) # ###

cor.test(data_clean$amount_gap, data_clean$raw_FICO_bank_card) # ##

cor.test(data_clean$amount_gap, data_clean$raw_l2c_score) # -

cor.test(data_clean$amount_gap, data_clean$duration_gap) # ###

cor.test(data_clean$amount_requested, data_clean$money_left) # #

cor.test(data_clean$payment_amount_approved, data_clean$amount_requested) # ##

cor.test(data_clean$payment_amount_approved, data_clean$money_left) # -

cor.test(data_clean$monthly_income_amount, data_clean$money_left) # ###

Good_Loan = data_clean[data_clean$flgGood == 1,]

summary(Good_Loan)

Bad_Loan = data_clean[data_clean$flgGood == 0,]

summary(Bad_Loan)





