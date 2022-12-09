library(RCurl) 
library(jsonlite)
library(tidyverse)
library(aws.s3)
library(caret)
library(class)
library(olsrr)
library(readxl)
library(e1071)
library(ggplot2)
library(ggthemes)
library(GGally)
library(dplyr)


#Upload AWS Keys
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIA4H4B6L3UX6CUKRHW",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-east-2")
#Get case2 from bucket
case2 <- read.table(textConnection(getURL("https://smuddsproject2.s3.us-east-2.amazonaws.com/CaseStudy2-data.csv"
)), sep=",", header=TRUE)
case2

#Write to file on computer in case R crashes again
write.csv(case2,"C:/Users/rache/Documents/DDS/Unit 14/case2.csv", row.names=FALSE)

#Read in case2 file
case2 <- read.csv("C:/Users/rache/Documents/DDS/Unit 14/case2.csv")

#Read in case2 No attrition file
case2NA <- read.csv("C:/Users/rache/Documents/DDS/Unit 14/CaseStudy2CompSet_No_Attrition.csv")

#Read in case2 No salary file
case2NS <- read_excel("C:/Users/rache/Documents/DDS/Unit 14/CaseStudy2CompSet_No_Salary.xlsx")


#Time to get to work: Find linear regression model 
#And which factors might be influential

case2x <- case2[,-10]
case2x <- case2x[,-22]
case2x <- case2x[,-26]


#change variables to factors and rename the levels to factored values
case2x$BusinessTravel <- as.factor(case2x$BusinessTravel)
case2x$Department <- as.factor(case2x$Department)
case2x$EducationField <- as.factor(case2x$EducationField)
case2x$Gender <- as.factor(case2x$Gender)
case2x$JobRole <- as.factor(case2x$JobRole)
case2x$MaritalStatus <- as.factor(case2x$MaritalStatus)
case2x$OverTime <- as.factor(case2x$OverTime)
case2x$Attrition <- as.factor(case2x$Attrition)


#Gender levels: 1=Male 2=Female
levels(case2x$Gender) <- c(1,2)
levels(case2x$Gender)
#OverTime levels: 1=Yes 2=No
levels(case2x$OverTime) <- c(1,2)
levels(case2x$OverTime)
#MaritalStatus levels: 1=Divorced 2=Married 3=Single
levels(case2x$MaritalStatus) <- c(1,2,3)
levels(case2x$MaritalStatus)
#BusinessTravel levels:1=Non-Travel, 2=Travel Frequently 3=Travel Rarely
levels(case2x$BusinessTravel) <- c(1,2,3)
levels(case2x$BusinessTravel)
#Department levels: 1=Human Resources(HR) 2=Research & Development(R&D) 3= Sales
levels(case2x$Department) <- c(1,2,3)
levels(case2x$Department)
#EducationField levels: 1= Human Resources(HR) 2=Life Sciences 3=Marketing
# 4=Medical 5=Other 6=Technical Degree
levels(case2x$EducationField) <- c(1,2,3,4,5,6)
levels(case2x$EducationField)
#JobRole levels: 1=Healthcare Representative 2=Human Resources 3=Laboratory Tech
# 4=Manager 5=Manufacturing Director 6=Research Director
# 7=Research Scientist 8=Sales Executive 9=Sales Representative
levels(case2x$JobRole) <- c(1,2,3,4,5,6,7,8,9)
levels(case2x$JobRole)
#Attrition levels:
levels(case2x$Attrition) <- c(1,2)
levels(case2x$Attrition)



case2x<-na.omit(case2x)

#Change the values back to numeric to make them all one variable rather than broken
#out by Job1 and Job2

case2x$OverTime<- as.numeric(case2x$OverTime)
case2x$BusinessTravel <- as.numeric(case2x$BusinessTravel)
case2x$Department <- as.numeric(case2x$Department)
case2x$EducationField <- as.numeric(case2x$EducationField)
case2x$Gender <- as.numeric(case2x$Gender)
case2x$JobRole <- as.numeric(case2x$JobRole)
case2x$MaritalStatus <- as.numeric(case2x$MaritalStatus)
case2x$Attrition <- as.integer(case2x$Attrition)



#Look at distribution of Job Role
x= case2%>%ggplot(aes(x=JobRole, fill=JobRole)) +geom_histogram(stat = "count") + ggtitle("Job Role Distribution")+
  theme_solarized()
x + scale_fill_hc()
#look at Attrition
x=case2%>%group_by(JobRole)%>%
  ggplot(aes(x=Attrition, fill=JobRole)) +geom_bar(position= 'dodge') + ggtitle("Attrition by Job Role")+
  theme_solarized()
x + scale_fill_hc()
#percentages
x=case2%>%group_by(JobRole)%>%
  ggplot(aes(x=Attrition, fill=JobRole)) +geom_bar(position= 'fill') + ggtitle("Attrition Percentages by Job Role")+
  theme_solarized()
x + scale_fill_hc()




#pairs of what I think is important
case2%>% select(JobSatisfaction,JobRole,OverTime,MaritalStatus)%>% ggpairs(aes(color=JobRole))




#Based on this look at job role and job satisfaction
x<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=JobSatisfaction,fill=JobRole)) +geom_bar(position ='dodge',color ="black") + ggtitle("Job Satisfaction by Job Role")+
  theme_solarized()  

x + scale_fill_hc()
#percentages
y<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=JobSatisfaction,fill=JobRole)) +geom_bar(position="fill",color ="black") + ggtitle("Job Satisfaction by Job Role")+
  theme_solarized()  

y + scale_fill_hc()


#OverTime
x<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=OverTime,fill=JobRole)) +geom_bar(stat = 'count',color ="black") + ggtitle("OverTime by Job Role" )+
  theme_solarized()  

x + scale_fill_hc()


x<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=OverTime,fill=JobRole)) +geom_bar(position="fill",color ="black") + ggtitle("OverTime by Job Role")+
  theme_solarized()  

x + scale_fill_hc()

#MaritalStatus
x<-case2%>%
  ggplot(aes(x=MaritalStatus)) +geom_bar(stat = 'count',color ="black") + ggtitle("Marital Status by Job Role" )+
  theme_solarized() +facet_grid(rows=vars(JobRole))

x + scale_fill_hc()


x<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=MaritalStatus,fill=JobRole)) +geom_bar(position="fill",color ="black") + ggtitle("Marital Status Percentage by Job Role")+
  theme_solarized()  

x + scale_fill_hc()


#Look at factors that influence Job Role the most
fitJR<- lm(JobRole~.,data=case2x)
summary(fitJR)
#use only the influential from original
fitJR2<- lm(JobRole~Department+JobLevel+MonthlyIncome+Education+OverTime+TrainingTimesLastYear,data=case2x)
summary(fitJR2)


#Monthly Income
case2 %>% group_by(JobRole)%>% ggplot(aes(x=MonthlyIncome,fill=JobRole))+
  geom_histogram(alpha=0.5,position="identity",color="black")+
  ggtitle("Monthly Income by Job Role")+facet_grid(rows=vars(JobRole))

case2%>% select(Department,JobRole,JobLevel,MonthlyIncome)%>% ggpairs(aes(color=JobRole))

case2%>% select(MonthlyIncome,JobRole,Education,TrainingTimesLastYear)%>% ggpairs(aes(color=JobRole))


#Education
x<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=Education,fill=JobRole)) +geom_bar(stat = 'count',color ="black") + ggtitle("Education by Job Role" )+
  theme_solarized()  

x + scale_fill_hc()
#percentage
x<-case2%>%group_by(JobRole)%>%
  ggplot(aes(x=Education,fill=JobRole)) +geom_bar(position ="fill",color ="black") + ggtitle("Education by Job Role" )+
  theme_solarized()  

x + scale_fill_hc()

case2 %>% group_by(JobRole)%>% ggplot(aes(x=Education,fill=JobRole))+
  geom_histogram(alpha=0.5,position="identity",color="black")+
  ggtitle("Education Levels by Job Role")+facet_grid(rows=vars(JobRole))


#look at Training Times
x=case2%>%group_by(JobRole)%>%
  ggplot(aes(x=TrainingTimesLastYear, fill=JobRole)) +geom_bar(position= 'dodge') + ggtitle("Trainings by Job Role")+
  theme_solarized()
x + scale_fill_hc()
#percentages
x=case2%>%group_by(JobRole)%>%
  ggplot(aes(x=TrainingTimesLastYear, fill=JobRole)) +geom_bar(position= 'fill') + ggtitle("Training Percentages by Job Role")+
  theme_solarized()
x + scale_fill_hc()



x=case2%>%group_by(JobRole)%>%
  ggplot(aes(y=OverTime,x=MonthlyIncome, color=JobRole)) +geom_point() +
  theme_solarized()
  x + scale_fill_hc()
##############################
  
###############################
#Salary model prediction
#Check all variables for Salary Prediction
fitSal <- lm(MonthlyIncome~.,data=case2x)
summary(fitSal)
  
#These are the most significant influencers of salary
fitSal2 <- lm(MonthlyIncome~BusinessTravel+Department+DistanceFromHome+JobLevel+JobRole+TotalWorkingYears+YearsWithCurrManager,data=case2x)
summary(fitSal2)
  
  
#JobLevel has the highest correlation to MonthlyIncome with TotalWorkingYears as the next highest.
#RMSE for this model is 1387.30 (which is well below the 3000 threshold).
fitSal3 <- lm(MonthlyIncome~JobLevel+TotalWorkingYears+YearsWithCurrManager,data=case2x)
  
summary(fitSal3)
  
  
  
  
  
predictionSal = predict(fitSal3,case2NS)
predictionSal
predictionSal2<- data.frame(MonthlyIncome = predictionSal)
predSal = data.frame(ID = case2NS[1], MonthlyIncome = predictionSal2)
predSal
  
  
  
write.csv(predSal,"C:/Users/rache/Documents/DDS/Unit 14/Case2PredictionsLiercke_Salary.csv", row.names=FALSE)
  
  
  
  
#################################
  
  
#####################
#Run a LM model for attrition with all variables
fit<- lm(Attrition~.,data=case2x)
summary(fit)

#Use significant variables from the initial LM model
fitA1<- lm(Attrition~JobInvolvement+JobSatisfaction+MaritalStatus+NumCompaniesWorked+
           OverTime+YearsSinceLastPromotion, data=case2x)

summary(fitA1)
#remove number of companies worked and try again
fitA2<- lm(Attrition~JobInvolvement+JobSatisfaction+MaritalStatus+
             OverTime+YearsSinceLastPromotion, data=case2x)
summary(fitA2)
fitA3<- lm(Attrition~JobInvolvement+JobSatisfaction+MaritalStatus+
             OverTime, data=case2x)
summary(fitA3)




#change variables to factors and rename the levels to factored values
case2NA$BusinessTravel <- as.factor(case2NA$BusinessTravel)
case2NA$Department <- as.factor(case2NA$Department)
case2NA$EducationField <- as.factor(case2NA$EducationField)
case2NA$Gender <- as.factor(case2NA$Gender)
case2NA$JobRole <- as.factor(case2NA$JobRole)
case2NA$MaritalStatus <- as.factor(case2NA$MaritalStatus)
case2NA$OverTime <- as.factor(case2NA$OverTime)



#Gender levels: 1=Male 2=Female
levels(case2NA$Gender) <- c(1,2)
levels(case2NA$Gender)
#OverTime levels: 1=Yes 2=No
levels(case2NA$OverTime) <- c(1,2)
levels(case2NA$OverTime)
#MaritalStatus levels: 1=Divorced 2=Married 3=Single
levels(case2NA$MaritalStatus) <- c(1,2,3)
levels(case2x$MaritalStatus)
#BusinessTravel levels:1=Non-Travel, 2=Travel Frequently 3=Travel Rarely
levels(case2NA$BusinessTravel) <- c(1,2,3)
levels(case2NA$BusinessTravel)
#Department levels: 1=Human Resources(HR) 2=Research & Development(R&D) 3= Sales
levels(case2NA$Department) <- c(1,2,3)
levels(case2NA$Department)
#EducationField levels: 1= Human Resources(HR) 2=Life Sciences 3=Marketing
# 4=Medical 5=Other 6=Technical Degree
levels(case2NA$EducationField) <- c(1,2,3,4,5,6)
levels(case2NA$EducationField)
#JobRole levels: 1=Healthcare Representative 2=Human Resources 3=Laboratory Tech
# 4=Manager 5=Manufacturing Director 6=Research Director
# 7=Research Scientist 8=Sales Executive 9=Sales Representative
levels(case2NA$JobRole) <- c(1,2,3,4,5,6,7,8,9)
levels(case2NA$JobRole)






#Change the values back to numeric to make them all one variable rather than broken
#out by Job1 and Job2

case2NA$OverTime<- as.numeric(case2NA$OverTime)
case2NA$BusinessTravel <- as.numeric(case2NA$BusinessTravel)
case2NA$Department <- as.numeric(case2NA$Department)
case2NA$EducationField <- as.numeric(case2NA$EducationField)
case2NA$Gender <- as.numeric(case2NA$Gender)
case2NA$JobRole <- as.numeric(case2NA$JobRole)
case2NA$MaritalStatus <- as.numeric(case2NA$MaritalStatus)





corr <- round(cor(case2x),1)
ggcorrplot(corr,lab = TRUE,lab_size = 1.5)



#Test this model on prediction data set
lmFIT= predict(fitA3, newdata = case2NA)
lmFIT1 = as.data.frame(lmFIT)
summary(fitA3)

fitA3<- lm(Attrition~JobInvolvement+JobSatisfaction+MaritalStatus+
             OverTime, data=case2x)
summary(fitA3)


rmse(case2x[],lmFIT)

case2x <- case2x %>% relocate(Attrition, .before = Age)


#################
mod_summaries <- list()

for(i in 3:ncol(case2x)){
  predictors_i <- colnames(case2x)[3:i]
  mod_summaries[[i - 1]] <- summary(
    lm(Attrition~., case2x[ , c("Attrition", predictors_i)]))
}

mod_summaries

mod_summaries[5]


#Put Attrition next to ID so we can use every variable after it to predict
case2x <- case2x %>% relocate(Attrition, .before = Age)



#Split the dataset into Attrition = Yes and Attrition = No
set.seed(100)
case2yes<- case2x %>% filter(Attrition == 2)
case2no<- case2x %>% filter(Attrition == 1)

#Create a train and test from the Attrition = Yes group
splitPerc = .70
trainIndices = sample(1:dim(case2yes)[1],round(splitPerc*dim(case2yes)[1]))
trainy = case2yes[trainIndices,]
testy = case2yes[-trainIndices,]
trainy
testy

#Split the Attrition = No data into a train and test then use the test set to split into another train and test
splitPerc = .70
trainIndices = sample(1:dim(case2no)[1],round(splitPerc*dim(case2no)[1]))
trainN = case2no[trainIndices,]
testN = case2no[-trainIndices,]
trainN
testN

#Split the testN into another train and test
splitPerc = .70
trainIndices = sample(1:dim(testN)[1],round(splitPerc*dim(testN)[1]))
trainN2 = testN[trainIndices,]
testN2 = testN[-trainIndices,]
trainN2
testN2


#Then combine the Attrition = Yes set with the second train and test set of Attrition = No
train = full_join(trainy,trainN2)
test = full_join(testy,testN2)
#########################################
#########################################
#########################################





################################
#test the variables from the LM model for Salary first
combined = full_join(train,test)
classifications = knn(train[,c(6,15,27)],test[,c(6,15,27)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))

#test this model to see which K is best
set.seed(100)
iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  splitPerc = .70
  trainIndices = sample(1:dim(case2no)[1],round(splitPerc*dim(case2no)[1]))
  trainN = case2no[trainIndices,]
  testN = case2no[-trainIndices,]
  train1 = full_join(trainy,trainN)
  test1 = full_join(testy,testN)
  combined = full_join(train1,test1)
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train = combined[trainIndices,]
  test = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(6,15,27)],test[,c(6,15,27)],train$Attrition, prob = TRUE, k = i)
    table(classifications,test$Attrition)
    CM = confusionMatrix(table(classifications,test$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")


#retest this model with K=i from plot
trainIndices = sample(1:dim(case2x)[1],round(splitPerc * dim(case2x)[1]))
train = case2x[trainIndices,]
test = case2x[-trainIndices,]
classifications = knn(train[,c(6,15,27)],test[,c(6,15,27)], train$Attrition,
                      prob = TRUE, k =10)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))

#This one has Sens = .83 and Spec = .43 (!!) so we will try another model










iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  splitPerc = .70
  set.seed(100)
  case2yes<- case2x %>% filter(Attrition == 2)
  case2no<- case2x %>% filter(Attrition == 1)
  
  #Create a train and test from the Attrition = Yes group
  splitPerc = .70
  trainIndices = sample(1:dim(case2yes)[1],round(splitPerc*dim(case2yes)[1]))
  trainy = case2yes[trainIndices,]
  testy = case2yes[-trainIndices,]
  
  trainIndices = sample(1:dim(case2no)[1],round(splitPerc*dim(case2no)[1]))
  trainN = case2no[trainIndices,]
  testN = case2no[-trainIndices,]

  trainIndices = sample(1:dim(testN)[1],round(splitPerc*dim(testN)[1]))
  trainN2 = testN[trainIndices,]
  testN2 = testN[-trainIndices,]
  

  train = full_join(trainy,trainN2)
  test = full_join(testy,testN2)
  
  
  combined = full_join(train1,test1)
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train = combined[trainIndices,]
  test = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],train$Attrition, prob = TRUE, k = i)
    table(classifications,test$Attrition)
    CM = confusionMatrix(table(classifications,test$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
set.seed(75)
trainIndices = sample(1:dim(case2x)[1],round(splitPerc * dim(case2x)[1]))
train1 = case2x[trainIndices,]
test1 = case2x[-trainIndices,]
classifications = knn(train1[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test1[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)], train1$Attrition,
                      prob = TRUE, k =15)
table(classifications,test1$Attrition)

confusionMatrix(table(classifications,test1$Attrition))














#################################

#Run an LM model for Attrition and see which variables are significant
#Use these to test a knn model for Attrition

fitAtt <- lm(Attrition~.,data=case2x)
summary(fitAtt)

fitAtt2<- lm(Attrition ~ Age+Department+DistanceFromHome+EnvironmentSatisfaction+
               JobInvolvement+JobSatisfaction+MaritalStatus+ NumCompaniesWorked+
               OverTime+ RelationshipSatisfaction+TotalWorkingYears+
               TrainingTimesLastYear+YearsInCurrentRole+
               YearsSinceLastPromotion+ YearsWithCurrManager, data=case2x)

summary(fitAtt2)


fitAtt3<- lm(Attrition ~ 
               JobInvolvement+JobSatisfaction+MaritalStatus+ NumCompaniesWorked+
               OverTime+ TotalWorkingYears, data=case2x)

summary(fitAtt3)


######################################################


#use these to test a knn model
accs = data.frame(accuracy = numeric(60), k = numeric(60))
for(i in 1:60)
{
  classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],train$Attrition, prob = TRUE, k = i)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#Test this model with K=i  from the plot
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))

#This model is at sens = .81 and spec = .5



#To Test we will use a split dataset of our train and test
combined = full_join(train,test)

iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test1[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)], train$Attrition,
                      prob = TRUE, k =13)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))

################################
#Accidentally left a few out, retest with these in the model
accs = data.frame(accuracy = numeric(60), k = numeric(60))
for(i in 1:60)
{
  classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)],train$Attrition, prob = TRUE, k = i)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#Test this model with K=i  from the plot
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))




#This was one a little worse than the first model

iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)],test1[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,28,29,31,32,33)], train$Attrition,
                      prob = TRUE, k =17)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


################################################
#Remove the least influential variables (33,31,28,27,3)

accs = data.frame(accuracy = numeric(60), k = numeric(60))
for(i in 1:60)
{
  classifications = knn(train[,c(6,7,11,14,17,18,21,22,25,29,32)],test[,c(6,7,11,14,17,18,21,22,25,29,32)],train$Attrition, prob = TRUE, k = i)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#Test this model with K=i  from the plot
classifications = knn(train[,c(6,7,11,14,17,18,21,22,25,29,32)],test[,c(6,7,11,14,17,18,21,22,25,29,32)], train$Attrition,
                      prob = TRUE, k =35)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))



#Test this for best average k=i

iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(6,7,11,14,17,18,21,22,25,29,32)],test1[,c(6,7,11,14,17,18,21,22,25,29,32)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(6,7,11,14,17,18,21,22,25,29,32)],test[,c(6,7,11,14,17,18,21,22,25,29,32)], train$Attrition,
                      prob = TRUE, k =19)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


#This is bad



########################################################

#Use the model with .50 and remove a few

accs = data.frame(accuracy = numeric(60), k = numeric(60))
for(i in 1:60)
{
  classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)],train$Attrition, prob = TRUE, k = i)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#Test this model with K=i  from the plot
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)], train$Attrition,
                      prob = TRUE, k =25)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


#Test this with average k value not just one

iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test1[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,31,32,33)], train$Attrition,
                      prob = TRUE, k =13)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


############################################################

#Use last one and slowly remove values



iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(3,6,7,11,14,17,18,21,22,25,27,29,32,33)],test1[,c(3,6,7,11,14,17,18,21,22,25,27,29,32,33)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(3,6,7,11,14,17,18,21,22,25,27,29,32,33)],test[,c(3,6,7,11,14,17,18,21,22,25,27,29,32,33)], train$Attrition,
                      prob = TRUE, k =15)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


#############################################################



iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(60), k = numeric(60))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(3,7,11,14,17,18,21,22,25,29,32,33)],test1[,c(3,7,11,14,17,18,21,22,25,29,32,33)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(3,7,11,14,17,18,21,22,25,29,32,33)],test[,c(3,7,11,14,17,18,21,22,25,29,32,33)], train$Attrition,
                      prob = TRUE, k =11)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


##############################################################
##############################################################

iterations = 250
numks = 30
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(14,17,18,21,22,32)],test1[,c(14,17,18,21,22,32)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[1,c(14,17,18,21,22,32)],test1[,c(14,17,18,21,22,32)], train$Attrition,
                      prob = TRUE, k =19)
table(classifications,test1$Attrition)

confusionMatrix(table(classifications,test1$Attrition))


########################################################################################################################################
#Use NB model to classify the one without Attrition values

# NB Loop for average of many training / test partition
iterations = 500
masterSpec = matrix(nrow = iterations)
masterSens = matrix(nrow = iterations)
splitPerc = .7 #Training / Test split Percentage
for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  
  model = naiveBayes(train1[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)],train1$Attrition,laplace = 1)
  table(predict(model,test1[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)]),test1$Attrition)
  CM = confusionMatrix(table(predict(model,test1[,c(6,7,14,17,18,21,22,27,29,31,32,33)]),test1$Attrition))
  masterSpec[j] = CM$byClass[2]
  masterSens[j] = CM$byClass[1]
}
MeanSpec = colMeans(masterSpec)
MeanSpec
MeanSens = colMeans(masterSens)
MeanSens

model = naiveBayes(train[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)],train$Attrition,laplace = 1)
table(predict(model,test[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)]),test$Attrition)
confusionMatrix(table(predict(model,test[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)]),test$Attrition))
CM = confusionMatrix(table(predict(model,test[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)]),test$Attrition))

#test this on overall ds
splitPerc = .7
trainIndices = sample(1:dim(case2x)[1],round(splitPerc * dim(case2x)[1]))
trainOverall = case2x[trainIndices,]
testOverall = case2x[-trainIndices,]
model = naiveBayes(trainOverall[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)],trainOverall$Attrition,laplace = 1)
table(predict(model,testOverall[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)]),testOverall$Attrition)
confusionMatrix(table(predict(model,testOverall[,c(6,7,11,14,17,18,21,22,27,29,31,32,33)]),testOverall$Attrition))

#Predict the data-set without Attrition using this model

case2NA2 <- case2NA
case2NA2 <- case2NA[,-9]
case2NA2 <- case2NA2[,-21]
case2NA2 <- case2NA2[,-25]
model = naiveBayes(trainOverall[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)],trainOverall$Attrition,laplace = 1)
case2NA2$Attrition = "Attrition"
case2NA2 <- case2NA2 %>% relocate(Attrition, .before = Age)
predictionAtt = predict(model,case2NA2)
predictionAtt
predictionAtt2<- data.frame(Attrition = predictionAtt)
predAtt = data.frame(ID = case2NA[1], Attrition = predictionAtt2)
levels(predAtt$Attrition) <- c("No", "Yes")
levels(predAtt$Attrition)


write.csv(predAtt,"C:/Users/rache/Documents/DDS/Unit 14/Case2PredictionsLiercke_Attrition.csv", row.names=FALSE)

################################################
################################################



# NB Loop for average of many training / test partition
iterations = 500
masterSpec = matrix(nrow = iterations)
masterSens = matrix(nrow = iterations)
splitPerc = .7 #Training / Test split Percentage
for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  
  model = naiveBayes(train1[,c(3,6,7,11,14,17,18,21,22,29,31,32,33)],train1$Attrition,laplace = 1)
  table(predict(model,test1[,c(3,6,7,11,14,17,18,21,22,29,31,32,33)]),test1$Attrition)
  CM = confusionMatrix(table(predict(model,test1[,c(3,6,7,11,14,17,18,21,22,29,31,32,33)]),test1$Attrition))
  masterSpec[j] = CM$byClass[2]
  masterSens[j] = CM$byClass[1]
}
MeanSpec = colMeans(masterSpec)
MeanSpec
MeanSens = colMeans(masterSens)
MeanSens

model = naiveBayes(train1[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)],train1$Attrition,laplace = 1)
table(predict(model,test1[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)]),test1$Attrition)
confusionMatrix(table(predict(model,test1[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)]),test1$Attrition))
CM = confusionMatrix(table(predict(model,test1[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)]),test1$Attrition))

#test this on overall ds
set.seed(75)
splitPerc = .7
trainIndices = sample(1:dim(case2x)[1],round(splitPerc * dim(case2x)[1]))
trainOverall = case2x[trainIndices,]
testOverall = case2x[-trainIndices,]
model = naiveBayes(trainOverall[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)],trainOverall$Attrition,laplace = 1)
table(predict(model,testOverall[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)]),testOverall$Attrition)
confusionMatrix(table(predict(model,testOverall[,c(3,6,7,11,14,17,18,21,22,27,29,31,32,33)]),testOverall$Attrition))

#Predict the data-set without Attrition using this model

predictionAtt = predict(model,case2NA)
predictionAtt
predictionAtt2<- data.frame(Attrition = predictionAtt)
predAtt = data.frame(ID = case2NA[1], Attrition = predictionAtt2)
levels(predAtt$Attrition) <- c("No", "Yes")
levels(predAtt$Attrition)


###############################################
predictionAtt = predict(model,case2NA)
predictionAtt
predictionAtt2<- data.frame(Attrition = predictionAtt)
predAtt = data.frame(ID = case2NA[1], Attrition = predictionAtt2)
levels(predAtt$Attrition) <- c("No", "Yes")
levels(predAtt$Attrition)


write.csv(predAtt,"C:/Users/rache/Documents/DDS/Unit 14/Case2PredictionsLiercke_Attrition.csv", row.names=FALSE)


















#################################################################################
iterations = 250
numks = 30
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(combined)[1],round(splitPerc * dim(combined)[1]))
  train1 = combined[trainIndices,]
  test1 = combined[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train1[,c(7,14,17,18,21,22,27,32)],test1[,c(7,14,17,18,21,22,27,32)],train1$Attrition, prob = TRUE, k = i)
    table(classifications,test1$Attrition)
    CM = confusionMatrix(table(classifications,test1$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

#test this with the average K=i value
classifications = knn(train[,c(7,14,17,18,21,22,27,32)],test[,c(7,14,17,18,21,22,27,32)], train$Attrition,
                      prob = TRUE, k =15)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))









##################################
classifications = knn(train[,c(6,15,27)],test[,c(6,15,27)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))

#test this model to see which K is best
accs = data.frame(accuracy = numeric(60), k = numeric(60))
for(i in 1:60)
{
  classifications = knn(train[,c(6,15,27)],test[,c(6,15,27)],train$Attrition, prob = TRUE, k = i)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#retest this model with K=i from plot
classifications = knn(train[,c(6,15,27)],test[,c(6,15,27)], train$Attrition,
                      prob = TRUE, k =45)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


















####################################
accs = data.frame(accuracy = numeric(30), k = numeric(30), sens = numeric(30), spec = numeric(30))
for(i in 1:30)
{
  classifications = knn(train[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)],test[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)],train$Attrition, prob = TRUE, k = i)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
  accs$sens[i] = CM$byClass[1]
  accs$spec[i] = CM$byClass[2]
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")



#loop for many ks and the average of many training / test 
iterations = 250
numks = 30
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(case2x)[1],round(splitPerc * dim(case2x)[1]))
  train = case2x[trainIndices,]
  test = case2x[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)],test[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)],train$Attrition, prob = TRUE, k = i)
    table(classifications,test$Attrition)
    CM = confusionMatrix(table(classifications,test$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

classifications = knn(train[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)],test[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)], train$Attrition,
                      prob = TRUE, k =8)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))





#test the LM model
classifications = knn(train[,c(14,17,18,21,22,32)],test[,c(14,17,18,21,22,32)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))




accs = data.frame(spec = numeric(33),sens = numeric(33), i = numeric(33))
for(i in 1:33)
{
  classifications = knn(train[,c(22,i)],test[,c(22,i)],train$Attrition, prob = TRUE, k = 5)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$spec[i] = CM$byClass[2]
  accs$sens[i] = CM$byClass[1]
  accs$i[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")
#this says 2,9 is best, let's check
classifications = knn(train[,c(22,16)],test[,c(22,16)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))



accs = data.frame(spec = numeric(33),sens = numeric(33), i = numeric(33))
for(i in 1:33)
{
  classifications = knn(train[,c(22,16,i)],test[,c(22,16,i)],train$Attrition, prob = TRUE, k = 5)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$spec[i] = CM$byClass[2]
  accs$sens[i] = CM$byClass[1]
  accs$i[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")
#this says 2,9 is best, let's check
classifications = knn(train[,c(22,16,18)],test[,c(22,16,18)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))


accs = data.frame(spec = numeric(33),sens = numeric(33), i = numeric(33))
for(i in 1:33)
{
  classifications = knn(train[,c(22,16,18,i)],test[,c(22,16,18,i)],train$Attrition, prob = TRUE, k = 5)
  table(test$Attrition,classifications)
  CM = confusionMatrix(table(test$Attrition,classifications))
  accs$spec[i] = CM$byClass[2]
  accs$sens[i] = CM$byClass[1]
  accs$i[i] = i
}

#this says 2,9 is best, let's check
classifications = knn(train[,c(22,16,18,15)],test[,c(22,16,18,15)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))




##############################################################
set.seed(100)
iterations = 250
numks = 60
masterAcc = matrix(nrow = iterations, ncol = numks)
splitPerc = .7
for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(case2x)[1],round(splitPerc * dim(case2x)[1]))
  train = case2x[trainIndices,]
  test = case2x[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(22,32,21,15,14,11,6,7,25,29,31)],test[,c(22,32,21,15,14,11,6,7,25,29,31)],train$Attrition, prob = TRUE, k = i)
    table(classifications,test$Attrition)
    CM = confusionMatrix(table(classifications,test$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

classifications = knn(train[,c(22,32,21,15,14,11,6,7,25,29,31)],test[,c(22,32,21,15,14,11,6,7,25,29,31)], train$Attrition,
                      prob = TRUE, k = 12)
table(classifications,test$Attrition)

confusionMatrix(table(classifications,test$Attrition))



classificationIn = knn.cv(case2x[,c(22,32,21,15,14,11,6,7,25,29,31)], case2x$Attrition,
                          prob = TRUE, k = 11 )
confusionMatrix(classificationIn, as.factor(case2x$Attrition))




#############################################
#all variables with correlation to attrition
classifications = knn(train[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)],test[,c(3,6,7,11,14,15,16,17,18,19,21,22,26,27,28,29,30,31,33)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))

#########################################

#highest correlation variables
classifications = knn(train[,c(14,15,18,19,22,27,31)],test[,c(14,15,18,19,22,27,31)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))
#######################################
classifications = knn(train[,c(22,18,31)],test[,c(22,18,31)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))
##############################################

classifications = knn(train[,c(22,15,16,18,26,28)],test[,c(22,15,16,18,26,28)], train$Attrition,
                      prob = TRUE, k =5)
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))
