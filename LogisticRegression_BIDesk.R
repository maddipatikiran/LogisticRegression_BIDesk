
#Reading Data set
Diabetes<-read.csv("diabetes.csv", na.strings = "")

#checking null values
sapply(Diabetes, function(df)
{
  sum(is.na(df)==T)/length(df)
})
#plot null values
library(Amelia)
missmap(Diabetes)

#Imputing NA values
Diabetes$Age[is.na(Diabetes$Age)]<- mean(Diabetes$Age,na.rm = T)
sum(is.na(Diabetes$Age))

#target varible data conversion
Diabetes$Outcome <- as.factor( Diabetes$Outcome)

#Data partition 
library(caret)
set.seed(100)
Dpartition<-createDataPartition(Diabetes$Outcome,p=0.7,list = FALSE)
Dtrain<-Diabetes[Dpartition,]
Dtest<-Diabetes[-Dpartition,]

#build model 
model <-glm(Outcome ~.,Dtrain[,-c(4,5,8)], family = "binomial") 
summary(model)
#pred
pred<- predict(model,Dtest[,-c(4,5,8,9)], type = "response")
pred
fpred<- ifelse(pred > 0.4, 1, 0)
fpred<- factor(fpred, levels=c(0, 1))

#confusion matrix
confusionMatrix(fpred,Dtest$Outcome)

  #ROC-AUC curve
library(ROCR)
ROCRPred <- prediction(fpred, Dtest$Outcome)
ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")
plot(ROCRPerf)
plot(ROCRPerf, colorize = TRUE)
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
abline(a=0, b=1)
auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
auc
auc <- round(auc, 4)
legend (.5,.4,auc, title = "AUC", cex =1)


