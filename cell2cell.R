setwd("C:\\Users\\eyon0\\OneDrive\\桌面\\BUMK758R")
library(foreign)
data<-read.spss("Cell2Cell_SPSS_Data.sav",to.data.frame=TRUE)


data[,c(1,3,29)]<-abs(data[,c(1,3,29)])
# Impute NA with medians
index<-apply(data[,c(1:8,27:29,31,32)],2,median,na.rm=T)
data[is.na(data[,1]),1]<-index[1]
data[is.na(data[,2]),2]<-index[2]
data[is.na(data[,3]),3]<-index[3]
data[is.na(data[,4]),4]<-index[4]
data[is.na(data[,5]),5]<-index[5]
data[is.na(data[,6]),6]<-index[6]
data[is.na(data[,7]),7]<-index[7]
data[is.na(data[,8]),8]<-index[8]
data[is.na(data[,27]),27]<-index[9]
data[is.na(data[,28]),28]<-index[10]
data[is.na(data[,29]),29]<-index[11]
data[is.na(data[,31]),31]<-index[12]
data[is.na(data[,32]),32]<-index[13]

#aggregate variable "CSA", decrease the levels in that variable. 
library(stringr)
data$CSA<-str_sub(data$CSA,1,3)
#factor transformation(71 income TBD)
data[,c(22,26,33:64,67,68,70,72,74,76)]<-lapply(data[,c(22,26,33:64,67,68,70,72,74,76)],factor)

#split validation & train, set dependent variable is CHURN 
train<-data[data$CALIBRAT==1,]
validation<-data[data$CALIBRAT==0,]
train<-train[,-c(77,78)]
validation<-validation[,-c(77,78)]
validation$CSA[validation$CSA=="HOP"]<-"LAX"
validation$CSA[validation$CSA=="SLU"]<-"LAX"


set.seed(123)
#logistic regression
train$CHURN=as.factor(train$CHURN)
validation$CHURN=as.factor(validation$CHURN)
LR = glm(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + 
           CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + 
           THREEWAY + OUTCALLS + INCALLS + PEAKVCE + MONTHS + UNIQSUBS + 
           ACTVSUBS + CSA + PHONES + EQPDAYS + AGE1 + CHILDREN + CREDITAA + 
           CREDITB + CREDITC + CREDITDE + PRIZMRUR + PRIZMTWN + REFURB + 
           WEBCAP + TRUCK + OCCHMKR + MARRYUN + MAILRES + RETACCPT + 
           NEWCELLY + INCMISS + INCOME + CREDITAD + SETPRCM + SETPRC + 
           RETCALL, family = binomial(link = "logit"), data = train)
vif(LR)
summary(LR)
#get auc and auc plot
PredictModel<-predict(LR, newdata=validation, type="response")
pred_LR=as.data.frame(PredictModel)
library(ROCR)
library(gplots)
pred <- prediction(PredictModel,as.numeric(validation$CHURN)-1)
performance(pred,'auc')@y.values
perf <- performance(pred,'tpr','fpr')
plot(perf)
abline(a=0,b=1)

#get lift and lift plot
probability<- cbind(as.numeric(validation$CHURN)-1,PredictModel)
list<-probability[order(probability[,2],decreasing=TRUE),]
Top10Pct=list[1:(0.1*nrow(list)),]
Top10Pct
LRLiftTest=(mean(Top10Pct[,1])/0.02)

perf_lift<- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf_lift, main="lift curve", colorize=T)
plotLift(PredictModel,validation$CHURN)

#Gradient Boosting Model
train$CHURN=as.numeric(train$CHURN)-1
validation$CHURN=as.numeric(validation$CHURN)-1
library(gbm)
GM= gbm(CHURN~.,distribution="bernoulli", data=train, n.trees=494)
GMPredict=predict.gbm( object= GM,newdata=validation, n.trees=494, type="response")
GMPredict=as.data.frame(GMPredict)

#get auc and auc plot
class(GMPredict)
pred_GM <- prediction(GMPredict,validation$CHURN)
performance(pred_GM,'auc')@y.values
perf_GM <- performance(pred_GM,'tpr','fpr')
plot(perf_GM)
abline(a=0,b=1)


#get lift and lift plot
probability_GB<- cbind(validation$CHURN,GMPredict)
list_GB<-probability_GB[order(probability_GB[,2],decreasing=TRUE),]
Top10_GB=list_GB[1:(1*nrow(list_GB)),]
Top10_GB
GBLiftTest=(mean(Top10_GB[,1])/0.02)
GBLiftTest

#perf_lift_GB<- performance(pred_GM, measure = "lift", x.measure = "rpp")
#plot(perf_lift_GB, main="lift curve", colorize=T)
plotLift(GMPredict,validation$CHURN)

#Random Forest
library(randomForest)
train$CHURN <- as.factor(train$CHURN)
validation$CHURN=as.factor(validation$CHURN)
train$CSA<-as.numeric(train$CSA)-1
validation$CSA<-as.numeric(validation$CSA)-1
RF<-randomForest(CHURN~., data= train, importance=TRUE, na.action=na.omit,
                 ntree=250)
set.seed(123)
varImpPlot(RF)

predrf<- predict(RF, validation,type = "prob")
predrf<-as.data.frame(predrf)
predrf#predicted class for test#

#get auc and auc plot
roc_obj <- roc(validation$CHURN,predrf[ ,2])

auc(roc_obj)

#calculate lift and lift plot
PRF<-cbind(validation$CHURN,predrf)
PRF=PRF[ ,-2]
ListRF<-PRF[order(PRF[,2], decreasing=TRUE),]#order from high to low
Top10PctCustRF<-ListRF[1:(0.1*nrow(ListRF)),]
RFLift<-(mean(as.numeric(Top10PctCustRF[,1])-1)/0.02)
RFLift
library(lift)
plotLift(predrf[ ,2],validation$CHURN)



