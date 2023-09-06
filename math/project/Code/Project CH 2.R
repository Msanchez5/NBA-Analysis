#########################
#Beginning of Chapter 2
#########################
#In chapter 2, we didn't have the data readily available. Thus we had extrapolate data and combine them to to then analyze
#In this chapter we were able to retrieve data from 97 to 2022
#We will focus on creating a model to predict win shares per 48 minutes
#The beginning part is mostly EDA, and chapter 2 in the report begins on line 453
###############################
#Combining different data sets
###############################

library(readxl) 
setwd('~/math/data/nba/ERA1')

files <- (Sys.glob("*.csv"))
mm=length(files)-1

Value1=rep(0,mm)
Value3=rep(0,mm)
Value4=rep(0,mm)
Value5=rep(0,mm)
Value6=rep(0,mm)
ScoreD=rep(0,mm)
for(i in 2:8)
{#PAASPort1<- read.csv("Andre24_02.csv", as.is=T, skip = 5, header = TRUE)
  PAASPort1<- read.csv(files[[i]], as.is=T, skip = 1, header = FALSE)
  #head<- read.csv("Andre24_02.csv",  header = F, nrows = 5, as.is = T)
  #head(PAASPort1)
  PAASPort2=PAASPort1[,2:8]
  Value1[i]=Module1Value(PAASPort2 )
  #Value1all[i]=Value1
  Value3[i]=Module3Value(PAASPort2 )
  #Value3
  Value4[i]=Module4Value(PAASPort2 )
  #Value4
  Value5[i]=Module5Value(PAASPort2 )
  #Value5
  
  Value6[i]=Module5Value(PAASPort2 )
  #Value6
  
  Module.Weight=c(3,1,1,1,5)
  ScoreD[i]=sum(c(3*Value1[i],Value3[i], Value4[i],Value5[i], 5*Value6[i])/sum(Module.Weight))
  
}
library(openxlsx)

path <- "~/math/data/nba/ERA3"
merge_file_name <- "~/math/data/nba/ERA3//era3.xlsx"

filenames_list <- list.files(path= path, full.names=TRUE)

All <- lapply(filenames_list,function(filename){
  print(paste("Merging",filename,sep = " "))
  read.xlsx(filename)
})

df <- do.call(rbind.data.frame, All)
write.xlsx(df,merge_file_name)

#Era 1: 97-2004

#Era 2: 05-2013

#Era 3: 14-22

library(openxlsx)
nbaera1 = read.xlsx("~/math/data/nba/ERA1//era1.xlsx")
attach(nbaera1)
nbaera2 = read.xlsx("~/math/data/nba/ERA2//era2.xlsx")
attach(nbaera2)
nbaera3 = read.xlsx("~/math/data/nba/ERA3//era3.xlsx")
attach(nbaera3)

names(nbaera1)
hist(nbaera1$`WS/48`)
plot(density((nbaera1$`WS/48`)))

boxplot(log(nbaera1$`WS/48`))

mean(nbaera1$`WS/48`)

########################
#EDA: Linear regression
########################

#ERA 1
lm.fit1= lm(nbaera1$`WS/48`~ nbaera1$PER)
summary(lm.fit1)
plot(lm.fit1)
cor(nbaera1$PER,nbaera1$BPM)

#ERA 2
lm.fit2= lm(nbaera2$`WS/48`~ nbaera2$Age + nbaera2$`eFG%`+ nbaera2$BPM + nbaera2$`TS%`)
summary(lm.fit2)
plot(lm.fit2)

#ERA 3
lm.fit3= lm(nbaera3$`WS/48`~ nbaera3$Age + nbaera3$`eFG%`+ nbaera3$BPM + nbaera3$`TS%`)
summary(lm.fit3)
plot(lm.fit3)

#Lakers

#Era 1
nbae1.lal=nbaera1[which((nbaera1$Tm=="LAL")),]

lm.fit4= lm(nbae1.lal$`WS/48`~nbae1.lal$Age+nbae1.lal$`eFG%`+nbae1.lal$BPM)
summary(lm.fit4)
plot(lm.fit4)

#Era 2
nbae2.lal=nbaera2[which((nbaera2$Tm=="LAL")),]

lm.fit5= lm(nbae2.lal$`WS/48`~ nbae2.lal$Age + nbae2.lal$`eFG%`+ nbae2.lal$BPM)
summary(lm.fit5)
plot(lm.fit5)

#Era 3
nbae3.lal=nbaera3[which((nbaera3$Tm=="LAL")),]

lm.fit6= lm(nbae3.lal$`WS/48`~ nbae3.lal$Age + nbae3.lal$`eFG%`+ nbae3.lal$BPM)
summary(lm.fit6)
plot(lm.fit6)

###############################
#Next Steps
###############################

#Investigate outliers, or remove them
#try quantile regression
#Make a normal y, then use aic step to find best predictors
#remove or transform y variable
#linear without outliers and quantile with outliers
#Random forest

#LOG
lm.fit8= lm(log(nbaera1$`WS/48`)~ log(nbaera1$Age + nbaera1$`eFG%`+ nbaera1$BPM + nbaera1$`TS%`))



#Eliminating outliers
hist(nbaera1$`WS/48`,labels = TRUE)
nbaera1.n=nbaera1[which((nbaera1$`WS/48`>-0.1 & nbaera1$`WS/48`<0.3)),]
hist(nbaera1.n$`WS/48`,labels = TRUE)

lm.fit1= lm(nbaera1.n$`WS/48`~ nbaera1.n$Age + nbaera1.n$`eFG%`+ nbaera1.n$BPM + nbaera1.n$`TS%`)
summary(lm.fit1)
plot(lm.fit1)

#################################
#Quantile regression
#################################

library(quantreg)

rqfit25 <- rq(nbaera1$`WS/48`~ nbaera1$Age + nbaera1$`eFG%`+ nbaera1$BPM + nbaera1$`TS%`, tau = 0.25)
rqfit75 <- rq(nbaera1$`WS/48`~ nbaera1$Age + nbaera1$`eFG%`+ nbaera1$BPM + nbaera1$`TS%`, tau = 0.75)
anova(rqfit25,rqfit75)

quantall<- rq(nbaera1$`WS/48`~  nbaera1$BPM + nbaera1$PER, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
summary(rq(nbaera1$`WS/48`~ nbaera1$BPM + nbaera1$PER, tau = 0.7))
#check fit

############################################################################
#AIC
lm.fit1= lm(nbaera1.o$`WS/48`~ nbaera1.o$Age + nbaera1.o$`eFG%`+ nbaera1.o$BPM + nbaera1.o$`TS%`)
lm.fit2= lm(nbaera1.o$`WS/48`~ nbaera1.o$Age + nbaera1.o$`eFG%`+ nbaera1.o$BPM + nbaera1.o$`TS%`+nbaera1.o$`3PAr`+nbaera1.o$`3P%`+nbaera1.o$PER+nbaera1.o$`TOV%`+nbaera1.o$FTr)
lm.fit3= lm(nbaera1.o$`WS/48`~ nbaera1.o$Age + nbaera1.o$BPM + nbaera1.o$`TS%`+nbaera1.o$`3PAr`+nbaera1.o$PER+nbaera1.o$FTr)
lm.fit4= lm(nbaera1.o$`WS/48`~.,data = nbaera1.o)
update(lm.fit4,-c(BPM,OWS))
summary(lm.fit4)
nbaera1.o=nbaera1.f[[, -c(1:3)]]
nbaera1.o = subset(nbaera1.n, select = -c(Rk,Player,Pos,Tm) )
summary(nbaera1.o$`2PA`)

AIC(lm.fit1)
AIC(lm.fit4)
library(MASS)
library(car)
stepAIC(lm.fit3, direction="both")
stepAIC(lm.fit4, direction="both")
library(AICcmodavg)
models <- list(lm.fit1, lm.fit2,lm.fit3,lm.fit4)
aictab(cand.set = models)
bictab(cand.set = models)

#Best model is fit 4, without BPM and OWS


########################################################################
#Random forest (with outliers and non-standardization)

nbaera1.ro = subset(nbaera1, select = -c(Rk,Player,Pos,Tm) )
colnames(nbaera1.ro) <- gsub("%", "pr", colnames(nbaera1.ro))
colnames(nbaera1.ro) <- gsub("2", "two", colnames(nbaera1.ro))
colnames(nbaera1.ro) <- gsub("3", "thr", colnames(nbaera1.ro))
colnames(nbaera1.ro) <- gsub("/48", "pfe", colnames(nbaera1.ro))
nbaera1.ro = subset(nbaera1.ro, select = -c(WS) )

library(randomForest)

rf.3=randomForest(nbaera1.ro$WSpfe ~ . ,data = nbaera1.ro, importance=TRUE)
importance(rf.3)
varImpPlot(rf.3)

#Most important are BPM, PER, DWS

dim(nbaera1.ro)

#Random forest (with outliers and standardization)
nbaera1_r <- as.data.frame(scale(nbaera1.r[1:46]))
nbaera1.r = subset(nbaera1, select = -c(Rk,Player,Pos,Tm) )
colnames(nbaera1_r) <- gsub("%", "pr", colnames(nbaera1_r))
colnames(nbaera1_r) <- gsub("2", "two", colnames(nbaera1_r))
colnames(nbaera1_r) <- gsub("3", "thr", colnames(nbaera1_r))
colnames(nbaera1_r) <- gsub("/48", "pfe", colnames(nbaera1_r))
nbaera1_r = subset(nbaera1_r, select = -c(WS) )

library(randomForest)

rf.1=randomForest(nbaera1_r$WSpfe ~ . ,data = nbaera1_r, importance=TRUE)
importance(rf.1)
varImpPlot(rf.1)

#Most important variables BPM, PER, DWS, ORB%, USG% 

names(nbaera1.r)
summary(nbaera1.r$`FG%`)

#################################################
#Normalizing the variables
#changing variable names


#################################################
#Normalizing data

nbaera1_std <- as.data.frame(scale(nbaera1.o[1:46]))
hist(nbaera1_std$`2PA`)
hist(nbaera1.o$`2PA`)


#######################
#Linear/ridge regression

lm.fit1= lm(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`)
plot(lm.fit1)
lm.fit2= lm(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`+nbaera1_std$`3PAr`+nbaera1_std$`3P%`+nbaera1_std$PER+nbaera1_std$`TOV%`+nbaera1_std$FTr)
plot(lm.fit1)
lm.fit3= lm(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$BPM + nbaera1_std$`TS%`+nbaera1_std$`3PAr`+nbaera1_std$PER+nbaera1_std$FTr)
plot(lm.fit1)
lm.fit4= lm(nbaera1_std$`WS/48`~.,data = nbaera1_std)
plot(lm.fit1)

library(glmnet)
x <- data.matrix(nbaera1_std[, c(nbaera1_std$Age, nbaera1_std$`eFG%`, nbaera1_std$BPM, nbaera1_std$`TS%`)])
model <- glmnet( x , nbaera1_std$`WS/48`, alpha = 0)

#########################################################
#Random forest (without outliers and standardization)
########################################################

nbaera1_rst <- subset(nbaera1_std, select = -c(WS) )

colnames(nbaera1_rst) <- gsub("%", "pr", colnames(nbaera1_rst))
colnames(nbaera1_rst) <- gsub("2", "two", colnames(nbaera1_rst))
colnames(nbaera1_rst) <- gsub("3", "thr", colnames(nbaera1_rst))
colnames(nbaera1_rst) <- gsub("/48", "pfe", colnames(nbaera1_rst))

library(randomForest)

rf.2=randomForest(nbaera1_rst$WSpfe ~ . ,data = nbaera1_rst, importance=TRUE)
importance(rf.2)
varImpPlot(rf.2)

#Most important variables OWS, DBPM, BPM, USG%, ORB%, TOV%, TRB%


######################
#Step aic

lm.fit1= lm(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`)
lm.fit2= lm(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`+nbaera1_std$`3PAr`+nbaera1_std$`3P%`+nbaera1_std$PER+nbaera1_std$`TOV%`+nbaera1_std$FTr)
lm.fit3= lm(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$BPM + nbaera1_std$`TS%`+nbaera1_std$`3PAr`+nbaera1_std$PER+nbaera1_std$FTr)
lm.fit4= lm(nbaera1_std$`WS/48`~.,data = nbaera1_std)

AIC(lm.fit1)
AIC(lm.fit4)
library(MASS)
library(car)
stepAIC(lm.fit3, direction="both")
stepAIC(lm.fit4, direction="both")
library(AICcmodavg)
models <- list(lm.fit1, lm.fit2,lm.fit3,lm.fit4)
aictab(cand.set = models)
bictab(cand.set = models)

#Fit4 most important 3PA, FT%, DWS, AGE, G, FTA, 

#######################
#Quantile regression
library(quantreg)

nbaera1_std <- as.data.frame(scale(nbaera1.o[1:46]))

rqfit25 <- rq(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`, tau = 0.25)
rqfit5 <- rq(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`, tau = 0.5)
rqfit75 <- rq(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`, tau = 0.75)
anova(rqfit25,rqfit5,rqfit75, joint = FALSE)

quantall<- rq(nbaera1_std$`WS/48`~ nbaera1_std$Age + nbaera1_std$`eFG%`+ nbaera1_std$BPM + nbaera1_std$`TS%`, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

#From RF without outliers and standard
quantall<- rq(nbaera1_std$`WS/48`~ OWS+ DBPM+ `USG%`+ `ORB%`+ `TRB%`,data = nbaera1_std, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

summary(rq(nbaera1_std$`WS/48`~ OWS+ DBPM+ `USG%`+ `ORB%`+ `TRB%`,data = nbaera1_std, tau = 0.9))


#Quantile, can we have correlated predictors
#Run predictors separtley
#try lasso
#try random forest for other eras
########################################################################
#ERA 1
#Random Forest

#Random forest (with outliers and non-standardization)

nbaera1.ro = subset(nbaera1, select = -c(Rk,Player,Pos,Tm) )
colnames(nbaera1.ro) <- gsub("%", "pr", colnames(nbaera1.ro))
colnames(nbaera1.ro) <- gsub("2", "two", colnames(nbaera1.ro))
colnames(nbaera1.ro) <- gsub("3", "thr", colnames(nbaera1.ro))
colnames(nbaera1.ro) <- gsub("/48", "pfe", colnames(nbaera1.ro))
nbaera1.ro = subset(nbaera1.ro, select = -c(WS) )

library(randomForest)

rf.3=randomForest(nbaera1.ro$WSpfe ~ . ,data = nbaera1.ro, importance=TRUE)
importance(rf.3)
varImpPlot(rf.3)

#Most important are 
#BIG: BPM, PER 
#Alot less: GS, ORB%, DWS

#####################################
#Quantile regression

library(quantreg)

quantall<- rq(nbaera1.ro$WSpfe~nbaera1.ro$BPM + nbaera1.ro$PER, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
rq(nbaera1.ro$`WSpfe`~nbaera1.ro$BPM + nbaera1.ro$PER, tau = 0.3)
summary(rq(nbaera1.ro$WSpfe~nbaera1.ro$BPM + nbaera1.ro$PER, tau = 0.3))

#######################
#Lasso

y <- nbaera1.ro$WSpfe
nbaera1.L = subset(nbaera1.ro, select = -c(WSpfe) )
x = data.matrix(nbaera1.L)
#x <- data.matrix(nbaera1.ro[, c('BPM', 'PER')])

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#cor(x), pick predictor variable, and eliminate all correlated variables




###############################################################################################



#Quantile Regression Forests

qf<-quantregForest::quantregForest(x,y, data=nbaera1_rf,subset=train, importance=TRUE)
varImpPlot(qf)
yhat = predict(qf,newdata=nbaera1_r[-train,])
sqrt(mean((yhat-y.test)^2))

#RMSE: 0.449
#
#Quantile regression

library(quantreg)
#BPM
quantall<- rq(WSpfe~BPM+DWS+USGpr+OWS+TSpr, data=nbaera1_r, subset= train, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
q<-rq(WSpfe~BPM+DWS+USGpr+OWS+TSpr, data=nbaera1_r, subset= train, tau = 0.7)
summary(q)
AIC.rq(q)

#AIC: 1813.25

yhat = predict.rq(q,newdata=nbaera1_r[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#QUANT BPM RMSE: 0.548

#PER
quantall<- rq(WSpfe~PER+DWS+USGpr+OWS, data=nbaera1_r, subset= train, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
q<-rq(WSpfe~PER+DWS+USGpr+OWS, data=nbaera1_r, subset= train, tau = 0.7)
summary(q)
AIC.rq(q)

#AIC: 705.9116

yhat = predict.rq(q,newdata=nbaera1_r[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#QUANT PER RMSE: 0.364

#######################
#Chapter 2
#######################

##############################################
#ERA 1
##############################################

##############################################
#Random forest (with outliers and standardization)

nbaera1.r = subset(nbaera1, select = -c(Rk,Player,Pos,Tm) )
nbaera1_r <- as.data.frame(scale(nbaera1.r[1:46]))

colnames(nbaera1_r) <- gsub("%", "pr", colnames(nbaera1_r))
colnames(nbaera1_r) <- gsub("2", "two", colnames(nbaera1_r))
colnames(nbaera1_r) <- gsub("3", "thr", colnames(nbaera1_r))
colnames(nbaera1_r) <- gsub("/48", "pfe", colnames(nbaera1_r))
nbaera1_r = subset(nbaera1_r, select = -c(WS) )

library(randomForest)

rf.1=randomForest(nbaera1_r$WSpfe ~ . ,data = nbaera1_r, importance=TRUE)
importance(rf.1)
varImpPlot(rf.1)

actual <- nbaera1_r$WSpfe
predicted <- unname(predict(rf.1, nbaera1_r))

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2

#R2= 0.9789


#Most important are 
#BIG: BPM, PER 
#Alittle less: DWS

cor(nbaera1_r$BPM,nbaera1_r$PER)

######################################
#Random forest prediction

y <- nbaera1_r$WSpfe
nbaera1_rf = subset(nbaera1_r, select = -c(WSpfe) )
x = data.matrix(nbaera1_rf)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nbaera1_r,subset=train,importance=TRUE)
varImpPlot(rf)
yhat = predict(rf,newdata=nbaera1_r[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Random forest prediction: 0.446
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-23.1987  -0.3041   0.1159   0.0000   0.4753  11.7148

#####################################

#######################
#Lasso
######################

y <- nbaera1_r$WSpfe
nbaera1_L = subset(nbaera1_r, select = -c(WSpfe) )
x = data.matrix(nbaera1_L)

options(max.print=1936)
cor(x)

#BPM: PER, OBPM 
nbaera1_L = subset(nbaera1_r, select = -c(WSpfe, PER, OBPM) )
x = data.matrix(nbaera1_L)
#PER: TSpr, OBPM, BPM
nbaera1_L = subset(nbaera1_r, select = -c(WSpfe, BPM, OBPM, TSpr) )
x = data.matrix(nbaera1_L)

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
#Min Lambda
best_lambda <- cv_model$lambda.min
#Max lambda to be within 1 standard error of the minimum
best_lambda <- cv_model$lambda.1se
best_lambda

print(cv_model)

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#BPM: R2 = 0.9058

#PER: R2 = 0.9633

###################
#Lasso prediction
###################

#BPM: PER, OBPM
nbaera1_L = subset(nbaera1_r, select = -c(WSpfe, PER, OBPM) )
x = data.matrix(nbaera1_L)
#PER: TSpr, OBPM, BPM
nbaera1_L = subset(nbaera1_r, select = -c(WSpfe, BPM, OBPM, TSpr) )
x = data.matrix(nbaera1_L)

y <- nbaera1_r$WSpfe
nbaera1_pcr = subset(nbaera1_r, select = -c(WSpfe) )
x = data.matrix(nbaera1_pcr)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Based on test
#Prediction for BPM: 0.215
#Prediction for PER: 0.215

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-23.1987  -0.3041   0.1159   0.0000   0.4753  11.7148

#################################
#PCR
#################################

library(pls)
#make this example reproducible
set.seed(1)

#fit PCR model Random Forrest
model <- pcr(nbaera1_r$WSpfe~.,data=nbaera1_r, scale=TRUE, validation="CV")
summary(model)

#Lasso BPM
model <- pcr(nbaera1_r$WSpfe~BPM+FTpr+TSpr+thrPAr+FTr+ORBpr+TRBpr+STLpr+BLKpr+TOVpr+USGpr+OWS+DWS, data=nbaera1_r, scale=FALSE, validation="CV")
summary(model)

predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nbaera1_r$WSpfe
nbaera1_pcr = subset(nbaera1_r, select = -c(WSpfe) )
x = data.matrix(nbaera1_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=14)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nbaera1_r$WSpfe~., data=nbaera1_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=14)
sqrt(mean((pcr.pred-y.test)^2))

#PCR prediction: 0.407
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-23.1987  -0.3041   0.1159   0.0000   0.4753  11.7148

###############################################
#TOP 10 teams

nbaera1
nba.1= nbaera1[which((nbaera1$Tm=="LAL")),]
nba.2= nbaera1[which((nbaera1$Tm=="UTA")),]
nba.3= nbaera1[which((nbaera1$Tm=="SAS")),]
nba.4= nbaera1[which((nbaera1$Tm=="IND")),]
nba.5= nbaera1[which((nbaera1$Tm=="POR")),]
nba.6= nbaera1[which((nbaera1$Tm=="SEA")),]
nba.7= nbaera1[which((nbaera1$Tm=="CHH")),]
nba.8= nbaera1[which((nbaera1$Tm=="SAC")),]
nba.9= nbaera1[which((nbaera1$Tm=="MIA")),]
nba.10= nbaera1[which((nbaera1$Tm=="DET")),]

nba.era1.T <- rbind(nba.1,nba.2,nba.3,nba.4,nba.5,nba.6,nba.7,nba.8,nba.9,nba.10)

nba.era1.T = subset(nba.era1.T, select = -c(Rk,Player,Pos,Tm) )
colnames(nba.era1.T) <- gsub("%", "pr", colnames(nba.era1.T))
colnames(nba.era1.T) <- gsub("2", "two", colnames(nba.era1.T))
colnames(nba.era1.T) <- gsub("3", "thr", colnames(nba.era1.T))
colnames(nba.era1.T) <- gsub("/48", "pfe", colnames(nba.era1.T))
nba.era1.T <- as.data.frame(scale(nba.era1.T))
nba.era1.T = subset(nba.era1.T, select = -c(WS) )

##################################
#Random Forest
##################################

library(randomForest)

rf.2=randomForest(nba.era1.T$WSpfe ~ . ,data = nba.era1.T, importance=TRUE)
importance(rf.2)
varImpPlot(rf.2)

#Important: BPM, PER

#########################
#Random forest prediction
#########################

y <- nba.era1.T$WSpfe
nba.era1.Tsub = subset(nba.era1.T, select = -c(WSpfe) )
x = data.matrix(nba.era1.Tsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nba.era1.T,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nba.era1.T[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.262
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.9755 -0.3066  0.1399  0.0000  0.5331  4.3044 

#####################
#Lasso prediction
#####################

#BPM: OBPM, DBPM, PER
nba.era1.TB = subset(nba.era1.T, select = -c(WSpfe, PER, OBPM, DBPM) )
x = data.matrix(nba.era1.TB)
#PER: TSpr, OBPM, BPM
nba.era1.TP = subset(nba.era1.T, select = -c(WSpfe, BPM, OBPM, TSpr) )
x = data.matrix(nba.era1.TP)

y <- nba.era1.T$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.411
#Prediction for PER: 0.248

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.9755 -0.3066  0.1399  0.0000  0.5331  4.3044 

#############
#PCR
#############

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nba.era1.T$WSpfe~.,data=nba.era1.T, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nba.era1.T$WSpfe
nba.era1.T_pcr = subset(nba.era1.T, select = -c(WSpfe) )
x = data.matrix(nba.era1.T_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=15)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nba.era1.T$WSpfe~., data=nba.era1.T_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=15)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.2955

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.9755 -0.3066  0.1399  0.0000  0.5331  4.3044 

##########################################################################
#Bottom 10 teams

nba.1= nbaera1[which((nbaera1$Tm=="MEM")),]
nba.2= nbaera1[which((nbaera1$Tm=="LAC")),]
nba.3= nbaera1[which((nbaera1$Tm=="GSW")),]
nba.4= nbaera1[which((nbaera1$Tm=="DEN")),]
nba.5= nbaera1[which((nbaera1$Tm=="TOR")),]
nba.6= nbaera1[which((nbaera1$Tm=="WAS")),]
nba.7= nbaera1[which((nbaera1$Tm=="CLE")),]
nba.8= nbaera1[which((nbaera1$Tm=="BOS")),]
nba.9= nbaera1[which((nbaera1$Tm=="NJN")),]
nba.10= nbaera1[which((nbaera1$Tm=="CHI")),]

nba.era1.B <- rbind(nba.1,nba.2,nba.3,nba.4,nba.5,nba.6,nba.7,nba.8,nba.9,nba.10)

nba.era1.B = subset(nba.era1.B, select = -c(Rk,Player,Pos,Tm) )
colnames(nba.era1.B) <- gsub("%", "pr", colnames(nba.era1.B))
colnames(nba.era1.B) <- gsub("2", "two", colnames(nba.era1.B))
colnames(nba.era1.B) <- gsub("3", "thr", colnames(nba.era1.B))
colnames(nba.era1.B) <- gsub("/48", "pfe", colnames(nba.era1.B))
nba.era1.B <- as.data.frame(scale(nba.era1.B))
nba.era1.B = subset(nba.era1.B, select = -c(WS) )

#####################
#Random Forest
#####################

library(randomForest)

rf.3=randomForest(nba.era1.B$WSpfe ~ . ,data = nba.era1.B, importance=TRUE)
importance(rf.3)
varImpPlot(rf.3)

#Most important: DWS, PER, PER
##################################
#Random forest prediction

y <- nba.era1.B$WSpfe
nba.era1.Bsub = subset(nba.era1.B, select = -c(WSpfe) )
x = data.matrix(nba.era1.Bsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nba.era1.B,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nba.era1.B[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.341
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-19.2515  -0.2182   0.1195   0.0000   0.4197   9.9138 

############################
#Lasso prediction
############################

#BPM: PER, OBPM, DBPM
nba.era1.BB = subset(nba.era1.B, select = -c(WSpfe, PER, OBPM, DBPM) )
x = data.matrix(nba.era1.BB)
#PER: OBPM, BPM
nba.era1.BP = subset(nba.era1.B, select = -c(WSpfe, BPM, OBPM) )
x = data.matrix(nba.era1.BP)

y <- nba.era1.B$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.349
#Prediction for PER: 0.1922

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-19.2515  -0.2182   0.1195   0.0000   0.4197   9.9138

######################
#PCR
######################

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nba.era1.B$WSpfe~.,data=nba.era1.B, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nba.era1.B$WSpfe
nba.era1.B_pcr = subset(nba.era1.B, select = -c(WSpfe) )
x = data.matrix(nba.era1.B_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=21)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nba.era1.B$WSpfe~., data=nba.era1.B_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=21)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.2666

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-19.2515  -0.2182   0.1195   0.0000   0.4197   9.9138 

#############################################
#ERA 2
#############################################

#######################################################
#Random forest (with outliers and standardization)
#######################################################

nbaera2.r = subset(nbaera2, select = -c(Rk,Player,Pos,Tm) )
nbaera2_r <- as.data.frame(scale(nbaera2.r))

colnames(nbaera2_r) <- gsub("%", "pr", colnames(nbaera2_r))
colnames(nbaera2_r) <- gsub("2", "two", colnames(nbaera2_r))
colnames(nbaera2_r) <- gsub("3", "thr", colnames(nbaera2_r))
colnames(nbaera2_r) <- gsub("/48", "pfe", colnames(nbaera2_r))
nbaera2_r = subset(nbaera2_r, select = -c(WS) )

library(randomForest)

rf.5=randomForest(nbaera2_r$WSpfe ~ . ,data = nbaera2_r, importance=TRUE)
importance(rf.5)
varImpPlot(rf.5)

#Most important variables 
#BIG: BPM, PER  
#Lesser: USG%, ORB%

############################
#Random forest prediction

y <- nbaera2_r$WSpfe
nbaera2_rsub = subset(nbaera2_r, select = -c(WSpfe) )
x = data.matrix(nbaera2_rsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nbaera2_r,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nbaera2_r[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.334
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-13.9604  -0.3579   0.1005   0.0000   0.5057  12.8343 

########################################
#Lasso
########################################
#Lasso prediction

#BPM and PER are not correlated in era 2

#BPM: OBPM
nbaera2_rB = subset(nbaera2_r, select = -c(WSpfe,OBPM) )
x = data.matrix(nbaera2_rB)
#PER: TSpr
nbaera2_rP = subset(nbaera2_r, select = -c(WSpfe,TSpr) )
x = data.matrix(nbaera2_rP)

y <- nbaera2_r$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.217
#Prediction for PER: 0.2204
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-13.9604  -0.3579   0.1005   0.0000   0.5057  12.8343 

############################
#PCR
############################

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nbaera2_r$WSpfe~.,data=nbaera2_r, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nbaera2_r$WSpfe
nbaera2_r_pcr = subset(nbaera2_r, select = -c(WSpfe) )
x = data.matrix(nbaera2_r_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=34)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nbaera2_r$WSpfe~., data=nbaera2_r_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=34)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.201

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-13.9604  -0.3579   0.1005   0.0000   0.5057  12.8343

####################################################################################
#TOP 10 teams

nba.1= nbaera2[which((nbaera2$Tm=="SAS")),]
nba.2= nbaera2[which((nbaera2$Tm=="DAL")),]
nba.3= nbaera2[which((nbaera2$Tm=="LAL")),]
nba.4= nbaera2[which((nbaera2$Tm=="DEN")),]
nba.5= nbaera2[which((nbaera2$Tm=="MIA")),]
nba.6= nbaera2[which((nbaera2$Tm=="PHO")),]
nba.7= nbaera2[which((nbaera2$Tm=="BOS")),]
nba.8= nbaera2[which((nbaera2$Tm=="CHI")),]
nba.9= nbaera2[which((nbaera2$Tm=="HOU")),]
nba.10= nbaera2[which((nbaera2$Tm=="ORL")),]

nba.era2.T <- rbind(nba.1,nba.2,nba.3,nba.4,nba.5,nba.6,nba.7,nba.8,nba.9,nba.10)

#######################################################
#Random forest (with outliers and standardization)
#######################################################

nba.era2.T = subset(nba.era2.T, select = -c(Rk,Player,Pos,Tm) )
nba.era2.T <- as.data.frame(scale(nba.era2.T))

colnames(nba.era2.T) <- gsub("%", "pr", colnames(nba.era2.T))
colnames(nba.era2.T) <- gsub("2", "two", colnames(nba.era2.T))
colnames(nba.era2.T) <- gsub("3", "thr", colnames(nba.era2.T))
colnames(nba.era2.T) <- gsub("/48", "pfe", colnames(nba.era2.T))
nba.era2.T = subset(nba.era2.T, select = -c(WS) )

library(randomForest)

rf.6=randomForest(nba.era2.T$WSpfe ~ . ,data = nba.era2.T, importance=TRUE)
importance(rf.6)
varImpPlot(rf.6)

#Most important variables 
#BIG: BPM, PER  

###################################
#Random forest prediction

y <- nba.era2.T$WSpfe
nba.era2.Tsub = subset(nba.era2.T, select = -c(WSpfe) )
x = data.matrix(nba.era2.Tsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nba.era2.T,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nba.era2.T[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.346

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-12.6128  -0.2898   0.1267   0.0000   0.4979   6.7454

###############################
#Lasso prediction
###############################

#BPM and PER are not correlated in era 2 for top teams
cor(data.matrix(nba.era2.T))

#BPM: OBPM
nba.era2.TB = subset(nba.era2.T, select = -c(WSpfe,OBPM) )
x = data.matrix(nba.era2.TB)
#PER: TSpr
nba.era2.TP = subset(nba.era2.T, select = -c(WSpfe,TSpr) )
x = data.matrix(nba.era2.TP)

y <- nba.era2.T$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.182 
#Prediction for PER: 0.188

#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-12.6128  -0.2898   0.1267   0.0000   0.4979   6.7454 

###############
#PCR
###############

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nba.era2.T$WSpfe~.,data=nba.era2.T, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nba.era2.T$WSpfe
nba.era2.T_pcr = subset(nba.era2.T, select = -c(WSpfe) )
x = data.matrix(nba.era2.T_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=34)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nba.era2.T$WSpfe~., data=nba.era2.T_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=34)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.157

#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-12.6128  -0.2898   0.1267   0.0000   0.4979   6.7454  


###########################################
#Bottom 10 teams

nba.1= nbaera2[which((nbaera2$Tm=="MIN")),]
nba.2= nbaera2[which((nbaera2$Tm=="CHA")),]
nba.3= nbaera2[which((nbaera2$Tm=="SAC")),]
nba.4= nbaera2[which((nbaera2$Tm=="WAS")),]
nba.5= nbaera2[which((nbaera2$Tm=="TOR")),]
nba.6= nbaera2[which((nbaera2$Tm=="NYK")),]
nba.7= nbaera2[which((nbaera2$Tm=="BRK")),]
nba.11= nbaera2[which((nbaera2$Tm=="NJN")),]
nba.8= nbaera2[which((nbaera2$Tm=="MIL")),]
nba.9= nbaera2[which((nbaera2$Tm=="GSW")),]
nba.10= nbaera2[which((nbaera2$Tm=="LAC")),]

nba.era2.B <- rbind(nba.1,nba.2,nba.3,nba.4,nba.5,nba.6,nba.7,nba.8,nba.9,nba.10,nba.11)

#####################################################
#Random forest (with outliers and standardization)
#####################################################

nba.era2.B = subset(nba.era2.B, select = -c(Rk,Player,Pos,Tm) )
nba.era2.B <- as.data.frame(scale(nba.era2.B))

colnames(nba.era2.B) <- gsub("%", "pr", colnames(nba.era2.B))
colnames(nba.era2.B) <- gsub("2", "two", colnames(nba.era2.B))
colnames(nba.era2.B) <- gsub("3", "thr", colnames(nba.era2.B))
colnames(nba.era2.B) <- gsub("/48", "pfe", colnames(nba.era2.B))
nba.era2.B = subset(nba.era2.B, select = -c(WS) )

library(randomForest)

rf.6=randomForest(nba.era2.B$WSpfe ~ . ,data = nba.era2.B, importance=TRUE)
importance(rf.6)
varImpPlot(rf.6)

#Most important: PER, BPM

#############################
#Random forest prediction

y <- nba.era2.B$WSpfe
nba.era2.Bsub = subset(nba.era2.B, select = -c(WSpfe) )
x = data.matrix(nba.era2.Bsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nba.era2.B,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nba.era2.B[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.397

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-13.39165  -0.32494   0.08118   0.00000   0.46699  12.97529 

##############################
#Lasso prediction
##############################

#BPM and PER are correlated in era 2 for top teams
cor(data.matrix(nba.era2.B))

#BPM: PER, OBPM
nba.era2.BB = subset(nba.era2.B, select = -c(WSpfe,OBPM,PER) )
x = data.matrix(nba.era2.BB)
#PER: TSpr, OBPM, BPM
nba.era2.BP = subset(nba.era2.B, select = -c(WSpfe,TSpr,OBPM,BPM) )
x = data.matrix(nba.era2.BP)

y <- nba.era2.B$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.367 
#Prediction for PER: 0.219

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-13.39165  -0.32494   0.08118   0.00000   0.46699  12.97529

##################
#PCR
##################

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nba.era2.B$WSpfe~.,data=nba.era2.B, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nba.era2.B$WSpfe
nba.era2.B_pcr = subset(nba.era2.B, select = -c(WSpfe) )
x = data.matrix(nba.era2.B_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=20)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nba.era2.B$WSpfe~., data=nba.era2.B_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=20)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.293

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-13.39165  -0.32494   0.08118   0.00000   0.46699  12.97529 


##################################################
#ERA 3
#################################################

############################################################################
#Random forest (with outliers and standardization)
############################################################################

nbaera3.r = subset(nbaera3, select = -c(Rk,Player,Pos,Tm) )
nbaera3_r <- as.data.frame(scale(nbaera3.r))

colnames(nbaera3_r) <- gsub("%", "pr", colnames(nbaera3_r))
colnames(nbaera3_r) <- gsub("2", "two", colnames(nbaera3_r))
colnames(nbaera3_r) <- gsub("3", "thr", colnames(nbaera3_r))
colnames(nbaera3_r) <- gsub("/48", "pfe", colnames(nbaera3_r))
nbaera3_r = subset(nbaera3_r, select = -c(WS) )

library(randomForest)

rf.7=randomForest(nbaera3_r$WSpfe ~ . ,data = nbaera3_r, importance=TRUE)
importance(rf.7)
varImpPlot(rf.7)

#Most important variables 
#BIG: PER, BPM 
#Alot less: OBPM

##############################
#Random forest prediction

y <- nbaera3_r$WSpfe
nbaera3_rsub = subset(nbaera3_r, select = -c(WSpfe) )
x = data.matrix(nbaera3_rsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nbaera3_r,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nbaera3_r[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.255

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-11.42759  -0.33986   0.06396   0.00000   0.43268  23.18753 

#############################
#Lasso prediction
#############################

#BPM and PER are correlated in era 3
cor(data.matrix(nbaera3_r))

#BPM: PER, OBPM
nbaera3_rB = subset(nbaera3_r, select = -c(WSpfe,OBPM,PER) )
x = data.matrix(nbaera3_rB)
#PER: TSpr, OBPM, BPM
nbaera3_rP = subset(nbaera3_r, select = -c(WSpfe,TSpr,OBPM,BPM) )
x = data.matrix(nbaera3_rP)

y <- nbaera3_r$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.375 
#Prediction for PER: 0.212

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-11.42759  -0.33986   0.06396   0.00000   0.43268  23.18753

#####################
#PCR
#####################

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nbaera3_r$WSpfe~.,data=nbaera3_r, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nbaera3_r$WSpfe
nbaera3_r_pcr = subset(nbaera3_r, select = -c(WSpfe) )
x = data.matrix(nbaera3_r_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=31)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nbaera3_r$WSpfe~., data=nbaera3_r_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=31)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.209

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-11.42759  -0.33986   0.06396   0.00000   0.43268  23.18753 

#############################################
#TOP 10 teams

nba.1= nbaera3[which((nbaera3$Tm=="GSW")),]
nba.2= nbaera3[which((nbaera3$Tm=="TOR")),]
nba.3= nbaera3[which((nbaera3$Tm=="LAC")),]
nba.4= nbaera3[which((nbaera3$Tm=="SAS")),]
nba.5= nbaera3[which((nbaera3$Tm=="BOS")),]
nba.6= nbaera3[which((nbaera3$Tm=="HOU")),]
nba.7= nbaera3[which((nbaera3$Tm=="MIA")),]
nba.8= nbaera3[which((nbaera3$Tm=="UTA")),]
nba.9= nbaera3[which((nbaera3$Tm=="POR")),]
nba.10= nbaera3[which((nbaera3$Tm=="OKC")),]

nba.era3.T <- rbind(nba.1,nba.2,nba.3,nba.4,nba.5,nba.6,nba.7,nba.8,nba.9,nba.10)

###################################################
#Random forest (with outliers and standardization)
###################################################

nba.era3.T = subset(nba.era3.T, select = -c(Rk,Player,Pos,Tm) )
nba.era3.T <- as.data.frame(scale(nba.era3.T))

colnames(nba.era3.T) <- gsub("%", "pr", colnames(nba.era3.T))
colnames(nba.era3.T) <- gsub("2", "two", colnames(nba.era3.T))
colnames(nba.era3.T) <- gsub("3", "thr", colnames(nba.era3.T))
colnames(nba.era3.T) <- gsub("/48", "pfe", colnames(nba.era3.T))
nba.era3.T = subset(nba.era3.T, select = -c(WS) )

library(randomForest)

rf.7=randomForest(nba.era3.T$WSpfe ~ . ,data = nba.era3.T, importance=TRUE)
importance(rf.7)
varImpPlot(rf.7)

#Most important variables 
#BIG: PER, BPM 

###################################
#Random forest prediction

y <- nba.era3.T$WSpfe
nba.era3.Tsub = subset(nba.era3.T, select = -c(WSpfe) )
x = data.matrix(nba.era3.Tsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nba.era3.T,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nba.era3.T[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.611

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.1251 -0.3001  0.0439  0.0000  0.3646 18.8040 

############################
#Lasso prediction
############################

#BPM and PER are correlated in era 3 for top teams
cor(data.matrix(nba.era3.T))

#BPM: PER, OBPM,DPM
nba.era3.TB = subset(nba.era3.T, select = -c(WSpfe,OBPM,PER,DBPM) )
x = data.matrix(nba.era3.TB)
#PER: TSpr, OBPM, BPM
nba.era3.TP = subset(nba.era3.T, select = -c(WSpfe,TSpr,OBPM,BPM) )
x = data.matrix(nba.era3.TP)

y <- nba.era3.T$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.536 
#Prediction for PER: 0.258

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.1251 -0.3001  0.0439  0.0000  0.3646 18.8040

##############################
#PCR
##############################

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nba.era3.T$WSpfe~.,data=nba.era3.T, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nba.era3.T$WSpfe
nba.era3.T_pcr = subset(nba.era3.T, select = -c(WSpfe) )
x = data.matrix(nba.era3.T_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=20)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nba.era3.T$WSpfe~., data=nba.era3.T_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=20)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.424

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.1251 -0.3001  0.0439  0.0000  0.3646 18.8040 

################################################
#BOTTOM 10 teams

nba.1= nbaera3[which((nbaera3$Tm=="ORL")),]
nba.2= nbaera3[which((nbaera3$Tm=="NYK")),]
nba.3= nbaera3[which((nbaera3$Tm=="SAC")),]
nba.4= nbaera3[which((nbaera3$Tm=="DET")),]
nba.5= nbaera3[which((nbaera3$Tm=="MIN")),]
nba.6= nbaera3[which((nbaera3$Tm=="LAL")),]
nba.7= nbaera3[which((nbaera3$Tm=="BRK")),]
nba.8= nbaera3[which((nbaera3$Tm=="PHI")),]
nba.9= nbaera3[which((nbaera3$Tm=="NOP")),]
nba.10= nbaera3[which((nbaera3$Tm=="PHO")),]

nba.era3.B <- rbind(nba.1,nba.2,nba.3,nba.4,nba.5,nba.6,nba.7,nba.8,nba.9,nba.10)
attach(nba.era3.B)

####################################################
#Random forest (with outliers and standardization)
####################################################

nba.era3.B = subset(nba.era3.B, select = -c(Rk,Player,Pos,Tm) )
nba.era3.B <- as.data.frame(scale(nba.era3.B))

colnames(nba.era3.B) <- gsub("%", "pr", colnames(nba.era3.B))
colnames(nba.era3.B) <- gsub("2", "two", colnames(nba.era3.B))
colnames(nba.era3.B) <- gsub("3", "thr", colnames(nba.era3.B))
colnames(nba.era3.B) <- gsub("/48", "pfe", colnames(nba.era3.B))
nba.era3.B = subset(nba.era3.B, select = -c(WS) )

library(randomForest)

rf.7=randomForest(nba.era3.B$WSpfe ~ . ,data = nba.era3.B, importance=TRUE)
importance(rf.7)
varImpPlot(rf.7)

#Most important variables 
#BIG: PER, BPM 

################################
#Random forest prediction

y <- nba.era3.B$WSpfe
nba.era3.Bsub = subset(nba.era3.B, select = -c(WSpfe) )
x = data.matrix(nba.era3.Bsub)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

set.seed(1)

rf=randomForest(WSpfe~.,data=nba.era3.B,subset=train,importance=TRUE)
yhat = predict(rf,newdata=nba.era3.B[-train,])
plot(yhat, y.test)
abline(0,1)
sqrt(mean((yhat-y.test)^2))

#Prediction: 0.34

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-10.85342  -0.33803   0.07756   0.00000   0.47382   4.95832

#####################################
#Lasso prediction
#####################################

#BPM and PER are correlated in era 3 for Bottom teams
cor(data.matrix(nba.era3.B))

#BPM: PER, TSpr, OBPM
nba.era3.BB = subset(nba.era3.B, select = -c(WSpfe,TSpr,OBPM,PER) )
x = data.matrix(nba.era3.BB)
#PER: FGpr, TSpr, OBPM, BPM
nba.era3.BP = subset(nba.era3.B, select = -c(WSpfe,FGpr,TSpr,OBPM,BPM) )
x = data.matrix(nba.era3.BP)

y <- nba.era3.B$WSpfe

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

library(glmnet)
lasso.mod=glmnet(x[train,],y[train],alpha=1)
plot(lasso.mod)#coefficient plot

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.1se
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))
summary(y)

#Prediction for BPM: 0.237 
#Prediction for PER: 0.164

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-10.85342  -0.33803   0.07756   0.00000   0.47382   4.95832 

##########################
#PCR
##########################

library(pls)
#make this example reproducible
set.seed(1)

model <- pcr(nba.era3.B$WSpfe~.,data=nba.era3.B, scale=TRUE, validation="CV")
summary(model)

predplot(model)
predplot(model)
coefplot(model)

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

y <- nba.era3.B$WSpfe
nba.era3.B_pcr = subset(nba.era3.B, select = -c(WSpfe) )
x = data.matrix(nba.era3.B_pcr)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=21)
summary(pcr.fit)
coefplot(pcr.fit)

set.seed(1)
train=sample(1:nrow(x), nrow(x)*.7)
test=(-train)
y.test=y[test]

model <- pcr(nba.era3.B$WSpfe~., data=nba.era3.B_pcr,subset=train, scale=TRUE, validation="CV")
validationplot(model,val.type="MSEP")
pcr.pred=predict(model,x[test,],ncomp=21)
sqrt(mean((pcr.pred-y.test)^2))

#Prediction: 0.227

#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-10.85342  -0.33803   0.07756   0.00000   0.47382   4.95832  


#Lasso used in standardized
#cor(x), pick predictor variable, and eliminate all correlated variables

#Introduce new variable
##############################################################################

#run pcr for all variables
#RUn for top 5 worst teams and best teams
#prediction for all methods
####################################################################################################

#Use Quantile to make prediction, picking from rf and lasso, not including correlated variables
#Present better the results
#mse to pick best method
#AIC for all regression to pick best method
#AIC.rq for AIC in quantile

###################################
#RMSE Comparison
###################################

############
#Era1
############

#Random forest RMSE: 0.446
#Lasso RMSE: for BPM: 0.215*
#            for PER: 0.215*
#PCR RMSE: v

#TOP 10
#Random forest RMSE: 0.262
#Lasso RMSE: for BPM: 0.411
#            for PER: 0.248*
#PCR RMSE: 0.2955

#BOTTOM 10:
#Random forest RMSE: 0.341
#Lasso RMSE: for BPM: 0.349
#            for PER: 0.1922***
#PCR RMSE: 0.2666

mean(0.446,0.262,0.341)
mean(0.215,0.411,0.349)
mean(0.215,0.248,0.1922)
mean(0.407,0.2955,0.2666)

#Lasso is best choice

#############
#Era2
#############

#Random forest RMSE: 0.334
#Lasso RMSE: for BPM: 0.217
#            for PER: 0.2204
#PCR RMSE: 0.201*

#TOP 10
#Random forest RMSE: 0.346
#Lasso RMSE: for BPM: 0.182
#            for PER: 0.188
#PCR RMSE: 0.157***

#BOTTOM 10:
#Random forest RMSE: 0.397
#Lasso RMSE: for BPM: 0.367
#            for PER: 0.219*
#PCR RMSE: 0.293

mean(0.334,0.346,0.397)
mean(0.217,0.182,0.367)
mean(0.2204,0.188,0.219)
mean(0.201,0.157,0.293)

#PCR is best choice

###################################
#Era3
#Random forest RMSE: 0.255
#Lasso RMSE: for BPM: 0.375
#            for PER: 0.212
#PCR RMSE: 0.209*

#TOP 10
#Random forest RMSE: 0.611
#Lasso RMSE: for BPM: 0.536
#            for PER: 0.258*
#PCR RMSE: 0.424

#BOTTOM 10:
#Random forest RMSE: 0.34
#Lasso RMSE: for BPM: 0.237
#            for PER: 0.164***
#PCR RMSE: 0.227

mean(0.255,0.611,0.34)
mean(0.375,0.536,0.237)
mean(0.212,0.258,0.164)
mean(0.209,0.424,0.227)

#PCR is best choice

#Put in latex why we don't use linear regression (plots)
#conclusion just summarize the whole paper
#add future work: to show what could be done differently or expand it
#Dont put code of things we didnt do
#add tables for data description


