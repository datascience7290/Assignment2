
library("leaps")
# reading csv file
consolidate1 <- read.csv("sampleFormat.csv")
consolidate1 <- as.data.frame(consolidate1)

# removing extra columns from dataframe
drops <- c("Conditions","Wind_Direction","Date")
consolidate <- consolidate1[ , !(names(consolidate1) %in% drops)]

# creating training and testing data
smp_size <- floor(0.75 * nrow(consolidate))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(consolidate)), size = smp_size)

#Split the data into training and testing
train <- consolidate[train_ind, ]
test <- consolidate[-train_ind, ]
##Linear Model
lm.fit = lm(kWh ~ ., data = train)
summary(lm.fit)

#forward Regression
fwdreg=regsubsets(kWh ~.,data=train, nvmax = 7, method="forward")
summary(fwdreg)

#Backward Regression
backreg=regsubsets(kWh ~.,data=train,nvmax = 7, method="backward")
summary(backreg)

#Stepwise Regression
library(MASS)
fit <- lm(kWh~.,data=train)
step <- stepAIC(fit, direction="both")
step$anova # display results

#Modified Linear Model
# hit and trial by adding and deleting the parameters to determine best possible fit
lm.fit <- lm(kWh ~  TemperatureF +Month + Day + Hour, data = train)
lm.fit <- lm(kWh ~  TemperatureF +Month + Day + Hour+Day.of.Week+PeakHour+Weekday , data = train)
lm.fit <- lm(kWh ~  Month+Day+Hour+PeakHour+Sea_Level_PressureIn+Weekday,data = train)
lm.fit <- lm(kWh ~  TemperatureF +Month + Day + Hour+PeakHour+Weekday+Dew_PointF , data = train)
lm.fit <- lm(kWh ~  TemperatureF +Month + Day + Hour+PeakHour+Weekday+Dew_PointF+Humidity+VisibilityMPH , data = train)
lm.fit <- lm(kWh ~  TemperatureF +Month + Day + Hour+PeakHour+Weekday+Dew_PointF+Humidity , data = train)
lm.fit <- lm(kWh ~  TemperatureF +Month + Day + Hour+Day.of.Week+PeakHour+Weekday+Dew_PointF+Humidity+Sea_Level_PressureIn+VisibilityMPH , data = train)

# variables took from forward regression
lm.fit<-  lm(kWh ~ Month+Hour+PeakHour+Sea_Level_PressureIn+Weekday+Day.of.Week+TemperatureF,data = train)
summary(lm.fit)

# variables took from backward regression
lm.fit<-  lm(kWh ~ Month+Hour+PeakHour+Weekday+Day.of.Week+Sea_Level_PressureIn+TemperatureF+Dew_PointF,data = train)
summary(lm.fit)

# variables referred from stepwise regression
lm.fit <- lm(kWh ~ Month + Day + Hour +Day.of.Week +Weekday + PeakHour + Dew_PointF + Humidity + Sea_Level_PressureIn + VisibilityMPH + 
               Wind_SpeedMPH + WindDirDegrees + TemperatureF, data=train)
summary(lm.fit)
plot(lm.fit)


######lm.ridge
library(glmnet)
x <- model.matrix(kWh ~ ., train)
# response variable
y <- train$kWh
# method cv.glment method where we have given response and predicting variables 
# where measure = "area under ROC curve" and alpha = 0
cv.ridge <- cv.glmnet(x,y,alpha =0,parallel=TRUE,standardize=TRUE,type.measure='auc')
# plotting ridge
plot(cv.ridge)
# calculating minimum value of lambda
l <- cv.ridge$lambda.min
cv.ridge$lambda.1se
# calculate the value of coefficients for the predicting variables 
res <- predict(cv.ridge, s=l,type="coefficients")
res
coef(cv.ridge, s=cv.ridge$lambda.min)
summary(cv.ridge)
## variables taken from ridge
lm.fit <- lm(kWh ~ Day.of.Week + Month+ Day+ Hour+ Humidity + VisibilityMPH + WindDirDegrees + Weekday + PeakHour + TemperatureF +Dew_PointF + Sea_Level_PressureIn 
             + Wind_SpeedMPH, data=train)
summary(lm.fit)
plot(lm.fit)

######lm.lasso
x <- model.matrix(kWh ~ ., train)
y <- train$kWh
# cv.glmnet where alpha = 1 and measure = "area under ROC curve"
cv.lasso <- cv.glmnet(x,y,alpha =1,parallel=TRUE,standardize=TRUE,type.measure='auc')
plot(cv.lasso)
# calculating minimum lambda
l <- cv.lasso$lambda.min
cv.lasso$lambda.1se
# calculate the coefficients of predicting parameters
res <- predict(cv.lasso, s=l,type="coefficients")
res
coef(cv.lasso, s=cv.lasso$lambda.min)
summary(cv.lasso)
####elastic net
x <- model.matrix(kWh ~ ., train)
y <- train$kWh
### cv.glmnet where alpha = 0.5 and measure = "area under ROC curve"
cv.elnet <- cv.glmnet(x,y,alpha =0.5,parallel=TRUE,standardize=TRUE,type.measure='auc')
plot(cv.elnet)
l <- cv.elnet$lambda.min
cv.elnet$lambda.1se
# calculating the coeffecients of predicting parameters
res <- predict(cv.elnet, s=l,type="coefficients")
res
coef(cv.elnet, s=cv.elnet$lambda.min)
summary(cv.elnet)
## varibales taken from lasso
lm.fit <- lm(kWh ~ Day.of.Week + Month+ Day+ Hour+ Humidity + VisibilityMPH + WindDirDegrees + Weekday + PeakHour + TemperatureF +Dew_PointF + Sea_Level_PressureIn 
             + Wind_SpeedMPH , data=train)
summary(lm.fit)
## variables taken from elastic net
lm.fit <- lm(kWh ~ Day.of.Week + Month+ Day+ Hour+ Humidity + VisibilityMPH + WindDirDegrees + Weekday + PeakHour + TemperatureF +Dew_PointF + Sea_Level_PressureIn 
             + Wind_SpeedMPH , data=train)
summary(lm.fit)

#Measures of predictive accuracy
install.packages("forecast")
library(forecast)
# predict method to predict value of kWh
pred = predict(lm.fit, test)
# checking the accuracy of predicted kWH value through RMSE,  MAE,  MPE  and  MAPE 
accuracy(pred, test$kWh)
View(pred)

####writing csv
library(broom)
tidy_lmfit <- tidy(coef(lm.fit))
tidy_lmfit[,1:2]
account <- c("Account No", unique(train$Account))
tidy_lmfit <- rbind(account,(tidy_lmfit[,1:2]))
write.csv(tidy_lmfit,"RegressionOutputs.csv")
#Transpose
account <- unique(train$Account)
acc<-accuracy(pred,test$kWh)
tyu<-as.data.frame(acc)
View(tyu)
PerformanceMetrics <- t(tyu[1:5])
PerformanceMetrics<-rbind(account,PerformanceMetrics)
View(PerformanceMetrics)
write.csv(PerformanceMetrics,"PerformanceMetrics.csv")