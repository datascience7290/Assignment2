install.packages("chron")
install.packages("dplyr")
install.packages("plyr")
install.packages("reshape2")
library(reshape2)
library(chron)
library(dplyr)
library(plyr)
setwd("/Users/hinagandhi/desktop/Assignment2")
data1 <- read.csv("forecastData.csv",header=T,sep=',')
View(data1)
# dropping unused rows
dropsRows <- c("X","Gust_SpeedMPH","PrecipitationIn","Events", "DateUTC","TimeEDT")
data1 <- data1[ , !(names(data1) %in% dropsRows)]
colnames(data1)[colnames(data1)=="Day of Week"] <- "Day.of.Week"
# extracting time
data1$Hour <- hours(as.POSIXlt(data1$Time))
# extracting date
data1$Date <- as.Date(as.POSIXlt(data1$Time))
write.csv(data1,"newfile.csv")
#Extracting wind speed
data1$Wind_SpeedMPH<- as.numeric(as.character(data1$Wind_SpeedMPH))
#Extracting Temperature
data1$TemperatureF <- as.numeric(data1$TemperatureF)
#Extracting Dew Point
data1$Dew_PointF<-as.numeric(data1$Dew_PointF)
#Extracting Sea level Pressure
data1$Sea_Level_PressureIn<-as.numeric(data1$Sea_Level_PressureIn)
#Extracting Visibility 
data1$VisibilityMPH<-as.numeric(data1$VisibilityMPH)
#Extracting Humidity
data1$Humidity<-as.numeric(data1$Humidity)
write.csv(data1,"newfile1.csv")
#data1$r<- NULL
data1$Date <- as.Date(data1$Date, "%m/%d/%Y")

# extract year
data1$Year<-as.numeric(format(data1$Date, "%Y"))
# extract month
data1$Month<-as.numeric(format(data1$Date, "%m"))
#extract day
library(chron)
data1$Day<-as.numeric(format(data1$Date, "%d"))
data1["Day.of.Week"] <- day.of.week(data1$Month,data1$Day,data1$Year)
isWeekday <-!is.weekend(data1$Date)
data1$Weekday <-ifelse(isWeekday,1,0)
k <- 1
j <- 0

#peakhour
write.csv(data1, "test.csv")
for(i in seq(from=1, to=nrow(data1)))
{  
  if(data1$Hour[i] >= 7 && data1$Hour[i]<= 19 )
    data1$PeakHour[i] <- 1
  else
    data1$PeakHour[i] <- 0
}
View(data1)

newdataframe<-data1[ ,!(names(data1)) %in% c("Conditions","Wind_Direction","Time")]
#View(newdataframe)
#reorder
newdataframe <- newdataframe[c("Date","Hour","Year","Month","PeakHour","Day.of.Week","Weekday","Day","TemperatureF","Dew_PointF","Humidity","Sea_Level_PressureIn","VisibilityMPH","Wind_SpeedMPH","WindDirDegrees")]
View(newdataframe)
#remove na
for(j in 9:ncol(newdataframe))
{
  if(is.na(newdataframe[1,j]) )
  {
    newdataframe[1,j] <- 0
  }
} 
for(j in 9:ncol(newdataframe))
{
  for(i in 2:nrow(newdataframe)) {
    if(is.na(newdataframe[i,j]) )
    {
      newdataframe[i,j] <- newdataframe[i-1,j]
    }
  }
}
#outliers
# when outlier is in first row 
for(j in 9:ncol(newdataframe))
{
  if(newdataframe[1,j] < (mean(newdataframe[[j]]) - (1.5)*sd(newdataframe[[j]]))) 
  {
    newdataframe[1,j] <- (mean(newdataframe[[j]]) - (1.5)*sd(newdataframe[[j]]))
  }
  if(newdataframe[1,j] > (mean(newdataframe[[j]]) + (1.5)*sd(newdataframe[[j]])))
  {
    newdataframe[1,j] <- (mean(newdataframe[[j]]) + (1.5)*sd(newdataframe[[j]]))
  }
}
for(j in 9:ncol(newdataframe))
{
  for(i in 2:nrow(newdataframe)) {
    if((newdataframe[i,j] < (mean(newdataframe[[j]]) - (1.5)*sd(newdataframe[[j]]))) |
       (newdataframe[i,j] > (mean(newdataframe[[j]]) + (1.5)*sd(newdataframe[[j]])))) 
    {newdataframe[i,j] <- newdataframe[i-1,j]}
  }
}

write.csv(newdataframe,"newdataframe1.csv")
View(newdataframe)
newdataframe <- aggregate(newdataframe[, c(9,10,11,12,13,14,15)], by = list(newdataframe$Date, newdataframe$Hour), mean, na.rm = F)
colnames(newdataframe)[colnames(newdataframe)=="Group.1"] <- "Date"
colnames(newdataframe)[colnames(newdataframe)=="Group.2"] <- "Hour"
newdataframe <- newdataframe[order(newdataframe$Date),]
newdataframe1<-data1[c("Date","Hour","Year","Month","PeakHour","Day.of.Week","Weekday","Day")]
View(newdataframe)
View(newdataframe1)
n<-newdataframe1[!duplicated(newdataframe1[c("Date","Hour","Year","Month","PeakHour","Day.of.Week","Weekday","Day")]),]
View(n)

newdataframe2<-left_join(newdataframe,n,by = c("Date"="Date", "Hour"= "Hour"))
View(newdataframe2)
newdataframe2 <- newdataframe2[c("Date","Hour","Year","Month","PeakHour","Day.of.Week","Weekday","Day","TemperatureF","Dew_PointF","Humidity","Sea_Level_PressureIn","VisibilityMPH","Wind_SpeedMPH","WindDirDegrees")]
write.csv(newdataframe2,"newdataframe2.csv")

library(forecast)
pred = predict(lm.fit, newdataframe2)
summary(pred)
KWH<-pred
View(KWH)
data1 <- newdataframe2[,c("Date","Hour","TemperatureF")]
forecastOutput <- cbind(data1,KWH)
write.csv(forecastOutput,paste("forecastOutput_Account_",unique(train$Account),paste(".csv")))
