library("reshape2")
# reading the csv files
data1 <- read.csv("/Users/hinagandhi/desktop/Assignment2/rawData1.csv",,header=T,sep=',')
data2 <- read.csv("/Users/hinagandhi/desktop/Assignment2/rawData2.csv",,header=T,sep=',')
data <- rbind(data1,data2)
data <- as.data.frame(data)
View(data)
# keeping rows having units in kwh
kWHList <- sapply (data$Units, function(x) x == 'kWh')
data_frame <- data[kWHList,]
# view data frame
View(data_frame)
#Transforming and changing
x <- data_frame
#View(x)
trans <- t(x[4:292])
# melting data
md <- melt(data_frame, id=(c("Account", "Date", "Channel", "Units")))
View(md)
# ordering by date having format in month/day/year
md <- md[order(as.Date(md$Date, format="%m/%d/%Y")),]
# removing  some channels like channel 1 and channel 3
md  <- md[md$Channel != "507115423 1 kWh", ]
md  <- md[md$Channel != "507115423 3", ] 
# calculating aggregated sum of kwh 
View(md)
kwh_total <- NULL
k <- 1
j <- 0
new_data_frame <- NULL
for(i in seq(from=1, to=nrow(md), by=12))
{  
  # making hour to 0 whenever hour becomes 24
   if(j %% 24 == 0)
     j = 0;
   if(j >= 7 && j<= 19 )
     kwh_total$PeakHour[k] <- 1
   else
     kwh_total$PeakHour[k] <- 0
  # summing up kwh from 0 to 23 hours for each date
   kwh_total$kWh[k] <- sum(md$value[i:(i+11)])
   kwh_total$Hour[k] <- j
  # creating new data frame to add row with aggregated sum
   new_data_frame <- rbind(new_data_frame, md[i,])
   k = k + 1
   j <- j+1
}
# dropping unnecessary columns
drops <- c("Units","variable","value","Channel")
new_data_frame <- new_data_frame[ , !(names(new_data_frame) %in% drops)]
# column binding data frame kwh_total
md <- cbind(new_data_frame, kwh_total)
View(md)
md$Date <- as.Date(md$Date, "%m/%d/%Y")

# extract year
md$Year<-as.numeric(format(md$Date, "%Y"))
# extract month
md$Month<-as.numeric(format(md$Date, "%m"))
#extract day
md$Day<-as.numeric(format(md$Date, "%d"))

md <- md[order(md$Date),]

md$month<- NULL
md$value<-NULL
# chron package to split year into day, month and year
library(chron)

md["Day of Week"] <- day.of.week(md$Month,md$Day,md$Year)
isWeekday <-!is.weekend(md$Date)
md$Weekday <-ifelse(isWeekday,1,0)
# reordering columns in r
md <- md[c( "Account", "Date", "kWh","Month","Day","Year","Hour","Day of Week","Weekday","PeakHour")]

# removing NAs if any
View(md)
if(is.na(md$kWh[1]) )
{
  md$kWh[1] <- 0
}

for(i in 2:nrow(md)) {
  if(is.na(md$kWh[i]) )
    {
    md$kWh[i] <- md$kWh[i-1]
    }
}


# finding outliers
# In row1 if there is an outlier then replace it with mean(md$kWh) - ((1.5)*sd(md$kWh))
if(md$kWh[1] < (mean(md$kWh) - ((1.5)*sd(md$kWh)))) 
{
  md$kWh[1] <- (mean(md$kWh) - ((1.5)*sd(md$kWh)))
}
if(md$kWh[1] > (mean(md$kWh) + ((1.5)*sd(md$kWh))))
{
  md$kWh[1] <- mean(md$kWh) + ((1.5)*sd(md$kWh))
}
# from row 2 to nrows replace it by previous row value
for(i in 2:nrow(md)) {
  if((md$kWh[i] < (mean(md$kWh) - ((1.5)*sd(md$kWh)))) |
     (md$kWh[i] > (mean(md$kWh) + ((1.5)*sd(md$kWh))))) 
  {md$kWh[i] <- md$kWh[i-1]}
}
View(md)
# scraping data from the weather website
install.packages("weatherData")
library(weatherData)
install.packages("devtools")
library("devtools")
install_github("Ram-N/weatherData")
getWeatherData <- getWeatherForDate("KBOS", start_date=min(md$Date),
                                    end_date = max(md$Date),
                                    opt_detailed=T, opt_all_columns=T)

#View(getWeatherData)
dropsRows <- c("Gust_SpeedMPH","PrecipitationIn","Events", "DateUTC","TimeEDT","TimeEST")
# making all the columns in numeric format
getWeatherData <- getWeatherData[ , !(names(getWeatherData) %in% dropsRows)]
getWeatherData$Hour <- hours(as.POSIXlt(getWeatherData$Time))
getWeatherData$Date <- as.Date(as.POSIXlt(getWeatherData$Time))
getWeatherData$Wind_SpeedMPH<- as.numeric(getWeatherData$Wind_SpeedMPH)
getWeatherData$TemperatureF <- as.numeric(getWeatherData$TemperatureF)
getWeatherData$Dew_PointF<-as.numeric(getWeatherData$Dew_PointF)
getWeatherData$Sea_Level_PressureIn<-as.numeric(getWeatherData$Sea_Level_PressureIn)
getWeatherData$VisibilityMPH<-as.numeric(getWeatherData$VisibilityMPH)
getWeatherData$Humidity<-as.numeric(getWeatherData$Humidity)
write.csv(getWeatherData,"getWeatherData.csv")
View(getWeatherData)
# remove NAs and outliers
# replacing NA value for row 1 with 0
for(j in 3:ncol(getWeatherData))
{
    if(is.na(getWeatherData[1,j]) )
    {
      getWeatherData[1,j] <- 0
    }
} 
# for all columns and from row 2 to nrows replace the outlier by previous row value
for(j in 3:ncol(getWeatherData))
{
  for(i in 2:nrow(getWeatherData)) {
    if(is.na(getWeatherData[i,j]) )
    {
      getWeatherData[i,j] <- getWeatherData[i-1,j]
    }
  }
} 
newdf <- getWeatherData
asdkj<- newdf[, c("Hour","Date","Conditions","Wind_Direction")]
asdkj <-mutate(group_by(asdkj, Date, Hour), 
               Conditions = replace(as.character(Conditions), !complete.cases(Conditions), "") ,Wind_Direction = replace(as.character(Wind_Direction), !complete.cases(Wind_Direction), "")) %>%
  summarise(Conditions = paste(Conditions, collapse = " / "),Wind_Direction= paste(Wind_Direction, collapse = " / "))
# aggregating data by hour and date
agg <- aggregate(getWeatherData[, c(2,3,4,5,6,8,10)], by = list(getWeatherData$Date, getWeatherData$Hour), mean, na.rm = F)
colnames(agg)[colnames(agg)=="Group.1"] <- "Date"
colnames(agg)[colnames(agg)=="Group.2"] <- "Hour"
agg <- left_join(agg,asdkj, by = c("Date"="Date", "Hour"= "Hour"))
agg <- agg[order(agg$Date),]
agg$Wind_SpeedMPH[is.na(agg$Wind_SpeedMPH)]<-0
View(agg)
agg <- agg[c("Date","Hour","Conditions","Wind_Direction","TemperatureF","Dew_PointF","Humidity","Sea_Level_PressureIn","VisibilityMPH","Wind_SpeedMPH","WindDirDegrees")]
# removing na
for(j in 3:ncol(agg))
{
    if(is.na(agg[1,j]) )
    {
      agg[1,j] <- 0
    }
}
# removing na from row to nrows
for(j in 3:ncol(agg))
{
  for(i in 2:nrow(agg)) {
    if(is.na(agg[i,j]) )
    {
      agg[i,j] <- agg[i-1,j]
    }
  }
} 

# merging two files
View(agg)
View(md)
install.packages("dplyr")
library(dplyr)
# considering left to merge two files
consolidate <- left_join(md,agg, by = c("Date"="Date", "Hour"= "Hour"))
View(consolidate)
# removing na
for(j in 13:ncol(consolidate))
{
  if(is.na(consolidate[1,j]))
    {
      consolidate[1,j] <- 0
    }
}

for(j in 13:ncol(consolidate))
{
  for(i in 2:nrow(consolidate)) {
    if(is.na(consolidate[i,j]))
    {
      consolidate[i,j] <- consolidate[i-1,j]
    }
  }
}
for(j in 11:12)
{
  if(is.na(consolidate[1,j]))
  {
    consolidate[1,j] <- " "
  }
}

for(j in 11:12)
{
  for(i in 2:nrow(consolidate)) {
    if(is.na(consolidate[i,j]))
    {
      consolidate[i,j] <- consolidate[i-1,j]
    }
  }
}

write.csv(consolidate, "consolidate.csv")
# when outlier is in first row 
for(j in 13:ncol(consolidate))
{
    if(consolidate[1,j] < (mean(consolidate[[j]]) - (1.5)*sd(consolidate[[j]]))) 
    {
      consolidate[1,j] <- (mean(consolidate[[j]]) - (1.5)*sd(consolidate[[j]]))
    }
    if(consolidate[1,j] > (mean(consolidate[[j]]) + (1.5)*sd(consolidate[[j]])))
    {
      consolidate[1,j] <- (mean(consolidate[[j]]) + (1.5)*sd(consolidate[[j]]))
    }
}
# removing outliers from row 2 to nrows for all columns 
for(j in 13:ncol(consolidate))
{
  for(i in 2:nrow(consolidate)) {
    if((consolidate[i,j] < (mean(consolidate[[j]]) - (1.5)*sd(consolidate[[j]]))) |
       (consolidate[i,j] > (mean(consolidate[[j]]) + (1.5)*sd(consolidate[[j]])))) 
    {consolidate[i,j] <- consolidate[i-1,j]}
  }
}
View(consolidate)
# writing cleaned data in sampleFormat.csv
write.csv(consolidate, "sampleFormat.csv", row.names = FALSE)
