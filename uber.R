#importing required libraries 
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
#reading data
uberData <- read.csv("Uberdata_Da.csv")
#understanding dataset
summary(uberData)
str(uberData)
head(uberData)
tail(uberData)
#View(uberData)
dim(uberData)
#in Status we have 2 levels trip completed and cancelled
unique(uberData$Status)

#data preprocessing & data cleaning

#checking for missing values 
any(is.na(uberData))
#how many missing values are there?
sum(is.na(uberData))
#Checking which col has missing values
summary(uberData)
# from summary we can see Lat has 18 missing values and Lan has 22 missing values
#now we have to fill missing values with mean or median
uberData$Lat[is.na(uberData$Lat)]<-mean(uberData$Lat,na.rm=TRUE)
uberData$Lon[is.na(uberData$Lon)]<-mean(uberData$Lon,na.rm=TRUE)

#after filling checking for missing values
any(is.na(uberData))

#extracting data For data visualization we are separating these
uberData$Date <- ymd(uberData$Date)
uberData$day <- factor(day(uberData$Date))
uberData$month <- factor(month(uberData$Date,label = TRUE))
uberData$year <- factor(year(uberData$Date))
uberData$daysofweek <- factor(wday(uberData$Date,label = TRUE))
uberData$hour <- factor(hour(hms(uberData$Time)))
uberData$minute <- factor(minute(hms(uberData$Time)))
uberData$second <- factor(second(hms(uberData$Time)))

dim(uberData)#now we have 13 cols
head(uberData)
str(uberData)

unique(uberData$year)

#Checking for outliers of numerical cols
summary(uberData)

# we have 2 numerical cols to check for outliers
k<-uberData# storing in dummy variable
boxplot(k$Lat,main="lat boxplot")
y<-boxplot(k$Lat,plot=FALSE)$out
k<-k[-which(k$Lat %in% y),]
boxplot(k$Lat,col="#660033",main="lat boxplot")$out
dim(k)
dim(uberData)#not much data is lossed so removed the outliers

boxplot(k$Lon,main="lan boxplot")
y<-boxplot(k$Lon,plot=FALSE)$out
k<-k[-which(k$Lon %in% y),]
y<-boxplot(k$Lon,col="darkmagenta",main="lan boxplot")$out
dim(k)
dim(uberData)#not much data is lossed so removed the outliers

uberData<-k
boxplot(uberData$Lat)$out
boxplot(uberData$Lon)$out
dim(uberData)

unique(uberData$second)#as there is only single level so removing this col.
uberData=subset(uberData,select=-c(second))
dim(uberData)


dataUber<-uberData# storing in dummy variable

status <- dataUber%>%
  group_by(Status)%>%
  summarize(Total=n())
head(status)
ggplot(status,aes(Status,Total,fill=Status))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Status")+theme_light()

yearu <- dataUber%>%
  group_by(year,daysofweek)%>%
  summarize(Total=n())
head(yearu)
ggplot(yearu,aes(year,Total,fill=year))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Trips By Day And Month")+theme_light()+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a"))

#data analysis
#24 hours from 0 to 23 finding total trips in every hour
dataUber <- uberData
hD <- dataUber%>%group_by(hour)%>%
  dplyr::summarize(Total=n())
head(hD,10)

#Trips in  Every Hour
ggplot(hD,aes(x=hour,y=Total))+
  geom_bar(aes(fill=hour),stat = "identity")+theme_light()+
  ggtitle("Trips in Every Hour")

#Number Of Trips Taking Place During Months and year
monthGroup <- dataUber%>%
  group_by(month,year)%>%
  dplyr::summarize(Total=n())
head(monthGroup)
#Number Of Trips Taking Place During Months in every year
ggplot(monthGroup,aes(month,Total,fill=year))+
  geom_bar(stat = "identity")+
  ggtitle("Number Of Trips Taking Place During Months in every year")+theme_light()+
  scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2"))
#Number Of Trips Taking Place During Year of every month
ggplot(monthGroup,aes(year,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Number Of Trips Taking Place During Year of every month")+theme_light()+
 scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028", "#FFE699","#FFF9B6",
                            "#CC1011","#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"))
#Trips By Hour And Month
mH <- dataUber%>%group_by(month,hour)%>%
  dplyr::summarize(Total=n())
head(mH,10)
#Plotting Trips During Every hour Of The Month
ggplot(mH,aes(hour,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Hour And Month")+
 scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028","#FFE699","#FFF9B6",
                            "#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"))
#Plotting Trips During Every Month in hours
ggplot(mH,aes(month,Total,fill=hour))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Month And Hour")

#Trips By day And Month
dayGroup <- dataUber%>%group_by(day,month)%>%
  dplyr::summarize(Total=n())
head(dayGroup,10)
#Plotting Trips During Every day Of The Month
ggplot(dayGroup,aes(day,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips Every Day in month")+
  scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028",  "#FFE699","#FFF9B6",
                             "#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"))
#Plotting Trips During Every Month with days
ggplot(dayGroup,aes(month,Total,fill=day))+
  geom_bar(stat = "identity")+
  ggtitle("Trips in every month of Every Day")

#Trips By weeks of Day And Month
monthWeekday <- dataUber%>%
  group_by(month,daysofweek)%>%
  summarize(Total=n())
head(monthWeekday)

#Plotting Trips During Every weeks of Day 
ggplot(monthWeekday,aes(daysofweek,Total,fill=daysofweek))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Every week of Day ")+theme_light()+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a", "#f78228", "#c05cf2","#50d9d2", "#31d45c"))

#Plotting Trips During Every  weeks of Day And Month
ggplot(monthWeekday,aes(month,Total,fill=daysofweek))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By weekDay And Month")+theme_light()+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a", "#f78228", "#c05cf2","#50d9d2", "#31d45c"))

#Number Of Trips By Bases
ggplot(dataUber,aes(Base,fill=Base))+
  geom_bar()+ggtitle("Trips By Bases")

ggplot(dataUber,aes(Base,fill=month))+
  geom_bar(position = "dodge")+
  ggtitle("Trips By Bases And Month")+theme_light()+
  scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028",  "#FFE699","#FFF9B6",
                             "#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"))
ggplot(dataUber,aes(month,fill=Base))+
  geom_bar(position = "dodge")+
  ggtitle("Trips By Bases And Month")+theme_light()+
  scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028",  "#FFE699"))

ggplot(dataUber,aes(hour,fill=Base))+
  geom_bar()+
  ggtitle("Trips By Bases And Hour")+theme_light()+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a", "#f78228", "#c05cf2","#50d9d2", "#31d45c"))

ggplot(dataUber,aes(Base,fill=hour))+
  geom_bar()+
  ggtitle("Trips By Bases And Hour")+theme_light()

ggplot(dataUber,aes(Base,fill=daysofweek))+
  geom_bar(position = "dodge")+
  ggtitle("Trips By Bases And Day Of Week")+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a", "#f78228", "#c05cf2","#50d9d2", "#31d45c"))

ggplot(dataUber,aes(daysofweek,fill=Base))+
  geom_bar(position = "dodge")+
  ggtitle("Trips By Bases And Day Of Week")+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a", "#f78228", "#c05cf2","#50d9d2", "#31d45c"))
#creating heatmap

#heat map by hour and day
dayHour <- dataUber%>%
  group_by(day,hour)%>%
  summarize(Total=n())

ggplot(dayHour,aes(day,hour,fill=Total))+
  geom_tile(color="White")+
  ggtitle("Heat Map By Hour And Day")

#Heat Map By Month And Day
daymg <- dataUber%>%
  group_by(month,day,daysofweek)%>%
  dplyr::summarize(Total=n())

ggplot(daymg,aes(day,month,fill=month))+
  geom_tile(color="white")+
  ggtitle("Heat Map By Month And Day")

#heatmap by month and day of week
ggplot(monthWeekday,aes(daysofweek,month,fill=daysofweek))+
  geom_tile(color="white")+
  ggtitle("Heat Map By Month And Day Of Week")
ggplot(monthWeekday,aes(daysofweek,month,fill=Total))+
  geom_tile(color="white")+
  ggtitle("Heat Map By Month And Day Of Week")

#Heat Map By Month And Bases
monthBase <- dataUber%>%
  group_by(Base,month)%>%
  summarize(Total=n())

ggplot(monthBase,aes(Base,month,fill=Total))+
  geom_tile(color="white")+
  ggtitle("Heat Map By Month And Bases")+theme_light()

#Heat Map By Day Of Week And Bases
daysBases <- dataUber%>%
  group_by(Base,daysofweek)%>%
  summarize(Total=n())

ggplot(daysBases,aes(Base,daysofweek,fill=Total))+
  geom_tile(color="white")+
  ggtitle("Heat Map By Day Of Week And Bases")+
  ylab("Day Of Week")

#average number of passengers that avail Uber trips in a day
passengersdata=nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Sun'))+
  nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Mon'))+
  nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Tue'))+
  nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Wed'))+
  nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Thr'))+
  nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Fri'))+
  nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Sat'))
avg.passengers=passengersdata/7
cat("Average number of passengers that avail Uber trips in a day=",avg.passengers,"\n")

weekday_unique=unique(dataUber[c("day","month", "daysofweek")])
#average number of passengers that avail Uber trips in sunday
sun_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Sun'))/nrow(subset(weekday_unique,daysofweek=="Sun")))
#average number of passengers that avail Uber trips in monday
mon_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Mon'))/nrow(subset(weekday_unique,daysofweek=="Mon")))
#average number of passengers that avail Uber trips in tuesday
tue_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Tue'))/nrow(subset(weekday_unique,daysofweek=="Tue")))
#average number of passengers that avail Uber trips in wednesday
wed_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Wed'))/nrow(subset(weekday_unique,daysofweek=="Wed")))
#average number of passengers that avail Uber trips in thursday
thu_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Thu'))/nrow(subset(weekday_unique,daysofweek=="Thu")))
#average number of passengers that avail Uber trips in friday
fri_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Fri'))/nrow(subset(weekday_unique,daysofweek=="Fri")))
#average number of passengers that avail Uber trips in satday
sat_passengers<-as.integer(nrow(subset(dataUber, Status!="Cancelled"&daysofweek=='Sat'))/nrow(subset(weekday_unique,daysofweek=="Sat")))
unique(dataUber$daysofweek)
weekDay<-c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')
Avg<-c(sun_passengers,mon_passengers,tue_passengers,wed_passengers,thu_passengers,fri_passengers,sat_passengers)
t=data.frame(weekDay,Avg)

cat("Average number of passengers that avail Uber trips in Sunday=",sun_passengers,"\n")
cat("Average number of passengers that avail Uber trips in Monday=",mon_passengers,"\n")
cat("Average number of passengers that avail Uber trips in Tuesday=",tue_passengers,"\n")
cat("Average number of passengers that avail Uber trips in Wednesday=",wed_passengers,"\n")
cat("Average number of passengers that avail Uber trips in Thursday=",thu_passengers,"\n")
cat("Average number of passengers that avail Uber trips in Friday=",fri_passengers,"\n")
cat("Average number of passengers that avail Uber trips in Saturday=",sat_passengers,"\n")

t1<- t%>%group_by(weekDay)%>%
  dplyr::summarize(Avg)

ggplot(t1,aes(weekDay,Avg,fill=weekDay))+
  geom_bar(stat = "identity")+
  ggtitle("Avg trips of week of a day")+
  scale_fill_manual(values=c("#FFCCD2", "#FF9292", "#fc219a", "#f78228", "#c05cf2","#50d9d2", "#31d45c"))

###the peak hours when there's maximum traffic in the app
peakhours<- dataUber%>%group_by(hour)%>%
  dplyr::summarize(Total=n())

ggplot(peakhours,aes(x=hour,y=Total,fill=hour))+
  geom_bar(stat = "identity")+theme_light()+
  ggtitle("Trips in Every Hour")

#days with the highest number of trips in a month
ggplot(monthWeekday,aes(month,Total,fill=daysofweek))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Day And Month")+theme_light()+
  scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028",  "#FFE699","#FFF9B6",
                             "#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"))
ggplot(monthWeekday,aes(daysofweek,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Day And Month")+theme_light()+
  scale_fill_manual(values=c("#fc219a", "#FF9292", "#FFCCD2", "#f7f028",  "#FFE699","#FFF9B6",
                             "#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"))

