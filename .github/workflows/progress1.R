library(readxl)
library(lubridate)
library(dplyr)
library(ISLR)
library(corrplot)
library(caret)
library(nnet)


Data <- read.csv("C:/Users/Thomas/Desktop/DATA/RR/P-Data1.csv",header = T, stringsAsFactors = F)
Data1 <-select(Data,CRIME.CATEGORY,Gender,Age.Group,Distance.Category,Time2,Month)
attach(Data1)
View(Data1)
##Forming dummy variables
Data1$Crime <- as.factor(Data1$Crime)

#Gender

Data1$Gender <- as.factor(Data1$Gender)
Gender.Female <- ifelse(Gender == "Female",0,1)
Gender.Female
#Month
Data1$Month <- as.factor(Data1$Month)
Month.Midmonth <- ifelse(Month == "Mid Month",1,0)
Month.EndMonth <- ifelse(Month == "End Month",1,0)

#Time
Data1$Time2 <- as.factor(Data1$Time2)
Time2.EarlyMorning <- ifelse(Time2 == "Early Morning",1,0)
Time2.LateyMorning <- ifelse(Time2 == "Late Morning",1,0)
Time2.Afternoon <- ifelse(Time2 == "Afternoon",1,0)
Time2.EarlyNight <- ifelse(Time2 == "Early Night",1,0)
Time2.Night <- ifelse(Time2 == "Night",1,0)
Time2.LateNight <- ifelse(Time2 == "Late Night",1,0)

for(i in 1:nrow(Data1)){
  if (Data1$CRIME.CATEGORY[i]=="NON-VIOLENT CRIMES"){
    Data1$CRIME.CATEGORY[i]<-0
  }
  else{
    Data1$CRIME.CATEGORY[i]<-1
    
  }
}
Data1$CRIME.CATEGORY<-as.numeric(Data1$CRIME.CATEGORY)

Model<-glm(CRIME.CATEGORY ~ Gender+Age.Group+Distance.Category+Time2+Month,
           data = Data1,family = binomial)
Model
summary(Model)
library(fmsb)
libry(VIF)
VIF
library(cars)
as.data.frame(Model)
sum(is.na(Data1))
xtabs(~CRIME.CATEGORY+rank ,data=Data1)
load(VIF)
library(VIF)
library(CARS)
install.packages("fmbs")
library("fmsb")