#Q2
## Read in data
drug<-read.table("~/Documents/2013 Fall/STAT656BioStatistics/HW1/CholDrug.txt",header=T)

## Create frequency table
table(drug$gender,drug$dose)

## Table with response means at different factor combination level
tapply(drug$Y,drug[,-1],mean)

#Q3
## Read in data
mitchell<-read.table("~/Documents/2013 Fall/STAT656BioStatistics/HW1/Mitchell.txt",header=T)

## Histogram
hist(mitchell$Temp, col='gray')
hist(mitchell$Temp,xlab='Temperature',main='Soil Temperature in Mitchell, Nebraska', breaks=c(-10,0,10,20, 30),col='gray')
hist(mitchell$Temp,xlab='Temperature',main='Soil Temperature in Mitchell, Nebraska', breaks=30,col='gray')
hist(mitchell$Temp,xlab='Temperature',main='Soil Temperature in Mitchell, Nebraska', breaks=15,col='gray')

## Line plot
plot(mitchell$Month,mitchell$Temp,type='l', xlab='Month',ylab='Soil Temperature',main='Soil Temperature over Month in Mitchell, Nebraska', col='blue')

#Q4
## Read in data
ozone<-read.csv("~/Documents/2013 Fall/STAT656BioStatistics/HW1/ozone.csv",header=T)

## Scatter plot
pairs(ozone[,-c(5:6)],main='Pairwise Scatterplots of Ozone Data in New York, 1973', col=c("grey", "red","blue","green"))

## Boxplots
boxplot(Ozone ~ Month,data=ozone,xlab='Month',ylab='Ozone',main='Box Plot by Ozone', col='grey')
boxplot(Solar.R~Month,data=ozone,xlab='Month',ylab='Solar',main='Box Plots by Solar.R', col='red')
boxplot(Wind~Month,data=ozone,xlab='Month',ylab='Wind',main='Box Plots by Wind', col='blue')
boxplot(Temp~Month,data=ozone,xlab='Month',ylab='Temp',main='Box Plots by Temp', col='green')

#Q5.
##3.13. p.54
### Create dataset
pull_strength<- c("Under 50", "Under 60", "Under 70", "Under 80", "Under 90", "Under 100", "Above 100")
cases_observed<- c(10,42,140,168,113,22,24)
pull<-cbind(data.frame(pull_strength),cases_observed)

### Intervals
interval<-cut(cases_observed, seq(from=45, to=105, by = 10)) 

### Quantiles
q25<- (25/100) * (1+sum(cases_observed))
q50<- (50/100) * (1+sum(cases_observed))
q75<- (75/100) * (1+sum(cases_observed))

### Mean, std, [mean, 1std]%
pull_interval<-(rep(seq(from=45,to=105,by=10),cases_observed))
mean<-mean(pull_interval)
std<-sd(pull_interval)

onestd_interval<-c(mean-std, mean+std)
onestd_percent<-(140+168+113)/519

#3.15. p.58
## Create dataset
ipge<-c(500,500,301,272,226,183,183,177,136,118,60,254,172,168,150,148,144,130,121,100,88)
ca<-c(13.3,11.2,13.4,11.5,11.4,11.6,11.7,12.1,12.5,12.2,18.0,10.1,9.4,9.3,8.6,10.5,10.3,10.5,10.2,9.7,9.2)
hyperca<-c(rep(c("Y"), 11), rep(c("N"), 10))
plasma<-data.frame(ipge,ca,hyperca)

## Mean and std
mean<-tapply(plasma$ipge,plasma$hyperca,mean)
std<-tapply(plasma$ipge,plasma$hyperca,sd)

## Box plots for plasma iPGE levels for each group
boxplot(ipge~hyperca, data=plasma, xlab='Hypercalcemia (N or Y)',ylab='Mean Plasma iPGE', main='Plasma Prostaglandin E (iPGE) Levels', col='yellow')

## Plot relations between plasma iPGE and serum calcium levels 
plot(plasma$ca,plasma$ipge,xlab='Mean Serum Calcium',ylab='Mean Plasma iPGE')
fit.plasma<-lm(ipge~ca, data=plasma)
abline(fit.plasma)
