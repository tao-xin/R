#Q1
##a.lambda is the mean occurrence per interval, the probability of having x occurrences within a given interval
x<-0:15 #look at the first 15 probabilities
plot(dpois(x, 1),type='h',lwd=2,ylim=c(0,.4),xlab='Number of Occurrence',ylab='P(Y=y)', main="Poisson", col='blue')
par(new=TRUE) #Superimpose plot
plot(dpois(x, 5),type='h',lwd=2,ylim=c(0,.4),xlab='Number of Occurrence',ylab='P(Y=y)', col='green')
par(new=TRUE) #Superimpose plot
plot(dpois(x, 10),type='h',lwd=2,ylim=c(0,.4),xlab='Number of Occurrence',ylab='P(Y=y)', col='red')
legend(11, 0.4, c("blue=1", "green=5", "red=10"))
par(new=FALSE)

##b. t Distribution
x <- seq(-4, 4, .01)
normal.curve <- dnorm(x)
df <- c(1,5,50)
colors <- c("purple", "red", "green","black")
labels <- c("df=1", "df=5", "df=50","normal")
plot(x,normal.curve, lty=2, xlab='x', ylab='Density', main="t Distribution-Comparision of Different Degress of Freedom")
for (i in 1:3){lines(x, dt(x,df[i]), lwd=2, col=colors[i])}
legend(-4, 0.4, inset=.1, title="Distributions", labels, lwd=2, lty=c(1,1,1,2), col=colors)
par(new=FALSE)

##c. Chi-squared Distribution
x <- seq(0, 8)
df <- c(1, 5, 15)
colors <- c("purple", "red", "green")
labels <- c("df=1", "df=5", "df=15")
plot(x,dchisq(x, df[i]), type="n", ylim=c(0,0.4), xlab='x',ylab='Distribution',
     main="Chi-squared Distribution-Comparision of Different Degress of Freedom")
for (i in 1:3){lines(x, dchisq(x, df[i]), lwd=2,col=colors[i])}
legend(0, 0.42, inset=.1, title="Distributions", labels, lwd=2, lty=c(1,1,1), col=colors)

#Q2
##a.Ha: mean=2800
P_type2error <- pnorm(3481, mean = 2800, sd = 90.6)-pnorm(3119, mean = 2800, sd = 90.6)
P_type2error
power <- 1-P_type2error
power

##b.Ha: mean=3100
P_type2error <- pnorm(3481, mean = 3100, sd = 90.6)-pnorm(3119, mean = 3100, sd = 90.6)
P_type2error
power <- 1-P_type2error
power

##c.Ha: mean=3200
P_type2error <- pnorm(3481, mean = 3200, sd = 90.6)-pnorm(3119, mean = 3200, sd = 90.6)
P_type2error
power <- 1-P_type2error
power

##d.Ha: mean=3400
P_type2error <- pnorm(3481, mean = 3400, sd = 90.6)-pnorm(3119, mean = 3400, sd = 90.6)
P_type2error
power <- 1-P_type2error
power

##e.Ha: mean=2800
P_type2error <- pnorm(3481, mean = 3500, sd = 90.6)-pnorm(3119, mean = 3500, sd = 90.6)
P_type2error
power <- 1-P_type2error
power

##f.Ha: mean=3600
P_type2error <- pnorm(3481, mean = 3600, sd = 90.6)-pnorm(3119, mean = 3600, sd = 90.6)
P_type2error
power <- 1-P_type2error
power

#Q3
##a. t-test, CI and hypothesis testing
age=c(225,174,274,164,130,96,102,80,81,148,130,48,68,64,234,24,187,117,42,38,28,53,120,66,
      176,120,77,79,108,117,96,80,87,85,61,65,68,139,307,185,150,88,108,60,108,95,25,80,
      143,57,53,90,76,99,29,110,113,67,22,118,47,34,206,104,90,157,80,171,23,92,115,87,42,77,65,45,32,44)
t.test(age,mu=100)
t.test(age,mu=100,alternative='less')
t.test(age,mu=100,alternative='greater')

##b. Create a histogram
hist(age,breaks=seq(from=1,to=331,by=30),col='blue',main='Frequency Distribution of Age at Death 78 SIDS Cases') 

#Q4 5.11
nikel_Legion<-c(65, 24, 52, 86, 120, 82, 399, 87, 139)
nikel_control<-c(12, 10, 31, 6, 5, 5, 29, 9, 12)
t.test(nikel_Legion,nikel_control,paired=T)

#Q5
##a.box plots
pipeline<-read.table("~/Documents/2013 Fall/STAT656BioStatistics/HW2/pipeline.txt",header=T)
par(mfrow=c(1,2))
boxplot(Field ~ Batch, data=pipeline,xlab='Batch', ylab='Field',main='Box Plot - Field by Batch', col='green')
boxplot(Lab~ Batch, data=pipeline,xlab='Batch', ylab='Lab',main='Box Plot - Lab by Batch', col='red')

##b. 2-sample t-tests (2-sided) for Lab (alpha = .05)
b1<-pipeline[pipeline$Batch==1,]
b2<-pipeline[pipeline$Batch==2,]
b3<-pipeline[pipeline$Batch==3,]
b4<-pipeline[pipeline$Batch==4,]
b5<-pipeline[pipeline$Batch==5,]
b6<-pipeline[pipeline$Batch==6,]

t.test(b1$Lab,b2$Lab)
t.test(b1$Lab,b3$Lab)
t.test(b1$Lab,b4$Lab)
t.test(b1$Lab,b5$Lab)
t.test(b1$Lab,b6$Lab)
t.test(b2$Lab,b3$Lab)
t.test(b2$Lab,b4$Lab)
t.test(b2$Lab,b5$Lab)
t.test(b2$Lab,b6$Lab)
t.test(b3$Lab,b4$Lab)
t.test(b3$Lab,b5$Lab)
t.test(b3$Lab,b6$Lab)
t.test(b4$Lab,b5$Lab)
t.test(b4$Lab,b6$Lab)
t.test(b5$Lab,b6$Lab)

##c. Compare Field values vs. Lab values. Perform the test (alpha = .10) 
t.test(pipeline$Field,pipeline$Lab, alpha = .10)
