##### STAT656 Biostatistics Final #####
##### December 9, 2013 Fall #####
##### Xin Tao #####

# Q1. Health Plan and Drug Cost
## Read data into a table 
drug<- read.table("drugcost.txt", header=T)
drug
drug2<-subset(drug,select=-c(ID)) #drop the location variable
drug2

## Preliminary examination of the data 
is.data.frame(drug2)
summary(drug2)
pairs(drug2, main='Scatterplot Matrix')

## Fit full model with all variables in
fit_full <- lm (COST ~ . , data=drug2)
summary(fit_full)

## Fit model with with Minnesota values exculded
drug3<-subset(drug,ID!="MN1") #drop MN1
drug4<-subset(drug3,ID!="MN2") #drop MN2
drug5<-subset(drug4,ID!="MN3") #drop MN3
drug6<-subset(drug5,select=-c(ID)) #drop the location variable
drug6

fit_nomin <- lm (COST ~ . , data=drug6)
summary(fit_nomin)

## Fit model with with only RXPM, GS, RI in
fit_RGR <- lm (COST ~ RXPM + GS + RI , data=drug6)
summary(fit_RGR)

## Residual plot and Q-Q plot
par(mfrow=c(1,2))
plot(fitted(fit_RGR), resid(fit_RGR), main="Residuals vs. Fitted Values (Y)", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="3")
abline(h=0, lty=2)
qqnorm(resid(fit_RGR), main="Normal Q-Q Plot (Y)", col="3")
qqline(resid(fit_RGR))

# Q2. Hydrocarbon Emmisions 
## Read data into a table 
sniffer<- read.table("sniffer.txt", header=T)
sniffer

## Scatter plots of all pairs of variables
is.data.frame(sniffer)
summary(sniffer)
pairs(sniffer, main='Scatterplot Matrix for Sniffer Data')

## Check residual patterns for each variable
fit_TT <- lm (Y ~ TankTemp, data=sniffer)
fit_GT <- lm (Y ~ GasTemp, data=sniffer)
fit_TP <- lm (Y ~ TankPres, data=sniffer)
fit_GP <- lm (Y ~ GasPres, data=sniffer)
par(mfrow=c(2,2))
plot(sniffer$TankTemp, resid(fit_TT), main="Tank Temp. vs. Fitted Values (Y)", 
     xlab="Tank Temperature", ylab="Residuals", pch=19, col="2")
abline(h=0, lty=2)
plot(sniffer$GasTemp, resid(fit_GT), main="Gas Temp. vs. Fitted Values (Y)", 
     xlab="Gasoline Temperature", ylab="Residuals", pch=19, col="4")
abline(h=0, lty=2)
plot(sniffer$TankPres, resid(fit_TP), main="Tank Pres. vs. Fitted Values (Y)", 
     xlab="Tank Pressure", ylab="Residuals", pch=19, col="4")
abline(h=0, lty=2)
plot(sniffer$GasPres, resid(fit_GP), main="Gas Pres. vs. Fitted Values (Y)", 
     xlab="Gasoline Pressure", ylab="Residuals", pch=19, col="4")
abline(h=0, lty=2)

## Fit a reduce model with only GasTemp, TankPress, GasPress in
fit_reduce <- lm (Y ~ GasTemp + TankPres + GasPres , data=sniffer)
summary(fit_reduce)

## Fit full model with all variables in
fit_full <- lm (Y ~ . , data=sniffer)
summary(fit_full)

## Calculates all possible regessions for subset selection
par(mfrow=c(1,1))
allpossregs(Y ~ . , data=sniffer, Cp.plot = True)

## Stepwise regression, both direction, starting at full model
full.step <- step(fit_full, direction='both')
full.step <- step(fit_full, direction='forward')

## Residual plot and Q-Q plot of the full model
par(mfrow=c(1,2))
plot(fitted(fit_full), resid(fit_full), main="Residuals vs. Fitted Values (Y)", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="4")
abline(h=0, lty=2)
qqnorm(resid(fit_full), main="Normal Q-Q Plot (Y)", col="4")
qqline(resid(fit_full))

## Residual plot and Q-Q plot of the reduced model
par(mfrow=c(1,2))
plot(fitted(fit_reduce), resid(fit_reduce), main="Residuals vs. Fitted Values (Y)", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="2")
abline(h=0, lty=2)
qqnorm(resid(fit_reduce), main="Normal Q-Q Plot (Y)", col="2")
qqline(resid(fit_reduce))

# Q3. Walleye Data
## Read data into a table 
fish<- read.table("walleye.txt", header=T)
head(fish)
summary(fish)
is.data.frame(fish)

## (a).
### Subsetting rows to select period=1 and 2
sub12 <- subset(fish, period < 3)

### add a logical variable pperiod indicating the vector of period
pperiod <- sub12$period > 1
pperiod

## correlation matrix
cor(sub12)

### Fit a logistic regression model
sub12.glm <- glm(pperiod ~ age * length, family=binomial, data=sub12)
anova(sub12.glm, test="Chisq")
summary(sub12.glm)

### Interpretation
alpha <- 0.8170728
beta1 <- 1.4926490
beta2 <- -0.0070069
beta3 <- -0.0027623

Exp_beta1 <- exp(beta1)
Exp_beta1
Exp_beta2 <- exp(beta2)
Exp_beta2
Exp_beta3 <- exp(beta3)
Exp_beta3

x1 <- 1
x2 <- 184.30996
pi_x <- 1 / (1 + exp(-(alpha + beta1*x1 + beta2*x2 + beta3* x1*x2)))
pi_x

## (b).
### Subsetting rows to select period=2 and 3
sub23 <- subset(fish, period > 1)

### add a logical variable pperiod indicating the vector of period
pperiod <- sub23$period > 2
pperiod

### Fit a logistic regression model
sub23.glm <- glm(pperiod ~ age * length, family=binomial, data=sub23)
anova(sub23.glm, test="Chisq")
summary(sub23.glm)

### Interpretation
alpha <- -2.2446907 
beta1 <- 1.1393658
beta2 <- -0.0019384 
beta3 <- -0.0018147

Exp_beta1 <- exp(beta1)
Exp_beta1

x1 <- 1
x2 <- 184.30996
pi_x <- 1 / (1 + exp(-(alpha + beta1*x1 + beta2*x2 + beta3* x1*x2)))
pi_x

## (c).
### Divide period into pre-1990 and post-1990
pperiod <- fish$period > 1
pperiod

### Fit a logistic regression model
sub1990.glm <- glm(pperiod ~ age * length, family=binomial, data=fish)
anova(sub1990.glm, test="Chisq")
summary(sub1990.glm)

### Interpretation
alpha <- 1.274495
beta1 <- 1.682239
beta2 <- -0.009466
beta3 <- -0.002788

Exp_beta1 <- exp(beta1)
Exp_beta1

x1 <- 1
x2 <- 184.30996
pi_x <- 1 / (1 + exp(-(alpha + beta1*x1 + beta2*x2 + beta3* x1*x2)))
pi_x

# Q4. Cancer Data
## a. Mantel-Haenszel and chi-squared test 
cancer<-array(c(11,10,25,27,16,12,4,20,14,7,5,12,6,10,9,3,6,0,10,8,1,0,3,4,4,7,2,1),
      dim=c(2,2,7), dimnames=list(Exposure=c("High","Low"),Cancer=c("Yes","No")))
cancer
mantelhaen.test(cancer)

## b. Testing for associaion between X and Y
cancer.sum <- apply(cancer, c(1,2), sum) # combine tables
cancer.sum
is.matrix(cancer.sum)
chisq.test(cancer.sum)

# Q5. Plasma Data
## Read data into a table 
plasma<- read.table("plasma.txt", header=T)
plasma
is.data.frame(plasma)

## a. Betaplasma between sex
### The t-test of Betaplasma between male and female
is.numeric(plasma$SEX)
male <- subset(plasma, SEX < 2)
female <- subset(plasma, SEX > 1)
boxplot(male$BETAPLASMA, female$BETAPLASMA, ylab="BETTAPLASMA", 
        names=c("Male","Female"), main='Boxplot of BETAPLASMA by SEX')
var.test(male$BETAPLASMA,female$BETAPLASMA)
t.test(male$BETAPLASMA,female$BETAPLASMA,var.equal=T)

### Nonparametric test
wilcox.test(male$BETAPLASMA,female$BETAPLASMA, exact=F)

## b. Betaplasma and Smokestat
### The t-test of Betaplasma for subjects in different smoke status
is.numeric(plasma$SMOKSTAT)
never <- subset(plasma, SMOKSTAT< 2)
former <- subset(plasma, SMOKSTAT > 1 & SMOKSTAT < 3)
current <- subset(plasma,SMOKSTAT > 2)
boxplot(never$BETAPLASMA, former$BETAPLASMA, current$BETAPLASMA, ylab="BETTAPLASMA", 
        names=c("Never","Former","Current"), main='Boxplot of BETAPLASMA by SMOKING STATUS')
library(car)
leveneTest(BETAPLASMA~factor(SMOKSTAT), center=mean, data=plasma)
anova(lm(BETAPLASMA~factor(SMOKSTAT),data=plasma))

### Nonparametric test
kruskal.test(BETAPLASMA~factor(SMOKSTAT),data=plasma)

## c. Multiple Regression
### Betaplasma
fit_betaplasma<-lm(BETAPLASMA ~ AGE+factor(SEX)+factor(SMOKSTAT)+QUETELET+factor(VITUSE)
                  +CALORIES+FAT+FIBER+ALCOHOL+CHOLESTEROL+BETADIET+RETDIET, data=plasma)
summary(fit_betaplasma)

### Residual plot and Q-Q plot
par(mfrow=c(1,2))
plot(fitted(fit_betaplasma), resid(fit_betaplasma), main="Residuals vs. Fitted Values (Y)", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="5")
abline(h=0, lty=2)
qqnorm(resid(fit_betaplasma), main="Normal Q-Q Plot (Y)", col="5")
qqline(resid(fit_betaplasma))

### Fit logarithm transformation on Y
fit_bplog<-lm(log(BETAPLASMA+1)~AGE+factor(SEX)+factor(SMOKSTAT)+QUETELET+factor(VITUSE)
              +CALORIES+FAT+FIBER+ALCOHOL+CHOLESTEROL+BETADIET+RETDIET, data=plasma)
summary(fit_bplog)

### Residual plot and Q-Q plot
par(mfrow=c(1,2))
plot(fitted(fit_bplog), resid(fit_bplog), main="Residuals vs. Fitted Values (log(Y))", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="6")
abline(h=0, lty=2)
qqnorm(resid(fit_bplog), main="Normal Q-Q Plot (log(Y))", col="6")
qqline(resid(fit_bplog))

### Retplasma
fit_retplasma<-lm(RETPLASMA ~ AGE+factor(SEX)+factor(SMOKSTAT)+QUETELET+factor(VITUSE)
                   +CALORIES+FAT+FIBER+ALCOHOL+CHOLESTEROL+BETADIET+RETDIET, data=plasma)
summary(fit_retplasma)

### Residual plot and Q-Q plot
par(mfrow=c(1,2))
plot(fitted(fit_retplasma), resid(fit_retplasma), main="Residuals vs. Fitted Values (Y)", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="orange")
abline(h=0, lty=2)
qqnorm(resid(fit_retplasma), main="Normal Q-Q Plot (Y)", col="orange")
qqline(resid(fit_retplasma))

### Fit logarithm transformation on Y
fit_rplog<-lm(log(RETPLASMA)~AGE+factor(SEX)+factor(SMOKSTAT)+QUETELET+factor(VITUSE)
              +CALORIES+FAT+FIBER+ALCOHOL+CHOLESTEROL+BETADIET+RETDIET, data=plasma)
summary(fit_rplog)

### Residual plot and Q-Q plot
par(mfrow=c(1,2))
plot(fitted(fit_bplog), resid(fit_bplog), main="Residuals vs. Fitted Values (log(Y))", 
     xlab="Fitted Values", ylab="Residuals", pch=19, col="darkgreen")
abline(h=0, lty=2)
qqnorm(resid(fit_bplog), main="Normal Q-Q Plot (log(Y))", col="darkgreen")
qqline(resid(fit_bplog))

##### End #####
