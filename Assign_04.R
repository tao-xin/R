#Q1 Problem 6.14, p.198
## a. Mantel-Haenszel and chi-squared test 
smoke<-array(c(7,55,31,269,7,20,18,112,7,33,24,114,
               40,88,45,172,34,50,24,55,27,55,24,58,30,34,17,17), dim=c(2,2,7),
             dimnames=list(Cups_per_Day=c(">=5","<5"),Groups=c("MI","Control")))
mantelhaen.test(smoke)

## Testing for associaion between X and Y
coffee<-matrix(c(152,335,183,797), nrow=2)
chisq.test(coffee)

#Q2 UCBAdmissions
UCBAdmissions
as.data.frame(UCBAdmissions)

## build a data frame
UCB <- as.data.frame.table(UCBAdmissions["Admitted", ,])
UCB
names(UCB)[3] <- "admit"
UCB$reject <- as.data.frame.table(UCBAdmissions["Rejected", ,])$Freq
UCB$Gender <- relevel(UCB$Gender,ref = "Male")

## Bound last two columns to form response as categorical 0 and 1
admit <- "cbind(admit,reject) ~ Gender * Dept"

## glm function with interaction
UCB.glm <- glm(admit, family=binomial(logit), data=UCB)
anova(UCB.glm, test="Chisq")
summary(UCB.glm)

## glm function without interaction
UCB2.glm <- glm(cbind(admit,reject)~Gender + Dept,family=binomial(logit), data=UCB)
anova(UCB2.glm,test="Chisq")
summary(UCB2.glm)

#Q3
## Part(a)
chisq.test(c(78,71,87,86),p=c(1/4,1/4,1/4,1/4))
expected<-chisq.test(c(78,71,87,86),p=c(1/4,1/4,1/4,1/4))$expected
observed<-c(78,71,87,86)
chisq<-sum((expected-observed)^2/expected)
chisq

## Part(b)
chisq.test(c(40,19,40,43),p=c(1/4,1/4,1/4,1/4))

## Part(c)
chisq.test(c(50,48,46,34),p=c(1/4,1/4,1/4,1/4))
chisq.test(c(30,40,36,35),p=c(1/4,1/4,1/4,1/4))

## Part(d)
combined<-array(c(50,30,95,40,78,48,40,93,19,71,46,36,88,40,87,
                  34,35,83,43,86),dim=c(5,4))
chisq.test(combined)

#Q4
ownership<-matrix(c(583,139,524,145,24,59,86,24,74,182,31,145),nrow=3,
            dimnames=list(c("L/S","Lap Only","None"),
                          c("Individuals","Rental","Lease","Other Corporate")))
chisq.test(ownership)
chisq.test(ownership)$residual

ownership.pval<-pnorm(abs(chisq.test(ownership)$residual),lower.tail=F)*2
ownership.pval
ownership.pval*6

#Q5
airquality
airquality$Month <- factor(airquality$Month) 
summary(fm1<-aov(Ozone~Month, data=airquality))

TukeyHSD(fm1,"Month",ordered=TRUE)
plot(TukeyHSD(fm1, "Month"))
boxplot(Ozone~Month, data=airquality,xlab='Month',ylab='Ozone')



####################### Alternative Q2 ###########################
ftable(UCBAdmissions,row.vars="Dept",col.vars=c("Gender","Admit"))
margin.table(UCBAdmissions,2:1)
round(prop.table(margin.table(UCBAdmissions, 2:1),1),3)
UCB <- as.data.frame(UCBAdmissions)
library(reshape2)
UCB2 <- dcast(UCB, Gender + Dept ~ Admit, value.var="Freq")
options(contrasts=c("contr.treatment", "contr.poly"))
UCB.glm <- glm(cbind(Admitted, Rejected) ~ Dept + Gender, family=binomial, data=UCB2)
summary(UCB.glm)


