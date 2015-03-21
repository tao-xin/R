#Q1
## a1.exact binimial test
binom.test(x=82, n=82+118, p=.5)

## a2.single-sample propotion test
prop.test(x=82, n=82+118, p=.5)

## b1.exact binimial test
binom.test(x=156, n=156+121, p=0.53, alternative="greater",conf.level=.90)

## b2.single-sample propotion test
prop.test(x=156, n=156+121, p=0.53, alternative="greater",conf.level=.90)

## two-sample proportions test
prop.test(x=c(118,156), n=c(82+118,156+121), conf.level=.95)

#Q2
## a. matrix forms and McNemar's test
nonwhites <-
matrix(c(3, 11, 3, 9),
       nrow = 2,
       dimnames = list("Control" = c("Yes", "No"),
                       "Case" = c("Yes", "No")))
nonwhites
mcnemar.test(nonwhites)

married <-
matrix(c(8,41,10,46),
       nrow = 2,
       dimnames = list("Control" = c("Yes", "No"),
                       "Case" = c("Yes", "No")))
married
mcnemar.test(married)

age <-
matrix(c(5,7,33,57),
       nrow = 2,
       dimnames = list("Control" = c("Yes", "No"),
                       "Case" = c("Yes", "No")))
age
mcnemar.test(age)

## b & c. odds ration to estimate relative risk & 90% CI for the relative risk

### for subset nonwhite
OddsRatio_nonwhites <- function(nonwhites,alpha=0.1,referencerow=2)
{
  numrow <- nrow(nonwhites)
  myrownames <- rownames(nonwhites)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    n21 <- nonwhites[referencerow,1]
    n22 <- nonwhites[referencerow,2]
    if (i != referencerow)
    {
      n11 <- nonwhites[i,1]
      n12 <- nonwhites[i,2]
      
      # calculate the odds ratio
      pairedoddsratio <- n21 / n12
      print(paste("category =", rowname, ", odds ratio = ", pairedoddsratio))
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- (1 + pairedoddsratio) * sqrt(pairedoddsratio/(n12+n21))
      
      # sigma is the standard error of estimate of log of odds ratio
      z <- qnorm(1-(alpha/2))
      lowervalue <- pairedoddsratio - (z * sigma)
      uppervalue <- pairedoddsratio + ( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
}

### for subset married
OddsRatio_married <- function(married,alpha=0.1,referencerow=2)
  {
    numrow <- nrow(married)
    myrownames <- rownames(married)
    for (i in 1:numrow)
    {
      rowname <- myrownames[i]
      n21 <- married[referencerow,1]
      n22 <- married[referencerow,2]
      if (i != referencerow)
      {
        n11 <- married[i,1]
        n12 <- married[i,2]
        
        # calculate the odds ratio
        pairedoddsratio <- n21 / n12
        print(paste("category =", rowname, ", odds ratio = ", pairedoddsratio))
        
        # calculate a confidence interval
        confidenceLevel <- (1 - alpha)*100
        sigma <- (1 + pairedoddsratio) * sqrt(pairedoddsratio/(n12+n21))
        
        # sigma is the standard error of estimate of log of odds ratio
        z <- qnorm(1-(alpha/2))
        lowervalue <- pairedoddsratio - (z * sigma)
        uppervalue <- pairedoddsratio + ( z * sigma)
        print(paste("category =", rowname, ", ", confidenceLevel,
                    "% confidence interval = [",lowervalue,",",uppervalue,"]"))
      }
    }
  } 

### for subset age
OddsRatio_age <- function(age,alpha=0.1,referencerow=2)
{
  numrow <- nrow(age)
  myrownames <- rownames(age)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    n21 <- age[referencerow,1]
    n22 <- age[referencerow,2]
    if (i != referencerow)
    {
      n11 <- age[i,1]
      n12 <- age[i,2]
      
      # calculate the odds ratio
      pairedoddsratio <- n21 / n12
      print(paste("category =", rowname, ", odds ratio = ", pairedoddsratio))
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- (1 + pairedoddsratio) * sqrt(pairedoddsratio/(n12+n21))
      
      # sigma is the standard error of estimate of log of odds ratio
      z <- qnorm(1-(alpha/2))
      lowervalue <- pairedoddsratio - (z * sigma)
      uppervalue <- pairedoddsratio + ( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
}
OddsRatio_nonwhites(nonwhites, alpha=0.10)
OddsRatio_married(married, alpha=0.10)
OddsRatio_age(age, alpha=0.10)

#Q3
## a. contingency table
tree<-read.table("~/Documents/2013 Fall/STAT656BioStatistics/HW4/treedata.txt",header=T)
tree
table<-table(tree)
table
contingtable<-addmargins(table)
contingtable

## b. logistic regression
tree.glm<-glm(y~SPP,family=binomial,data=tree)
summary(tree.glm)

## c. conditional probability is the same using a and b
### using part a
pi0_a <- 306/436
pi1_a <- 90/497
pi0_a
pi1_a

### using part b
pi0_b <- 1/(1+exp(-(0.8561)))
pi1_b <- 1/(1+exp(-(0.8561-2.3651*1)))
pi0_b
pi1_b
 
#Q4
## a. odds ratio
OddsRatio <- (130*90)/(306*407)
OddsRatio

LogCoefficient <- -2.3651
Exp_LogCoeffient <- exp(LogCoefficient)
Exp_LogCoeffient

## b. 95% CI for odds ratio
n11 <- 130 
n12 <- 306 
n21 <-407 
n22 <- 90
alpha <- 0.05
confidenceLevel <- (1 - alpha)*100

### sigma is the standard error of estimate of log of odds ratio
sigma <- sqrt((1/n11) + (1/n12) + (1/n21) + (1/n22))
z <- qnorm(1-(alpha/2))
lowervalue <- OddsRatio * exp(-z * sigma)
uppervalue <- OddsRatio * exp( z * sigma)
print(paste(confidenceLevel,
            "% confidence interval = [",lowervalue,",",uppervalue,"]"))

## c.
sqrt((1/n11) + (1/n12) + (1/n21) + (1/n22))

#Q5
## a. fit model with main effect only
admit<-read.csv("~/Documents/2013 Fall/STAT656BioStatistics/HW4/admitdata.csv", header=T)
admit.glm<-glm(admit~ . , family=binomial, data=admit)
summary(admit.glm)

## b. interpretation
alpha <- -4.949378
beta1 <- 0.002691
beta2 <- 0.754687

x1 <- 380
x2 <- 3.6099999
pi_x <- 1 / (1 + exp(-(alpha + beta1 * x1 + beta2 * x2)))
pi_x

x1 <- 700
x2 <- 3.92000008
pi_x <- 1 / (1 + exp(-(alpha + beta1 * x1 + beta2 * x2)))
pi_x


Q2 Alternative 
RelativeRisk <- function(nonwhites,alpha=0.05,referencerow=2)
{
  numrow <- nrow(nonwhites)
  myrownames <- rownames(nonwhites)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    n21 <- nonwhites[referencerow,1]
    n22 <- nonwhites[referencerow,2]
    if (i != referencerow)
    {
      n11 <- nonwhites[i,1]
      n12 <- nonwhites[i,2]
      n1. <- n11 + n12
      n2. <- n21 + n22
      probN11GivenN1. <- n11/n1.
      probN21GivenN2. <- n21/n2.
      
      # calculate the relative risk
      relativeRisk <- probN11GivenN1. / probN21GivenN2.
      print(paste("category =", rowname, ", relative risk = ",relativeRisk))
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- sqrt((1/n11) - (1/n1.) +
                      (1/n21) - (1/n2.))
      
      # sigma is the standard error of estimate of log of relative risk
      z <- qnorm(1-(alpha/2))
      lowervalue <- relativeRisk * exp(-z * sigma)
      uppervalue <- relativeRisk * exp( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
}


# c. 90% confidence interval
## nonwhite
## lrr-log of the risk ration which is asymptotically normal with variance vlrr
lrr <- log ((n11/n22) /(n21/n12)]
vlrr <- 1/n11 + 1/n12 + 1/n21 +1/n22

lrr <- log ((3*9) /(11*3))
vlrr <- 1/3 + 1/3 + 1/11 +1/9
z <- qnorm(1-(0.1/2))

## so an approximate 90% CI for the odds ratio is
lb <- exp (lrr - 1.644854 *sqrt(vlrr))
hb <- exp (lrr + 1.644854 * sqrt(vlrr))
lb
hb

## married
lrr <- log ((8*46) /(41*10))
vlrr <- 1/8 + 1/10 + 1/41 +1/46
lb <- exp (lrr - 1.644854 *sqrt(vlrr))
hb <- exp (lrr + 1.644854 * sqrt(vlrr))
lb
hb

## age
lrr <- log ((5*57) /(7*33))
vlrr <- 1/5 + 1/33 + 1/7 +1/57
lb <- exp (lrr - 1.644854 *sqrt(vlrr))
hb <- exp (lrr + 1.644854 * sqrt(vlrr))
lb
hb
