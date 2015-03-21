#Q1
## residual plot
airquality
fit.aq <- lm(Ozone ~ Solar.R + Wind + Temp,data=airquality)
summary(fit.aq)
plot(fitted(fit.aq),resid(fit.aq)); abline(h=0,lty=2)
qqnorm(resid(fit.aq)); qqline(resid(fit.aq))

## sqrt transform
fit_sqrt.aq <- lm(sqrt(airquality$Ozone) ~ Solar.R + Wind + Temp,data=airquality)
summary(fit_sqrt.aq)
plot(fitted(fit_sqrt.aq),resid(fit_sqrt.aq)); abline(h=0,lty=2)
qqnorm(resid(fit_sqrt.aq)); qqline(resid(fit_sqrt.aq))

## log transform
fit_log.aq <- lm(log(airquality$Ozone) ~ Solar.R + Wind + Temp,data=airquality)
summary(fit_log.aq)
plot(fitted(fit_log.aq),resid(fit_log.aq)); abline(h=0,lty=2)
qqnorm(resid(fit_log.aq)); qqline(resid(fit_log.aq))
