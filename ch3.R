library(readr)
library(ggplot2)
library(sjstats)
library(car)
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

#8
Auto <- read_csv("~/R/ISLR/Auto.csv")
Auto$horsepower <- as.numeric(Auto$horsepower)
Auto <- na.omit(Auto)

fit <- lm(data=Auto, mpg~horsepower)
summary(fit)

#i yes - Adjusted R-squared:  0.6049
#ii p-value: < 2.2e-16 is close to 0: relationship is strong
#iii horsepower  -0.157845 : negative
#iv 


new <- data.frame(horsepower = 98)
predict.lm(fit, new, se.fit = TRUE)
predict.lm(fit, new, se.fit = FALSE, interval = "confidence")
predict.lm(fit, new, se.fit = FALSE, interval = "prediction")

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

#b

fit <- lm(data=Auto, mpg~horsepower)
ggplot(data=Auto, aes(y=mpg, x=horsepower)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs horsepower") + 
  annotate("text", x = 150, y = 40, label = equation(fit), parse = TRUE)

#c
plot(fit$fitted.values, fit$residuals,
     pch = 16, col="red",
     main = "Residuals by Fitted Values",
     ylab = "Standarized Residuals",
     xlab = "Fitted Values")
abline(0,0)

# residuals vs fitted plot shows that the relationship is non-linear

#9

pairs(Auto[1:8])
cor(Auto[1:8])

Auto <- Auto[1:8]
fit <- lm(mpg~., data=Auto)
summary(fit)

# There is a relationship between predictors and response
# Multiple R-squared:  0.8215
# weight, year, origin and displacement have statistically significant relationships
# 0.75 coefficient for year suggests that later model year cars have better/higher mpg

plot(fit$fitted.values, fit$residuals,
     pch = 16, col="red",
     main = "Residuals by Fitted Values",
     ylab = "Standarized Residuals",
     xlab = "Fitted Values")
abline(0,0)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

# evidence of non-linearity
# observation # 14 has high leverage

fit <- lm(mpg~weight+year+origin+displacement, data=Auto)
summary(fit)
fit <- lm(mpg~weight+year+origin*displacement, data=Auto)
summary(fit)
fit <- lm(mpg~weight+displacement+origin*year, data=Auto)
summary(fit)
fit <- lm(mpg~year+displacement+origin*weight, data=Auto)
summary(fit)
fit <- lm(mpg~weight+origin+year*displacement, data=Auto)
summary(fit)
fit <- lm(mpg~origin+displacement+year*weight, data=Auto)
summary(fit)
fit <- lm(mpg~year+origin+displacement*weight, data=Auto)
summary(fit)
fit <- lm(mpg~year+weight+displacement*origin, data=Auto)
summary(fit)

# interaction have statistically significant relationships

fit1 <- lm(mpg~log(displacement)+weight+year+origin, data=Auto)
summary(fit1) #Multiple R-squared:  0.8216
fit2 <- lm(mpg~displacement^2+weight+year+origin, data=Auto)
summary(fit2) #Multiple R-squared:  0.818
fit3 <- lm(mpg~poly(displacement,2)+weight+year+origin, data=Auto)
summary(fit3) #Multiple R-squared:  0.8419
fit4 <- lm(mpg~poly(displacement,3)+weight+year+origin, data=Auto)
summary(fit4) #Multiple R-squared:  0.8445
fit9 <- lm(mpg~sqrt(displacement)+weight+year+origin, data=Auto)
summary(fit9) #Multiple R-squared:  0.8178

anova(fit,fit1,fit2,fit3,fit4)

fit5 <- lm(mpg~displacement+log(weight)+year+origin, data=Auto)
summary(fit5) #Multiple R-squared:  0.8425
fit6 <- lm(mpg~displacement+weight^2+year+origin, data=Auto)
summary(fit6) #Multiple R-squared:  0.8181
fit7 <- lm(mpg~displacement+poly(weight,2)+year+origin, data=Auto)
summary(fit7) #Multiple R-squared:  0.8519
fit8 <- lm(mpg~displacement+poly(weight,3)+year+origin, data=Auto)
summary(fit8) #Multiple R-squared:  0.852
fit10 <- lm(mpg~displacement+sqrt(weight)+year+origin, data=Auto)
summary(fit10) #Multiple R-squared:  0.8325

anova(fit,fit5,fit6,fit7,fit8)

anova(fit, fit3, fit7)

#10
data(Carseats)

fit <- lm(data=Carseats, Sales~Price+Urban+US)
summary(fit)
rse(fit)
summary(fit)$adj.r.squared
anova(fit)
#  RSE = [1] 2.472492
#  R^2 = [1] 0.2335123

#Sales: Unit sales (in thousands) at each location
#Price: Price company charges for car seats at each site 
#Urban: A factor with levels No and Yes to indicate whether the store is in an urban or rural location
#US: A factor with levels No and Yes to indicate whether the store is in the US or not

#Price (-0.054459): Sales are $54 lower for each dollar increase in Price - statistically significant
#UrbanYes (-0.021916): Sales are 22 lower for Urban locations - not statistically significant
#USYes (1.200573): Sales are $1,201 higher in the US locations - statistically significant

# Sales(y) = -0.054459 * Price + -0.021916 * UrbanYes + 1.200573 * USYes

#Price and USYes since the p-value is < 0.05

fit <- lm(data=Carseats, Sales~Price+US)
summary(fit)
rse(fit)
summary(fit)$adj.r.squared
anova(fit)
# RSE = [1] 2.469397
# R^2 = [1] 0.2354305

#This model has a slightly better/lowe) RSE value and one less predictor variable (less complex)

confint(fit)

plot(fit$fitted.values, fit$residuals,
     pch = 16, col="red",
     main = "Residuals by Fitted Values",
     ylab = "Standarized Residuals",
     xlab = "Fitted Values")
abline(0,0)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

leveragePlots(fit)  # leverage plots
plot(hatvalues(fit))

# 15
data(Boston)
any(is.na(Boston))

Boston$chas <- factor(Boston$chas, labels = c("N","Y"))


fit1 <- lm(data=Boston, crim~zn)
fit2 <- lm(data=Boston, crim~indus)
fit3 <- lm(data=Boston, crim~chas)
fit4 <- lm(data=Boston, crim~nox)
fit5 <- lm(data=Boston, crim~rm)
fit6 <- lm(data=Boston, crim~age)
fit7 <- lm(data=Boston, crim~dis)
fit8 <- lm(data=Boston, crim~rad)
fit9 <- lm(data=Boston, crim~tax)
fit10 <- lm(data=Boston, crim~ptratio)
fit11 <- lm(data=Boston, crim~black)
fit12 <- lm(data=Boston, crim~lstat)
fit13 <- lm(data=Boston, crim~medv)


summary(fit1)
summary(fit2) 
summary(fit3) # not significant
summary(fit4)
summary(fit5) 
summary(fit6) 
summary(fit7)
summary(fit8)
summary(fit9)
summary(fit10)
summary(fit11)
summary(fit12)
summary(fit13)

# extract p-value from model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") 
    stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

names(Boston)[-1]  # all the potential predictors

results <- combn(names(Boston), 2, 
                 function(x) { lmp(lm(Boston[, x])) }, 
                 simplify = FALSE)
vars <- combn(names(Boston), 2)
names(results) <- paste(vars[1,],vars[2,],sep="~")
results[1:13]  # p-values for response=crim

ggplot(data=Boston, aes(y=crim, x=chas)) +
  geom_point() +
  labs(title ="Scatterplot of crim vs chas") 

fit <- lm(data=Boston, crim~.)
summary(fit)

# Reject null hypothesis for the following:
#zn            0.044855   0.018734   2.394 0.017025 *  
#nox         -10.313535   5.275536  -1.955 0.051152 .  
#dis          -0.987176   0.281817  -3.503 0.000502 ***
#rad           0.588209   0.088049   6.680 6.46e-11 ***
#black        -0.007538   0.003673  -2.052 0.040702 *  
#lstat         0.126211   0.075725   1.667 0.096208 .  
#medv         -0.198887   0.060516  -3.287 0.001087 ** 

# Only a few predictors have statistically significant impact 
# when given the presence of other predictors.

results <- combn(names(Boston), 2, 
                 function(x) { coefficients(lm(Boston[, x])) }, 
                 simplify = FALSE)
(coef.uni <- unlist(results)[seq(2,26,2)])
(coef.multi <- coefficients(fit)[-1])
plot(coef.uni, coef.multi)

#d

fit1 <- lm(data=Boston, crim~poly(zn,3))
fit2 <- lm(data=Boston, crim~poly(indus,3))
fit4 <- lm(data=Boston, crim~poly(nox,3))
fit5 <- lm(data=Boston, crim~poly(rm,3))
fit6 <- lm(data=Boston, crim~poly(age,3))
fit7 <- lm(data=Boston, crim~poly(dis,3))
fit8 <- lm(data=Boston, crim~poly(rad,3))
fit9 <- lm(data=Boston, crim~poly(tax,3))
fit10 <- lm(data=Boston, crim~poly(ptratio,3))
fit11 <- lm(data=Boston, crim~poly(black,3))
fit12 <- lm(data=Boston, crim~poly(lstat,3))
fit13 <- lm(data=Boston, crim~poly(medv,3))


summary(fit1)
summary(fit2) 
summary(fit4)
summary(fit5) 
summary(fit6) 
summary(fit7)
summary(fit8)
summary(fit9)
summary(fit10)
summary(fit11)
summary(fit12)
summary(fit13)
