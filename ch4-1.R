library(ggplot2)
library(dplyr)


data(Default)
str(Default)

Default.s <- Default %>%  filter(student == "Yes")
Default.ns <- Default %>%  filter(student == "No")


ggplot(data=Default, aes(x=balance,y=income, color=default)) +
  geom_point()

ggplot(data=Default, aes(x=balance,y=income, color=default, shape=student)) +
  geom_point()
ggplot(data=Default.s, aes(x=balance,y=income, color=default)) +
  geom_point()
ggplot(data=Default.ns, aes(x=balance,y=income, color=default)) +
  geom_point()

ggplot(data=Default, aes(x=default,y=balance)) +
  geom_boxplot()

ggplot(data=Default, aes(x=default,y=income)) +
  geom_boxplot()

fit <- glm(default~balance, data=Default, family=binomial)
summary(fit)
fit <- glm(default~student, data=Default, family=binomial)
summary(fit)
fit <- glm(default~income, data=Default, family=binomial)
summary(fit)
fit <- glm(default~balance+student+income, data=Default, family=binomial)
summary(fit)
# AIC: 1579.5

Default$fit.prob <- fit.prob <- predict(fit, Default, type="response")
Default$fit.pred <- fit.pred <- ifelse(fit.prob > 0.5, "Yes", "No")
table(fit.pred, Default$default)
mean(fit.pred == Default$default)  # Accuracy= [1] 0.9732

ggplot(data=Default, aes(x=balance, y=fit.prob, color=student)) +
  geom_point()

ggplot(data=Default, aes(x=balance, y=fit.prob, color=fit.pred)) +
  geom_point()

ggplot(data=Default, aes(x=income, y=fit.prob, color=student)) +
  geom_point()

ggplot(data=Default, aes(x=income, y=fit.prob, color=fit.pred)) +
  geom_point()


fit <- glm(default~balance+income, data=Default, family=binomial)
summary(fit)

Default$fit.prob <- fit.prob <- predict(fit, Default, type="response")
Default$fit.pred <- fit.pred <- ifelse(fit.prob > 0.5, "Yes", "No")
table(fit.pred, Default$default)
mean(fit.pred == Default$default)  # Accuracy= [1] 0.9737
# AIC: 1585

fit <- glm(default~balance+student, data=Default, family=binomial)
summary(fit)
#AIC: 1577.7  (BEST AIC)

Default$fit.prob <- fit.prob <- predict(fit, Default, type="response")
Default$fit.pred <- fit.pred <- ifelse(fit.prob > 0.5, "Yes", "No")
table(fit.pred, Default$default)
mean(fit.pred == Default$default)  # Accuracy= [1] 0.9733

fit <- glm(default~income+student, data=Default, family=binomial)
summary(fit)
#AIC: 2913.5

Default$fit.prob <- fit.prob <- predict(fit, Default, type="response")
Default$fit.pred <- fit.pred <- ifelse(fit.prob > 0.5, "Yes", "No")
table(fit.pred, Default$default)
mean(fit.pred == Default$default)  # Accuracy= [1] 0.9667

set.seed(1)
set.seed(2)
set.seed(3)
set.seed(4)

train <- sample(nrow(Default), nrow(Default)*0.50)
fit2 <- glm(default ~ income + balance, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)  
# [1] 0.028
[1] 0.0276
[1] 0.0248
[1] 0.0258
[1] 0.0262

train <- sample(nrow(Default), nrow(Default)*0.50)
fit2 <- glm(default ~ income + balance + student, data=Default, family=binomial, subset=train)
prob2 <- predict(fit2, Default[-train,], type="response")
pred2 <- ifelse(prob2 > 0.5, "Yes", "No")
table(pred2, Default[-train,]$default)
mean(Default[-train,]$default != pred2)  

[1] 0.0274

set.seed(1)

fit <- glm(default~income+balance, data=Default, family=binomial)
summary(fit)

set.seed(1)
boot.fn <- function(df, trainid) {
  return(coef(glm(default ~ income + balance, data=df, family=binomial, subset=trainid)))
}
boot.fn(Default, 1:nrow(Default))  # check match with summary

require(boot)
boot(Default, boot.fn, R=100)
