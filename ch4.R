library(ggplot2)
library(sjstats)
library(car)

data(Weekly)
str(Weekly)
cor(Weekly[1:8])

# Volume and Year are positively correlated 0.84194162

ggplot(data=Weekly, aes(x=Volume, y=Year)) +
  geom_point()

fit <- glm(Direction~Volume+Lag1+Lag2+Lag3+Lag4+Lag5, data=Weekly, family=binomial)
summary(fit)

# Lag2 appears to be statistically significant

logit.prob <- predict(fit, Weekly, type="response")
logit.pred <- ifelse(logit.prob > 0.5, "Up", "Down")
table(logit.pred, Weekly$Direction)
mean(logit.pred == Weekly$Direction)  # Accuracy=0.56


train.yrs <- Weekly$Year %in% (1990:2008)
train <- Weekly[train.yrs,]
test <- Weekly[!train.yrs,]

fit2 <- glm(Direction~Lag2, data=train, family=binomial)
fit2.prob <- predict(fit2, test, type="response")
fit2.pred <- ifelse(fit2.prob > 0.5, "Up", "Down")
table(fit2.pred, test$Direction)
mean(fit2.pred == test$Direction)  # Accuracy=0.625

#LDA
fit.lda <- lda(Direction~Lag2, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
table(fit.lda.pred, test$Direction)
mean(fit.lda.pred == test$Direction)  # Accuracy=0.625

#QDA

fit.qda <- qda(Direction~Lag2, data=train)
fit.qda.pred <- predict(fit.qda, test)$class
table(fit.qda.pred, test$Direction)
mean(fit.qda.pred == test$Direction)  # Accuracy=0.587

#KNN
require(class)
set.seed(1)
train.X <- as.matrix(train$Lag2)
test.X <- as.matrix(test$Lag2)
knn.pred <- knn(train.X, test.X, train$Direction, k=1)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.5

#Best model LDA and Logistic Regression Accuracy=0.625



knn.pred <- knn(train.X, test.X, train$Direction, k=5)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.529

knn.pred <- knn(train.X, test.X, train$Direction, k=10)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.596

knn.pred <- knn(train.X, test.X, train$Direction, k=15)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.587

knn.pred <- knn(train.X, test.X, train$Direction, k=20)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.577

knn.pred <- knn(train.X, test.X, train$Direction, k=25)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.538

knn.pred <- knn(train.X, test.X, train$Direction, k=30)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  # Accuracy=0.567

data(Auto)
Auto <- Auto[1:8]
Auto$mpg01 <- Auto$mpg > median(Auto$mpg)

cor_table <- cor(Auto)
cor_table$mpg01
#cylinders, displacement, horsepower, weight

ggplot(data=Auto, aes(x=cylinders, fill=mpg01)) +
  geom_histogram(position = "fill")

ggplot(data=Auto, aes(x=displacement, fill=mpg01)) +
  geom_histogram(position = "fill")

ggplot(data=Auto, aes(x=horsepower, fill=mpg01)) +
  geom_histogram(position = "fill")

ggplot(data=Auto, aes(x=weight, fill=mpg01)) +
  geom_histogram(position = "fill")

ggplot(data=Auto, aes(x=acceleration, fill=mpg01)) +
  geom_histogram(position = "fill")

ggplot(data=Auto, aes(x=year, fill=mpg01)) +
  geom_histogram(position = "fill")

ggplot(data=Auto, aes(x=origin, fill=mpg01)) +
  geom_histogram(position = "fill")

set.seed(1)
trainid <- sample(1:nrow(Auto), nrow(Auto)*0.7 , replace=F)  # 70% train, 30% test
train <- Auto[trainid,]
test <- Auto[-trainid,]

#cylinders, displacement, horsepower, weight

fit.lda <- lda(mpg01~cylinders+displacement+horsepower+weight, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
table(fit.lda.pred, test$mpg01)
mean(fit.lda.pred == test$mpg01)  # Accuracy=0.924

fit.qda <- qda(mpg01~cylinders+displacement+horsepower+weight, data=train)
fit.qda.pred <- predict(fit.qda, test)$class
table(fit.qda.pred, test$mpg01)
mean(fit.qda.pred == test$mpg01)  # Accuracy=0.898
summary(fit.qda)

fit.glm <- glm(mpg01~cylinders+displacement+horsepower+weight, data=train, family=binomial)
summary(fit.glm)

logit.prob <- predict(fit.glm, test, type="response")
logit.pred <- ifelse(logit.prob > 0.5, TRUE, FALSE)
table(logit.pred, test$mpg01)
mean(logit.pred == test$mpg01)  # Accuracy=0.890

set.seed(1)
train.X <- as.matrix(train$cylinders, train$displacement, train$horsepower, train$weight)
test.X <- as.matrix(test$cylinders, test$displacement, test$horsepower, test$weight)
knn.pred <- knn(train.X, test.X, train$mpg01, k=1)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)  # Accuracy=0.949
knn.pred <- knn(train.X, test.X, train$mpg01, k=10)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)  # Accuracy=0.949
knn.pred <- knn(train.X, test.X, train$mpg01, k=20)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)  # Accuracy=0.949
knn.pred <- knn(train.X, test.X, train$mpg01, k=30)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)  # Accuracy=0.949
knn.pred <- knn(train.X, test.X, train$mpg01, k=50)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)  # Accuracy=0.949
knn.pred <- knn(train.X, test.X, train$mpg01, k=100)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)  # Accuracy=0.949


Boston$crim01 <- Boston$crim > median(Boston$crim)
Boston <- Boston[-4]
cor(Boston)

sort(cor(Boston)[1,])  # pred2 = tax, rad (highest correlations with crim)
sort(cor(Boston)[14,])  # pred2 = tax, rad (highest correlations with crim)

set.seed(1)
trainid <- sample(1:nrow(Boston), nrow(Boston)*0.7 , replace=F)  # 70% train, 30% test
train <- Boston[trainid,]
test <- Boston[-trainid,]


fit.glm <- glm(crim01~rad+tax, data=train, family=binomial)
fit.glm.prob <- predict(fit.glm, test, type="response")
fit.glm.pred <- ifelse(fit.glm.prob > 0.5, TRUE, FALSE)
table(fit.glm.pred, test$crim01)
mean(fit.glm.pred == test$crim01)  # Accuracy=0.724

#LDA
fit.lda <- lda(crim01~rad+tax, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
table(fit.lda.pred, test$crim01)
mean(fit.lda.pred == test$crim01)  # Accuracy=0.724

#QDA

fit.qda <- qda(crim01~rad+tax, data=train)
fit.qda.pred <- predict(fit.qda, test)$class
table(fit.qda.pred, test$crim01)
mean(fit.qda.pred == test$crim01)  # Accuracy=0.743

#KNN
require(class)
set.seed(1)
train.X <- as.matrix(train$rad, train$tax)
test.X <- as.matrix(test$rad, test$tax)
knn.pred <- knn(train.X, test.X, train$crim01, k=1)
table(knn.pred, test$crim01)
mean(knn.pred == test$crim01)  # Accuracy=0.757
