library(readr)
library(ggplot2)
require(ISLR)
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


#
Auto <- read_csv("~/R/ISLR/Auto.csv")
fix(Auto)
dim(Auto)
Auto = na.omit(Auto)
dim(Auto)

plot1 <- ggplot(data=Auto, aes(x=cylinders, y=mpg)) 
plot2 <- ggplot(data=Auto, aes(y=mpg)) 


plot1 + geom_point()
plot2 + geom_histogram()

Auto$horsepower <- as.numeric(Auto$horsepower)
pairs(Auto[,1:8])


pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

summary(Auto)


#8
#a
data(College)

college <- read_csv("~/R/ISLR/College.csv")

#c
summary(college)
pairs(college[,3:13])

college$Private <- as.factor(college$Private)

ggplot(data=college, aes(y=Outstate, x=Private)) +
  geom_boxplot()


summary(college$Outstate)
summary(college$Private)

college$Elite <- college$Top10perc > 50

summary(college$Elite)
ggplot(data=college, aes(y=Outstate, x=Elite)) +
  geom_boxplot()

#v
ggplot(data=college, aes(x=Apps)) +
  geom_histogram()
ggplot(data=college, aes(x=Accept)) +
  geom_histogram()
ggplot(data=college, aes(x=Enroll)) +
  geom_histogram()

ggplot(data=college, aes(x=Top10perc)) +
  geom_histogram()
ggplot(data=college, aes(x=Top25perc)) +
  geom_histogram()
ggplot(data=college, aes(x=F.Undergrad)) +
  geom_histogram()
ggplot(data=college, aes(x=P.Undergrad)) +
  geom_histogram()
ggplot(data=college, aes(x=Outstate)) +
  geom_histogram()

ggplot(data=college, aes(x=Room.Board)) +
  geom_histogram()
ggplot(data=college, aes(x=Books)) +
  geom_histogram()
ggplot(data=college, aes(x=Personal)) +
  geom_histogram()
ggplot(data=college, aes(x=PhD)) +
  geom_histogram()
ggplot(data=college, aes(x=Terminal)) +
  geom_histogram()

ggplot(data=college, aes(x=S.F.Ratio)) +
  geom_histogram()
ggplot(data=college, aes(x=perc.alumni)) +
  geom_histogram()
ggplot(data=college, aes(x=Expend)) +
  geom_histogram()
ggplot(data=college, aes(x=Grad.Rate)) +
  geom_histogram()

#9

Auto <- read_csv("~/R/ISLR/Auto.csv")
Auto$horsepower <- as.numeric(Auto$horsepower)
Auto = na.omit(Auto)
str(Auto)

#Quantitative: mpg, cylinders (can treat as qual too), displacement, horsepower, weight, acceleration, year
#Qualitative: origin, name

sapply(Auto[,1:7], range)
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

tmp <- Auto[-(10:85),]
sapply(tmp[,1:7], range)
sapply(tmp[,1:7], mean)
sapply(tmp[,1:7], sd)


pairs(Auto[,1:7])
cor_table <- cor(Auto[,1:7])
cor_table

#mpg is negatively correlated with cylinders, displacement, horsepower, and weight

  fit <- lm(data=Auto, mpg~cylinders)
  ggplot(data=Auto, aes(y=mpg, x=cylinders)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs cylinders") + 
  annotate("text", x = 7, y = 45, label = equation(fit), parse = TRUE)

  fit <- lm(data=Auto, mpg~displacement)
  ggplot(data=Auto, aes(y=mpg, x=displacement)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs displacement") + 
  annotate("text", x = 300, y = 40, label = equation(fit), parse = TRUE)

  fit <- lm(data=Auto, mpg~horsepower)
  ggplot(data=Auto, aes(y=mpg, x=horsepower)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs horsepower") + 
  annotate("text", x = 150, y = 40, label = equation(fit), parse = TRUE)

  fit <- lm(data=Auto, mpg~weight)
  ggplot(data=Auto, aes(y=mpg, x=weight)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs weight") + 
  annotate("text", x = 4000, y = 40, label = equation(fit), parse = TRUE)

  fit <- lm(data=Auto, mpg~acceleration)
  ggplot(data=Auto, aes(y=mpg, x=acceleration)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs acceleration") + 
  annotate("text", x = 11, y = 40, label = equation(fit), parse = TRUE)

  fit <- lm(data=Auto, mpg~year)
  ggplot(data=Auto, aes(y=mpg, x=year)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  labs(title ="Scatterplot of mpg vs year") + 
  annotate("text", x = 75, y = 42, label = equation(fit), parse = TRUE)

  
  #10
  require(MASS)
  data(Boston)
  str(Boston)
  
  nrow(Boston)
  ncol(Boston)
  
  pairs(Boston)
  
  cor_table <- cor(Boston)
  cor_table
  
  
  fit <- lm(data=Boston, crim~rad)
  ggplot(data=Boston, aes(y=crim, x=rad)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red", se = TRUE) +
    labs(title ="Scatterplot of crim vs rad") + 
    annotate("text", x = 7, y = 40, label = equation(fit), parse = TRUE)
  
  Boston$high <- Boston$rad > 20
  fit <- lm(data=Boston, crim~rad)
  ggplot(data=Boston, aes(y=crim, x=rad, col = high)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red", se = TRUE) +
    labs(title ="Scatterplot of crim vs rad")  
  
  Boston$high <- Boston$tax > 600
  fit <- lm(data=Boston, crim~tax)
  ggplot(data=Boston, aes(y=crim, x=tax, col = high)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red", se = TRUE) +
    labs(title ="Scatterplot of crim vs tax")
  
  median(Boston$ptratio)
  