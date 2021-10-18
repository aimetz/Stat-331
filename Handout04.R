############################
# Name: Aiden Metz         #
# STAT 331: Handout 5      #
############################

setwd(dir="C:/Users/student/Desktop/Stat331")
smoke <- read.csv("smoking.csv", colClasses = c("character", rep(NA, 7)))
head(smoke)
smoke$exercise <- factor(smoke$exercise, ordered=T, levels=c("None", "Some Days",
                                                              "Daily"))
smoke$currentSmk <- factor(smoke$currentSmk)
smoke$sex <- factor(smoke$sex)

head(smoke)

library(table1)

smoke$bmi = (smoke$weight/smoke$height^2*703)

table1(~ age + sex + weight + bmi + 
         currentSmk + numCigs, data=smoke)
hist(smoke$numCigs)
mean(smoke$numCigs)
summary(smoke$numCigs)
smoke$numCigs[smoke$numCigs == -9] <- NA
smoke$numCigs
summary(smoke$numCigs)
mean(smoke$numCigs, na.rm=T)
head(smoke)
length(smoke$numCigs)
hist(smoke$numCigs)

sum(is.na(smoke$numCigs))
# 796 NAs, 796 people dont smoke so this number makes sense

plot(smoke$weight, smoke$height)
model <- lm(weight~height, data=smoke)
model
is.list(model)
names(model)
class(model$coefficients)
is.numeric(model$coefficients)
model$coefficients[2]
is.vector(model$coefficients)

model[[5]]
names(model)
plot(smoke$weight, model[[5]])
plot(smoke$weight, model$fitted.values)
points(100:250, model$coefficients[2]*100:250+model$coefficients[1], col="red")
?points
plot(smoke$weight, smoke$height)
with(smoke, plot(height, weight))
abline(model)

?rnorm
mylist <- list(first=rnorm(10), second=c("Ann", "Bob", "Mango"), last=smoke)
mylist
mylist[[2]]
mylist$first
mylist$first[5]
mylist$last$numCigs
mylist$last[1000, 8]
mylist$last[1000, 7]
head(smoke)


weed <- read.csv("CA Cannabis License Search.csv", skip = 2)
head(weed)

date <- as.POSIXct(weed$issueDate, format="%m/%d/%Y %H:%M")
weed$date <- date