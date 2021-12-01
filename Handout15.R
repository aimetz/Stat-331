############################
# Name: Aiden Metz         #
# STAT 331: Handout 15     #
############################

setwd("C:/Users/student/Desktop/Stat331")
lex <- read.csv("lexile.csv")
plot(lex$height, lex$lexile)

cor(lex$height, lex$lexile) 
# Taller = older reading scores = older

install.packages("GGally")
GGally::ggpairs(lex)

cor(lex[,c(5, 1:4)])
pairs(lex[, c(5,1,2,4)])

library(ggplot2)
ggplot(lex, aes(x=height, y=lexile)) +geom_point()+geom_smooth(method="lm", se=F)         

model <- lm(lexile~height, lex)
summary(model)

model <- lm(lexile~height+age+grade, lex)
summary(model)

model <- lm(lexile~height+age+grade+male, lex)
summary(model)
names(model)
hist(model$residuals)
plot(model$fitted.values, model$residuals)
plot(model)

plot(model$qr, model$residuals)

