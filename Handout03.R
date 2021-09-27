############################
# Name: Aiden Metz         #
# STAT 331: Handout 3      #
############################

getwd()
setwd(dir="C:/Users/student/Desktop/Stat331")
rData <- read.table("cereal.csv", header=T, sep=",", quote="\"")
cereal <- read.csv("cereal.csv")
names(cereal)[15] <- "servSize"
cerealTemp <- cereal
cerealTemp$rating[75]
cerealTemp$rating[75] <- 35.25
read.csv("https://raw.githubusercontent.com/ulund/STAT-331/main/cereal.csv")
hist(cereal$rating, main="Distribution of Cereal Ratings", xlab = "Rating")
boxplot(cereal$rating, main="Distribution of Cereal Ratings", ylab = "Rating")

boxplot(cereal$rating~cereal$mfr, main="Distribution of Cereal Ratings", 
        ylab = "Rating", xlab="Manufacturer")
stripchart(cereal$rating~cereal$mfr, vertical=T, pch=16, cex=.5, method="jitter",
           jitter=.2, add=T)
?stripchart
plot(cereal$fiber, cereal$rating)
plot(cereal$sugar, cereal$rating)
cereal2 <- cereal[cereal$rating<80,]
plot(cereal2$sugar, cereal2$rating)
summary(cereal2)
by(data=cereal$rating, INDICES = cereal$mfr, FUN=var)
table(cereal$mfr)
table(cereal$mfr)/length(cereal$mfr)
barplot(table(cereal$mfr)/length(cereal$mfr))
barplot(table(cereal$type))
