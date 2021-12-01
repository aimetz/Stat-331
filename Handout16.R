############################
# Name: Aiden Metz         #
# STAT 331: Handout 16     #
############################

setwd("C:/Users/student/Desktop/Stat331")
load("CA and NV Sample.rdata")
can <- CA_NV_Quakes

can$tertile <- ifelse(can$mag <= quantile(can$mag, 1/3), yes=1,
                      no=ifelse(can$mag <= quantile(can$mag, 2/3), yes=2, no=3))
head(can)

tab <- table(can$state, can$tertile)
tab
tab <- data.frame(prop.table(tab, margin=1))
tab
library(ggplot2)
tab
ggplot(data = tab) + geom_bar(aes(x=Var1, y=Freq, fill=Var2), stat="identity",
                              position = "dodge", color="black", width=.5)

chisq.test(can$tertile, can$state)

shapiro.test(can$mag)
shapiro.test(can$mag[can$state=="California"])
shapiro.test(can$mag[can$state=="Nevada"])
qqnorm(can$mag, pch = 1, frame = FALSE)
qqline(can$mag, col = "steelblue", lwd = 2)
ggplot(can) + geom_density(aes(x=mag, fill=state))
install.packages("reshape2")
data(tips, package="reshape2")
head(tips)
plot(tips$day)
tips$day <- factor(tips$day, ordered=T, levels=c("Thur", "Fri", "Sat", "Sun"))

tipaov <- aov(tip~day, data=tips)
ggplot(tips, aes(x=day, y=tip)) + geom_boxplot()
summary(tipaov$fitted.values)
names(tipaov)

model.tables(tipaov, type="means")
model.tables(tipaov, type="effects")
boxplot(tips$perc)
tipsn <- tips[tips$perc<.4,]
tipsn$perc <- tipsn$tip/tipsn$total_bill

qqnorm(tipsn$perc, pch = 1, frame = FALSE)
qqline(tipsn$perc, col = "steelblue", lwd = 2)

bartlett.test(perc~day, tipsn)

ggplot(tips) + geom_density(aes(x=tip, fill=day, alpha=.3))

