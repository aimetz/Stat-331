############################
# Name: Aiden Metz         #
# STAT 331: Handout 9      #
############################
getwd()
setwd("C:/Users/student/Desktop/Stat331")
ot <- read.csv("OceanTemps.csv")
rowSums(ot[,2:13])/12
colSums(ot[,2:13])/nrow(ot)
rownames(ot) <- ot[,1]
ot<-ot[,2:13]
rowSums(ot)/12
plot(rownames(ot), rowSums(ot)/12, type="l")
points(rownames(ot), rowSums(ot)/12, pch=15)
colnames(ot)
plot(1:12, colSums(ot)/ncol(ot), type="l", xaxt="n")
axis(side=1, at=1:12, labels=colnames(ot)[1:12])
points(1:12, rowSums(ot)/12, pch=15)

apply(ot, 1, mean)
apply(ot, 2, mean)
apply(ot, 1, sd)
apply(ot, 2, sd)
fun <- function(vec){
  c(n=length(vec), mean=mean(vec), sd=sd(vec), se=sd(vec)/sqrt(length(vec)))
}
(apply(ot, 1, fun))
m <- data.frame(t(apply(ot, 1, fun)))
m
m$lower <- m$mean-2*m$se
m$upper <- m$mean+2*m$se
m

t(lapply(ot, fun))
t(sapply(ot, fun))
tl <- reshape(ot, varying=1:12, timevar="month", idvar="year", v.names="temp", direction="long")
rownames(tl)<- 1:nrow(tl)
tl$year <- tl$year+1959
tl
tl<-tl[order(tl$year, tl$month),]
tl$time<-tl$year+(tl$month-1)/12

class(tl)
tl
plot(tl[tl$time<1965,]$time, tl[tl$time<1965,]$temp, type="l")
points(tl[tl$time<1965,]$time, tl[tl$time<1965,]$temp, pch=196)
head(tl)
aggregate(temp~month, tl, mean)
aggregate(temp~year, tl, fun)
tapply(tl$temp, tl$month, mean)


cereal <- read.csv("cereal.csv")
attach(cereal)
hist(sugars)
search()
detach(cereal)
search()
with(data=cereal, table(sugars))
