############################
# Name: Aiden Metz         #
# STAT 331: Handout 8      #
############################

nums <- rnorm(500, 70, 15)
hist(nums)
summary(nums)
nums[which(nums<0)]<- 0
summary(nums)

x<-0:140
plot(x, dnorm(x, 70, 15), type="l")
pnorm(100, 70, 15)-pnorm(60, 70, 15)
mean(nums<100 & nums>60)

bnorm<- function(l, u, m=0, sd=1){
  pnorm(u, m, sd)-pnorm(l, m, sd)
}
bnorm(-1, 1)
bnorm(-2, 2)
bnorm(-3, 3)
bnorm(0, 140, 70, 15)
bnorm(55, 85, 70, 15)
bnorm(90, 110, 100, 10)
bnorm(60, 100, 70, 15)

flips <- sample(x=c("H", "T"), size=20, replace=T)
mean(flips=="H")

rbinom(1, 20, .5)

bin <- rbinom(100000, 34, .15)
barplot(prop.table(table(bin)))

spokes <- seq(from=1, to=35)
d <- dbinom(spokes, 34, .15)
plot(spokes, d, type="h")
sum(d)

1-pbinom(0, 10000, .001)
1-pbinom(9, 10000, .001)

tcasts<- c()
for (i in 1:10000){
  fish<-0
  casts<-0
  while(fish<3){
    cast<-sample(x=c("fish", "no fish"), size=5, prob=c(.05, .95), replace=T)
    casts <- casts+1
    fish<-fish+sum(cast=="fish")
  }
  tcasts <- c(tcasts, casts)
}
mean(tcasts)
sd(tcasts)
hist(tcasts)

tcasts<- c()
for (i in 1:100000){
  fish<-0
  casts<-0
  while(fish<3){
    fish<-fish+rbinom(1, 5, .05)
    casts <- casts+1
  }
  tcasts <- c(tcasts, casts)
}
rownames(table(tcasts))
simul.distn<-data.frame(x=rownames(table(tcasts)), pmf=as.numeric(prop.table(table(tcasts))))
plot(simul.distn, type="h", col="blue")
points(simul.distn$x, simul.distn$pmf, add=T, col="orange")

mu <- sum(as.numeric(simul.distn$x)*simul.distn$pmf)
mu
sqrt(sum((as.numeric(simul.distn$x)-mu)^2*simul.distn$pmf))
