############################
# Name: Aiden Metz         #
# STAT 331: Handout 7      #
############################

# 1
#  a
myFun <- function(vec, digits=2)
{
  round(mean(vec), digits)
}
myFun(rnorm(400023))

mySummary <- function(vec)
{
  return(c(n=length(vec), mean=mean(vec), SD=sd(vec), Med=median(vec),
           Min=min(vec), Max=max(vec)))
}
mySummary(c(1,2,3))

mySummary <- function(vec, stats="all")
{
  if (stats=="meanSD")
  {
    return(c(n=length(vec), mean=mean(vec), SD=sd(vec)))
  }
  else if (stats=="5number")
  {
    return(c(n=length(vec), Min=min(vec), quantile(vec, .25),
             Med=median(vec), quantile(vec,.75), Max=max(vec)))
  }
  else
  {
    return(c(n=length(vec), mean=mean(vec), SD=sd(vec),
             Min=min(vec), quantile(vec, .25),
             Med=median(vec), quantile(vec,.75), Max=max(vec)))
  }
}
mySummary(rnorm(30, 5, 2), "all")

set.seed(521)
test <- rnorm(90, 95, 8)
sum(test>100)
tempCat <- c("above", "below")
table(ifelse(test>100, yes=tempCat[1], no=tempCat[2]))



set.seed(5166)
raceEth <- sample(x=c("Wt", "Bl", "Hisp", "Amer In/AK Nat", "Asian",
                      "Haw or PI", "Other", "Multi"),
                  size=100, replace=T)
table(ifelse(raceEth=="Wt", yes="White", no="Non-White"))
table(ifelse(raceEth=="Wt", yes="White", no=
               ifelse(raceEth=="Bl", yes="Black", no=
                        ifelse(raceEth=="Hisp", yes="Hispanic", no="Other"))))


df= data.frame(year=sample(c(1, 2, 3, 4), size=10, replace=T),
               campus=sample(c("Yes", "No"), size=10, replace=T),
               mealPlan=sample(c("Yes", "No"), size=10, replace=T))
(df$year==1 | df$mealPlan=="Yes")


setwd(dir="C:/Users/student/Desktop/Stat331/earthquake")
new <- read.csv("query2000.csv")
for (i in 2001:2020)
{
  filename<- paste0("query", as.character(i), ".csv")
  new <- rbind(new, read.csv(filename))
}

tail(new)
for (file in list.files())
  file
file

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
