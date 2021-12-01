############################
# Name: Aiden Metz         #
# STAT 331: Handout 11     #
############################

setwd("C:/Users/student/Desktop/Stat331")
cereal <- read.csv("cereal.csv")
library(ggplot2)
g <- ggplot(data=cereal)
g+geom_histogram(aes(rating), fill="lightblue", color="black", 
                 breaks=seq(0, 100, by=5))+
  scale_x_continuous(limits=c(0,100),breaks=seq(0, 100, by=10))+
  labs(title="Distn of Cereal Rating", x="Rating", y="Freq")
g+geom_density(aes(rating), fill="lightyellow", color="black", 
                 breaks=seq(0, 100, by=5))+
  scale_x_continuous(limits=c(0,100),breaks=seq(0, 100, by=10))+
  labs(title="Distn of Cereal Rating", x="Rating", y="Freq")


ggplot(cereal, aes(rating))+geom_histogram(aes(y=..density..),
                                           fill="lightblue", color="black", 
                breaks=seq(0, 100, by=5))+
  scale_x_continuous(limits=c(0,100),breaks=seq(0, 100, by=10))+
  labs(title="Distn of Cereal Rating", x="Rating", y="Freq")+
  geom_density()

g+geom_boxplot(aes(y=rating), outlier.shape=14)
g+geom_boxplot(aes(x=rating), outlier.shape=14, fill="pink")+
labs(title="Distn of Cereal Rating", x="Rating")
g+geom_boxplot(aes(y=rating, x=mfr), outlier.shape=14, fill="Orange")

g+geom_boxplot(aes(y=rating, x=mfr), outlier.shape=14, fill="Orange")+
  geom_jitter(aes(x=mfr,y=rating))

g+geom_bar(aes(x=mfr), fill="magenta", color="brown")
mfrTable<-table(cereal$mfr)/length(cereal$mfr)
plotData=data.frame(mfrTable)
names(plotData)<-c("mfr", "props")
ggplot(data=plotData, aes(x=mfr, y=props))+
  geom_bar(stat="identity", fill="darkred", color="green")+
  labs(title="Distn of Cereal Mfr(n=77)", x="Manu", y="prop")
cereal$hiSugar <- cereal$sugars>median(cereal$sugars)
install.packages("dplyr")
library(plyr)
library(dplyr)
c2 <- cereal %>% filter(mfr!="A")
c2
g <- ggplot(data=c2)
g+geom_bar(aes(x=mfr, fill=hiSugar), position = "fill")
"x10":"x20"
cm <- tibble(read.csv("ChildMortality.csv", skip=2))
cm1 <- cm %>% filter(country %in% c("United States", "Afghanistan", "Mexico"))
cml <- reshape(data.frame(cm1), direction="long", varying=2:302,
               timevar="year", times=c(1800:2100), idvar="country",
               v.names="mortality")
rownames(cml)<-1:903
us <- cml[cml$country=="United States"&cml$year<=2020&cml$year>=1900,]
g <- ggplot(data=us, aes(x=year, y=mortality))+ geom_point(color="orange")
g+geom_line(color="magenta")

s19 <- cml[cml$year>=1900&cml$year<=2020,]
g <- ggplot(data=s19, aes(x=year, y=mortality, color=country))+ geom_point()
g+geom_line(aes(group=country))+facet_wrap(~country)
g+geom_line(aes(group=country))+facet_grid(rows=vars(country))
