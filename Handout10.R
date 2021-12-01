############################
# Name: Aiden Metz         #
# STAT 331: Handout 10     #
############################

setwd("C:/Users/student/Desktop/Stat331")

install.packages("dplyr")
install.packages("plyr")
library(plyr)
library(dplyr)
search()

smoke <- read.csv("smoking.csv")
smoke.t<-tibble(smoke)
smoke.n <- smoke%>%filter(currentSmk=="No")
smoke.y<- smoke%>%filter(currentSmk=="Yes")
smoke.y

smoke.n <- smoke[smoke$currentSmk=="No",]
smoke.y<- smoke[smoke$currentSmk=="Yes",]
smoke.n

smoke.n <- smoke.t%>%filter(currentSmk=="No")
smoke.y<- smoke.t%>%filter(currentSmk=="Yes")
smoke.n
smoke.fy <- smoke.t%>%filter(currentSmk=="Yes", sex=="Female") 
smoke.fy
smoke.fy %>% summarize(mean=mean(numCigs))
?match
smoke.t %>% filter(exercise  %in% c("Some Days", "Daily"))
smoke.t %>% filter(exercise =="Some Days"| exercise =="Daily")

smoke.t %>% select(height, weight) %>%
   mutate(heightM=height*2.54, weightM=weight*.45, BMI= 10000*weightM/(heightM^2))

smoke.t %>% summarise(mean(numCigs), sd(numCigs), mean(height), sd(height))


smoke.t %>% group_by(sex) %>% summarise(mean=mean(numCigs), sd=sd(numCigs))
tapply(smoke.t$numCigs, smoke.t$sex, mean)
with(smoke.t, tapply(numCigs, sex, mean))
smoke.t$letter <- sample(c("a", "b", "c", "d"), 1000, replace = T)
smoke.t %>% arrange(letter, id)

bmis <- smoke.t %>%
  mutate(heightM=height*2.54, weightM=weight*.45, 
         BMI= 10000*weightM/(heightM^2)) %>%
  group_by(sex, exercise) %>%
  summarise(meanBMI=mean(BMI)) %>%
  arrange(-meanBMI)

class(bmis)
with(bmis, barplot(meanBMI, names.arg = paste(sex,"/",exercise, sep="")))
?barplot







d<-14*13
a <- 8*7/d
b <- 2*8*2/d
c <- 2/d
q<- 2*8*4/d
e <- 2 *4*2/d
f <- 4*3/d
a+b+c+q+e+f
a
b
c
q
e
f
