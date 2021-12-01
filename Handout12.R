############################
# Name: Aiden Metz         #
# STAT 331: Assignment 12  #
############################

setwd("C:/Users/student/Desktop/Stat331")

library(haven)
admit <- read_sas("admit.sas7bdat")
stress17 <- read_sas("stress2017.sas7bdat")
stress18 <- read_sas("stress2018.sas7bdat")
m1<-merge(stress17, stress18, by=c("name", "id"), all=T, suffixes=c("17", "18"))
stressAll <- merge(admit, m1, by=c("name", "id"), all=T)
stressAll
dim(stressAll)
library(plyr)
library(dplyr)
#stressAll %>% select(-c("id", "name")) 
stressAll %>% group_by(sex) %>% summarise(mean.maxhr18=mean(maxhr18, na.rm=T))
stressAll
library(ggplot2)
ggplot(stressAll, aes(x=maxhr17, y=weight)) + geom_point()
stressAll$years <- ifelse(!is.na(stressAll$maxhr17) & !is.na(stressAll$maxhr18),yes=4,
                          no=ifelse(!is.na(stressAll$maxhr17), yes=3,
                          no=ifelse(!is.na(stressAll$maxhr18), yes=2,
                                    no=1)))
stressAll$years
stressAll$years <- factor(stressAll$years, levels=c(1, 3, 2, 4), labels=c("neither", 
                                                      "2017", "2018", "Both"))
ggplot(stressAll, aes(x=years)) + geom_bar()


library(jsonlite)
wibr <- fromJSON("wibr.json")
class(wibr)
structure(wibr)
wibrdf <- data.frame(wibr)
colnames(wibrdf)
sort(table(wibrdf$elements.ZIP))
#53218
head(wibrdf)
write.csv(stressAll, "stressAll.csv")
arson <- wibrdf[wibrdf$elements.Arson==1,]
arson$elements.ZIP <- as.character(arson$elements.ZIP)
tab <- data.frame(sort(table(arson$elements.ZIP), decreasing=T))
tab <- tab[order(-tab$Freq),][1:10,]
ggplot(tab) + geom_col(aes(x=Var1, y=Freq)) + labs(x="ZIP")
tab
