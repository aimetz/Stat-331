############################
# Name: Aiden Metz         #
# STAT 331: Handout 6      #
############################

setwd(dir="C:/Users/student/Desktop/Stat331")
cd1 <- read.csv("clickData1.csv")
cd2 <- read.csv("clickData2.csv")
dem <- read.csv("demographics.csv")
dem
cd1
cd2
all <- cbind(rbind(cd1, cd2), dem)
all
table(all$ad)
mean(all$ad)
# Prop of people that clicked on add
prop.table(table(all$ad, all$incCat), margin=2)

cd <- read.csv("clickAll.csv")
demAll <- read.csv("demogAll.csv")
cd
merge(x=cd, y=demAll, by="shopper", all.x=T)

All <- merge(x=cd, y=demAll, by="shopper", all=T)
complete.cases(All)

All[!complete.cases(All),]

order(cd$shopper)
?order
cd[ order(cd$shopper), ]
cd[ order(cd$webpage, -cd$ad, cd$shopper), ]

l1 <- read.csv("location1.csv")
l2 <- read.csv("location2.csv")
nv <- read.csv("normalValues.txt")

a3 <- merge(l1, l2, all=T)
a3[order(a3$ssn),]
nv
a4 <- cbind(a3, nv)
a4 <- a4[order(a4$ssn),]
a4$ok1 <- a4$meas1 < a4$normal1
a4$ok2 <- a4$meas2 < a4$normal2
a4$ok3 <- a4$meas3 < a4$normal3
a4
