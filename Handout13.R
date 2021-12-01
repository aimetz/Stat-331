############################
# Name: Aiden Metz         #
# STAT 331: Handout 13     #
############################

setwd("C:/Users/student/Desktop/Stat331/earthquake")

e20 <- read.csv("query2020.csv")

grep(pattern = "San Luis Obispo", x = e20$place)

mam <- grep(pattern = "Mammoth", x = e20$place)
mam
length(mam)
lp <- grep(pattern = "Lone Pine", x = e20$place)
lp
e20[lp,]$mag

e20[grep(pattern = "town", x = tolower(e20$place)),][,c(5,14)]

e20$nearLP <- grepl(pattern = "Lone Pine", x = e20$place)

sum(grepl(pattern = "Lone Pine", x = e20$place))
length(grep(pattern = "Lone Pine", x = e20$place))

gl <- grepl(pattern = "Lone Pine", x = e20$place)
g <- grep(pattern = "Lone Pine", x = e20$place)
e20[gl, c("mag", "place")]
e20[g, c("mag", "place")]

mean(e20[g, "mag"])

e20[which.max(e20[, "mag"]),]$place

e20$time
e20$year <- substr(x=e20$time, 1, 4)
e20$month <- substr(x=e20$time, 6, 7)
e20$day <- substr(x=e20$time, 9, 10)
e20[1:20, c("time", "year", "month", "day")]
barplot(table(e20$month))

e20[1:6, c("time","place","mag")]
e20[grep(",", e20$place, invert = T), "place"]
length(grep(",", e20$place, invert = T))
sum(1-grepl(",", e20$place))
cp <- regexpr(", ", e20$place)
table(cp)
#-1 for if it does not have ", "
e20$startval <- ifelse(cp == -1, yes=1, no=cp+2)
states <- substr(x=e20$place, start=e20$startval, stop=nchar(e20$place))
table(states)
e20$states<-ifelse(states=="southern Idaho",yes="Idaho",
                     no=ifelse(states=="western Texas", yes="Texas",
                               no=ifelse(states=="CA",yes="California",
                                         no=ifelse(states==
                                          "California-Nevada border region",
                                          yes="CA/NV Border",
                                          no=ifelse(states=="B.C., MX",
                                                    yes="Baja California",
                                                    no=ifelse(states==
                                                                "NV Earthquake",
                                                              yes="Nevada",
                                                              no=ifelse(
                                                                states=="",
                                                                yes="Missing",
                                                                no=states)))))))
names(table(e20$states))
sort(table(e20$states), decreasing = T)
s20 <- read.csv("query2020 - Subset.csv")
s20
s20$newPlace <- s20$place
s20$newPlace <- sub("CA", "California", s20$newPlace)
s20$newPlace <- sub("southern", "Southern Region,", s20$newPlace)
s20$newPlace <- sub("B.C., MX", "B.C. Mexico", s20$newPlace)
s20[11,]$newPlace <- ", Nevada"
s20[15,]$newPlace <- ",  "
s20$newPlace
g <- grep(", ", s20$newPlace, invert = T)
s20$newPlace[g] <- paste0(", ", s20[g,]$newPlace)
m <- matrix(unlist(strsplit(s20$newPlace, ", ")), ncol=2, byrow=T)
?unlist
m[1, 2]
s20$region <- m[,1]
s20$state <- m[,2]
s20
s20[,c("mag", "region", "state")]
