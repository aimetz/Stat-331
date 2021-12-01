############################
# Name: Aiden Metz         #
# STAT 331: Handout 1      #
############################

x<- c(1,2,3,4,5,6,7,8,9,10)
y<- 45+3*x
plot(x, y)
y<- 34+3*x + rnorm(10, 0, 4)
plot(x, y)

############################
# Name: Aiden Metz         #
# STAT 331: Handout 2      #
############################


vec1 <- c(1, 8, 13)
vec2 <- c("cat", "mouse", "cheese steak")
vec3 <- c(1:100)
vec4 <- seq(1, 99, 2)
vec5 <- seq(0, 500, 500/350)
vec6 <- rep(c(1, 2, 3), times=10)
vec7 <- rep(1:50, each=3)
vec8 <- c(1:50, 49:1)
vec9 <- c(rep(1, 10)/(1:10))
vec10 <- c((1:10)^3)
class(vec1)
class(vec2)
class(vec10)
# 1 and 10 are numeric, 2 is character
vec9+vec10
# Yes! you can add them
vec1+vec2
# No you cannot add them

ages <- c(17, 19, 16, 34, 28, 34, 65, 72, 47, 52)
ages[4]
ages[c(1, 10)]
ages[c(1:4, 6:10)]
ages[7] = 56
ages > 34
ages[ages > 34]
agesOlder <- ages[ages > 34]
agesOlder[agesOlder == 34]
agesOlder[agesOlder != 34]
# Returns how many values are TRUE
sum(ages>34)
ages>34

ages[length(ages)]
length(ages[ages>34])

cbind(c(1, 2, 3), c(4, 5, 6))
cbind(c(1, 2), c(3, 4), c(5, 6))
options(myseed = 653)
BPs <- rnorm(50, 110, 15)
weights <- rnorm(50, 200, 34)
matrix <- cbind(BPs, weights)
matrix
matrix[10, 1]
matrix[10, 2]
matrix[1:10,]
matrix[11:(length(matrix)/ncol(matrix)),]
matrix[((length(matrix)/ncol(matrix))-9):(length(matrix)/ncol(matrix)),]
plot(matrix[,1], matrix[,2])
ints <- as.integer(runif(50, 1, 4))
ints
set.seed(413)
incomes <- c("Low", "Medium", "High")
incomes[ints]

cbind(matrix, incomes[ints])
# Coerses numerical values to characters
df = data.frame(BPs, weights, incomes[ints])
str(df)
df[, 3]
df$incomes.ints.
df
