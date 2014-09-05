# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 2 Quiz
#
# A. Leopold HILLAH
# ################################################### #

##### Question 1 #####

myvec <- rep(seq(4,13),2)

##### Question 2 #####

mycharvec <- as.character(myvec)

##### Question 3 #####

myfacvec <- as.factor(mycharvec)

##### Question 4 #####

levels(myfacvec)

##### Question 5 #####

myformvec <- 3 * myvec ^ 2 - 4 * myvec + 1

##### Question 6 #####

regmx <- function(x, y) {
        z <- solve(crossprod(x)) %*% crossprod(x,y)
}

X <- matrix(c(1,5,8,1,4,9,1,6,4,1,2,7,1,3,4,1,2,9,1,7,6,1,8,4), nrow=8, ncol=3, byrow=T)
Y <- c(45.2, 46.9,31.0,35.3,25.0,43.1,41.0,35.1)
Z <- regmx(X,Y)
print(Z)

##### Question 7 #####

nlist <- list(a='first', b=1, c='third', d=4, e=5, f='last')

##### Question 8 #####

col1 <- rep(c('a','b','c','d','e'),5)
col2 <- sample(c('level1','level2','level3'), 25, replace = TRUE)
col2 <- factor(col2)
col3 <- 26-seq(1:25)
col4 <- seq(as.Date("10/08/2014","%d/%m/%Y"), by="day", length.out=25)

mydf <- data.frame(col1, col2, col3, col4, stringsAsFactors=FALSE) 

##### Question 9 #####

levels(mydf$col2)[4] <- 'level4'
mydf <- rbind(mydf, list('f','level4',30,as.Date('07/09/2014','%d/%m/%Y')))

##### Question 10 #####

tempdf <- read.csv("temperatures.csv", stringsAsFactors=FALSE) 

##### Question 11 #####

measuredf <- read.table(file.choose(), header=TRUE)

measuredf <- read.table(file="/Users/another_user/data/measurements.txt", header=TRUE)

##### Question 12 #####

webdf <- read.table(file="https://data.cityofnewyork.us/Environment/Street-Tree-Census-Brooklyn.txt", sep="|", header=TRUE)

##### Question 13 #####

fac12 <- 1
for (i in seq(1:12)) {
        fac12 <- i * fac12
}
print(fac12)
# 479001600

##### Question 14 #####

capital <- 1500
interest <- 0.0324
for (i in seq(1:6)) {
        balance <- capital * (1 + interest)
}
sprintf("%.2f", balance)
#1548.60

##### Question 15 #####

nvect <- seq(1, length.out=20)
sum3 <- 0
i <- 1
while (i<=20) {
        sum3 <- sum3 + i
        i <- i + 2
}
print(sum3)   
# 100

##### Question 16 #####

x <- 2
poly <- 0
for (i in seq(1:10)) {
        poly <- poly + x^i
}
print(poly)
# 2046

##### Question 17 #####

x <- 2
poly <- 0
i <- 1
while (i<=10) {
        poly <- poly + x^i
        i <- i + 1
}
print(poly)
# 2046

##### Question 18 #####

poly <- 2 * (1 - 2 ^ 10) / (1 - 2)
# 2046

##### Question 19 #####

nvect <- seq(from = 20, to = 50, by = 5)

##### Question 20 #####

cvect <- rep("example", 10)

##### Question 21 #####

quadratic <- function(a, b, c) {
        delta <- b^2 - 4 * a * c
        if (delta <0) stop ("No solution exists to the quadratic. Exiting ...")
        sol1 <- (-b + sqrt(delta)) / (2 * a)
        sol2 <- (-b - sqrt(delta)) / (2 * a)     
        if (sol1 == sol2) solution <- sol1 else solution <- c(sol1, sol2)
}

(quadratic(5,6,1))
