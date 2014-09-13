# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 3 Quiz
#
# A. Leopold HILLAH
# ################################################### #

##### Question 1 #####

vmean <- function(x) {
        vsum <- 0
        vcount <- length(x)
        for (i in 1:vcount) vsum <- vsum + x[i]
        return (vsum/vcount)
}

# sample call:
# vect <- c(1,2,3,4,5,6,7)
# vmean(vect)
# 4


##### Question 2 #####

vmean2 <- function(x) {
        vsum <- 0
        xidx <- which(!(is.na(x)))
        vcount <- length(xidx)
        for (i in xidx) vsum <- vsum + x[i]
        return (vsum/vcount)
}

# sample call:
# vect <- c(1,2,3,4,NA,5,6,7)
# vmean2(vect)
# 4


##### Question 3 #####

gcd_recursive <- function (x, y){
        return ( ifelse(y, gcd_recursive(y, x %% y), x) )
}

# sample call:
# gcd_recursive(16,24)
# 8


##### Question 4 #####

gcd_euclid <- function(x, y) {
        if (x == 0) return(y)
        if (y == 0) return(x)
        while (x != y) {
                if (x > y) x <- x - y else y <- y - x
        }
        return ( x )
}

# sample call:
# gcd_euclid(12,22)
# 2


##### Question 5 #####

polynom <- function(x, y) {
        return ( x^2 * y + 2 * x * y - 2 * x * y^2 )
}

# sample call:
# polynom(1,2)
# -2


##### Question 6 #####

dfprice <- read.csv('week-3-price-data.csv')
dfmodel <- read.csv('week-3-make-model-data.csv')
dfmodelprice <- merge(dfmodel, dfprice)
nrow(dfprice)
# 28
nrow(dfmodel)
# 8
nrow(dfmodelprice)
# 27! This is not expected.


##### Question 7 #####

dfprice <- read.csv('week-3-price-data.csv')
dfmodel <- read.csv('week-3-make-model-data.csv')
dfmodelprice <- merge(dfmodel, dfprice, all = TRUE)

nrow(dfprice)
# 28
nrow(dfmodel)
# 8
nrow(dfmodelprice)
# 28 This is expected!


##### Question 8 #####

dfmprice2010 <- subset(dfmodelprice, Year == 2010)
nrow(dfmprice2010)
# 14


##### Question 9 #####

dfmpricered <- subset(dfmodelprice, Color == "Red" & Price > 10000)
nrow(dfmpricered)
# 4
ncol(dfmpricered)
# 8


##### Question 10 #####

dfmpricered2 <- subset(dfmpricered, select = -c(ModelNumber,Color))
nrow(dfmpricered2)
# 4
ncol(dfmpricered2)
# 6


##### Question 11 #####

charcount <- function (x) {
        ncnt <- vector(mode="numeric")
        for (i in seq_along(x)) {
                ncnt[i] <- length(unlist(strsplit(x[i], '')))
        }  
        return(ncnt)
}


##### Question 12 #####

strmerge <- function (x, y) {
        if (length(x) != length(y)) stop("Sorry, vectors do not have the same size!", call. = FALSE )
        return(paste(x,y))
}


##### Question 13 #####

strsub <- function (x) {
        vowels <- 'aeiouyAEIOUY'
        for (i in seq_along(x)) {
                if (nchar(x[i]) > 2) {
                        y <- unlist(strsplit(x[i], ''))
                        for (j in 1:(length(y)-2L)) {
                                if (grepl(y[j],vowels)) return(substr(x[i],j,j+2))
                        }
                }
        }
        message("Sorry, there is no match in the character vector provided.")
}  


##### Question 14 #####

cmonth <- 12
cday <- 23
cyear <- 2014

dfdt <- data.frame(cmonth, cday, cyear)
dfdt$cdate <- as.Date(paste(dfdt$cmonth, dfdt$cday, dfdt$cyear, sep="-"), "%m-%d-%Y")


##### Question 15 #####

mystring <- "05-30-2014"
mydate <- as.Date(mystring, "%m-%d-%Y")


##### Question 16 #####

mystring <- "05-30-2014"
mydate <- as.Date(mystring, "%m-%d-%Y")
mymonth <- as.character(mydate, "%m")


##### Question 17 #####

mydtseq <- seq(from = as.Date("01-01-2005", "%m-%d-%Y"), to = as.Date("12-31-2014", "%m-%d-%Y"), by="day")

