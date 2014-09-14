# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 3 - Project 1
#
# A. Leopold HILLAH
# ################################################### #

##### Question 1 #####

entropy <- function(d) {
        esum <- 0
        if (is.factor(d) | is.numeric(d) | is.character(d)) {
                # generate table of frequencies from vector (values are equal to 1 or more)
                t <- table(d)
                # scale t to get relevant proportions
                p <- t / sum(t)
                # compute entropy
                esum <- -sum(p * log2(p))
                }
        return (esum)
}


##### Question 2 #####

infogain <- function(d, a) {
        entropy2 <- function(t) {
                # input t is a table of frequencies
                # exclude 0 values from vector (or set 0 * log2(0) = 0)
                t <- t[t>0]
                # scale t to get relevant proportions
                p <- t / sum(t)
                # compute entropy
                esum <- -sum(p * log2(p))
                return (esum)
        }
        
        n <- length(d)
        # get freqency table from both vectors (prtitions of vector d according to attribute vector a)
        pd <- as.matrix(table(d, a))
        # Get number of columns in table (m value)
        m <- ncol(pd)
        isum <- 0
        # compute conditional entropy
        for (j in 1:m) {
                isum <- isum + (sum(pd[,j]) / n) * entropy2(pd[,j])
                }
        # get information gain (diference in entropies)
        igain <- entropy(d) - isum
        return (igain)
}


##### Question 3 #####

decide <- function(x, c) {
        # Initialize lists of results
        decision <- {}
        gains <- {}
        # Fill Information Gains vector with column number
        rd <- c(1:ncol(x))
        # Extract target d
        d <- x[,c]
        # Mark target column as NA
        rd[c] <- NA
        # Loop over candidate attributes column a = rd[-c] and compute information gains
        for (i in rd[-c]) {
                rd[i] <- infogain(d, x[,i])
                gains[names(x)[i]] <- rd[i]
        }
        # Get identiy of the attribute that maximizes the information gain
        decision$max <- which.max(rd)
        # Get details of the information gains for each candidate attributes.
        decision$gains <- gains
        return (decision)
}


##### Sample Run #####
# > dataset <- read.csv("entropy-test-file.csv")
# > entropy(dataset$answer)
# [1] 0.9832692
# > infogain(dataset$answer,dataset$attr1)
# [1] 2.411565e-05
# > infogain(dataset$answer,dataset$attr2)
# [1] 0.2599038
# > infogain(dataset$answer,dataset$attr3)
# [1] 0.002432707
# > infogain(dataset$answer,dataset$answer)
# [1] 0.9832692
# > decide(dataset,4)
# $max
# [1] 2
# 
# $gains
# attr1        attr2        attr3 
# 2.411565e-05 2.599038e-01 2.432707e-03 
# 
# > 