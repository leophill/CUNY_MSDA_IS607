# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 3 Assignment
#
# A. Leopold HILLAH
# ################################################### #

##### Question 1 #####

nacount <- function(v) {
        return(sum(is.na(v)))
}


##### Question 2 #####

nacountd <- function(d) {
        return(sapply(d, nacount, USE.NAMES = TRUE))
}


##### Question 3 #####

isummary <- function(vi) {
        vis <- sort(vi, na.last = NA)
        vlen <- length(vi[!(is.na(vi))])
        minimum <- vis[1]
        maximum <- vis[vlen]
        mean <- sum(vis)/vlen
        median <- ifelse(vlen %% 2 == 1, vis[(vlen %/% 2) +1], (vis[vlen %/% 2] + vis[(vlen %/% 2) + 1]) / 2)
        quartile_1st <- vis[floor((vlen+3)/4)] + ((vlen+3)/4 - floor((vlen+3)/4)) * (vis[floor((vlen+3)/4) + 1] - vis[floor((vlen+3)/4)])
        quartile_3rd <- vis[floor((3 * vlen + 1) / 4)] + ((3 * vlen +1) / 4 - floor((3 * vlen + 1) / 4)) * (vis[floor((3 * vlen +1) / 4) + 1] - vis[floor((3 * vlen +1) / 4)])
        stdev <- sqrt(sum((vis-mean)^2)/(vlen-1))
        missing <- nacount(vi)
        isum <- c(minimum, maximum, mean, median, quartile_1st, quartile_3rd, stdev, missing)
        names(isum) <- c('Minimum', 'Maximum', 'Mean', 'Median', '1st.Quartile', '3rd.Quartile', 'StDev', 'Missing')
        return (isum)
}


##### Question 4 #####

csummary <- function(vc) {
        distincts <- length(unique(vc[!(is.na(vc))]))
        most <- names(which.max(table(vc[!(is.na(vc))])))
        mtimes <- max(table(vc[!(is.na(vc))]))
        missing <- nacount(vc)
        csumm <- c(distincts, most, mtimes, missing)
        names(csumm) <- c('No.of.Distincts', 'Most.Occuring', 'Most.Count', 'Missing')
        return (csumm)
}


##### Question 5 #####

lsummary <- function(vl) {
        trues <- table(vl)['TRUE']
        falses <- table(vl)['FALSE']
        trueprop <- trues / (trues + falses)
        missing <- nacount(vl)
        csl <- c(trues, falses, trueprop, missing)
        names(csl) <- c('No.of.Trues', 'No.of.False', 'True.Proportion', 'Missing')
        return (csl)
}


##### Question 6 #####

dsummary <- function(df) {
        dresult <- list()
        for (nm in names(df)) {
                if (is.numeric(df[,nm]))  dresult[[nm]] <- isummary(df[,nm])
                if (is.character(df[,nm]))  dresult[[nm]] <- csummary(df[,nm])
                if (is.logical(df[,nm]))  dresult[[nm]] <- lsummary(df[,nm])
        }
        return (dresult)
}        
        