# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 6 Assignment
#
# A. Leopold HILLAH
# ################################################### #

##### Part 1 - rvest package #####

# Loading required libraries

if (!require(rvest)) install_github("hadley/rvest")
if (!require(stringr)) install.packages("stringr")

# Scraping amazon.com web site using rvest package

amz_url <- "http://www.amazon.com/Machine-Learning-R-Brett-Lantz/dp/1782162143"
ML_book <- amz_url %>% html() %>% html_nodes("#productDetailsTable")
book_details <- ML_book %>% html_nodes("li") %>% html_text()  

# Cleaning up extracted data

book_details <- gsub("\\t", "", book_details)
book_details <- gsub("\\n", "", book_details)
book_details <- str_trim(book_details)
book_details[8] <- gsub("\\s*function.*;\\s*", "", book_details[8])
book_details[9] <- gsub("#", "", paste(gsub("\\).*$", "", book_details[9]), ")", sep=""))
book_details <- read.table(text = book_details, sep = ":", colClasses = "character")
book_details$V1 <- str_trim(book_details$V1)
book_details$V2 <- str_trim(book_details$V2)
names(book_details) <- c("PROPERTY", "VALUE")

##### Part 2 - XML package #####

# Loading required libraries

if (!require(XML)) install.packages("XML")
if (!require(stringr)) install.packages("stringr")

# Scraping amazon.com web site using XML package

amz_url <- "http://www.amazon.com/Machine-Learning-R-Brett-Lantz/dp/1782162143"
amz_page <- htmlTreeParse(amz_url, useInternal=TRUE)
ML_book <- xpathSApply(amz_page, "//*[@id='productDetailsTable']")
book_details <- xpathSApply(ML_book[[1]], "//li", xmlValue)[58:66]

# Cleaning up extracted data

book_details <- gsub("\\t", "", book_details)
book_details <- gsub("\\n", "", book_details)
book_details <- str_trim(book_details)
book_details[8] <- gsub("\\s*function.*;\\s*", "", book_details[8])
book_details[9] <- gsub("#", "", paste(gsub("\\).*$", "", book_details[9]), ")", sep=""))
book_details <- read.table(text = book_details, sep = ":", colClasses = "character")
book_details$V1 <- str_trim(book_details$V1)
book_details$V2 <- str_trim(book_details$V2)
names(book_details) <- c("PROPERTY", "VALUE")

# PART 3: FEEDBACK FROM EXPERIENCE WORKING WITH BOTH PACKAGES

# Working with the rvest package was pretty straightforward. Although the XML package was not too much more complicated,
# it toolk me a bit more time to ome up with the best solution in the fewer steps
# In the end, both packages proved to be equivalently efficient, with more flexibility on rvest side.