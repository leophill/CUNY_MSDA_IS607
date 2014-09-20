# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 4 Quiz
#
# A. Leopold HILLAH
# ################################################### #


##### Loading required libraries #####

if (!(require(plyr)) | !(require(reshape2)) | !(require(ggplot2)) | !(require(gridExtra))) {
        install.packages("plyr", "reshape2", "ggplot2", "gridExtra")
}

##### Loading Movie Data #####

dfmovies <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="", stringsAsFactors = FALSE)
str(dfmovies)


##### Question 1 #####

dfdec <- subset(dfmovies, select=c(year))
dfdec <- ddply(dfdec, "year", mutate, decade = year %/% 10)

p1 <- ggplot(dfdec, aes(x=decade)) + 
        geom_histogram(binwidth=1, colour="darkblue", fill="white")+
        xlab("Decade")+
        ylab("Total Number of Movies")+
        scale_x_continuous(breaks=seq(180, 202, 1))+
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1))

print(p1)


##### Question 2 #####

# Convert data subset to long format keeping year, length, rating and vote, and creating a new variable for genre
dfmovieslg <- melt(dfmovies,id.vars=c("year","length","rating","votes"),
                   measure.vars=c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance", "Short"),
                   variable.name="genre", value.name="belongs")

# Filter on belongs column and remove it
dfmovieslg <- subset(dfmovieslg, belongs == 1, select = -c(belongs))

# Average IMDB user ratings

dfrating <- ddply(dfmovieslg, c("genre"), summarize, avg.rating = mean(rating, na.rm=T))
print(dfrating)

p2 <- ggplot(dfrating, aes(x=as.factor(genre), y=avg.rating)) + 
        geom_bar(stat="identity", colour="darkgreen", fill="yellow")+
        xlab("Genre")+
        ylab("Average User Rating")+
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1))

print(p2)

# Average IMDB user ratings over time

dfyrating <- ddply(dfmovieslg, c("year", "genre"), summarize, avg.rating = mean(rating, na.rm=T))
print(dfyrating)

p3 <- ggplot(dfyrating, aes(x=year, y=avg.rating, colour=as.factor(genre))) + 
        geom_line(stat="identity", size=1) +
        xlab("Year") +
        ylab("Average User Rating") +
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1)) +
        scale_colour_discrete(name  ="Movie Genre")

print(p3)


##### Question 3 #####

fit = lm (rating ~ length, data=dfmovieslg)
summary(fit)

# The correlation coefficient is 0.006389 which suggests that there is no relationship between movie length and rating.
# To confirm, a scatter plot with a regression line and confidence interval is drawn

p3 <- ggplot(dfmovieslg, aes(x=length, y=rating, colour=as.factor(genre))) + 
        geom_point(shape=1, position=position_jitter()) +
        geom_smooth(method=lm)  + # Add linear regression line
        xlab("Movie Length") +
        ylab("User Rating") +
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1)) +
        scale_colour_discrete(name  ="Movie Genre")

print(p3)

# Regression model underfits the data, with no real relationships shown.


##### Question 4 #####

fit = lm (rating ~ as.factor(genre), data=dfmovieslg)
summary(fit)

# A box plot is drawn for each genre to show the distribution of the data per movie length.

means <- ddply(dfmovieslg, c("genre"), summarize, avg = round(mean(length, na.rm=T)))
print(means)

p4 <- ggplot(dfmovieslg, aes(x=as.factor(genre), y=length)) + 
        geom_boxplot(aes(fill = genre), show_guide = FALSE) +
        stat_summary(fun.y=mean, geom="point", color="yellow", shape=18, size=3) + 
        geom_text(data = means, aes(label = avg, y = avg - 25)) +
        xlab("Movie Genre") +
        ylab("Movie Length") +
        scale_y_continuous(breaks=seq(0, 1000, 100))+
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1))

print(p4)

# Across movie genres, the media legnth is at most 100mn for 75% or more data.
# This however varies across genres. So there seems to be some relationship between
# genres and movie length.



##### Question 5 #####

# Structure of dfmovieslg

str(dfmovieslg)
pairs(dfmovieslg[,1:4])
cors <- cor(dfmovieslg[,1:4])


p5a <- ggplot(dfmovieslg, aes(x=year, y=votes)) + 
        geom_point(shape=1, position=position_jitter()) +
        stat_smooth(method=lm)  + 
        xlab("Year") +
        ylab("User Votes") +
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1)) +
        scale_colour_discrete(name  ="Movie Genre")
        

p5b <- ggplot(dfmovieslg, aes(x=rating, y=votes)) + 
        geom_point(shape=1, position=position_jitter()) +
        stat_smooth(method=lm)  + 
        xlab("User Ratings") +
        ylab("User Votes") +
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1)) +
        scale_colour_discrete(name  ="Movie Genre")


p5c <- ggplot(dfmovieslg, aes(x=length, y=votes)) + 
        geom_point(shape=1, position=position_jitter()) +
        stat_smooth(method=lm)  + 
        xlab("Movie Length") +
        ylab("User Votes") +
        theme(text = element_text(size=20),axis.text.x = element_text(angle=30, vjust=1)) +
        scale_colour_discrete(name  ="Movie Genre")


grid.arrange(p5a, p5b, p5c, ncol = 2, main = "User Votes vs other variables")

