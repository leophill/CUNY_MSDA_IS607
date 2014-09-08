# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 2 Assignment
#
# A. Leopold HILLAH
# ################################################### #

##### Question 1 #####

##### Question 1.a #####

queue <- c("James", "Mary", "Steve", "Alex", "Patricia")

##### Question 1.b #####

# add 6th element
queue[length(queue)+1] <- "Harold"

##### Question 1.c #####

queue <- queue[-1]

##### Question 1.d #####

queue <- c(queue[1],queue[4],queue[2:3],queue[5])

##### Question 1.e #####

# remove 5th element
queue <- queue[-length(queue)]

##### Question 1.f #####

# remove 4th element
queue <- queue[-length(queue)]

##### Question 1.g #####

idx <- which(queue == 'Patricia')
print(idx)
# Patricia is 2nd in the queue

##### Question 1.h #####

qcount <- length(queue)
print(qcount)
# There are 3 people left in the queue


##### Question 2 #####

quadratic <- function(a, b, c) {
        delta <- b^2 - 4 * a * c
        if (delta <0) stop ("No solution exists to the quadratic. Exiting ...", call. = FALSE)
        sol1 <- (-b + sqrt(delta)) / (2 * a)
        sol2 <- (-b - sqrt(delta)) / (2 * a)     
        if (sol1 == sol2) solution <- sol1 else solution <- c(sol1, sol2) 
        message("Solution(s) to the quadratic equation: ", paste(solution, collapse=" & "))
}

# sample call: quadratic(1,4,3)


##### Question 3 #####

nb1000 <- c(1:1000)
nbfinal <- nb1000 [!(nb1000 %% 3 == 0) & !(nb1000 %% 7 == 0) & !(nb1000 %% 11 == 0)]
print(nbfinal) 


##### Question 4 #####

pythagorian <- function(f, g, h) {
        params <- c(f, g, h)
        params <- sort(params)
        check <- ((params[1]^2 + params[2]^2) == (params[3]^2))
        if (check) message("Great! We have a Pythagorean Triple!") else message ("Sorry, this is no Pythagorean Triple!")
        return(check)
}

# sample true case : pythagorian(5,3,4)
# sample false case: pythagorian(1,3,2)