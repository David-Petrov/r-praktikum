###
# scatter plot and correlation
x <- rnorm(100, 5, 1)
y <- 2*x
plot(x,y)
cor(x,y)

x <- rnorm(100, 5, 1)
y <- (-2)*x
plot(x,y)
cor(x,y)

x <- rnorm(100, 5, 1)
y <- 2*x + rnorm(100, 0, 1)
plot(x,y)
cor(x,y)

x <- rnorm(100, 5, 1)
y <- (-2)*x + rnorm(100, 0, 1)
plot(x,y)
cor(x,y)

x <- rnorm(100, 5, 1)
y <- rnorm(100, 5, 1)
plot(x,y)
cor(x,y)


###

library(MASS)
data(survey)
attach(survey)


###
# zad43
# a
median( Pulse, na.rm=T )
mean( Pulse, na.rm=T )
sd( Pulse, na.rm=T )

# b
median( Pulse[Sex=="Female"], na.rm=T )
mean( Pulse[Sex=="Female"], na.rm=T )
sd( Pulse[Sex=="Female"], na.rm=T )

# v
median( Pulse[Age<=25], na.rm=T )
mean( Pulse[Age<=25], na.rm=T )
sd( Pulse[Age<=25], na.rm=T )

# g
median( Pulse[Exer=="Freq"], na.rm=T )
mean( Pulse[Exer=="Freq"], na.rm=T )
sd( Pulse[Exer=="Freq"], na.rm=T )

# d
median( Pulse[Smoke=="Never" & Exer=="Freq"], na.rm=T )
mean( Pulse[Smoke=="Never" & Exer=="Freq"], na.rm=T )
sd( Pulse[Smoke=="Never" & Exer=="Freq"], na.rm=T )


###
# zad44
boxplot( Pulse ~ Exer )

boxplot( Pulse[Exer=="Freq"],
         Pulse[Exer=="None"],
         Pulse[Exer=="Some"] )

median( Pulse[Exer=="Freq"], na.rm=T )
median( Pulse[Exer=="None"], na.rm=T )
median( Pulse[Exer=="Some"], na.rm=T )

mean( Pulse[Exer=="Freq"], na.rm=T )
mean( Pulse[Exer=="None"], na.rm=T )
mean( Pulse[Exer=="Some"], na.rm=T )


###
# zad45
grades <- read.table("exam_grades.txt", header=T)
attach(grades)

# a
plot( test1, course.grade )
plot( test2, course.grade )
plot( test3, course.grade )

cor( test1, course.grade, use="complete.obs" )
cor( test2, course.grade, use="complete.obs" )
cor( test3, course.grade, use="complete.obs" ) 

# b
boxplot( course.grade ~ sex )

median( course.grade[sex=="Man"] )
median( course.grade[sex=="Woman"] )

mean( course.grade[sex=="Man"] )
mean( course.grade[sex=="Woman"] )

# v
boxplot( course.grade ~ semester )


###
# zad46
# a
x <- runif(500, 3, 7)

# b
x <- rexp(500, 1/5)

# v
x <- rnorm(500, 5, 1)

i1 <- min( mean(x)-3*sd(x), min(x) )
i2 <- max( mean(x)+3*sd(x), max(x) )
boxplot( x, ylim=c(i1,i2), horizontal=T )
points( mean(x), 1, pch=18, col="darkorange1", cex=1.7 )
points( mean(x)-3*sd(x), 1, pch="[", col="darkorange1", cex=1.5 )
points( mean(x)+3*sd(x), 1, pch="]", col="darkorange1", cex=1.5 )


###
