###
# zad56
e03sum <- replicate( 10000, sum( rexp(3, 1/5) ) )
e07sum <- replicate( 10000, sum( rexp(7, 1/5) ) )
e10sum <- replicate( 10000, sum( rexp(10, 1/5) ) )
e30sum <- replicate( 10000, sum( rexp(30, 1/5) ) )
e90sum <- replicate( 10000, sum( rexp(90, 1/5) ) )
e200sum <- replicate( 10000, sum( rexp(200, 1/5) ) )

hist(e03sum, prob=T)
hist(e07sum, prob=T)
hist(e10sum, prob=T)
hist(e30sum, prob=T)
hist(e90sum, prob=T)
hist(e200sum, prob=T)


###
# zad57
e03mean <- replicate( 10000, mean( rexp(3, 1/5) ) )
e07mean <- replicate( 10000, mean( rexp(7, 1/5) ) )
e10mean <- replicate( 10000, mean( rexp(10, 1/5) ) )
e30mean <- replicate( 10000, mean( rexp(30, 1/5) ) )
e90mean <- replicate( 10000, mean( rexp(90, 1/5) ) )
e200mean <- replicate( 10000, mean( rexp(200, 1/5) ) )

hist(e03mean, prob=T)
hist(e07mean, prob=T)
hist(e10mean, prob=T)
hist(e30mean, prob=T)
hist(e90mean, prob=T)
hist(e200mean, prob=T)
curve( dnorm(x, 5, 5/sqrt(200)), add=T, lwd=2 )


###
# zad58
u03mean <- replicate( 10000, mean( runif(3, 2, 8) ) )
u07mean <- replicate( 10000, mean( runif(7, 2, 8) ) )
u10mean <- replicate( 10000, mean( runif(10, 2, 8) ) )
u30mean <- replicate( 10000, mean( runif(30, 2, 8) ) )
u90mean <- replicate( 10000, mean( runif(90, 2, 8) ) )
u200mean <- replicate( 10000, mean( runif(200, 2, 8) ) )

hist(u03mean, prob=T)
hist(u07mean, prob=T)
hist(u10mean, prob=T)
hist(u30mean, prob=T)
hist(u90mean, prob=T)
hist(u200mean, prob=T)


###
# zad59
z1.ci <- function(x.bar, sigma, n, alpha) {
  b1 <- x.bar - qnorm(1-alpha/2)*(sigma/sqrt(n))
  b2 <- x.bar + qnorm(1-alpha/2)*(sigma/sqrt(n))
  c(b1, b2)
}

z1.ci( x.bar=61.9, sigma=4.1, n=66, alpha=0.05 )

z1.ci( x.bar=61.9, sigma=4.1, n=88, alpha=0.05 )


###
# t distribution
curve( dt(x, df=10), from=-5, to=5, lwd=1.8, col="orange" )
curve( dnorm(x,0,1), add=T, lwd=1.8, lty="dashed", col="blue" )

curve( dt(x, df=30), from=-5, to=5, lwd=1.8, col="orange" )
curve( dnorm(x,0,1), add=T, lwd=1.8, lty="dashed", col="blue" )

curve( dt(x, df=90), from=-5, to=5, lwd=1.8, col="orange" )
curve( dnorm(x,0,1), add=T, lwd=1.8, lty="dashed", col="blue" )


###
# zad60
t1.ci <- function(x.bar, s, n, alpha) {
  b1 <- x.bar - qt(1-alpha/2, df=n-1)*(s/sqrt(n))
  b2 <- x.bar + qt(1-alpha/2, df=n-1)*(s/sqrt(n))
  c(b1, b2)
}

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)

t1.ci( x.bar=mean(x), s=sd(x), n=length(x), alpha=0.05 )

t1.ci( x.bar=mean(x), s=sd(x), n=length(x), alpha=0.1 )

t.test( x, conf.level=0.95 )$conf.int[1:2]

t.test( x, conf.level=0.90 )$conf.int[1:2]


###
# zad61
prop1.ci <- function(x, n, alpha) {
  p.hat <- x/n
  b1 <- p.hat - qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n)
  b2 <- p.hat + qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n)
  c(b1, b2)
}

prop1.ci( x=22, n=38, alpha=0.05 )

prop.test( x=22, n=38, conf.level=0.95, correct=F )$conf.int[1:2]


###
# zad62
med1.ci <- function(x, alpha=0.05, nboot=1000) {
  x <- x[is.finite(x)]
  nx <- length(x)
  est1 <- median(x)
  med1.bt <- replicate( nboot, median( sample( x, size=nx, replace=TRUE ) ) )
  ci <- quantile( med1.bt, probs=c(alpha/2, 1-alpha/2), names=FALSE )
  list( est.med1=est1, ci=ci )
}

salk <- read.table("salk.txt")

med1.ci( salk[,1], alpha=0.05, nboot=10000 )
med1.ci( salk[,2], alpha=0.05, nboot=10000 )


###
