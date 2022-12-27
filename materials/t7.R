###
myplot1 <- function(rs.mean, rs.sd, true.val) {
  i1 <- min(rs.mean) - max(rs.sd)
  i2 <- max(rs.mean) + max(rs.sd)
  
  plot( 1, xlim=c(0,11), ylim=c(i1,i2), pch="", 
        xlab="", ylab="",  xaxt="n" )
  abline( h=true.val, col="red", lwd=1.4 )
  arrows( c(1:10), rs.mean-rs.sd, c(1:10), rs.mean+rs.sd, 
          code=3, angle=90, 
          length=0.05, lwd=1.4, col=c("gray55", "blue3") )
  points( c(1:10), rs.mean, pch=20, type="p", 
          col=c("gray55", "blue3") )
  axis( side=1, at=seq(1.5, 9.5, 2), 
        labels=c(20, 50, 100, 500, 1000), tick=FALSE )
}


###
# zad49
sim.norm.prob <- function(n, mu, sigma, a) {
  x <- rnorm(n, mu, sigma)
  est1 <- pnorm(a, mean(x), sd(x))
  est2 <- sum(x<=a)/n
  c(est1, est2)
}

# a = 3.4
rs1 <- replicate( 30000, sim.norm.prob(n=20, mu=5, sigma=1, a=3.4) )
rs2 <- replicate( 30000, sim.norm.prob(n=50, mu=5, sigma=1, a=3.4) )
rs3 <- replicate( 30000, sim.norm.prob(n=100, mu=5, sigma=1, a=3.4) )
rs4 <- replicate( 30000, sim.norm.prob(n=500, mu=5, sigma=1, a=3.4) )
rs5 <- replicate( 30000, sim.norm.prob(n=1000, mu=5, sigma=1, a=3.4) )

rs <- rbind( rs1, rs2, rs3, rs4, rs5 )

rs.mean <- apply(rs, 1, mean)
rs.sd <- apply(rs, 1, sd)
true.val <- pnorm(3.4, 5, 1)

myplot1(rs.mean, rs.sd, true.val)

# a = 4.5
rs1 <- replicate( 30000, sim.norm.prob(n=20, mu=5, sigma=1, a=4.5) )
rs2 <- replicate( 30000, sim.norm.prob(n=50, mu=5, sigma=1, a=4.5) )
rs3 <- replicate( 30000, sim.norm.prob(n=100, mu=5, sigma=1, a=4.5) )
rs4 <- replicate( 30000, sim.norm.prob(n=500, mu=5, sigma=1, a=4.5) )
rs5 <- replicate( 30000, sim.norm.prob(n=1000, mu=5, sigma=1, a=4.5) )

rs <- rbind( rs1, rs2, rs3, rs4, rs5 )

rs.mean <- apply(rs, 1, mean)
rs.sd <- apply(rs, 1, sd)
true.val <- pnorm(4.5, 5, 1)

myplot1(rs.mean, rs.sd, true.val)


###
# zad50
sim.unif.med <- function(n, b){
  x <- runif(n, 0, b)
  est1 <- max(x)/2
  est2 <- median(x)
  c(est1, est2)
}

rs1 <- replicate( 30000, sim.unif.med(n=20, b=8) )
rs2 <- replicate( 30000, sim.unif.med(n=50, b=8) )
rs3 <- replicate( 30000, sim.unif.med(n=100, b=8) )
rs4 <- replicate( 30000, sim.unif.med(n=500, b=8) )
rs5 <- replicate( 30000, sim.unif.med(n=1000, b=8) )

rs <- rbind( rs1, rs2, rs3, rs4, rs5 )

rs.mean <- apply(rs, 1, mean)
rs.sd <- apply(rs, 1, sd)
true.val <- 4

myplot1(rs.mean, rs.sd, true.val)


###
# zad51
sim.exp.med <- function(n, lambda) {
  x <- rexp(n, lambda)
  est1 <- mean(x)*log(2)
  est2 <- median(x)
  c(est1, est2)
}

rs1 <- replicate( 30000, sim.exp.med(n=20, lambda=1/5) )
rs2 <- replicate( 30000, sim.exp.med(n=50, lambda=1/5) )
rs3 <- replicate( 30000, sim.exp.med(n=100, lambda=1/5) )
rs4 <- replicate( 30000, sim.exp.med(n=500, lambda=1/5) )
rs5 <- replicate( 30000, sim.exp.med(n=1000, lambda=1/5) )

rs <- rbind( rs1, rs2, rs3, rs4, rs5 )

rs.mean <- apply(rs, 1, mean)
rs.sd <- apply(rs, 1, sd)
true.val <- 5*log(2)

myplot1(rs.mean, rs.sd, true.val)


###
# zad52
sim.norm.sd <- function(n, mu, sigma) {
  x <- rnorm(n, mu, sigma)
  x.bar <- mean(x)
  est1 <- sqrt( sum((x - x.bar)^2)/n )
  est2 <- sqrt( sum((x - x.bar)^2)/(n-1) )
  c(est1, est2)
}

rs1 <- replicate( 30000, sim.norm.sd(n=20, mu=3, sigma=2) )
rs2 <- replicate( 30000, sim.norm.sd(n=50, mu=3, sigma=2) )
rs3 <- replicate( 30000, sim.norm.sd(n=100, mu=3, sigma=2) )
rs4 <- replicate( 30000, sim.norm.sd(n=500, mu=3, sigma=2) )
rs5 <- replicate( 30000, sim.norm.sd(n=1000, mu=3, sigma=2) )

rs <- rbind( rs1, rs2, rs3, rs4, rs5 )

rs.mean <- apply(rs, 1, mean)
rs.sd <- apply(rs, 1, sd)
true.val <- 2

myplot1(rs.mean, rs.sd, true.val)

# 52 b
rs.mean2 <- apply(rs^2, 1, mean)
rs.sd2 <- apply(rs^2, 1, sd)
true.val2 <- 4

myplot1(rs.mean2, rs.sd2, true.val2)

# (!)  rs.mean2  е различно от  (rs.mean)^2


###
