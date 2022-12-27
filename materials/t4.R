###
# zad26
par(mfrow=c(1,2))

x1 <- runif(500, 2, 3)
hist(x1, probability=T)
curve( dunif(x, 2, 3), from=2, to=3, add=T, lwd=2 )

x1 <- runif(5000, 2, 3)
hist(x1, probability=T)
curve( dunif(x, 2, 3), from=2, to=3, add=T, lwd=2 )


###
# zad27
par(mfrow=c(1,2))

x2 <- rexp(500, 1/7)
hist(x2, probability=T)
curve( dexp(x, 1/7), from=0, to=max(x2), add=T, lwd=2 )

x2 <- rexp(5000, 1/7)
hist(x2, probability=T)
curve( dexp(x, 1/7), from=0, to=max(x2), add=T, lwd=2 )


###
# zad28
par(mfrow=c(1,2))

x3 <- rnorm(500, 0, 1)
hist(x3, probability=T)
curve( dnorm(x, 0, 1), add=T, lwd=2 )

x3 <- rnorm(5000, 0, 1)
hist(x3, probability=T)
curve( dnorm(x, 0, 1), add=T, lwd=2 )


###
# zad29
# X = количество душ гел в опаковка
# X ~ U(248, 255)

# P(X<250) = P(X<=250)
punif(250, 248, 255)

# v=? P(X=>v) = 0.95
# P(X<v) = 0.05
qunif(0.05, 248, 255)

punif(248.35, 248, 255)


###
# zad30
# X = време на живот на лазерен диод
# X ~ Exp(1/10)

# P(X>10) = 1 - P(X<=10)
1 - pexp(10, 1/10)

# P(7 < X < 11) = P(X<11) - P(X<7)
pexp(11, 1/10) - pexp(7, 1/10)

# t=? P(X=>t) = 0.97
# P(X<t) = 0.03
qexp(0.03, 1/10)

pexp(0.3045921, 1/10)


###
# zad31
# X = време от зареждане до изтощаване на батерия
# X ~ N(mu=260, sigma=50)

# P(X>240) = 1 - P(X<=240)
1 - pnorm(240, 260, 50)

# P(180 < X < 300) = P(X<300) - P(X<180)
pnorm(300, 260, 50) - pnorm(180, 260, 50)

# t=? P(X>t) = 0.9
# P(X<=t) = 0.1
qnorm(0.1, 260, 50)


###
# zad32
# X = изразходвано количество кашкавал за седмица
# X ~ N(mu=41, sigma=5)

# P(X>51) = 1 - P(X<=51)
1 - pnorm(51, 41, 5)

# t=? P(X<=t) = 0.99
qnorm(0.99, 41, 5)


###
# zad33
# X = количество бира, изпита от Иван за една седмица
# X ~ Exp(1/3)

# P(X>10) = 1 - P(X<=10)
p1 <- 1 - pexp(10, 1/3)

# Y = брой седмици (от следв. 5) в които Иван изпива повече от 10 литра
# Y ~ Bi(n=5, p=p1)

# P(Y<=3)
pbinom(3, 5, p1)


###
