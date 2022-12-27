###
# zad76
sim1 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  ci <- t.test(x, conf.level=1-alpha)$conf.int[1:2]
  cnd1 <- ( ci[1] <= 7 & ci[2] >= 7 )
  cnd1
}

rs1 <- replicate( 10000, sim1(n=500, alpha=0.05) )
table(rs1)/length(rs1)


###
# zad77
sim2 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  p.val <- t.test(x, mu=7)$p.value
  cnd2 <- ( p.val > alpha )
  cnd2
}

rs2 <- replicate( 10000, sim2(n=500, alpha=0.05) )
table(rs2)/length(rs2)


###
# zad78
sim3 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  ci <- t.test(x, conf.level=1-alpha)$conf.int[1:2]
  p.val <- t.test(x, mu=7)$p.value
  cnd1 <- ( ci[1] <= 7 & ci[2] >= 7 )
  cnd2 <- ( p.val > alpha )
  # cnd1 == cnd2
  c( cnd1, cnd2 )
}

rs3 <- replicate( 10000, sim3(n=500, alpha=0.05) )
cnd1r <- rs3[1, ]
cnd2r <- rs3[2, ]
table(cnd1r, cnd2r)


###
# zad79
sim4 <- function(n, mu, delta) {
  x <- rnorm(n, mean=mu, sd=1)
  y <- rnorm(n, mean=mu+delta, sd=1)
  t.test(x,y)$p.value
}

rs4 <- replicate( 10000, sim4(n=1000, mu=5, delta=0.2) )
sum(rs4<=0.05)/length(rs4)


###
# zad80
# p_i = вероятност да се падне i, i=1...6
# H0: p_i = 1/6, i=1...6
# H1: p_i != 1/6 за поне едно i

x <- c(28, 36, 36, 30, 27, 23)
probs <- rep(1/6, 6)

n <- sum(x)
k <- length(probs)
chi2.obs <- sum( (x - n*probs)^2 / (n*probs) )
chi2.obs
p.value <- 1-pchisq(chi2.obs, df=k-1)
p.value

chisq.test(x, p=probs)
chisq.test(x, p=probs)$p.value

# p.value > 0.05 
# => нямаме основания да отхвърлим H0.
# Можем да считаме, че зарът е правилен 
# (всяка от страните се пада с вероятност 1/6).

tb1 <- rbind( probs, x/sum(x) )
colnames(tb1) <- as.character(c(1:6))
rownames(tb1) <- c("probability (H0)", "observed freq")
barplot(tb1, beside=T, legend=T, ylim=c(0,0.3))


###
# zad81
# p_E = вероятност да срещнем буква Е
# p_T = вероятност да срещнем буква T
# ...
# H0: (p_E, p_T, ...) = (0.1202, 0.0910, ...)
# H1: (p_E, p_T, ...) != (0.1202, 0.0910, ...)

load("letterFreq.RData")
probs*100
x1

chisq.test(x1, p=probs)

# p.value < 0.05 
# => отхвърляме H0 в полза на H1.
# Нямаме основания да твърдим, че вероятностите за срещане 
# на съответните букви са както в английския език.

tb1 <- rbind( probs, x1/sum(x1) )
rownames(tb1) <- c("probability (H0)", "observed freq")
barplot(tb1, beside=T, legend=T, args.legend=list(x="top"))


###
# zad84
library(MASS)
tb <- table( survey$Sex, survey$Smoke )
tb

# H0: пушенето и пола са независими
# H1: пушенето и пола не са независими (има връзка между пушене и пол)

n <- sum(tb)
df <- (nrow(tb)-1)*(ncol(tb)-1)
sex <- apply(tb, 1, sum)
smk <- apply(tb, 2, sum)

expected <- (sex %o% smk)/n
observed <- tb
chi2.obs <- sum( (observed - expected)^2 / expected )
chi2.obs
p.value <- 1-pchisq(chi2.obs, df)
p.value

chisq.test(tb)

# p.value > 0.05 
# => нямаме основания да отхвърлим нулевата хипотеза.
# Нямаме основания да твърдим, че има връзка между пушенето и пола.

observed
chisq.test(tb)$observed
expected
chisq.test(tb)$expected


###
# zad85
eyecol <- read.table("ManWomanEye.txt", header=T)
attach(eyecol)
tb <- table(man, woman, deparse.level=2)
tb

# H0: цвета на очите на мъжа и цвета на очите на жената са независими
# H1: има връзка между цвета на очите на мъжа и цвета на очите на жената

n <- sum(tb)
df <- (nrow(tb)-1)*(ncol(tb)-1)
males <- apply(tb, 1, sum)
females <- apply(tb, 2, sum)

expected <- (males %o% females)/n
observed <- tb
chi2.obs <- sum( (observed - expected)^2 / expected )
chi2.obs
p.value <- 1-pchisq(chi2.obs, df)
p.value

chisq.test(tb)

# p.value < 0.05 
# => отхвръляме нулевата хипотеза в полза на алтернативната.
# Можем да твърдим, че има връзка между
# цвета на очите на мъжа и цвета на очите на жената.

mosaicplot(tb)
barplot(tb)
barplot( prop.table(tb, 1) )
barplot( prop.table(tb, 2) )


###

