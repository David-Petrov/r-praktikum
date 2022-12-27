###
# zad1
# dice

Nrep <- 20000
dice <- sample( c(1:6), Nrep, replace=T )
sum(dice==6)/Nrep


###
# zad2
# gifts

sim.gifts <- function(k) {
  x <- sample( c(1:k), k, replace=F )
  d <- x - c(1:k)
  any(d==0)
}

prob.gifts <- function(Nrep, k) {
  rs <- replicate( Nrep, sim.gifts(k) )
  sum(rs)/length(rs)
}

prob.gifts(100000, 20)

# for large k 
1-1/exp(1)


###
# zad3
# birthdays

sim.bday <- function(k) {
  x <- sample( c(1:365), k, replace=T )
  anyDuplicated(x) > 0
}

prob.bday <- function(Nrep, k) {
  rs <- replicate( Nrep, sim.bday(k) )
  sum(rs)/length(rs)
}

prob.bday(100000, 25)

# other solutions
prob.bday1 = function(Nrep, k) {
  M <- replicate( Nrep, sample( c(1:365), k, replace=T ) )
  v <- apply( M, 2, anyDuplicated )
  sum(v>0)/length(v) 
}

prob.bday2 = function(Nrep, k) {
  M <- replicate( Nrep, sample( c(1:365), k, replace=T ) )
  v <- apply( M, 2, function(x) max(tabulate(x)) )
  sum(v>=2)/length(v) 
} 

prob.bday1(100000, 25)
prob.bday2(100000, 25)


###
# zad4
# exam 20 questions

sim.exam <- function() {
  q <- c( rep(1,3), rep(2,17) )
  x <- sample( q, 2, replace=F )
  sum(x)==3
}

prob.exam <- function(Nrep) {
  rs <- replicate( Nrep, sim.exam() )
  sum(rs)/length(rs)
}

prob.exam(100000)


###
# zad5
# 5 keys

sim.keys <- function() {
  x <- sample( c(1:5), 5, replace=F )
  x[5]==1
}

prob.keys <- function(Nrep) {
  rs <- replicate( Nrep, sim.keys() )
  sum(rs)/length(rs)
}

prob.keys(100000)


### 
# zad6
# three ants

sim.ants <- function() {
  a <- vector("numeric", 3)
  a[1] <- sample( c(2,3), 1 )
  a[2] <- sample( c(1,3), 1 )
  a[3] <- sample( c(1,2), 1 ) 
  all( c(1,2,3) %in% a ) 
}

prob.ants <- function(Nrep) {
  rs <- replicate( Nrep, sim.ants() )
  sum(rs)/length(rs)
}

prob.ants(100000)


###
# zad7
# cards in boxes

sim.bw <- function() { 
  card <- sample( c("bb", "ww", "bw"), 1 )
  side <- sample( c(1,2), 1 )
  up <- substr( card, start=side, stop=side ) 
  c(up, card)
}

prob.bw <- function(Nrep) {
  rs <- replicate( Nrep, sim.bw() )
  sum(rs[2,]=="ww") / sum(rs[1,]=="w") 
}

sim.bw()
prob.bw(100000)


###
# zad8

sim.balls <- function() {
  x <- sample( c(1:5), 2, replace=T )
  x[1]==x[2]
}

prob.balls <- function(Nrep) {
  rs <- replicate( Nrep, sim.balls() )
  sum(rs)/length(rs)
}  

prob.balls(100000)


###
# zad9

sim.socks <- function() {
  x <- sample( c(1,1,2,2,3,3), 2, replace=F )
  x[1]==x[2]
}

prob.socks <- function(Nrep) {
  rs <- replicate( Nrep, sim.socks() )
  sum(rs)/length(rs)
}

prob.socks(100000)


###