library(MASS)
data(survey)
?survey

?is.na   # help
which( is.na(survey$Smoke) )
which( is.na(survey$Pulse) )

sum( survey$Smoke=="Never" )
sum( survey$Smoke=="Never", na.rm=T )


###
# zad36
table(survey$Exer)

attach(survey)
table(Exer)

sort( table(Exer), decreasing=T )
100*table(Exer)/length(Exer)
100*prop.table( table(Exer) )

barplot( table(Exer) )
barplot( sort( table(Exer), decreasing=T ) )
barplot( 100*table(Exer)/length(Exer) )
pie( table(Exer) )
pie( table(Exer), col=c("red", "yellow", "blue") )


###
# zad37
table(Smoke)
table(Smoke, useNA="ifany")
which( is.na(Smoke) )

sort( table(Smoke), decreasing=T )
100*table(Smoke)/length(Smoke)

barplot( table(Smoke) )
barplot( sort( table(Smoke), decreasing=T ) )
barplot( 100*table(Smoke)/length(Smoke) )
pie( table(Smoke) )


###
# zad38
# a
tb1 <- 100*table(Smoke)/length(Smoke)
tb1
tb1[2]
tb1["Never"]
# отг. 79.75

# b
table(Smoke, Exer)
table(Smoke, Exer)[2, 1]
table(Smoke, Exer)["Never", "Freq"]
# отг. 87

# v
tab.smoke.exer <- table(Smoke, Exer)
# Таблица, в която сумата на всички стойности е 100%:
100*prop.table( tab.smoke.exer )
100*prop.table( tab.smoke.exer )[2, 1]
100*prop.table( tab.smoke.exer )["Never", "Freq"]
# отг. 36.86

# g
# Таблица, в която сумата на всеки ред е 100%:
100*prop.table( tab.smoke.exer, margin=1 )
100*prop.table( tab.smoke.exer, margin=1 )[2, 1]
100*prop.table( tab.smoke.exer, margin=1 )["Never", "Freq"]
# отг. 46.03

# d
# Таблица, в която сумата на всяка колона е 100%:
100*prop.table( tab.smoke.exer, margin=2 )
100*prop.table( tab.smoke.exer, margin=2 )[2, 1]
100*prop.table( tab.smoke.exer, margin=2 )["Never", "Freq"]
# отг. 75.65


###
# zad39
barplot( table(Smoke, Exer) )

barplot( table(Smoke, Exer), legend=T )

barplot( table(Smoke, Exer), legend=T,
         args.legend=list(x="top") )

barplot( 100*prop.table( tab.smoke.exer, 2 ), 
         legend=T, xlim=c(0,5),
         args.legend=list(x="right") )


###
# zad40
barplot( table(Exer, Smoke), legend=T, 
         args.legend=list(x="topright") )

barplot( table(Exer, Smoke), legend=T, 
         beside=T,
         args.legend=list(x="topright") )

tab.exer.smoke <- table(Exer, Smoke)

barplot( 100*prop.table( tab.exer.smoke, 2 ), 
         legend=T, xlim=c(0,6),
         args.legend=list(x="right") )


###
# zad41
table(Pulse)
table(Pulse, useNA="ifany")

pulse.grp <- cut(Pulse, breaks=seq(30,110,10))
pulse.grp
table(pulse.grp)

barplot( table(pulse.grp) )
hist(Pulse)
hist(Pulse, breaks=seq(30,110,5))

stripchart(Pulse, method="stack", pch=20)
stripchart(Pulse, method="stack", pch=18)
stripchart(Pulse, method="stack", pch=1)


###
# zad42
table(Age)

age.grp <- cut(Age, breaks=seq(15,75,10))
table(age.grp)

barplot( table(age.grp) )
hist(Age)

stripchart(Age, method="stack", pch=20)


###
