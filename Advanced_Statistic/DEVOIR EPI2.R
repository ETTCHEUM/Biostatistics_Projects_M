getwd()

setwd("C:/Users/TCHEUMENI/Documents/MASTER EN BIOSTATISTIQUE/SCRIPT  MODELISATION")
smp <-read.csv2("C:/Users/TCHEUMENI/Documents/MASTER EN BIOSTATISTIQUE/SCRIPT  MODELISATION/smp2.csv")
 
install.packages("prettyR")
library(prettyR)

describe(smp) 
 describe(smp, num.desc=c("min", "max", "valid.n", "mean", "sd"))
 names(smp)
 desc
 
 smp2 <- smp[ smp$dur.interv > 15, ]
 describe(smp2, num.desc=c("min", "max", "valid.n", "mean", "sd"))
 
 names(smp2)
 
 table(smp2$place, useNA = "always")
 
 str(smp2)
 
 
 mod1 <- lm(dur.interv~ scz.cons + dep.cons + abus + grav.cons + char + ptsd.cons + age + factor(place), data= smp ) 
summary (mod1) 


#########  regresion logistique

smp.rl <- smp[,c("dep.cons","ptsd.cons","abus","separation")]
smp.rl <- na.omit(smp.rl)

y <- smp.rl$dep.cons
x1 <- smp.rl$ptsd.cons
x2 <- smp.rl$abus
x3 <- smp.rl$separation


lh <- function(b) {
  resu <- 0
  for(i in 1:nrow(smp.rl)) {
    if (y[i]==1) { resu <- resu + log(exp(b[1]+b[2]*x1[i]+b[3]*x2[i]+b[4]*x3[i])/(1+exp(b[1]+b[2]*x1[i]+b[3]*x2[i]+b[4]*x3[i]))) }
    else         { resu <- resu + log(1-exp(b[1]+b[2]*x1[i]+b[3]*x2[i]+b[4]*x3[i])/(1+exp(b[1]+b[2]*x1[i]+b[3]*x2[i]+b[4]*x3[i]))) }
  }
  resu }
optim(c(0,0,0,0), lh, method="BFGS", control=list(fnscale=-1))

glm(dep.cons~ptsd.cons+abus+separation,data=smp.rl,family="binomial")


names (smp)
describe(smp[, c("suicide.hr","abus","discip","duree","age")], num.desc=c("mean","sd","median","min","max","valid.n"))
describe(factor(smp$place))


getwd()

scl <- read.csv2("C:/Users/TCHEUMENI/Documents/MASTER EN BIOSTATISTIQUE/SCRIPT  MODELISATION/outils_hdrs.csv")
names(scl)


table(scl$NUMERO)
table(scl$VISIT)

install.packages("reshape2")

library (reshape2)
scl.w <- dcast(scl, NUMERO~VISIT, value.var = "HDRS")
scl.w <- scl.w[,-c(1,10)]
int <- scl.w
int[is.na(int)] <- -1

library(dplyr)

install.packages("dplyr")

scl$VISIT.NUM <- recode(scl$VISIT,"J0"=1,"J4"=2,"J7"=3, "J14"=4, "J21"=5, "J28"=6, "J42"=7, "J56"=8 )

scl$VISIT.NUM <- recode(scl$VISIT,J0=1,J4=2,J7=3,J14=4, J21=5,J28=6,J42=7,J56=8)

scl$VISIT.NUM <- recode(scl$VISIT, 1="J0", 2="J4", 3="J7", 4="J14", 5="J21", 6="J28", 7="J42", 8="J56" )

?recode

scl.W$VISIT.NUM <- recode(scl.W$VISIT,"J0"=1,"J4"=2,"J7"=3, "J14"=4, "J21"=5, "J28"=6, "J42"=7, "J56"=8 )

mod3 <- lm(HDRS~factor(VISIT), data=scl)
summary (mod3)

drop1(mod3, .~., test = "F")

scl$VISIT.NUM <- recode(scl$VISIT, "1='J0', 2='J4', 3='J7', 4='J14', 5='J21', 6='J28', 7='J42', 8='J56'" )

scl$VISIT.NUM <- recode(scl$VISIT,J0=1,J4=2,J7=3,J14=4,J21=5,J28=6,J42=7,J56=8)
