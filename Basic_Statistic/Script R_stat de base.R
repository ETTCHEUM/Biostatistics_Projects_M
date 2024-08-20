## TP retinol (stat de base)

# definition de l'environnement numérique.
getwd()

setwd("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/STAT DE BASE/TEST STAT DE BASE")

ret <- read.csv2("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/STAT DE BASE/presentationTPretinol.csv")


## explorations des variables
str(ret)

# changement du modes des variables.

#####sexe

ret$sexe.b <- as.factor(ret$sexe) 

ret$sexe.b <- factor(ret$sexe, levels= c("1", "2"), labels =c("Masculin", "Féminin"))
                        
levels(ret$sexe.b) 
str(ret$sexe) 
ret$sexe

####Tabac

ret$tabac
ret$tabac.c <- as.factor(ret$tabac)
levels(ret$tabac)
ret$tabac.c <- factor(ret$tabac.c, levels = c("1", "2", "3"), labels = c("jamais", "autrefois", "actuellement"))
ret$tabac

#### Vitamines

ret$vitamine.c <- as.factor(ret$vitamine)
levels(ret$vitamine)

ret$vitamine.c <- factor(ret$vitamine.c, levels = c("1", "2", "3"), labels = c("oui souvent", "oui pas souvent", "non"))


str(ret)


###Description des variables

## 1) description synthétique

install.packages("prettyR")
library(prettyR)

describe(ret, num.desc = c("mean", "median", "var", "sd", "valid.n", "min", "max"))


###2) description graphique


### 2a) variables qualitatives

# sexe

table(ret$sexe, useNA = "always")
prop.table(table(ret$sexe))

barplot(table(ret$sexe), col = "pink")
pie(table(ret$sexe, useNA = "always"))

### Tabac
table(ret$tabac, useNA = "always")
barplot(table(ret$tabac), col = "green", main = "Diagramme en baton")
pie(table(ret$tabac))

#### Vitamines

table(ret$vitamine)
barplot(table(ret$vitamine), col = "yellow", main = "Diagramme en batons des vitamines", xlab = "Catégories" )
pie(table(ret$vitamine)).


####2b) variables quantitatives

##1) age

boxplot(ret$age)

hist(ret$age, xlab = "age",ylim=c(0,0.031) , breaks = 10, prob=T, ylab = "percent", col = rainbow(3, s=1, v=1), main = "Distribution d'age")
lines(density(ret$age))

plot(ret$age)
qqline(ret$age)
qqnorm(ret$age)

###2) cholesterol

boxplot(ret$cholesterol)
hist(ret$cholesterol, col="pink")
lines(density(ret$cholesterol))

help(abline)

### relations existante entre toutes les paires possibles de variables

names (ret)


install.packages("corrplot")

library(corrplot)

varia1 <- c("retplasma", "age", "sexe", "tabac",  "cholesterol", "retdiet",  "bmi", "alcool",  "vitamine")


round(cor(ret[,varia1], use= "complete.obs"),2)
 

corrplot(cor(ret[, varia1]), methods= "circle", bg= "yellow")            


## analyse à composante principales


install.packages("psy")
library(psy)


mdspca(ret[,varia1])

sphpca(ret[,varia1])

?mdspca

str(ret)
 ret$vitamine.c 
  
  
####regression lineaire et recherchons les interaction entre les variables


mod1 <- lm(retplasma ~ age + sexe.b + tabac.c + cholesterol + retdiet +  bmi + alcool + vitamine.c, data = ret)
summary(mod1)

drop1(mod1,.~., test = "F")
exp(coef(mod1))


mod2 <- lm(retplasma~ age*sexe.b +tabac.c + cholesterol + retdiet+  bmi +alcool+ vitamine.c, data = ret)
summary(mod2)
drop1(mod2,.~., test = "F")
exp(coef(mod2))

# vérifions les condition de validité du model
   # normalité
hist(resid(mod2), col= "green", probability = T, ylim = c(0, 0.0025))
lines(density(resid(mod2)))

qqnorm(resid(mod2), prob=T, main = "qqplot")
qqline(resid(mod2))


?qqnorm

# transformons la variables retinol en deux au niveau de la mediane et recourron a la regression logistique.

median(ret$retplasma)


ret$retplasma.b <- ifelse(ret$retplasma> median(ret$retplasma), 1, 2)
table(ret$retplasma, ret$retplasma.b, useNA = "ifany")
 table(ret$retplasma.b, useNA = "ifany")
 
 
 ## regression logistique
 
 modl3 <- glm(retplasma.b ~ age + sexe.b + tabac.c + cholesterol + retdiet + bmi + alcool + vitamine.c, data = ret, family= "binomial")
 summary(modl3)
drop1(modl3, .~., test = "Chisq")
 exp(coef(modl3))
 
 but <- glm(retplasma.b ~ age + sexe.b, data = ret, family = "binomial")
 summary(but)
 
 ?glm
 
 ## condition de validité
 
 qqnorm(resid(modl3))
 qqline(resid(modl3))

 

 
 
 apply(ret$age, ret$sexe.b, mean) 
 
 table(ret$age, ret$sexe.b, useNA = "ifany")
 
 
 
 
 
 
 
t.test(ret$age~ret$sexe.b)

t.test(ret$ageret$sexe.b==1], ret$age[ret$sexe.b==2])

ret$age[ret$sexe.b == 1]


table(ret$age[ret$sexe.b==1])


anova(lm(ret$age~ret$vitamine.c))
anova(lm(ret$age~ret$tabac.c))
anova(lm(ret$age~ret$sexe.b))


boxplot(ret$age~ret$sexe.b, prob=T, col="pink")

plot(ret$age, ret$bmi)

abline(lm(ret$age~ ret$bmi))

chisq.test(ret$age, ret$cholesterol)


fb <- function (x){
  describe(x, num.desc = c("min", "max"))
}

fb(ret)


tapply(ret$age, ret$sexe.b, mean)

t.test(ret$age~ret$sexe.b)

boxplot(ret$age~ret$tabac)
anova(lm(ret$age~ret$tabac))
table(ret$age, ret$tabac)
? table

tapply(ret$age, ret$tabac, mean)


chisq.test(ret$age, ret$bmi)

t.test(ret$age, mu=50)

? t.test


miss <- (100x/)

fop <- function(x){
  
  describe(x, num.desc = c("mean", "var", "sd", "valid.n", miss))
  
}


t.test(ret$age[ret$sexe.b == Masculin], ret$age[ret$sexe.b == Féminin])

with(ret, t.test(age[sexe.b == Masculin], age[sexe.b == Féminin]))



