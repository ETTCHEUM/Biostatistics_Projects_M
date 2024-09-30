
 Etienne ###  DOMMINIQUE statistique avancée


getwd ()
setwd("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Fichier U15 BIOSTAT")

table1 <- read.table("table_mere.txt", dec= ".",  sep="/t", na.strings = "NA")
help(read.txt)

table1 <- read.table("table_mere.txt", sep="\t", dec=".", na.string="NA", header=T)

table1 <- read.table("table_mere.txt", sep="\t", dec=".", na.string="NA", header=T)

head(table1)
dim(table1)
str(table1)

table(table1$Poids, useNA = "always")
table1$poids
table1$poids

table1[,"age"]

table1[rev(order(table1$age)),]

table1[ ,table1$age]

table1$age_cat <- ifelse (is.na(table1$age), NA,
                                 ifelse(table1$age <= 25, 1,
                                         ifelse(table1$age <= 30, 2,
                                                ifelse (table1$age <= 35, 3, 4)))

table1$age_cat <- ifelse(table1$age <= 25, 1, ifelse(table1$age <= 30, 2, ifelse(table1$age <= 35, 3, 4)))

    

table (table1$age_cat)                      

tapply(table1$age, table1$age_cat, str) 


str(table1)

table1$age_cat <- as.factor(table1$age_cat)

table1$age_cat_label <- factor(table1$age_cat, levels = c(1,2,3,4), labels = c("<=25", "25-30", "30-35", "<35"))

table (table1$age_cat_label)

tapply(table1$age, table1$age_cat_label, summary)

help ("tapply")


table1[order(table1$age,table1$bmi)]

names(table1)


fum <- table1[table1$Tabac==1, ]

table(fum$Tabac, fum$age_cat_label)

prop.table(table(fum$Tabac, fum$age_cat_label))

table( fum$age_cat_label, fum$Tabac, useNA = "always")

table (fum$Tabac, useNA = "always")

fummeuse <- table1[!is.na(table1$Tabac) & table1$Tabac==1,]

table1$fumeur4cl <- ifelse(table1$Tabac==1 & table1$age <20, 1,
                           ifelse(table1$Tabac==1 & table1$age >=20, 2,
                                  ifelse(table1$Tabac==0 & table1$age <20, 3, 
                                        ifelse(table1$Tabac==0 & table1$age >=20, 4, NA) )))

table(table1$age, table1$fumeur4cl)


table(table1$fumeur4cl, table1$Tabac)

tapply(table1$age, table1$fumeur4cl, summary)


## merge 2 fonctions


table_enfant <- read.csv2("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Fichier U15 BIOSTAT/table_enfant.csv")
table_mere <- read.table("table_mere.txt", header=T, sep= "\t", na.string = "NA", dec = ".")

table1 <- read.table("table_mere.txt", sep="\t", dec=".", na.string="NA", header=T)



help(read.table)


table_mere_enfant <- merge(table_mere, table_enfant, by="id")

tb1 <- merge(table_mere, table_enfant, by="id", all.x = T)

tb2 <- merge(table_mere, table_enfant, by="id", all.y = T)

tb3 <- merge(table_mere, table_enfant, by="id", all = T)

indiv_new2 <- read.table("indiv_new2.txt", header = T)

install.packages("tidiverse")
library(plyr)

table4 <- rbind.fill(table_mere, indiv_new2)


table (table1$Tabac, table1$age_cat, useNA = "always")

prop.table(table (table1$Tabac, table1$age_cat, useNA = "always"))

prop.table(table (table1$Tabac, table1$age_cat, useNA = "always"),1)
prop.table(table (table1$Tabac, table1$age_cat, useNA = "always"),2)


pie(table1$age_cat)

pie(table(table1$age_cat_label, useNA = "always"))

pie(table(table1$age_cat, useNA = "always"))


packageName(tidiverse)

barplot(table(table1$age_cat, useNA = "always"), xlab = "age_cat", ylab = "tail",col = "pink",  main = "barplot" )

hist(table(table1$age_cat, useNA = "always"), xlab = "age_cat", ylab = "tail",col = "pink",  main = "barplot" )

boxplot(table1$bmi~table1$age_cat_label)
help(scatter.plot)

scatter.smooth(table1$bmi~table1$age)
abline(lm(table1$bmi~table1$age_cat_label))

hist(table(table1$bmi))
abline(lm(table1$bmi))

plot(table1$bmi, table1$age)
lines(table1$bmi)


t.test(table1$bmi, table1$age)

install.packages("prettyR")

library(prettyR)

str(table1)
describe (table1, num.desc = c("min", "max", "valid.n", "sd", "mean", "median" ))

table1$Tabac <- as.factor(table1$Tabac)

names(table1)

glm(Tabac ~ Poids + age + Taille , table1, family = "binomial" )

help(glm)

help("describe")

table(table1$age_cat, table1$Tabac, deparse.level = 2)

boxplot(table1$age_cat~ table1$Tabac)

boxplot(table1$Tabac ~ table1$age_cat)


hist(table1$Taille, breaks = 3, )
lines(density(table1))

boxplot(table_mere_enfant$poidsnaiss ~ table1$Tabac)

plot(table_mere_enfant$poidsnaiss ~ table1$age, col="red")
abline(lm(table_mere_enfant$poidsnaiss ~ table1$age))

table (tb1$Sexe)

CHI2=chisq.test(table (tb1$Sexe), correct = F)

CHI2$expected


getwd()

load(file = )

help(load)

load("tp3.RData")


names(tp2_ex3)
str(tp2_ex3)

table(tp2_ex3$ttt, tp2_ex3$succes, deparse.level=2)

prop.table(table(tp2_ex3$ttt, tp2_ex3$succes, deparse.level=2))


binom.test(397, 1268)


table(tp2_ex3$fumeur, tp2_ex3$succes, deparse.level=2)

FF=chisq.test(table(tp2_ex3$fumeur, tp2_ex3$succes, deparse.level=2), correct = F)
FF

FF$expected

str(table_dosage)
table(table_dosage$dosage, table_dosage$castem)
t.test(table_dosage$dosage, table_dosage$castem)

t.test(table_dosage$dosage~table_dosage$castem)


t.test(table_dosage$dosage[table_dosage$castem==0], mu=50)

t.test(table_dosage$dosage[table_dosage$castem==0], table_dosage$dosage[table_dosage$castem==1])



## ANOVA


model1 <- lm(Taille~age_cat_label, table1)
exp(coef(model1))


mat1 <- matrix(1:12, nrow = 3, ncol = 4)

mat1

exp(confint(model1))
help(two by two)

Tt <- c("6,7, 9, 4, 0, 9, 7, 6, 5)



help(2 by 2)


anova(lm(Taille~age_cat_label, table1))


drop1(model1,.)
help(drop1)


t.test(table1$Taille~table1$age_cat_label)


cor.test(table1$Poids, table1$Taille)


cor.test(table1$Poids, table1$bmi)



help(cor)


summary(model1)

drop1(model1, ., chisq)

help (drop1)


str(tb1)

table(tb1$Sexe)


tb1$Sexe <- as.factor(tb1$Sexe)

tb1$Tabac <- as.factor(tb1$Tabac)

attach(tb1)
lm( poidsnaiss ~ Tabac, Taille, data=tb1)


lm(data=tb1,  poidsnaiss ~ Tabac, Taille )


lm( tb1$poidsnaiss ~ tb1$Tabac, tb1$Taille)


glm(  Sexe~  Taille + poidsnaiss, data=tb1, family = binomial)

names(tb1)


install.packages("psy")

library(psy)


cor(age ~ Tabac + Taille + Poidsnaiss, data=tb1)

cor(data=tb1, age ~ Tabac,
    
    
  

cor(data= tb1, age, Taille, na.rm=T) 

mfrow


X <- rnorm(50)  
Y <- rnorm(50) 

plot(density(Y))

plot(X,Y)



hist(table1$age, col = "pink", nclass = 7,lines(density(table1$age)))


help(hist)

density(X,Y)


cor( tb1$age, tb1$Taille) 

help(cor)

function(x) {x^2} 

X <- 4

function(4)


help(glm)

class(tb1)

ncaracter
nchar("actuariat")



ble <- c("4, 9, 8, 7, 5, 3, 2, 8, 4, 3")

ble <- c(r=4, g=9)

ble <- c(1:10)
1:10

5+7

unique(Tt)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 