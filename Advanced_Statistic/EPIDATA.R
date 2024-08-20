##### Datamanagement / Description #####

library(prettyR)
library(RColorBrewer)

install.packages("gtsummary")
library(gtsummary)
library(ggplot2)


installed.packages("mice")
library(mice)

 
library(MatchIt)
 installed.packages("MatchIt")
 
library(MatchThem)
 installed.packages("MatchThem")
 

installed.packages("cobalt")
 installed.packages("")
 library(cobalt)
 
 installed.packages("modelsummary")
library(modelsummary)
 
 
library(survival)
library(survminer)
library(tidyverse)

 installed.packages("patchwork")
 library(patchwork)
 
 install.packages("Hmisc")
 library(Hmisc)
 
 install.packages("survival")
 install.packages("lattice")
 library(lattice)
 install.packages("ggplot2")
 install.packages("Hmisc")
 ?hmisc
 

### COULEURS ###
col.qual<-brewer.pal(n = 8, name = 'Set2')

col.seq<-brewer.pal(n = 9, name = 'YlGn')
col.div<-brewer.pal(n = 11, name = 'RdYlBu')#'PiYG')

### Fonctions pour prettyR::describe()
nmiss<-function(x,na.rm=FALSE) { sum(is.na(x)) }
pctmiss<-function(x,na.rm=FALSE) { sum(is.na(x))/length(x)*100 }
q1<- function(x,na.rm=TRUE) {
  if(na.rm) x<-x[!is.na(x)]
  return(quantile(x, probs=0.25, na.rm=TRUE)) }
q3<- function(x,na.rm=TRUE) {
  if(na.rm) x<-x[!is.na(x)]
  return(quantile(x, probs=0.75, na.rm=TRUE)) }

### lecture fichier csv
df_rhc<-read.csv2('C:/Users/TCHEUMENI/Documents/MASTER EN BIOSTATISTIQUE/EPIDEMIO WORKS/EPIDE WORKS CODE/rhc devoir épidémio-RC csv.csv',
                  stringsAsFactors = T)
str(df_rhc)
head(df_rhc)

# Transformation variable Medical History en facteur
var_mh<-c('CARDIOHX','CHFHX','DEMENTHX','PSYCHHX','CHRPULHX','RENALHX',
          'LIVERHX','GIBLEDHX','MALIGHX','IMMUNHX','TRANSHX','AMIHX')
df_rhc[,var_mh]<-lapply(df_rhc[,var_mh], factor, level=c(0,1), 
                        labels=c('No','Yes'))
var_dat<-c('SADMDTE','DTHDTE','LSTCTDTE','DSCHDTE')

# Transformation date factor en date format date

df_rhc[,var_dat]<-lapply(df_rhc[,var_dat], as.Date, format='%d/%m/%Y')

# Transormation Activities of Daily Living
df_rhc$ADLD3P<-factor(df_rhc$ADLD3P)

# Transormation EDU en catégories
df_rhc$EDU.cat<-cut(df_rhc$EDU, breaks=c(-Inf,6,9,12,Inf),
                    labels=c('Primary','Lower Secondary','Upper secondary',
                             'Post-secondary and tertiary'))
with(df_rhc,(table(EDU,EDU.cat)))
# Transormation SCOMA1 en catégories
df_rhc$SCOMA1.cat<-cut(df_rhc$SCOMA1,breaks=c(-Inf,44,Inf),
                       labels=c('Minor-moderate','Severe'))
with(df_rhc,(table(SCOMA1,SCOMA1.cat)))

# Changement de référence
df_rhc$INCOME<-relevel(df_rhc$INCOME, ref=4)
df_rhc$CA<-factor(df_rhc$CA, levels=c('No','Yes','Metastatic'))
df_rhc$RACE<-relevel(df_rhc$RACE, ref=3)

# Changement de valeur manquante
levels(df_rhc$CAT2)[1]<-'NA'

# Description globale des items
prettyR::describe(df_rhc, num.desc = c("valid.n", "pctmiss","mean","sd","min",
                                       "q1","median","q3","max"))

# Modifications valeurs
df_rhc[df_rhc$WTKILO1<30,'WTKILO1']<-NA
df_rhc[df_rhc$TEMP1==0,'TEMP1']<-NA
df_rhc[df_rhc$MEANBP1==0,'MEANBP1']<-NA
df_rhc[df_rhc$RESP1==0,'RESP1']<-NA
df_rhc[df_rhc$HRT1==0,'HRT1']<-NA
df_rhc[df_rhc$PAFI1<30|df_rhc$PAFI1>650,'PAFI1']<-NA
df_rhc[df_rhc$PACO21<10,'PACO21']<-NA
df_rhc[df_rhc$HEMA1<10,'HEMA1']<-NA
df_rhc[df_rhc$ALB1>10,'ALB1']<-NA

prettyR::describe(df_rhc, num.desc = c("valid.n", "pctmiss","mean","sd","min",
                                       "q1","median","q3","max"))

# Vérification date #
table(with(df_rhc, SADMDTE>DTHDTE), useNA = 'always')
table(with(df_rhc, SADMDTE>LSTCTDTE), useNA = 'always')
table(with(df_rhc, SADMDTE>DSCHDTE), useNA = 'always')
table(with(df_rhc, LSTCTDTE>DTHDTE), useNA = 'always')
df_rhc[with(df_rhc, !is.na(DTHDTE) & LSTCTDTE>DTHDTE),
       c('PTID', 'DEATH',var_dat)]
(ptid_lst<-df_rhc[with(df_rhc, !is.na(DTHDTE) & LSTCTDTE>DTHDTE),'PTID'])
df_rhc[df_rhc$PTID %in% ptid_lst,'LSTCTDTE']<-df_rhc[df_rhc$PTID %in% ptid_lst,
                                                     'DTHDTE']
table(with(df_rhc, DSCHDTE>DTHDTE), useNA = 'always')
table(with(df_rhc, DSCHDTE>LSTCTDTE), useNA = 'always')

# Check DTH30 & T3D30
df_rhc$delay.dth<-ifelse(is.na(df_rhc$DTHDTE),NA,df_rhc$DTHDTE-df_rhc$SADMDTE)
with(df_rhc, df_rhc[DTH30=='Yes'&T3D30!=delay.dth,c('PTID','SADMDTE','DTHDTE',
                                                    'T3D30','delay.dth')])
with(df_rhc, table(DTH30, delay.dth>30, useNA = 'always'))

# Suppression variables valeurs manquantes
df_rhc<-subset(df_rhc, select=-c(URIN1,CAT2,ADLD3P))
# Suppression variables inutiles
df_rhc<-subset(df_rhc, select=-c(SADMDTE,DSCHDTE,DTHDTE,LSTCTDTE,SURV2MD1,DEATH,
                                 delay.dth,EDU,SCOMA1))

### groupes de variables
var.qt<-names(Filter(is.numeric,subset(df_rhc,select=-c(ROWNAMES,PTID,T3D30))))
var.ql<-names(Filter(is.factor,df_rhc))

### Description des variables
table1 <- tbl_summary(
  df_rhc[,c(var.qt, var.ql)],
  statistic = list(all_continuous() ~'{mean} ({sd}) - {min},{max} 
                     - {median} ({p25}-{p75}) - {p_miss}%',
                   all_categorical() ~'{n} ({p}%)'),
  digits = all_continuous() ~2,
  missing='no') %>%
  modify_header(label='**Variable**') %>%
  bold_labels() %>%
  italicize_levels()
table1

distr_plot_qt<-function(var,lab,ymax=0,cells=40) {
  #png(paste('output/descr_',var,'.png',sep=''), width=500, height=250) 
  layout(mat=matrix(c(2,1), nrow=2, ncol=1),heights=c(1,3))
  par(mar = c(5, 4, 0, 0))
  m<-mean(df_rhc[,var], na.rm=TRUE)
  stddev<-sd(df_rhc[,var], na.rm=TRUE)
  rangenorm<-c(m-3.72*stddev,m+3.72*stddev)
  density1 <- density(df_rhc[,var], na.rm = TRUE)
  density2 <- dnorm(pretty(rangenorm, 200), mean = m, sd = stddev)
  if (max(density1$y)>ymax) ymax<-max(density1$y);
  hist(df_rhc[,var], ylim=c(0,ymax), col=col.qual[3], breaks=cells,
       xlab=lab, ylab='Densité', freq=FALSE, main='', cex.lab=1.2)
  lines(density1, lty=2, lwd=2,col=col.qual[2])
  lines(pretty(rangenorm,200), density2, lty=2, lwd=2,col=col.qual[1])
  par(mar = c(0, 4, 0, 0))
  boxplot(df_rhc[,var], col=col.qual[3], xaxt='n',yaxt='n',bty='n',
          frame=F,horizontal=TRUE, pars=list(outcol=col.qual[2]))
  #dev.off() 
}

distr_plot_qt('AGE', 'Age',ymax=0.0275)
distr_plot_qt('DAS2D3PC', 'Duke Activity Status Index',ymax=0.2) 
distr_plot_qt('T3D30', 'Temps de suivi') 
distr_plot_qt('APS1', 'Apache III score') 
distr_plot_qt('WTKILO1', 'Weight in Kg')
distr_plot_qt('TEMP1', 'Temperatue',cells=60) 
distr_plot_qt('MEANBP1', 'Mean blood pressure',ymax=0.0225) 
distr_plot_qt('RESP1', 'Respiratory rate',ymax=0.05) 
distr_plot_qt('HRT1', 'Heart rate') 
distr_plot_qt('PAFI1', 'PAO2/FIO2 ratio',ymax=0.0065) 
distr_plot_qt('PACO21', 'PaCO2') 
distr_plot_qt('PH1', 'pH') 
distr_plot_qt('WBLC1', 'WBC') 
distr_plot_qt('HEMA1', 'Hematocrit') 
distr_plot_qt('SOD1', 'Sodium',ymax=0.075) 
distr_plot_qt('POT1', 'Potassium',ymax=0.65) 
distr_plot_qt('CREA1', 'Creatinine') 
distr_plot_qt('BILI1', 'Bilirubine',cells=200) 
distr_plot_qt('ALB1', 'Albumine',ymax=2.5) 



##### Bivariate #####

## Summary bivariate

install.packages("Hmisc")
library(Hmisc)

# Continuous variables

qt_summary <- function(by_var) {
  summary <- df_rhc %>%
    select(by_var, all_of(var.qt)) %>%
    rename(group = by_var) %>%
    pivot_longer(all_of(var.qt), names_to = "variable", values_to = "value") %>%
    group_by(group,variable) %>%
    summarise(ci = list(mean_cl_boot(value) %>% 
                          rename(mean=y, lwr=ymin, upr=ymax))) %>% 
    unnest(cols = c(ci))
  summary
}


lst.p<-list()
i<-0
for (var in var.qt) {
  i<-i+1
  p<-ggplot(data=sum_qt[sum_qt$variable==var,],
            aes(x=group, y=mean, ymin=lwr, ymax=upr)) +
    geom_pointrange(aes(col=group)) +
    geom_vline(xintercept = 2.5, linetype=2)+
    geom_errorbar(aes(ymin=lwr, ymax=upr,col=group),width=0.5,cex=1)+ 
    facet_wrap(~variable,strip.position="left", scales = "free_y") +
    scale_fill_brewer(palette = 'Set2') + 
    labs(x = NULL, y = NULL) +
    guides(color = guide_legend(reverse=TRUE)) +
    theme(legend.title = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"))+
    coord_flip()
  lst.p[[i]]<-p
}

png('figures/forest_qt.png',
    width=700,height=700)
wrap_plots(lst.p, ncol=3)  + plot_layout(guides = "collect")
dev.off()



# Qualitative variables

ql_summary <- function(by_var) {
  summary <- var.ql[var.ql!=by_var] %>%      
    str_c(paste0(by_var,' ~ '), .) %>%  
    map(.f = ~glm(formula = as.formula(.x),
                  family = "binomial",
                  data = df_rhc)) %>%        
    map(.f = ~tidy(.x,exponentiate = TRUE,conf.int = TRUE)) %>%
    bind_rows() %>% 
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    select(term, estimate, conf.low, conf.high) %>%
    filter(term != '(Intercept)')
  summary
}
sum_ql_swang <- ql_summary('SWANG1') %>% 
  mutate(group="RHC")
sum_ql_dth <- ql_summary('DTH30') %>% 
  mutate(group="DTH30")
sum_ql <- rbind(sum_ql_swang, sum_ql_dth)


png('figures/forest_ql.png',
    width=700,height=900)
ggplot(data=sum_ql,
       aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, col=group)) +
  geom_pointrange(aes(col=group), position=position_dodge(width =0.6)) +
  geom_hline(yintercept = 1, linetype=2)+
  geom_vline(xintercept =(1:length(sum_ql$term))+0.5, linetype=2)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,col=group),width=0.5,cex=1, 
                position=position_dodge(width = 0.6))+ 
  scale_fill_brewer(palette = 'Set2') +
  scale_y_log10() +
  #facet_wrap(~term,strip.position="left",ncol=1,scales = "free_y") +
  labs(x = NULL, y = NULL) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(legend.title = element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()

dev.off()


##### Imputation  & Analysis #####

df_toimput<-subset(df_rhc, select=-c(META,ORTHO,POT1,RACE,RENALHX))
df_toimput$DTH30.num<-ifelse(df_toimput$DTH30=='Yes',1,0)
with(df_toimput,table(DTH30.num,DTH30))

var_mis<-c('MEANBP1','HRT1','RESP1','PAFI1','ALB1','HEMA1','PACO21','WTKILO1')

df_imp<-mice(df_toimput, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check<-complete(df_imp)
# vérification imputation
png('figures/imputation-stripplot-.png')
stripplot(df_imp,MEANBP1+HRT1+RESP1+PAFI1+ALB1+HEMA1+PACO21+WTKILO1~.imp)
dev.off()
png('figures/imputation-bwplot-.png')
bwplot(df_imp, MEANBP1+HRT1+RESP1+PAFI1+ALB1+HEMA1+PACO21+WTKILO1~.imp)
dev.off()
png('figures/imputation-densityplot-.png')
densityplot(df_imp)
dev.off()

var_notps<-c('ROWNAMES','T3D30','DTH30','SWANG1','PTID','DTH30.num')
var_ps<-names(df_toimput)[!(names(df_toimput) %in% var_notps)]
mod_formul<-paste('SWANG1~',paste(var_ps,collapse='+'),sep='')
mod_formul


cal.val<-0.10
matched_df<-matchthem(as.formula(mod_formul), df_imp, approach='within',
                      method='nearest',caliper=cal.val)

#bal.tab(matched_df, stats = c('m', 'ks'), imp.fun = 'max')
bal.tab(matched_df, un = TRUE, stats = c('m', 'ks'),
        imp.fun = 'max')$Balance.Across.Imputations[1,]
bal.tab(matched_df)$Observations
bal.tab(matched_df)$Observations[2,]/bal.tab(matched_df)$Observations[1,]*100
bal.plot(matched_df, var.name='distance',colors = col.qual[1:2],type='density')
love.plot(matched_df, stats = c("m"),
          thresholds = c(m = .1),
          shapes = c("triangle", "circle"), 
          colors = col.qual[1:2],
          alpha=0.5,
          agg.fun='range', binary='std',
          wrap=20, abs=TRUE)



plot(matched_df, type='hist')
plot(matched_df, type='jitter')
bal.plot(matched_df,var.name='HEMA1',which='both',mirror=TRUE)


## régression logistique
# Si avec p-value
res_reglog<-with(matched_df, glm(DTH30~SWANG1, family=binomial))
output_reglog<-pool(res_reglog, dfcom=NULL)
summary(output_reglog, conf.int = TRUE)
treg1<-tbl_regression(res_reglog, exponentiate=TRUE, conf.int = TRUE)

# unadjusted
res_reglog2<-with(df_imp, glm(DTH30~SWANG1 + INCOME, family=binomial))
output_reglog2<-pool(res_reglog2, dfcom=NULL)
summary(output_reglog2, conf.int = TRUE)
treg2<-tbl_regression(res_reglog2, exponentiate=TRUE, conf.int = TRUE)

treg_merge <- tbl_merge(tbls = list(treg2, treg1),
                        tab_spanner = c("**Unadjusted**", "**Adjusted**"))
treg_merge


res_reglog2<-with(df_imp, glm(DTH30~SWANG1+ INCOME+ EDU.cat, family=binomial))
output_reglog2<-pool(res_reglog2, dfcom=NULL)
summary(output_reglog2, conf.int = TRUE)
treg2<-tbl_regression(res_reglog2, exponentiate=TRUE, conf.int = TRUE)

treg_merge <- tbl_merge(tbls = list(treg2, treg1),
                        tab_spanner = c("**Unadjusted**", "**Adjusted**"))
treg_merge

res_reglog2<-with(df_imp, glm(SWANG1~ DTH30 + INCOME+ EDU.cat, family=binomial))

summary(res_reglog2)
exp(coef(res_reglog2))
drop1(res_reglog2, .~., test = "F")

table(df_rhc$SWANG1,df_rhc$INCOME, deparse.level = 2)

table(df_rhc$SWANG1)

?table()

t.test(df_rhc$SWANG1,df_rhc$INCOME)
str(df_rhc)

df_rhc$INCOME.nu <- as.integer(df_rhc$INCOME)
str(df_rhc)
t.test(df_rhc$SWANG1,df_rhc$INCOME.nu)
table(df_rhc$SWANG1,df_rhc$INCOME,  deparse.level = 2)

table(df_rhc$SWANG1, useNA = "always")

df_rhc$EDU.cat

prettyR::describe(table(df_rhc$SWANG1,df_rhc$INCOME, deparse.level = 2), num.desc = c("valid.n", "pctmiss","mean","sd","min",
                                       "q1","median","q3","max"))

#### ADD VALIDATION CONDITIONS

# Si sans p-value
mod_reglog<-list()
mod_reglog[['Unadjusted (OR [CI95])']]<-res_reglog2
mod_reglog[['Unadjusted (OR [CI95])']]<-mice::pool(mod_reglog[['Unadjusted (OR [CI95])']])
mod_reglog[['Adjusted (OR [CI95])']]<-res_reglog
mod_reglog[['Adjusted (OR [CI95])']]<-mice::pool(mod_reglog[['Adjusted (OR [CI95])']])
modelsummary(mod_reglog, exponentiate = TRUE,
             estimate  = "{estimate} [{conf.low}, {conf.high}]",
             fmt = "%.2f",
             statistic = NULL,
             coef_omit = "Intercept",
             coef_rename=c('SWANG1RHC'='RHC'))

## Cox
install.packages("survdiff")
install.packages("survival")
library(survival)
library(survdiff)

?survdiff

fit<-with(matched_df,survfit(Surv(T3D30, DTH30.num)~SWANG1))
with(matched_df, survdiff(fit))
summary(fit)

res_surv<-with(matched_df, coxph(Surv(T3D30, DTH30.num)~SWANG1))
output_surv<-pool(res_surv, dfcom=NULL)
summary(output_surv, conf.int = TRUE)
tsurv1<-tbl_regression(res_surv, exponentiate=TRUE, conf.int = TRUE)

# unadjusted
res_surv2<-with(df_imp, coxph(Surv(T3D30, DTH30.num)~SWANG1))
output_surv2<-pool(res_surv2, dfcom=NULL)
summary(output_surv2, conf.int = TRUE)
tsurv2<-tbl_regression(res_surv2, exponentiate=TRUE, conf.int = TRUE)
tsurv_merge <- tbl_merge(tbls = list(tsurv2, tsurv1),
                         tab_spanner = c("**Unadjusted**", "**Adjusted**"))
tsurv_merge

##### ADD CURVE
##### ADD VALIDATION CONDITIONS

# Si sans p-value
mod_surv<-list()
mod_surv[['Unadjusted (HR [CI95])']]<-res_surv2
mod_surv[['Unadjusted (HR [CI95])']]<-mice::pool(mod_surv[['Unadjusted (HR [CI95])']])
mod_surv[['Adjusted (HR [CI95])']]<-res_surv
mod_surv[['Adjusted (HR [CI95])']]<-mice::pool(mod_surv[['Adjusted (HR [CI95])']])
modelsummary(mod_surv, exponentiate = TRUE,
             estimate  = "{estimate} [{conf.low}, {conf.high}]",
             fmt = "%.2f",
             statistic = NULL,
             coef_omit = "Intercept",
             coef_rename=c('SWANG1RHC'='RHC')) 

df.imp_check$DTH30.num<-ifelse(df.imp_check$DTH30=='Yes',1,0)
with(df.imp_check,table(DTH30.num,DTH30))
mod.cox<-coxph(Surv(T3D30, DTH30.num)~SWANG1, data=df.imp_check)
summary(mod.cox)
p<-ggsurvplot(mod.cox, conf.int = TRUE)
p
plot(survfit(mod.cox))
tbl_regression(mod.cox, exponentiate=TRUE, conf.int = TRUE)
fit.compl<-survfit(Surv(T3D30, DTH30.num)~SWANG1, data=df.imp_check)
fit.compl
summary(fit.compl)
class(fit.compl)
p<-ggsurvplot(fit.compl, conf.int = TRUE)
p

?ggsurvplot

install.packages("ggsurvplot")
library(ggsurv)
##### ADD VALIDATION CONDITIONS


save(list = ls(), file = "all_environnement.Rdata")
? save()

? PNG
??PNG
