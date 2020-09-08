
# ---- Prepare the data (can skip without the csv file)-----------------------------------------------------
getwd()
# setwd()
dir()

db_new <- read.csv("DatabaseI&II_20180703.csv")
colnames(db_new) <- c("Publication","Contributor","Year","season","Koppen_org","Climate","climate","city","Country","Building","Ventilation","Mix.operation","Heat.mode",
                      "Age","Sex","TSV","Acc","Preference","vel_acc","vel_pref",
                      "Comfort","PMV","PPD","SET","Clo","Met","act10","act20","act30","act40",
                      "Ta","TAAV_F","Ta_h","Ta_h_F","Ta_m","Ta_m_F","Ta_l","Ta_l_F",
                      "Top","TOP_F","Tr","TRAV_F","TGO","TGO_F","Tg_h","Tg_h_F","Tg_m","Tg_m_F","Tg_l","Tg_l_F",
                      "Rh","rh_pref","rh_sen","V","VELAV_FPM","v_h","v_h_fpm","v_m","v_m_fpm","v_l","v_l_fpm","Height","Weight",
                      "PCEC1","PCEC2","PCEC3","PCEC4","PCEC5","To","prev_ta_F","Database")

columns<-c("Database","Contributor","Country","Building","Ventilation","Koppen_org","Climate","TSV","Acc","Age","Sex","Ta","Tr","V","Rh","Clo","Met",
           "Top","To","SET","PMV","PPD")

db_cor <- data.frame(db_new[,columns])

# This is used for consistent the value variable style in number (easy for calculation)
db_cor $TSV <- as.numeric(as.character(db_cor $TSV)) 
db_cor $Acc <- as.numeric(as.character(db_cor $Acc))
db_cor $Country <- as.factor(as.character(db_cor $Country))
db_cor $Contributor <- as.factor(as.character(db_cor $Contributor))
db_cor $Ta <- as.numeric(as.character(db_cor $Ta))
db_cor $Tr <- as.numeric(as.character(db_cor $Tr))
db_cor $V <- as.numeric(as.character(db_cor $V))
db_cor $Rh <- as.numeric(as.character(db_cor $Rh))
db_cor $Clo <- as.numeric(as.character(db_cor $Clo))
db_cor $Met <- as.numeric(as.character(db_cor $Met))
db_cor $Top <- as.numeric(as.character(db_cor $Top))
db_cor $To <- as.numeric(as.character(db_cor $To))
db_cor $SET <- as.numeric(as.character(db_cor $SET))
db_cor $PMV <- as.numeric(as.character(db_cor $PMV))
db_cor $PPD <- as.numeric(as.character(db_cor $PPD))
db_cor $Age <- as.factor(as.character(db_cor $Age))
db_cor $Sex <- as.factor(as.character(db_cor $Sex))
db_cor $Climate <- as.factor(as.character(db_cor $Climate))
db_cor $Koppen_org <- as.factor(as.character(db_cor $Koppen_org))

db_cor <- subset(db_cor, subset = TSV != "NA")
db_cor <- subset(db_cor, subset = Acc != "NA")


c <- nrow(db_cor) # Clean up the TSV value, some of them are between 1 and 2, e.g. 1.25
cTSV <- matrix(NA,c,1)
cPMV <- matrix(NA,c,1)
Sensation <- matrix(NA,c,1)
PMV_sen <- matrix(NA,c,1)
Acc_sen <- matrix(NA,c,1)


for (i in 1:c) {
  if (db_cor$TSV[i] < -2.5){
    cTSV[i] <- -3
    Sensation[i] <- c("Cold")
  }
  else if (db_cor$TSV[i] < -1.5){
    cTSV[i] <- -2
    Sensation[i] <- c("Cool")
  }
  else if (db_cor$TSV[i] < -0.5){
    cTSV[i] <- -1
    Sensation[i] <- c("S_cool")
  }
  else if (db_cor$TSV[i] <= 0.5){
    cTSV[i] <- 0
    Sensation[i] <- c("Neutral")
  }
  else if (db_cor$TSV[i] <= 1.5){
    cTSV[i] <- 1
    Sensation[i] <- c("S_warm")
  }
  else if (db_cor$TSV[i] <= 2.5){
    cTSV[i] <- 2
    Sensation[i] <- c("Warm")
  }
  else if (db_cor$TSV[i] > 2.5){
    cTSV[i] <- 3
    Sensation[i] <- c("Hot")
  }
}

for (i in 1:c) {
  if (db_cor$PMV[i] < -2.5){
    cPMV[i] <- -3
    PMV_sen[i] <- c("Cold")
  }
  else if (db_cor$PMV[i] < -1.5){
    cPMV[i] <- -2
    PMV_sen[i] <- c("Cool")
  }
  else if (db_cor$PMV[i] < -0.5){
    cPMV[i] <- -1
    PMV_sen[i] <- c("S_cool")
  }
  else if (db_cor$PMV[i] <= 0.5){
    cPMV[i] <- 0
    PMV_sen[i] <- c("Neutral")
  }
  else if (db_cor$PMV[i] <= 1.5){
    cPMV[i] <- 1
    PMV_sen[i] <- c("S_warm")
  }
  else if (db_cor$PMV[i] <= 2.5){
    cPMV[i] <- 2
    PMV_sen[i] <- c("Warm")
  }
  else if (db_cor$PMV[i] > 2.5){
    cPMV[i] <- 3
    PMV_sen[i] <- c("Hot")
  }
  print(i) 
}


for (i in 1:c) {
  if (db_cor$Acc[i] < 0.5){
    if (db_cor$TSV[i] < -1.5) {
      Acc_sen[i] <- c("Unacc_cool")
    }
    else if (db_cor$TSV[i]  <= 1.5){
      Acc_sen[i] <- c("Unacc_neu")
    }
    else if (db_cor$TSV[i]  > 1.5){
      Acc_sen[i] <- c("Unacc_warm")
    }
  }
  else if (db_cor$Acc[i]  > 0.5){
    if (db_cor$TSV[i]  < -1.5) {
      Acc_sen[i] <- c("Acc_cool")
    }
    else if (db_cor$TSV[i]  <= 1.5){
      Acc_sen[i] <- c("Acc_neu")
    }
    else if (db_cor$TSV[i]  > 1.5){
      Acc_sen[i] <- c("Acc_warm")
    }
  }
  print(i) 
}

db_1 <- cbind(db_cor,cTSV,Sensation,Acc_sen)
db_1 $cTSV <- as.numeric(as.character(db_1 $cTSV))
db_1 $Sensation <- as.factor(as.character(db_1 $Sensation))
db_1 $Acc_sen <- as.factor(as.character(db_1 $Acc_sen))

db_1 <- transform(db_1,
                Sensation = factor(
                  Sensation, levels=c("Cold","Cool","S_cool","Neutral","S_warm","Warm","Hot"), ordered=TRUE),
                Acc_sen = factor(
                  Acc_sen, levels=c("Unacc_cool","Acc_cool","Acc_neu","Acc_warm","Unacc_warm"), ordered=TRUE))

source("fixed-polr.R")

save(db_1, file = "TSV-ACC-model.RData")



# ---- Start analysis--------------------------------------------------------------------------------------
getwd()
# load("TSV-ACC-model.RData") # Load data only
load("TSV-ACC-model-image.RData") # Load the entire image

require(Rcpp)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(directlabels)
library(splines)
library(MASS)
library(grid)
library(scales)
library(aod)
library(rpart)
library(reshape)
library(reshape2)
library(coin)
library(lme4)
library(GGally)
library(ggthemes)
library(comf)
library(effsize)
library(kernlab)
library(mgcv)
library(caret)
library(gridExtra)

db <- db_1 %>%
  filter(Building == "Office") %>%
  # filter(Building == "Office"| Building == "Classroom"| Building == "Others") %>%
  # filter(Ventilation =="Air Conditioned" | Ventilation =="Naturally Ventilated" | Ventilation =="Mixed Mode")
  filter(Ventilation =="Air Conditioned") %>%  #Just try AC office here %>%
# filter(Ventilation =="Naturally Ventilated")#Just try NV office here
# filter(Ventilation =="Mixed Mode")#Just try NV office here
  filter(Climate =="A")

db <- subset(db, Acc_sen != "Unacc_neu")

acc_sen.colors <- c(Unacc_cool="cornflowerblue", Acc_cool="lightblue1", Unacc_warm="brown", Acc_warm="lightpink", Acc_neu="palegreen")
p1 <- ggplot(db, aes(Ta, order=Acc_sen))+ geom_bar(aes(fill=Acc_sen),  binwidth=1, position="fill", alpha=0.7) +
  # xlim(15,31)+
  theme(axis.text.x=element_text(size=7, hjust=0.6, colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'))+
  scale_fill_manual(values=acc_sen.colors) + ggtitle("Actual thermal acceptance-sensation response"); p1



# ---- Actual Ordinal logistic regression use in paper (Ta) -----------------------------------------
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(rms)

model.olr.full <- polr(Acc_sen ~ Ta, data = db, Hess = TRUE, method = "logistic")
summary(model.olr.full)

ctable <- coef(summary(model.olr.full)) # Store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # p-value calculation
(ctable <- cbind(ctable, "p value" = p))
ci <- confint(model.olr.full) # default method gives profiled CIs
confint.default(model.olr.full) # CIs assuming normality
exp(coef(model.olr.full)) ## odds ratios
exp(cbind(OR = coef(model.olr.full), ci)) ## OR and CI

## Create new dataset for simulation
newdat <- data.frame(Ta = seq(15,35, by=0.1))
newdat_1 <- cbind(newdat, predict(model.olr.full, newdat, type = "probs"))
lnewdat <- melt(newdat_1, id.vars = c("Ta"), variable.name = "Level", value.name="Probability")
head(lnewdat)
p2 <- ggplot(lnewdat, aes(x = Ta, y = Probability, colour = Level)) + geom_line(); p2


# ---- POLR for Climate, Vent, Ta ---------------------------------------------------------
db_cli_vent_Ta <- db_1 %>%
  filter(Ta != "NA") %>%
  filter(Acc_sen != "Unacc_neu")%>%
  filter(Building == "Office") %>%
  filter(Ventilation =="Air Conditioned" | Ventilation == "Naturally Ventilated") %>%
  filter(Climate =="A" | Climate == "C")

p3 <- ggplot(db_cli_vent_Ta, aes(Ta, order=Acc_sen))+ geom_bar(aes(fill=Acc_sen),  binwidth=1, position="fill", alpha=0.7) +
  # xlim(15,31)+
  theme(axis.text.x=element_text(size=7, hjust=0.6, colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'))+
  scale_fill_manual(values=acc_sen.colors) + ggtitle("Actual thermal acceptance-sensation response") +
  facet_grid(Ventilation ~ Climate, labeller="label_both"); p3

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(rms)

model.olr.2 <- polr(Acc_sen ~ Ta + Ventilation + Climate, data = db_cli_vent_Ta, Hess = TRUE, method = "logistic"); summary(model.olr.2)
ctable <- coef(summary(model.olr.2)) # Store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # p-value calculation
(ctable <- cbind(ctable, "p value" = p))
ci <- confint(model.olr.2) # default method gives profiled CIs
confint.default(model.olr.2) # CIs assuming normality
exp(coef(model.olr.2)) ## odds ratios
exp(cbind(OR = coef(model.olr.2), ci)) ## OR and CI
## Create new dataset for simulation
newdat <- data.frame(
  Ventilation = rep(c("Air Conditioned","Naturally Ventilated"), 200),
  Climate = rep(c("A","C"), each = 200),
  Ta = rep(seq(from = 15, to = 35, length.out = 100), 4))
newdat_1 <- cbind(newdat, predict(model.olr.2, newdat, type = "probs"))
lnewdat <- melt(newdat_1, id.vars = c("Ta","Ventilation","Climate"), variable.name = "Level", value.name="Probability")
head(lnewdat)
p4 <- ggplot(lnewdat, aes(x = Ta, y = Probability, colour = Level)) + geom_line() +
 facet_grid(Ventilation ~ Climate, labeller="label_both"); p4


save.image(file = "TSV-ACC-model-image.RData")
