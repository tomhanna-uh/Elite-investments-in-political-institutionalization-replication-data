gc()
rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/Elite investments in political institutionalization replication data")

library(maxLik)
library(car)
library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)
library(censReg)
library(VGAM)
library(mvtnorm)
library(gmodels)
library(Hmisc)
library(MASS)
library(ordinal)
library(reshape)
library(stargazer)
library(AER)
library(glmx)
library(plm)
library(pglm)
library(mediation)
library(Hmisc)
library(coefplot)



library(haven)
PILA_2017 <- read_dta("PILA_2017.dta")
View(PILA_2017)



library(haven)
Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020 <- read_dta("Global Party Survey by Party Stata V1 10_Feb_2020.dta")
View(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020)

attach(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020)
NewPartyData <- Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020[ which(ISO=="ARG"|ISO=="BOL"|ISO=="BRA"|
                ISO=="CHL"|ISO=="COL"|ISO=="CRI"|ISO=="ECU"|ISO=="SLV"|ISO=="GTM"|ISO=="HND"|ISO=="MEX"|
                ISO=="NIC"|ISO=="PAN"|ISO=="PRY"|ISO=="PER"|ISO=="DOM"|ISO=="URY"|ISO=="VEN"),]

NewPartyData <- NewPartyData[,c("ID_GPS","ISO","Partyname","Partyabb","CPARTY","CPARTYABB","Type_Values",
                    "Type_Populism","Type_Populist_Values",
                    "V3","V8_Scale","V9","V18","V19","V20","V21")]


detach(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020)



##write.csv(PILA_2017, file = 'PILA2017a.csv')

##write.csv(NewPartyData, file = 'NPD.csv')

PILA2017.revised <- read.csv("C:/R Studio Files/Elite investments in political institutionalization replication data/PILA2017-revised.csv")

### Gave errors I could not quickly resolve
### MergedPartyData <- merge(PILA2017.revised,NewPartyData, by = "CPARTY", all = TRUE)
### Merged data manually in Excel

MergedPartyData <- read.csv("C:/R Studio Files/Elite investments in political institutionalization replication data/MergedPartyData.csv")

View(MergedPartyData)

MergedPartyData$LogValueInfusion <- log(MergedPartyData$valueinf)

MergedPartyData$LogRoutinization <- log(MergedPartyData$routinization)

MergedPartyData$LogPartyAge <- log(MergedPartyData$page)

MergedPartyData$LogPopulistRhetoric <- log(MergedPartyData$V8_Scale)

MergedPartyData$LogPopulistSaliency <- log(MergedPartyData$V9)

MergedPartyData$PeopleShouldDecide <- MergedPartyData$V19
MergedPartyData$LogPeopleDecide <- log(MergedPartyData$V19)

MergedPartyData$WillofthePeople <- MergedPartyData$V18
MergedPartyData$LogWillofPeople <- log(MergedPartyData$V18)

MergedPartyData$StrongmanRule <- MergedPartyData$V21
MergedPartyData$LogStrongmanRule <- log(MergedPartyData$V21)


### Comparing some summary statistics between the merged data and the original data sources
### Looking for any differences that might indicate bias

summary(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020$V8_Scale)
summary(MergedPartyData$V8_Scale)

summary(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020$Type_Values)
summary(MergedPartyData$Type_Values)

summary(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020$Type_Populism)
summary(MergedPartyData$Type_Populism)

summary(Global_Party_Survey_by_Party_Stata_V1_10_Feb_2020$Type_Populist_Values)
summary(MergedPartyData$Type_Populist_Values)

summary(PILA_2017$valueinf)
summary(MergedPartyData$valueinf)

summary(PILA_2017$routinization)
sd(PILA_2017$routinization)
summary(MergedPartyData$routinization)
sd(MergedPartyData$routinization)

#####
###Descriptive Statistics
#####

cor(MergedPartyData$V9,MergedPartyData$valueinf,use = "pairwise.complete.ob")

cor(MergedPartyData$V9,MergedPartyData$routinization,use = "pairwise.complete.ob")

cor(MergedPartyData$V8_Scale,MergedPartyData$page, use = "pairwise.complete.ob")

attach(MergedPartyData)

#####
##Plots
#####

hist(V8_Scale)
hist(V9)
hist(V21)
hist(page)
hist(valueinf)
hist(routinization)

with(MergedPartyData,scatter.smooth(page,V8_Scale, xlab = "Party Age", ylab = 
                                      "Level of populism (1-10)", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(Type_Populism,valueinf, xlab = "Level of populist-pluralist rhetoric (Ordinal Scale)", ylab = 
                                  "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(Type_Populist_Values,valueinf, xlab = "Typology of Rhetoric (Ordinal Scale)", ylab = 
                                      "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(Type_Populist_Values,routinization, xlab = "Typology of Rhetoric (Ordinal Scale)", ylab = 
                                      "Routinization", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(V9,valueinf, xlab = "Saliency of populist rhetoric", ylab = 
                                      "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))


with(MergedPartyData,scatter.smooth(V9,routinization, xlab = "Saliency of populist rhetoric", ylab = 
                                      "Routinization", lpars = list(col = "red", lwd = 5, lty = 1)))


with(MergedPartyData,scatter.smooth(V9,valueinf, xlab = "Saliency of populist rhetoric", ylab = 
                                      "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(V21,valueinf, xlab = "Favors strongman rule, opposes checks and balances", ylab = 
                                      "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))


with(MergedPartyData,scatter.smooth(V21,routinization, xlab = "Favors strongman rule, opposes checks and balances", ylab = 
                                      "routinization", lpars = list(col = "red", lwd = 5, lty = 1)))


with(MergedPartyData,scatter.smooth(V8_Scale,V9, xlab = "Favors strongman rule, opposes checks and balances", ylab = 
                                      "Populist Rhetoric", lpars = list(col = "red", lwd = 5, lty = 1)))


######
###Models
######

###Models based on saliency of populist rhetoric and Routinization

attach(MergedPartyData)

model1 <- lm(LogRoutinization ~ LogPopulistSaliency  + LogPartyAge)
summary(model1)

model1a <- lm(LogRoutinization ~ LogPopulistSaliency + LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model1a)

model1b <- lm(LogRoutinization ~ LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model1b)

model1c <- lm(LogRoutinization ~ LogPartyAge)
summary(model1c)

model1d <- lm(LogRoutinization ~ LogPopulistSaliency)
summary(model1d)

stargazer(model1,model1a,model1b,model1c,model1d)

detach(MergedPartyData)

###models based on Populist Saliency and Value Infusion

attach(MergedPartyData)

model2 <- lm(LogValueInfusion ~ LogPopulistSaliency  + LogPartyAge)
summary(model2)

model2a <- lm(LogValueInfusion ~ LogPopulistSaliency + LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model2a)

model2b <- lm(LogValueInfusion ~ LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model2b)

model2c <- lm(LogValueInfusion ~ LogPartyAge)
summary(model2c)

model2d <- lm(LogValueInfusion ~ LogPopulistSaliency)
summary(model2d)

stargazer(model2,model2a,model2b,model2c,model2d)

detach(MergedPartyData)

###Models based on Favoring Strongman Rules with both dependent variables and all controls


attach(MergedPartyData)

model3 <- lm(LogRoutinization ~ LogStrongmanRule + LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model3)

model3a <- lm(LogValueInfusion ~ LogStrongmanRule + LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model3a)

model3b <- lm(LogValueInfusion ~ LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model3b)

stargazer(model3,model3a,model3b)

detach(MergedPartyData)



attach(MergedPartyData)

model4 <- lm(LogRoutinization ~ LogWillofPeople + LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model4)

model4a <- lm(LogValueInfusion ~ LogWillofPeople + LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model4a)

model4b <- lm(LogValueInfusion ~ LogPartyAge + polar + enpp + subsid + legisoff + execoff + formation + group)
summary(model4b)

stargazer(model4,model4a,model4b)

detach(MergedPartyData)