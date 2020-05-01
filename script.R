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

model1 <- lm(routinization ~ V9 + page)
summary(model1)

model1a <- lm(routinization ~ V9 + polar + enpp + subsid + legisoff + execoff + formation + group + page)
summary(model1a)

model1b <- lm(routinization ~ polar + enpp + subsid + legisoff + execoff + formation + group + page)
summary(model1c)

model1c <- lm(routinization ~ page)

model1d <- lm(routinization ~ V9)

stargazer(model1,model1a,model1b,model1c,model1d)

detach(MergedPartyData)

####Anova - had to remove one row with NA

attach(MergedPartyData2)

MergedPartyData2 <- MergedPartyData

MergedPartyData2 <- MergedPartyData2[!rowSums(is.na(MergedPartyData2["V8_Scale"])),]

model1a <- lm(routinization ~ V8_Scale + polar + enpp + subsid + legisoff + execoff + formation + group + page, MergedPartyData2)
summary(model1a)

model1b <- lm(routinization ~ polar + enpp + subsid + legisoff + execoff + formation + group + page, MergedPartyData2)
summary(model1b)

anova1 <- anova(model1b,model1a)
anova1
summary(anova1)
stargazer(anova1)

detach(MergedPartyData2)

####

attach(MergedPartyData)

model1e <- lm(routinization ~ V9 + zpolar + zenpp + zsubsid + zlegisoff + zexecoff + zformation + zgroup + zpage)
summary(model1e)

model2 <- lm(valueinf ~ V9 + polar + enpp + subsid + legisoff + execoff + formation + group + page)
summary(model2)

model2e <- lm(valueinf ~ V9 + zpolar + zenpp + zsubsid + zlegisoff + zexecoff + zformation + zgroup + zpage)
summary(model2e)

stargazer(model1,model1e,model2,model2e)



model3a <- lm(routinization ~ page + as.factor(ctyname))
  
model3b <- lm(valueinf ~ page + as.factor(ctyname)) 

stargazer(model3a,model3b)

detach(MergedPartyData)