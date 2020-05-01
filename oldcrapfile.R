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


with(MergedPartyData,scatter.smooth(Type_Populism,valueinf, xlab = "Level of populist-pluralist rhetoric (Ordinal Scale)", ylab = 
                                      "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(Type_Populist_Values,valueinf, xlab = "Typology of Rhetoric (Ordinal Scale)", ylab = 
                                      "Values infusion", lpars = list(col = "red", lwd = 5, lty = 1)))

with(MergedPartyData,scatter.smooth(Type_Populist_Values,routinization, xlab = "Typology of Rhetoric (Ordinal Scale)", ylab = 
                                      "Routinization", lpars = list(col = "red", lwd = 5, lty = 1)))



saveRDS(MergedPartyData, file = "MergedPartyData.rds")

saveRDS(MergedPartyData2, file = "MergedPartyData2.rds")