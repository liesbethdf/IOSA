


demand.diff <- df.domesticDemand.coalExport[df.domesticDemand.coalExport$Year=="2035" & 
                                            df.domesticDemand.coalExport$Var=="Value.demandD"  & 
                                            df.domesticDemand.coalExport$Case=="BAU","Value"]-
               df.domesticDemand.coalExport[df.domesticDemand.coalExport$Year=="2035" & 
                                            df.domesticDemand.coalExport$Var=="Value.demandD"  & 
                                            df.domesticDemand.coalExport$Case=="2DS","Value"]

output.diff <- LI.d %*% diag(demand.diff)

InvestmentShare <- df.IOT2014$Capital.formation[1:50]/df.IOT2014$Output[1:50]

InvestmentShare.d <- InvestmentShare * s.d.gfcf
InvestmentShare.m <- InvestmentShare * s.m.gfcf

gfcf.reduc.d <- diag(InvestmentShare.d) %*% output.diff
gfcf.reduc   <- diag(InvestmentShare)   %*% output.diff

sum(gfcf.reduc)
sum(gfcf.reduc.d)

# 0.08 
GDP.2017 <-  4651785  #million Rand

sum(gfcf.reduc)  /GDP.2017 *100
sum(gfcf.reduc.d)/GDP.2017 *100

# 0.08 % of 2017 GDP
# 0.03 % of 2017 GDP
# more than half of investment is imported.