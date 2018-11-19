

results.demandExport    <- finaldemandDecomposition(df.domesticDemand.coalExport)
results.demandDomestic  <- finaldemandDecomposition(df.domesticDemand.coalDomestic)

NE.demandExport               <- results.demandExport[[10]][[2]]
NE.demandDomestic             <- results.demandDomestic[[10]][[2]]

NE.demandExport.BAU.2035      <- NE.demandExport[NE.demandExport$Year=="2035" & NE.demandExport$Case=="BAU", ]
NE.demandExport.2Deg.2035     <- NE.demandExport[NE.demandExport$Year=="2035" & NE.demandExport$Case=="2Deg", ]

NE.demandDomestic.BAU.2035    <- NE.demandDomestic[NE.demandDomestic$Year=="2035" & NE.demandDomestic$Case=="BAU", ]
NE.demandDomestic.2Deg.2035   <- NE.demandDomestic[NE.demandDomestic$Year=="2035" & NE.demandDomestic$Case=="2Deg", ]

sum(NE.demandExport.BAU.2035[,4])
sum(NE.demandExport.2Deg.2035[,4])

sum(NE.demandExport.BAU.2035[4,4])
sum(NE.demandExport.2Deg.2035[4,4])

sum(NE.demandExport.BAU.2035[,4]) - sum(NE.demandExport.BAU.2035[4,4])

sum(NE.demandExport.2Deg.2035[,4]) - sum(NE.demandExport.2Deg.2035[4,4])

sum(NE.demandExport.BAU.2035[28,4])  # General Machinery
sum(NE.demandExport.2Deg.2035[28,4]) # General Machinery
sum(NE.demandExport.BAU.2035[36,4])  # Trade
sum(NE.demandExport.2Deg.2035[36,4]) # Trade
sum(NE.demandExport.BAU.2035[38,4])  # Transport
sum(NE.demandExport.2Deg.2035[38,4]) # Transport
sum(NE.demandExport.BAU.2035[46,4])  # Computer
sum(NE.demandExport.2Deg.2035[46,4]) # Computer

sum(NE.demandExport.BAU.2035[,4]) - sum(NE.demandExport.BAU.2035[c(4,28,36,38,46),4])
sum(NE.demandExport.2Deg.2035[,4]) - sum(NE.demandExport.2Deg.2035[c(4,28,36,38,46),4])

sum(NE.demandDomestic.BAU.2035[,4])
NE.coalDomesticDemand.BAU.2035 <- data.frame(NE.demandDomestic.BAU.2035[,4])
colnames(NE.coalDomesticDemand.BAU.2035) <- "Value"
NE.coalDomesticDemand.BAU.2035$Industry <- Industry

order <- order(NE.coalDomesticDemand.BAU.2035$Value)

NE.coalDomesticDemand.BAU.2035 <- NE.coalDomesticDemand.BAU.2035[c(4,38),]

sum(NE.demandDomestic.2Deg.2035[,4])

NE.demandDomestic.2Deg.2035[4,4]


sum(NE.demandExport[NE.demandExport$Year=="2025" & NE.demandExport$Case=="BAU", 4 ])