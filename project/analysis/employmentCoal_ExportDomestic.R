
############ Estimate industry use of coal over period / assume 2% growth,  (3% growth as in NDC Burton, Caetano seems high ?)

coalUse.industry            <- data.frame(round(t(df.IOT2014.q[4,Order]),8))
colnames(coalUse.industry)  <- "tons coal"
coalUse.industry$Industry   <- Industry

order <- order(-coalUse.industry$`tons coal`)

coalUse.industry <- coalUse.industry[order,]

coalUse.others <- sum(coalUse.industry$`tons coal`[-c(1,2,3,4,5)])

############ 

results.demandExport    <- finaldemandDecomposition.q(df.domesticDemand.coalExport)
results.demandDomestic  <- finaldemandDecomposition.q(df.domesticDemand.coalDomestic)

results.demandExport.summ     <- results.demandExport[[11]][[2]]
results.demandDomestic.summ   <- results.demandDomestic[[11]][[2]]

results.demandExport.summ$Market      <- "Export"
results.demandDomestic.summ$Market  <- "Domestic"

results.demandTotal.summ              <- bind_rows(results.demandExport.summ, results.demandDomestic.summ)

results.demandTotal.summ[is.na(results.demandTotal.summ)] <- 0

temp  <- results.demandTotal.summ %>% gather(Variable, Value, -Year, - Scenario, -Industry, -Market)
temp <- temp %>% spread(Market, Value)
temp$Total <- NA
temp[!temp$Variable %in% c("Price.export","Price.domestic"),"Total"] <- temp[!temp$Variable %in% c("Price.export","Price.domestic"),"Domestic"] + temp[!temp$Variable %in% c("Price.export","Price.domestic"),"Export"] 

temp            <- temp %>% gather(Market, Value, -Year, -Scenario, -Industry, -Variable)
temp$VarMarket  <- paste(temp$Variable, temp$Market, sep=".")
temp[temp$Variable=="Gross.op.surplus","Variable"] <-  temp[temp$Variable=="Gross.op.surplus","VarMarket"]

df.plot1 <- temp %>% filter(temp$VarMarket %in% c("Value.demand.dom.Total", "Value.demand.imp.Total", "Output.value.Total", "Dom.inputs.Total",
                                               "Imp.inputs.Total", "Wages.Total", "Taxes.other.Total", "Gross.op.surplus.Export", "Gross.op.surplus.Domestic", 
                                               "Taxes.prod.Total"))

df.plot <- df.plot1 %>% filter(df.plot1$VarMarket %in% c("Value.demand.imp.Total", "Imp.inputs.Total", 
                                                     "Wages.Total", "Taxes.other.Total", "Gross.op.surplus.Export", "Gross.op.surplus.Domestic", 
                                                     "Taxes.prod.Total"))  %>% select(-Variable, -Market) %>% spread(VarMarket, Value)

df.plot$Taxes   <- df.plot$Taxes.other.Total + df.plot$Taxes.prod.Total
df.plot$Imports <- df.plot$Imp.inputs.Total + df.plot$Value.demand.imp.Total

df.plot <- df.plot  %>% select(-c(Taxes.other.Total, Taxes.prod.Total, Imp.inputs.Total, Value.demand.imp.Total)) %>% 
                        gather(Variable, Value, -Year, -Scenario, -Industry) %>%
                        spread(Industry, Value)


df.plot$Inot4 <- rowSums(df.plot[, Order]) - df.plot$I4

df.plot <- df.plot %>% select(Year, Scenario, Variable, I4, Inot4) %>%
                       gather(Industry, Value, -Year, -Scenario, -Variable, factor_key = TRUE)

df.plot$VarInd <- paste(df.plot$Variable, df.plot$Industry, sep=".") 

df.plot <- df.plot %>% select(-Variable, -Industry) %>% spread(VarInd, Value)

df.plot$Gross.op.surplus.Inot4 <- df.plot$Gross.op.surplus.Domestic.Inot4 + df.plot$Gross.op.surplus.Export.Inot4

df.plot <- df.plot %>% select(-Gross.op.surplus.Domestic.Inot4, -Gross.op.surplus.Export.Inot4) %>% gather(Variable, Value, -Year, -Scenario, factor_key = TRUE)

levels.Var <- rev(c("Imports.I4", "Imports.Inot4", 
                "Taxes.I4", "Wages.Total.I4", "Gross.op.surplus.Domestic.I4", "Gross.op.surplus.Export.I4", 
                "Taxes.Inot4", "Wages.Total.Inot4", "Gross.op.surplus.Inot4"))

labels.Var <- rev(c("Imports by coal", "Imports by rest of industry", 
                "Taxes, coal", "Wages, coal",   "Operating surplus, coal, exports", "Operating surplus, coal, domestic sales",
                "Taxes, rest of industry", "Wages, rest of industry",   "Operating surplus, rest of industry"))

df.plot$Variable  <- factor(df.plot$Variable, levels=levels.Var, labels=labels.Var)
df.plot           <- df.plot[order(df.plot$Variable),]

colours <- c("#3498db","#85c1e9",  "#2874a6", "#f1c40f", "#d4ac0d","#f7dc6f", "#b7950b", "#cb4335", "#d98880")    

p <- df.plot  %>% ggplot(aes(x=Year, y=Value, fill=Variable)) +
                  geom_bar(stat="identity") +
                  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Year", y="million Rand (constant 2014)") +
                  #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                  #scale_y_continuous(limits = c(0, 100000)) +
                  #coord_flip() +
                  scale_fill_manual(values=colours) +
                  labs(fill="Export revenue decomposition :") 
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ExportRevenue_Decomposition",scenarioHere, sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=22, height=10, units="cm", dpi=300)



############ 

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