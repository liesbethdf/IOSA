
############################################
############ Run model for domestic and export demand
############################################

results.demandExport    <- finaldemandDecomposition.q(df.domesticDemand.coalExport)
results.demandDomestic  <- finaldemandDecomposition.q(df.domesticDemand.coalDomestic)

results.demandExport.summ     <- results.demandExport[[11]][[2]]
results.demandDomestic.summ   <- results.demandDomestic[[11]][[2]]

results.demandExport.summ$Market      <- "Export"
results.demandDomestic.summ$Market    <- "Domestic"

results.demandTotal.summ              <- bind_rows(results.demandExport.summ, results.demandDomestic.summ)

results.demandTotal.summ[is.na(results.demandTotal.summ)] <- 0

temp        <- results.demandTotal.summ %>% gather(Variable, Value, -Year, - Scenario, -Industry, -Market)
temp        <- temp %>% spread(Market, Value)
temp$Total  <- NA
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
                "Taxes, coal", "Wages, coal", "Operating surplus, coal, domestic sales",  "Operating surplus, coal, exports", 
                "Taxes, rest of industry", "Wages, rest of industry",   "Operating surplus, rest of industry"))

df.plot$Variable  <- factor(df.plot$Variable, levels=levels.Var, labels=labels.Var)
df.plot           <- df.plot[order(df.plot$Variable),]

df.plot$Scenario  <- factor(df.plot$Scenario, levels=c("BAU","2DS"))
df.plot           <- df.plot[order(df.plot$Scenario),]


colours <- c("#3498db","#85c1e9",  "#2874a6", "#f1c40f", "#d4ac0d","#f7dc6f", "#b7950b", "#cb4335", "#d98880")    

p <- df.plot  %>% ggplot(aes(x=Year, y=Value, fill=Variable)) +
                  geom_bar(stat="identity") +
                  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Year", y="million Rand (constant 2018)") +
                  #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
                  #scale_y_continuous(limits = c(0, 100000)) +
                  #coord_flip() +
                  scale_fill_manual(values=colours) +
                  facet_wrap( ~ Scenario, scales="fixed", ncol=2) +
                  labs(fill="Coal revenue decomposition :") 
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ExportDomesticRevenue_Decomposition_constant2018","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=30, height=10, units="cm", dpi=300)

############ Plot with nominal data, plus add net present values with 8% discount rate

df.plot.nominal <- df.plot %>% spread(Variable, Value)

df.CPI.econo.t  <- df.CPI.econo[df.CPI.econo$Variable=="Deflator",!colnames(df.CPI.econo)=='2017']
df.CPI.econo.t  <- df.CPI.econo.t %>% gather(Year, Value, -Case, -Variable, -Unit) %>% select(-Unit)
df.CPI.econo.t  <- df.CPI.econo.t %>% spread(Variable, Value)
df.CPI.econo.t$Year <- as.integer(df.CPI.econo.t$Year)
df.CPI.econo.t$Case <- factor(df.CPI.econo.t$Case, levels=c("BAU","2DS"))
colnames(df.CPI.econo.t)[colnames(df.CPI.econo.t)=="Case"]      <- "Scenario"
colnames(df.CPI.econo.t)[colnames(df.CPI.econo.t)=="Deflator"]  <- "Deflator"

df.plot.nominal <- full_join(df.plot.nominal, df.CPI.econo.t, by = c("Scenario","Year"))

df.plot2 <- df.plot.nominal  %>% transmute_at(funs(./Deflator), .vars = seq(3:11)+2)
df.plot2$Year     <- df.plot.nominal$Year
df.plot2$Scenario <- df.plot.nominal$Scenario
#df.plot2$Deflator <- df.plot.nominal$Deflator

df.plot2 <- df.plot2 %>% gather(Variable, Value, -Year, -Scenario)

levels.Var <- rev(c("Imports by coal", "Imports by rest of industry", 
                    "Taxes, coal", "Wages, coal", "Operating surplus, coal, domestic sales",  "Operating surplus, coal, exports", 
                    "Taxes, rest of industry", "Wages, rest of industry",   "Operating surplus, rest of industry"))

df.plot2$Variable  <- factor(df.plot2$Variable, levels=levels.Var)
df.plot2           <- df.plot2[order(df.plot2$Variable),]

df.plot2$Scenario  <- factor(df.plot2$Scenario, levels=c("BAU","2DS"))
df.plot2           <- df.plot2[order(df.plot2$Scenario),]


colours <- c("#3498db","#85c1e9",  "#2874a6", "#f1c40f", "#d4ac0d","#f7dc6f", "#b7950b", "#cb4335", "#d98880")    

p <- df.plot2  %>% ggplot(aes(x=Year, y=Value, fill=Variable)) +
  geom_bar(stat="identity") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y="million Rand (nominal)") +
  #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
  #scale_y_continuous(limits = c(0, 100000)) +
  #coord_flip() +
  scale_fill_manual(values=colours) +
  facet_wrap( ~ Scenario, scales="fixed", ncol=2) +
  labs(fill="Coal revenue decomposition :") 
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ExportDomesticRevenue_Decomposition_nominal","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=30, height=10, units="cm", dpi=300)


############ 

NE.demandExport               <- results.demandExport[[10]][[2]]
NE.demandDomestic             <- results.demandDomestic[[10]][[2]]

NE.demandExport.BAU.2018      <- NE.demandExport[NE.demandExport$Year=="2018" & NE.demandExport$Case=="BAU", ]
NE.demandDomestic.BAU.2018    <- NE.demandDomestic[NE.demandDomestic$Year=="2018" & NE.demandDomestic$Case=="BAU", ]

NE.demandExport.BAU.2035      <- NE.demandExport[NE.demandExport$Year=="2035" & NE.demandExport$Case=="BAU", ]
NE.demandExport.2Deg.2035     <- NE.demandExport[NE.demandExport$Year=="2035" & NE.demandExport$Case=="2DS", ]

NE.demandDomestic.BAU.2035    <- NE.demandDomestic[NE.demandDomestic$Year=="2035" & NE.demandDomestic$Case=="BAU", ]
NE.demandDomestic.2Deg.2035   <- NE.demandDomestic[NE.demandDomestic$Year=="2035" & NE.demandDomestic$Case=="2DS", ]

sum(NE.demandExport.BAU.2018[,4])   # 48884.23
sum(NE.demandDomestic.BAU.2018[,4]) # 86856.7

sum(NE.demandExport.BAU.2018[4,4])  # 23214.59
sum(NE.demandDomestic.BAU.2018[4,4])# 41247.31

sum(NE.demandExport.BAU.2035[,4])   # 46044
sum(NE.demandExport.2Deg.2035[,4])  # 16362

sum(NE.demandExport.BAU.2035[4,4])  # 21866
sum(NE.demandExport.2Deg.2035[4,4]) #  7770

sum(NE.demandExport.BAU.2035[,4]) - sum(NE.demandExport.BAU.2035[4,4])  # 24178

sum(NE.demandExport.2Deg.2035[,4]) - sum(NE.demandExport.2Deg.2035[4,4])# 8592

sum(NE.demandExport.BAU.2035[16,4])  # CTL, coke oven       # 170
sum(NE.demandExport.2Deg.2035[16,4]) # CTL, coke oven       # 60
sum(NE.demandExport.BAU.2035[28,4])  # General Machinery    # 863
sum(NE.demandExport.2Deg.2035[28,4]) # General Machinery    # 307
sum(NE.demandExport.BAU.2035[33,4])  # Elec                 # 401
sum(NE.demandExport.2Deg.2035[33,4]) # Elec                 # 143
sum(NE.demandExport.BAU.2035[36,4])  # Trade                # 5829
sum(NE.demandExport.2Deg.2035[36,4]) # Trade                # 2071
sum(NE.demandExport.BAU.2035[38,4])  # Transport            # 8178
sum(NE.demandExport.2Deg.2035[38,4]) # Transport            # 2906
sum(NE.demandExport.BAU.2035[46,4])  # Computer             # 2686
sum(NE.demandExport.2Deg.2035[46,4]) # Computer             # 954

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

##################################### GDP loss because of lost wages

df.temp <- results.demandTotal.summ[results.demandTotal.summ$Year=="2035", c("Industry","Wages","Scenario","Market")] 

df.temp <- df.temp %>% spread(Market, Wages)

df.temp$Total <- df.temp$Export + df.temp$Domestic

df.temp <- df.temp %>% gather(Market, Wages, -Industry, -Scenario) 

df.temp <- df.temp[df.temp$Market=="Total",]
  
df.temp <- df.temp %>% spread(Industry, Wages)

df.temp$AllInd <- rowSums(df.temp[,1:50+2])
  
lostWages <- df.temp[df.temp$Scenario=="BAU", "AllInd"] - df.temp[df.temp$Scenario=="2Deg", "AllInd"]
  
  
lostWages/sum(df.IOT2014$Household[1:50]) #0.3 % of GDP lost due to reduced consumption 

setwd(dir.ANALYSIS)
