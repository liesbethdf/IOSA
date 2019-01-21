
############################################
############ Run model for domestic and export demand
############################################

results.demandExport    <- finaldemandDecomposition.q(df.domesticDemand.coalExport)
results.demandDomestic  <- finaldemandDecomposition.q(df.domesticDemand.coalDomestic)

cat("Decomposition of coal export in results.demandExport","\n","\n")
cat("Decomposition of domestic coal demand in results.demandDomestic","\n","\n")

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
rm(df.plot1)
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

############ Table with net present values (NPV)

df.table.NPV          <- df.plot2   #%>% spread(Variable, Value)
df.table.NPV$Var      <- paste(df.table.NPV$Scenario, df.table.NPV$Variable, sep=".")
df.table.NPV          <- df.table.NPV[,!colnames(df.table.NPV) %in% c("Scenario", "Variable")] %>% spread(Var, Value)
df.table.NPV$Discount <- rep(1/1.08,18)**seq(0,17)

variables.indices <- seq(1:(dim(df.table.NPV)[2]-2))+1

df.temp                           <- df.table.NPV  %>% transmute_at(funs(.*Discount), .vars = variables.indices)
df.table.NPV[,variables.indices]  <- df.temp
df.table.NPV$Currency             <- rep("ZAR m, nominal", dim(df.table.NPV)[1])
ex.rate                           <- df.CPI.econo[1,1:18+4]
df.temp.USD                       <- df.temp %>% transmute_all(funs(./t(ex.rate)))
df.table.NPV.USD                  <- df.table.NPV
df.table.NPV.USD[,variables.indices]  <- df.temp.USD
df.table.NPV.USD$Currency         <- rep("USD m, nominal", dim(df.table.NPV)[1])
rm(df.temp, df.temp.USD)

df.NPV <- df.table.NPV[1,-(dim(df.table.NPV)[2]-1)]
df.NPV[,variables.indices] <- colSums(df.table.NPV[,variables.indices])
df.NPV$Year <- "2018-2035"
df.NPV      <- df.NPV %>% gather(Variable,Net.Present.Value, -Year, -Currency)

df.NPV.USD <- df.table.NPV.USD[1,-(dim(df.table.NPV.USD)[2]-1)]
df.NPV.USD[,variables.indices] <- colSums(df.table.NPV.USD[,variables.indices])
df.NPV.USD$Year <- "2018-2035"
df.NPV.USD      <- df.NPV.USD %>% gather(Variable,Net.Present.Value, -Year, -Currency)

split       <- strsplit(df.NPV$Variable,split='.', fixed=TRUE)
part1       <- unlist(split)[2*(1:length(split))-1]
part2       <- unlist(split)[2*(1:length(split))]

df.NPV$Scenario <- part1
df.NPV$Variable <- part2

split       <- strsplit(df.NPV.USD$Variable,split='.', fixed=TRUE)
part1       <- unlist(split)[2*(1:length(split))-1]
part2       <- unlist(split)[2*(1:length(split))]

df.NPV.USD$Scenario <- part1
df.NPV.USD$Variable <- part2
rm(split, part1, part2)
df.NPV      <- df.NPV %>% spread(Scenario, Net.Present.Value)
df.NPV      <- df.NPV[,c(seq(1,dim(df.NPV)[2]-2),dim(df.NPV)[2],dim(df.NPV)[2]-1) ]
df.NPV$Diff <- df.NPV$BAU - df.NPV$'2DS'

df.NPV.USD      <- df.NPV.USD %>% spread(Scenario, Net.Present.Value)
df.NPV.USD      <- df.NPV.USD[,c(seq(1,dim(df.NPV.USD)[2]-2),dim(df.NPV.USD)[2],dim(df.NPV.USD)[2]-1) ]
df.NPV.USD$Diff <- df.NPV.USD$BAU - df.NPV.USD$'2DS'

df.NPV$Industry <- "Rest of Industry"
df.NPV[grepl("coal", df.NPV$Variable)=="TRUE", "Industry"]  <- "Coal"
df.NPV          <- df.NPV[order(df.NPV$Industry),] 

df.NPV$Diff.share <- df.NPV$Diff / sum(df.NPV$Diff) * 100
df.NPV.USD$BAU.share <- df.NPV.USD$BAU / sum(df.NPV.USD$BAU) * 100
df.NPV.USD$'2DS.share' <- df.NPV.USD$'2DS' / sum(df.NPV.USD$'2DS') * 100
df.NPV.USD$Diff.share <- df.NPV.USD$Diff / sum(df.NPV.USD$Diff) * 100

df.2018.USD <- df.table.NPV.USD[1,]
df.2018.USD <- df.2018.USD %>% gather(Variable, Value, -Year, - Discount, -Currency)
split       <- strsplit(df.2018.USD$Variable,split='.', fixed=TRUE)
part1       <- unlist(split)[2*(1:length(split))-1]
part2       <- unlist(split)[2*(1:length(split))]

df.2018.USD$Scenario <- part1
df.2018.USD$Variable <- part2
rm(split, part1, part2)

df.2018.USD             <- df.2018.USD %>% spread(Scenario, Value)
df.2018.USD$BAU.share   <- df.2018.USD$BAU/ sum(df.2018.USD$BAU) * 100
df.2018.USD$'2DS.share' <- df.2018.USD$'2DS'/ sum(df.2018.USD$'2DS') * 100


df.2035.USD <- df.table.NPV.USD[dim(df.table.NPV.USD)[1],]
df.2035.USD <- df.2035.USD %>% gather(Variable, Value, -Year, - Discount, -Currency)
split       <- strsplit(df.2035.USD$Variable,split='.', fixed=TRUE)
part1       <- unlist(split)[2*(1:length(split))-1]
part2       <- unlist(split)[2*(1:length(split))]

df.2035.USD$Scenario <- part1
df.2035.USD$Variable <- part2
rm(split, part1, part2)

df.2035.USD <- df.2035.USD %>% spread(Scenario, Value)
df.2035.USD$Diff <- df.2035.USD$BAU - df.2035.USD$'2DS'
df.2035.USD$BAU.share <- df.2035.USD$BAU/ sum(df.2035.USD$BAU) *100
df.2035.USD$'2DS.share' <- df.2035.USD$'2DS'/ sum(df.2035.USD$'2DS') *100
df.2035.USD$Diff.share <- df.2035.USD$Diff/ sum(df.2035.USD$Diff) *100

df.2018.USD$Year  <- as.character(df.2018.USD$Year)
df.2035.USD$Year  <- as.character(df.2035.USD$Year)
df.riskshares <- bind_rows(df.NPV.USD, df.2018.USD)
df.riskshares <- bind_rows(df.riskshares, df.2035.USD)

table <- xtable(df.riskshares)
setwd(dir.TABLES)
fileName.table <- paste0(paste("riskDistribution", "coal_DomExp",sep="_"),".tex")

print(table, file=fileName.table)

setwd(dir.ANALYSIS)
############ Table with employment numbers sur to export and domestic demand, in the coal sector and in other sectors

# Employment numbers from export demand and demand by the electricity sector
# NE.demandExport               <- results.demandExport[[10]][[2]]
# NE.demandDomestic             <- results.demandDomestic[[10]][[2]]
# 
# # Employment numbers from CTL and other industry (than Elec and CTL) and consumers
# # CTL  31 m tons constant over 2018 - 2035
# # Other industry, 16 m tons and final consumers 2 m tons, for a total of 18 m tons
# 
# demand.CTL   <- c(rep(0,3), 31, rep(0,46)) 
# demand.other <- c(rep(0,3), 18, rep(0,46))
# 
# employment.CTL    <-  diag(NE.per.production.q) %*% LI.d.q %*% diag(demand.CTL) 
# employment.other  <-  diag(NE.per.production.q) %*% LI.d.q %*% diag(demand.other)
#   
# NE.demandExport               <- results.demandExport[[10]][[2]]
# NE.demandDomestic             <- results.demandDomestic[[10]][[2]]
# 
# NE.demandExport.BAU.2018      <- NE.demandExport[NE.demandExport$Year=="2018" & NE.demandExport$Case=="BAU", ]
# NE.demandDomestic.BAU.2018    <- NE.demandDomestic[NE.demandDomestic$Year=="2018" & NE.demandDomestic$Case=="BAU", ]
# 
# NE.demandExport.BAU.2035      <- NE.demandExport[NE.demandExport$Year=="2035" & NE.demandExport$Case=="BAU", ]
# NE.demandExport.2Deg.2035     <- NE.demandExport[NE.demandExport$Year=="2035" & NE.demandExport$Case=="2DS", ]
# 
# NE.demandDomestic.BAU.2035    <- NE.demandDomestic[NE.demandDomestic$Year=="2035" & NE.demandDomestic$Case=="BAU", ]
# NE.demandDomestic.2Deg.2035   <- NE.demandDomestic[NE.demandDomestic$Year=="2035" & NE.demandDomestic$Case=="2DS", ]
# 
# sum(NE.demandExport.BAU.2018[,4])   # 48884.23
# sum(NE.demandDomestic.BAU.2018[,4]) # 86856.7
# 
# sum(NE.demandExport.BAU.2018[4,4])  # 23335
# sum(NE.demandDomestic.BAU.2018[4,4])# 41461
# 
# sum(NE.demandExport.BAU.2035[,4])   # 46044
# sum(NE.demandExport.2Deg.2035[,4])  # 16362
# 
# sum(NE.demandExport.BAU.2035[4,4])  # 21979
# sum(NE.demandExport.2Deg.2035[4,4]) #  7810
# 
# sum(NE.demandExport.BAU.2035[,4]) - sum(NE.demandExport.BAU.2035[4,4])  # 24201
# sum(NE.demandExport.2Deg.2035[,4]) - sum(NE.demandExport.2Deg.2035[4,4])#  8599
# 
# sum(NE.demandExport.BAU.2035[28,4])  # General Machinery    # 863
# sum(NE.demandExport.2Deg.2035[28,4]) # General Machinery    # 307
# sum(NE.demandExport.BAU.2035[36,4])  # Trade                # 5829
# sum(NE.demandExport.2Deg.2035[36,4]) # Trade                # 2071
# sum(NE.demandExport.BAU.2035[38,4])  # Transport            # 8186
# sum(NE.demandExport.2Deg.2035[38,4]) # Transport            # 2909
# sum(NE.demandExport.BAU.2035[46,4])  # Computer             # 2686
# sum(NE.demandExport.2Deg.2035[46,4]) # Computer             # 954
# 
# sum(NE.demandExport.BAU.2035[,4]) - sum(NE.demandExport.BAU.2035[c(4,28,36,38,46),4])
# sum(NE.demandExport.2Deg.2035[,4]) - sum(NE.demandExport.2Deg.2035[c(4,28,36,38,46),4])
# 
# sum(NE.demandDomestic.BAU.2035[,4])
# NE.coalDomesticDemand.BAU.2035 <- data.frame(NE.demandDomestic.BAU.2035[,4])
# colnames(NE.coalDomesticDemand.BAU.2035) <- "Value"
# NE.coalDomesticDemand.BAU.2035$Industry <- Industry
# 
# order <- order(NE.coalDomesticDemand.BAU.2035$Value)
# 
# NE.coalDomesticDemand.BAU.2035 <- NE.coalDomesticDemand.BAU.2035[c(4,38),]
# 
# sum(NE.demandDomestic.2Deg.2035[,4])
# 
# NE.demandDomestic.2Deg.2035[4,4]
# 
# sum(NE.demandExport[NE.demandExport$Year=="2025" & NE.demandExport$Case=="BAU", 4 ])

##################################### GDP loss because of lost wages

# df.temp <- results.demandTotal.summ[results.demandTotal.summ$Year=="2035", c("Industry","Wages","Scenario","Market")] 
# df.temp <- df.temp %>% spread(Market, Wages)
# df.temp$Total <- df.temp$Export + df.temp$Domestic
# df.temp <- df.temp %>% gather(Market, Wages, -Industry, -Scenario) 
# df.temp <- df.temp[df.temp$Market=="Total",]
# df.temp <- df.temp %>% spread(Industry, Wages)
# df.temp$AllInd <- rowSums(df.temp[,1:50+2])
# lostWages <- df.temp[df.temp$Scenario=="BAU", "AllInd"] - df.temp[df.temp$Scenario=="2Deg", "AllInd"]
# lostWages/sum(df.IOT2014$Household[1:50]) #0.3 % of GDP lost due to reduced consumption 
setwd(dir.ANALYSIS)
