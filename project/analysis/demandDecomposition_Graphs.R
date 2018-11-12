

############################################
########## graph decomposing export revenue into imports and value added, direct and indirect
#################### once for BAU, once for 2Deg, once for difference + total, summed over all years
############################################

df.outputDecomp <- outputDecomp.CE.summ

df.plot <- df.outputDecomp %>% select (Year, Scenario, Industry, Value.demand.dom, Value.demand.imp, Output.value, Dom.inputs,
                                       Imp.inputs, Wages, Taxes.other.value, Gross.op.surplus.value, Taxes.prod.value, VA.value)

df.plot[is.na(df.plot)] <- 0

df.plot <- df.plot %>% gather(Var, Value, -Year, -Scenario, -Industry) %>%
                       spread(Industry, Value)

df.plot$Inot4 <- rowSums(df.plot[,1:50 +3]) - df.plot$I4

df.plot <- df.plot %>% select(Year, Scenario, Var, I4, Inot4) %>%
                       gather(Industry, Value, -Year, -Scenario, -Var, factor_key = TRUE) %>%
                       spread(Var, Value) 

df.plot$Taxes <- df.plot$Taxes.other.value + df.plot$Taxes.prod.value
df.plot$Imports <- df.plot$Imp.inputs + df.plot$Value.demand.imp

df.plot <- df.plot %>% select(Year, Scenario, Industry, Imports, Wages, Gross.op.surplus.value, Taxes)  %>%
                       gather(Var, Value, -Year, -Scenario, -Industry)

levels(df.plot$Industry)[levels(df.plot$Industry)=="I4"] <- "Coal"
levels(df.plot$Industry)[levels(df.plot$Industry)=="Inot4"] <- "Others"

df.plot$stackVar  <- paste(df.plot$Industry,df.plot$Var, sep= " ")
df.plot           <- df.plot %>% spread(Scenario, Value)
df.plot$Diff      <- df.plot$BAU - df.plot$'2Deg'
df.plot           <- df.plot %>% gather(Scenario, Value, -Year, -Industry, -Var, -stackVar, factor_key = TRUE)

for (scenarioHere in c("BAU", "2Deg", "Diff"))
{
  #scenario <- "BAU"
  
  df.plot2 <- df.plot %>% filter(df.plot$Scenario==scenarioHere)
  
  levels.stackVar <- rev(c("Coal Imports", "Others Imports", 
                           "Coal Taxes", "Coal Wages",   "Coal Gross.op.surplus.value",
                           "Others Taxes", "Others Wages",   "Others Gross.op.surplus.value"))
  
  labels.stackVar <- rev(c("Imports by coal", "Imports by rest of industry", 
                           "Taxes, coal", "Wages, coal",   "Operating surplus, coal",
                           "Taxes, rest of industry", "Wages, rest of industry",   "Operating surplus, rest of industry"))
  
  df.plot2$stackVar <- factor(df.plot2$stackVar, levels=levels.stackVar, labels=labels.stackVar)
  df.plot2     <- df.plot2[order(df.plot2$stackVar),]
  
  colours <-c("#3498db","#85c1e9",  "#2874a6", "#f1c40f", "#f7dc6f", "#b7950b", "#cb4335", "#d98880")    
  
  p <- df.plot2 %>% ggplot(aes(x=Year, y=Value, fill=stackVar)) +
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

}

###### graph with BAU and 2Deg

df.plot2 <- df.plot %>% filter(df.plot$Scenario %in% c("BAU","2Deg"))

levels.stackVar <- rev(c("Coal Imports", "Others Imports", 
                         "Coal Taxes", "Coal Wages",   "Coal Gross.op.surplus.value",
                         "Others Taxes", "Others Wages",   "Others Gross.op.surplus.value"))

labels.stackVar <- rev(c("Imports by coal", "Imports by rest of industry", 
                         "Taxes, coal", "Wages, coal",   "Operating surplus, coal",
                         "Taxes, rest of industry", "Wages, rest of industry",   "Operating surplus, rest of industry"))

df.plot2$stackVar <- factor(df.plot2$stackVar, levels=levels.stackVar, labels=labels.stackVar)
df.plot2     <- df.plot2[order(df.plot2$stackVar),]

df.plot2$Scenario <- factor(df.plot2$Scenario, levels=c("BAU","2Deg"))
df.plot2     <- df.plot2[order(df.plot2$Scenario),]


colours <-c("#3498db","#85c1e9",  "#2874a6", "#f1c40f", "#f7dc6f", "#b7950b", "#cb4335", "#d98880")    


p <- df.plot2 %>% ggplot(aes(x=Year, y=Value, fill=stackVar)) +
  geom_bar(stat="identity") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y="million Rand (constant 2014)") +
  #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
  #scale_y_continuous(limits = c(0, 100000)) +
  #coord_flip() +
  scale_fill_manual(values=colours) +
  facet_wrap( ~ Scenario, scales="fixed", ncol=2) +
  labs(fill="Export revenue decomposition :") 
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ExportRevenue_Decomposition","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)






# df.plot2 <- df.outputDecomp %>% select (Year, Scenario, Industry, Value.demand.dom, Value.demand.imp, Output.value, Dom.inputs,
#                                        Imp.inputs, Wages, Taxes.other.value, Gross.op.surplus.value, Taxes.prod.value, VA.value)
# 
# df.plot2[is.na(df.plot2)] <- 0
# 
# df.plot2 <- df.plot2[1:50,]
# 
# dim(df.plot2)
# 
# colSums(df.plot2[,4:13]) - df.plot2[4,4:13]
# 
# df.plot2[4,4:13]























