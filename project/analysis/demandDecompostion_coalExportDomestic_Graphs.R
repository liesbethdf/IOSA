
############################################
############ graphs number of jobs and value added disributed over sectors
############################################

# results.demandExport    <- finaldemandDecomposition.q(df.domesticDemand.coalExport)

results.demandDomestic.VA           <- results.demandDomestic[[4]][[2]]
results.demandDomestic.Nemployees   <- results.demandDomestic[[10]][[2]]

results.demandExport.VA             <- results.demandExport[[4]][[2]]
results.demandExport.Nemployees     <- results.demandExport[[10]][[2]]

# resultsHereAll <- finaldemandDecomposition(df.domesticDemand.input)
# 
# resultsHereCoalExport <- finaldemandDecomposition(df.domDemand.coal.input.v2)
# 
# outputDecomp.CE.summ  <- resultsHereCoalExport[[16]][[2]]
# 
# outputDecomp.All      <- resultsHereAll[[1]][[2]]
# outputDecomp.CE       <- resultsHereAll[[1]][[2]]
# 
# resultsNemployees.All <- resultsHereAll[[10]][[2]]
# resultsNemployees.CE  <- resultsHereCoalExport[[10]][[2]]
# 
# valueaddedDecomp.vol.CE    <- resultsHereCoalExport[[3]][[2]]
# valueaddedDecomp.val.CE    <- resultsHereCoalExport[[15]][[2]]


############ graph for employment resulting from coal export :
plot.decompSectors <- function(scenarioReference, detail, df.resultsHere, colours, label.y.axis)
{
#scenarioReference <- "BAU"
#  detail <- 10
#  df.resultsHere <- resultsNemployees.CE
  
  df.plot <- df.resultsHere  %>% select(I4, Industry, Year, Var, Case) %>%
    spread(Industry,I4)
  
  orderHere <- order(-df.plot[df.plot$Year=="2018" & df.plot$Case==scenarioReference,1:50 +3])
  
  df.plot <- df.plot[,c(1,2,3,orderHere+3)]
  nHere   <- n-detail
  df.plot$Other.sectors <- rowSums(df.plot[,1:nHere +detail+3])
  
  cut1 <- detail + 3
  cut2 <- n - cut1
  end1 <- n - detail
  
  reorder.all     <- c(seq(1:cut1),dim(df.plot)[2],seq(1:end1) + cut1)
  reorder.select  <- c(seq(1:cut1),dim(df.plot)[2])
  
  df.plot.all     <-  df.plot[,reorder.all]
  levels.Industry <- c(colnames(df.plot)[1:10+3],"Other.sectors")
  df.plot <-  df.plot[,reorder.select] %>% gather(Industry, Value, - Year, -Var, -Case, factor_key = TRUE)
  
  
  ###### graph with BAU and 2DS
  
  
#  levels.Industry <- colnames(df.plot)[1:10+3]#c("I4", "I38", "I36",  "I46",  "I28",  "I40",  "I27",  "I42",  "I18",  "I50",  "Other.sectors")
  numselec        <- as.numeric(substr(levels.Industry[-length(levels.Industry)],2,3))
  labels.Industry <- c(Industry[numselec],"Rest of industry")
  
  df.plot$Industry <- factor(df.plot$Industry, levels=levels.Industry, labels=labels.Industry)
  df.plot          <- df.plot[order(df.plot$Industry),]
  
  df.plot$Case <- factor(df.plot$Case, levels=c("BAU","2DS"))
  df.plot          <- df.plot[order(df.plot$Case),]
  
  
  #colours <-c("#b7950b","#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72", "#5d6d7e")    
  #colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
  
  
  p <- df.plot %>% ggplot(aes(x=Year, y=Value, fill=Industry)) +
    geom_bar(stat="identity") +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Year", y=label.y.axis) +
    #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
    #scale_y_continuous(limits = c(0, 100000)) +
    #coord_flip() +
    scale_fill_manual(values=colours) +
    facet_wrap( ~ Case, scales="fixed", ncol=2) +
    labs(fill="Sector :") 
  
  results <- list(p,df.plot)
  return(results)
}

############ graph for employment distributed over sectors

# colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")
# p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandExport.Nemployees, colours=colours, label.y.axis="number of jobs")[[1]]
# 
# print(p)
# 
# setwd(dir.PLOTS)
# fileName.graph <- paste("NumberEmployees_Sector","Coal-Export","BAU_2DS_v2", sep="_")
# ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)

############ graph for value added distributed over sectors

# colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
# 
# p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandExport.VA, colours=colours, label.y.axis = "million Rand (constant 2014)")[[1]]
# plot(p)
# 
# setwd(dir.PLOTS)
# fileName.graph <- paste("ValueAdded_Sector","Coal-Export","BAU_2DS_v2", sep="_")
# ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)

############ Export only
############ bargraph with employment and value added distributed over sectors, for 2018 2DS and BAU, 2035 BAU, 2035 2DS

df.plot1 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandExport.Nemployees, colours=colours, label.y.axis="number of jobs")[[2]]
df.plot2 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandExport.VA, colours=colours, label.y.axis="value added")[[2]]

IndustryOrder1 <- levels(df.plot1$Industry)
IndustryOrder2 <- levels(df.plot2$Industry)
IndustryOrder  <- c(IndustryOrder1, IndustryOrder2[!IndustryOrder2 %in% IndustryOrder1])

df.plot1 <- df.plot1 %>% filter(df.plot1$Year %in% c(2018,2035))
df.plot2 <- df.plot2 %>% filter(df.plot2$Year %in% c(2018,2035))

df.plot <- bind_rows(df.plot1, df.plot2)

df.plot$CaseYear <- paste(df.plot$Case, df.plot$Year, sep=", ")

df.plot <- df.plot %>% filter(!df.plot$CaseYear=="2DS, 2018")
levelsHere <- c("BAU, 2018", "BAU, 2035", "2DS, 2035")
labelsHere <- c("BAU & 2DS, 2018", "BAU, 2035", "2DS, 2035")
df.plot$CaseYear <- factor(df.plot$CaseYear, levels=levelsHere, labels=labelsHere )

df.plot[df.plot$Var=="Employment", "Var"] <- "Employment, number of people" 
df.plot[df.plot$Var=="Gross value added value", "Var"] <- "Value added, million Rand (constant 2018)" 

levelsHere <- IndustryOrder
#labelsHere <- IndustryOrder
df.plot$Industry <- factor(df.plot$Industry, levels=levelsHere)#, labels=labelsHere )
#df.plot          <- df.plot[order(df.plot$IndustryOrder),]

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e", "#d4e6f1" ,"#1a5276")

p2  <-  df.plot %>% ggplot(aes(x=CaseYear, y=Value, fill=Industry)) +
        geom_bar(stat="identity") +
        #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "", y= "" ) +
        #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
        #scale_y_continuous(limits = c(0, 100000)) +
        #coord_flip() +
        scale_fill_manual(values=colours) +
        facet_wrap( ~ Var, scales="free", ncol=3) +
        labs(fill="Sector :") 
print(p2)

setwd(dir.PLOTS)
fileName.graph <- paste("ValueAdded_Employment_Sector","Coal-Export","constant2018","BAU_2DS_v2", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=24, height=11, units="cm", dpi=300)


############ same as above but export and domestic combined
############ bargraph with employment and value added distributed over sectors, for 2018 2DS and BAU, 2035 BAU, 2035 2DS

df.plot1 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandDomestic.Nemployees, colours=colours, label.y.axis="number of jobs")[[2]]
df.plot2 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandDomestic.VA, colours=colours, label.y.axis="value added")[[2]]
df.plot3 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandExport.Nemployees, colours=colours, label.y.axis="number of jobs")[[2]]
df.plot4 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=results.demandExport.VA, colours=colours, label.y.axis="value added")[[2]]

df.plot1$Value <- df.plot1$Value + df.plot3$Value
df.plot2$Value <- df.plot2$Value + df.plot4$Value

IndustryOrder1 <- levels(df.plot1$Industry)
IndustryOrder2 <- levels(df.plot2$Industry)
IndustryOrder  <- c(IndustryOrder1, IndustryOrder2[!IndustryOrder2 %in% IndustryOrder1])

df.plot1 <- df.plot1 %>% filter(df.plot1$Year %in% c(2018,2035))
df.plot2 <- df.plot2 %>% filter(df.plot2$Year %in% c(2018,2035))
df.plot  <- bind_rows(df.plot1, df.plot2)

df.plot$CaseYear <- paste(df.plot$Case, df.plot$Year, sep=", ")

df.plot <- df.plot %>% filter(!df.plot$CaseYear=="2DS, 2018")
levelsHere <- c("BAU, 2018", "BAU, 2035", "2DS, 2035")
labelsHere <- c("BAU & 2DS, 2018", "BAU, 2035", "2DS, 2035")
df.plot$CaseYear <- factor(df.plot$CaseYear, levels=levelsHere, labels=labelsHere )

df.plot[df.plot$Var=="Employment", "Var"] <- "Employment, number of people" 
df.plot[df.plot$Var=="Gross value added value", "Var"] <- "Value added, million Rand (constant 2018)" 

levelsHere <- IndustryOrder
#labelsHere <- IndustryOrder
df.plot$Industry <- factor(df.plot$Industry, levels=levelsHere)#, labels=labelsHere )
df.plot[df.plot$Var=="Gross value added value", "Var"] <- "Value added, million Rand (nominal)" 


colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e", "#d4e6f1" ,"#1a5276")

p3  <-  df.plot %>% ggplot(aes(x=CaseYear, y=Value, fill=Industry)) +
        geom_bar(stat="identity") +
        #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "", y= "" ) +
        scale_fill_manual(values=colours) +
        facet_wrap( ~ Var, scales="free", ncol=3) +
        labs(fill="Sector :") 
print(p3)

setwd(dir.PLOTS)
fileName.graph <- paste("ValueAdded_Employment_Sector","Coal-Domestic-Export","constant2018","BAU_2DS", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=24, height=11, units="cm", dpi=300)

############ same as above but export and domestic combined, in nominal values for value added
############ bargraph with employment and value added distributed over sectors, for 2018 2DS and BAU, 2035 BAU, 2035 2DS

deflator.2035 <- df.CPI.econo[df.CPI.econo$Variable=="Deflator" & df.CPI.econo$Case=="BAU","2035"]
df.plot[df.plot$Year=="2035" & df.plot$Var=="Value added, million Rand (constant 2018)","Value"] <- df.plot[df.plot$Year=="2035" & df.plot$Var=="Value added, million Rand (constant 2018)","Value"] / deflator.2035

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e", "#d4e6f1" ,"#1a5276")

p4  <-  df.plot %>% ggplot(aes(x=CaseYear, y=Value, fill=Industry)) +
  geom_bar(stat="identity") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y= "" ) +
  scale_fill_manual(values=colours) +
  facet_wrap( ~ Var, scales="free", ncol=3) +
  labs(fill="Sector :") 
print(p4)

setwd(dir.PLOTS)
fileName.graph <- paste("ValueAdded_Employment_Sector","Coal-Domestic-Export","nominal","BAU_2DS_v2", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=24, height=11, units="cm", dpi=300)

############ same as above but export and domestic combined, in nominal values at show -- jobs and VA at risk, which 2DS - BAU
############ bargraph with employment and value added distributed over sectors, for 2018 2DS and BAU, 2035 BAU, 2035 2DS

df.temp <- df.plot[!df.plot$CaseYear=="BAU & 2DS, 2018",] %>% select(-CaseYear) %>% spread(Case, Value)
df.temp$Value <- df.temp$BAU - df.temp$'2DS'
df.temp <- df.temp %>% select(-BAU, -'2DS')
df.temp$CaseYear <- "At risk, 2035\n(BAU - 2DS)"

levels.Var <- c("Employment, number of people", "Value added, million Rand (constant 2018)")
labels.Var <- c("Employment,\nnumber of people", "Value added,\nm Rand")

df.temp$Var <- factor(df.temp$Var, levels=levels.Var, labels=labels.Var)

df.plot <- df.temp

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e", "#d4e6f1" ,"#1a5276")

p4  <-  df.temp %>% ggplot(aes(x=CaseYear, y=Value, fill=Industry)) +
  geom_bar(stat="identity") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y= "" ) +
  scale_fill_manual(values=colours) +
  facet_wrap( ~ Var, scales="free", ncol=3) +
  labs(fill="Sector :") 
print(p4)

setwd(dir.PLOTS)
fileName.graph <- paste("ValueAdded_Employment_Sector","Coal-Domestic-Export","nominal","atRisk2035", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=16, height=11, units="cm", dpi=300)

setwd(dir.ANALYSIS)