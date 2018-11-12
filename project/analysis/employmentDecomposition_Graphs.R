
############################################
############ graphs number of jobs and value added disributed over sectors
############################################

resultsHereAll <- finaldemandDecomposition(df.domesticDemand.input)

resultsHereCoalExport <- finaldemandDecomposition(df.domDemand.coal.input.v2)

outputDecomp.CE.summ  <- resultsHereCoalExport[[16]][[2]]

outputDecomp.All      <- resultsHereAll[[1]][[2]]
outputDecomp.CE       <- resultsHereAll[[1]][[2]]

resultsNemployees.All <- resultsHereAll[[10]][[2]]
resultsNemployees.CE  <- resultsHereCoalExport[[10]][[2]]

valueaddedDecomp.vol.CE    <- resultsHereCoalExport[[3]][[2]]
valueaddedDecomp.val.CE    <- resultsHereCoalExport[[15]][[2]]


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
  
  
  ###### graph with BAU and 2Deg
  
  
#  levels.Industry <- colnames(df.plot)[1:10+3]#c("I4", "I38", "I36",  "I46",  "I28",  "I40",  "I27",  "I42",  "I18",  "I50",  "Other.sectors")
  numselec        <- as.numeric(substr(levels.Industry[-length(levels.Industry)],2,3))
  labels.Industry <- c(Industry[numselec],"Rest of industry")
  
  df.plot$Industry <- factor(df.plot$Industry, levels=levels.Industry, labels=labels.Industry)
  df.plot          <- df.plot[order(df.plot$Industry),]
  
  df.plot$Case <- factor(df.plot$Case, levels=c("BAU","2Deg"))
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

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=resultsNemployees.CE, colours=colours, label.y.axis="number of jobs")[[1]]

print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("NumberEmployees_Sector","Coal-Export","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)

############ graph for value added distributed over sectors

colours <-c("#b7950b",rev(c("#d6eaf8",  "#aed6f1", "#85c1e9", "#5dade2", "#3498db", "#2e86c1", "#2874a6", "#21618c", "#1b4f72")), "#5d6d7e")    
#p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=valueaddedDecomp.CE, colours=colours, label.y.axis = "million Rand (constant 2014)")
#plot(p)

p <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=valueaddedDecomp.val.CE, colours=colours, label.y.axis = "million Rand (constant 2014)")
plot(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ValueAdded_Sector","Coal-Export","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=28, height=9, units="cm", dpi=300)

############ bargraph with employment and value added distributed over sectors, for 2018 2Deg and BAU, 2035 BAU, 2035 2Deg

df.plot1 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=resultsNemployees.CE, colours=colours, label.y.axis="number of jobs")[[2]]

df.plot2 <- plot.decompSectors(scenarioReference="BAU", detail=10, df.resultsHere=valueaddedDecomp.val.CE, colours=colours, label.y.axis="number of jobs")[[2]]

IndustryOrder1 <- levels(df.plot1$Industry)

IndustryOrder2 <- levels(df.plot2$Industry)

IndustryOrder  <- c(IndustryOrder1, IndustryOrder2[!IndustryOrder2 %in% IndustryOrder1])

df.plot1 <- df.plot1 %>% filter(df.plot1$Year %in% c(2018,2035))
df.plot2 <- df.plot2 %>% filter(df.plot2$Year %in% c(2018,2035))

df.plot <- bind_rows(df.plot1, df.plot2)

df.plot$CaseYear <- paste(df.plot$Case, df.plot$Year, sep=" ")

df.plot <- df.plot %>% filter(!df.plot$CaseYear=="2Deg, 2018")
levelsHere <- c("BAU, 2018", "BAU, 2035", "2Deg, 2035")
labelsHere <- c("BAU & 2Deg, 2018", "BAU, 2035", "2Deg, 2035")
df.plot$CaseYear <- factor(df.plot$CaseYear, levels=levelsHere, labels=labelsHere )

df.plot[df.plot$Var=="Employment", "Var"] <- "Employment, number of people" 
df.plot[df.plot$Var=="Value added, value", "Var"] <- "Value added, million Rand (constant 2014)" 

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
fileName.graph <- paste("ValueAdded_Employment_Sector","Coal-Export","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=24, height=11, units="cm", dpi=300)



p1 <- df.plot %>% ggplot(aes(x=Var, y=Value, fill=Industry)) +
  geom_bar(stat="identity") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #labs(x = "Year", y=label.y.axis) +
  #scale_x_discrete(limits = rev(levels(df.plot$Industry.code))) +
  #scale_y_continuous(limits = c(0, 100000)) +
  #coord_flip() +
  scale_fill_manual(values=colours) +
  facet_wrap( ~ CaseYear, scales="fixed", ncol=3) +
  labs(fill="Sector :") 

print(p)




############ graph for number of jobs in the coal industry, sustained by export and by domestic demand

resultsNemployees.All <- resultsHereAll[[10]][[2]]

scenarioReference <- "BAU"
detail <- 10
df.resultsHere <- resultsNemployees.All

df.plot <- df.resultsHere  %>% filter(df.resultsHere$Industry=="I4") 






