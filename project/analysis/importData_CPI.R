

############################################
############ Import CPI inflation and exchange rate data
############################################

fileName.list <- c("CPI_TimeSeriesforEconomist_1901.xlsx")

i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.CPI.econo <- data.frame(read_excel(data, 
                                sheet=5, 
                                range = cell_limits(ul = c(1,1), lr = c(3, 20))))

yearsCPI <- colnames(df.CPI.econo)[2:20]
yearsCPI <- as.numeric(substr(yearsCPI, 2, 5))
colnames(df.CPI.econo) <- c("Variable",yearsCPI)
df.CPI.econo$Variable <- c("Inflation", "Exchange rate")
df.CPI.econo$Unit     <- c("rate", "ZAR/USD")
df.CPI.econo$Case     <- c("BAU","BAU") 
df.CPI.econo          <- df.CPI.econo[,c("Case","Variable","Unit",yearsCPI)]
df.CPI.econo          <- bind_rows(df.CPI.econo, df.CPI.econo)
df.CPI.econo$Case     <- c("2Deg","2Deg")

#df.CPI.econo <- df.CPI.econo %>% gather(Year,Value, -Variable)

############################################
############ Import CPI coal export data + graphs
############################################

#fileName.list <- c("CPI_coal_power_18.xlsx")
fileName.list <- c("CPI_coal_power_0119.xlsx")

i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.CPI <- data.frame(read_excel(data, 
                                    sheet=1, 
                                    range = cell_limits(ul = c(1,1), lr = c(9, 21))))

colnames(df.CPI)[4:21] <- yearsCPI

df.conversion           <- data.frame(matrix(rep(0,42),ncol=21))
colnames(df.conversion) <- colnames(df.CPI)
df.conversion$Case      <- c("BAU","2Deg")
df.conversion$Variable  <- "Exchange rate"
df.conversion$Unit      <- "ZAR/USD"
df.conversion[,1:18+3]  <- 14

df.CPI                  <- bind_rows(df.CPI,df.conversion)

df.priceZAR             <- data.frame(matrix(rep(0,42),ncol=21))
colnames(df.priceZAR)   <- colnames(df.CPI)
df.priceZAR$Case        <- c("BAU","2Deg")
df.priceZAR$Variable    <- "Export Price"
df.priceZAR$Unit        <- "ZAR/t"
df.priceZAR[1,1:18+3]   <- df.CPI[7,1:18+3] * df.CPI[9,1:18+3]
df.priceZAR[2,1:18+3]   <- df.CPI[8,1:18+3] * df.CPI[10,1:18+3]

df.CPI                  <- bind_rows(df.CPI,df.priceZAR)

df.valExport             <- data.frame(matrix(rep(0,42),ncol=21))
colnames(df.valExport)   <- colnames(df.CPI)
df.valExport$Case        <- c("BAU","2Deg")
df.valExport$Variable    <- "Export Value"
df.valExport$Unit        <- "ZAR m"
df.valExport[1,1:18+3]   <- df.CPI[3,1:18+3] * df.CPI[11,1:18+3]
df.valExport[2,1:18+3]   <- df.CPI[4,1:18+3] * df.CPI[12,1:18+3]

df.CPI                  <- bind_rows(df.CPI, df.valExport)

df.CPI        <- df.CPI %>% gather(Year, Value, -Case, -Variable, -Unit)
df.CPI$Year   <- as.numeric(df.CPI$Year)

df.CPI$Var      <- df.CPI$Variable
df.CPI$Var.type <- df.CPI$Variable
df.CPI$Var      <- substr(df.CPI$Var,8,8)
df.CPI[df.CPI$Var.type=="Export Value","Var"]      <- "Val"
df.CPI$Var      <- paste(df.CPI$Var,df.CPI$Case,sep="." )
df.CPI$Market   <- "Export"
df.CPI$Var.type[df.CPI$Var.type=="Export Volumes"] <- "Volume"
df.CPI$Var.type[df.CPI$Var.type=="Export Price"] <- "Price"
df.CPI$Var.type[df.CPI$Var.type=="Export Value"] <- "Value"
  
setwd(dir.ANALYSIS)

############ Graph of coal export data

df.plot         <- df.CPI %>% filter(df.CPI$Unit %in% c("mt","USD/t")) %>% select(-Var, -Var.type, -Unit) %>% spread(Variable, Value)
df.plot$"Value" <- df.plot$`Export Price` * df.plot$`Export Volumes`
df.plot         <- df.plot %>% gather(Variable, Value, -Case, -Year, -Market)
#df.plot         <- df.plot %>% filter(!df.plot$Variable=="Value")
df.plot$Case    <- factor(df.plot$Case, levels =c("BAU","2Deg"), labels=c("BAU","2DS"))
df.plot         <- df.plot[order(df.plot$Case),]

levels.Var      <- c("Export Volumes", "Export Price", "Value")
labels.Var      <- c("Exported volumes, mt", "Export price, USD/t", "Revenue from coal export, USD")

df.plot$Variable <- factor(df.plot$Variable, levels=levels.Var, labels=labels.Var)
df.plot          <- df.plot[order(df.plot$Variable),]



colours <- c("#5d6d7e", "#dc7633")
#facet.labels <- list('Export Price'="Export price, USD", 'Export Volumes'="Exported volumes, mtce", 'Value'="Revenue from coal export, USD")
#facet.labels <- c("Export price, USD", "Exported volumes, mtce", "Revenue from coal export, USD")


p <- df.plot %>% ggplot(aes(x=Year, y=Value)) +
#                 geom_line(aes(color=Case, linetype=Variable)) +
                 geom_line(aes(color=Case), size=0.8) +
                 #geom_area(aes(fill = Case, group = Case), alpha = 0.5, position = 'identity') +
                 scale_fill_manual(values = c("#aeb6bf","#dc7633")) +
                 #scale_color_brewer(palette="Paired") +
                 ylab("") +
                 scale_colour_manual(values=colours) +
                 facet_wrap(~ Variable, nrow=1, scales="free") #, labeller=as_labeller(facet.labels))
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("Coal-Export","CPI_VolPrice","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=30, height=6, units="cm", dpi=300)

setwd(dir.ANALYSIS)

############################################
############ Import CPI coal domestic data + graphs
############################################

fileName.list <- c("CPI_coal_power_0119.xlsx")

i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.CPI.domestic <- data.frame(read_excel(data, 
                                         sheet=2, 
                                         range = cell_limits(ul = c(1,1), lr = c(17, 22))))
df.CPI.domestic <- df.CPI.domestic %>% gather(Year, Value, -Case, -Plants, -Variable, -Unit)

df.CPI.domestic$Year      <- as.numeric(substr(df.CPI.domestic$Year, 2,5))
#unique(df.CPI.domestic$Variable)

df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal costs", "Variable"]        <- "Total cost, ZAR"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal costs", "Variable"]             <- "Total cost, ZAR"
df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal consumption", "Variable"]  <- "Total volume consumed, mt"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal consumption", "Variable"]       <- "Total volume consumed, mt"
df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal price", "Variable"]        <- "Price, ZAR/t"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal price", "Variable"]             <- "Price, ZAR/t"

df.CPI.domestic$Variable  <- factor(df.CPI.domestic$Variable)
levelsDomestic            <- c("Total volume consumed, mt", "Price, ZAR/t", "Total cost, ZAR")
df.CPI.domestic$Variable  <- factor(df.CPI.domestic$Variable, levels=levelsDomestic)
df.CPI.domestic           <- df.CPI.domestic %>% filter(!df.CPI.domestic$Unit=="index" & df.CPI.domestic$Plant=="All") 

df.plot         <- df.CPI.domestic

df.plot$Case    <- factor(df.plot$Case, levels =c("BAU","2Deg"), labels=c("BAU","2DS"))
df.plot         <- df.plot[order(df.plot$Case),]


colours <- c("#5d6d7e", "#dc7633")
#facet.labels <- list('Export Price'="Export price, USD", 'Export Volumes'="Exported volumes, mtce", 'Value'="Revenue from coal export, USD")
#facet.labels <- c("Export price, USD", "Exported volumes, mtce", "Revenue from coal export, USD")

p <- df.plot %>% ggplot(aes(x=Year, y=Value)) +
                  geom_line(aes(color=Case), size=0.8) +
                  scale_fill_manual(values = c("#aeb6bf","#dc7633")) +
                  ylab("") +
                  scale_colour_manual(values=colours) +
                  facet_wrap(~ Variable, nrow=1, scales="free") 
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("Coal-Domestic","CPI_VolPrice","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=30, height=6, units="cm", dpi=300)

setwd(dir.ANALYSIS)

############################################
############ Dataframe with export and domestic demand
############################################

df.CPI.total            <- df.CPI.domestic %>% select(-Plants)
df.CPI.total$Market     <- "Domestic"
df.CPI.total$Var.type   <- df.CPI.total$Variable
df.CPI.total$Var.type   <- factor(df.CPI.total$Var.type, levels=c("Total volume consumed, mt", "Price, ZAR/t", "Total cost, ZAR"), labels=c("Volume","Price","Value"))


df.CPI.total$Var[!df.CPI.total$Var.type=="Value"] <- substr(df.CPI.total$Var.type[!df.CPI.total$Var.type=="Value"],1,1)
df.CPI.total$Var[df.CPI.total$Var.type=="Value"]  <- substr(df.CPI.total$Var.type[df.CPI.total$Var.type=="Value"],1,3)
df.CPI.total$Var                                  <- paste(df.CPI.total$Var, df.CPI.total$Case, sep=".") 

df.CPI.temp <- df.CPI[,colnames(df.CPI.total)]

df.CPI.total <- bind_rows(df.CPI.total, df.CPI.temp)

#################### volume for determining employment

# df.CPI.temp           <- df.CPI.total %>% filter(df.CPI.total$Var.type=="Volume" & df.CPI.total$Unit=="mt") %>% select(-Variable) %>% spread(Market,Value)
# df.CPI.temp$CTL       <- 31.079
# #df.CPI.temp$final     <- 10000
# df.CPI.temp$otherInd  <- 15.000
# df.CPI.temp$Total     <- df.CPI.temp$Export + df.CPI.temp$Domestic + df.CPI.temp$CTL + df.CPI.temp$otherInd

# total industry (2014 IOT - electricity)
# 47357 - 27086 -8533 = 11783
# 3291 final demand in 2014 other than export
# total domestic tonnes sold 184416 (mining council)

# 2014 IOT value output
# 114230.41374
# price per ton
# (114230.41374 - 63582) / 184416 = 0.2746422

# tonnes represented by other demand than export + elec : (20271 + 3291)/0.2746422 = 85.791

setwd(dir.ANALYSIS)

############################################
############ Import CPI coal domestic data + graphs
############################################

fileName.list <- c("CPI_oil.xlsx")

i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.CPI.oil <- data.frame(read_excel(data, 
                                sheet=1, 
                                range = cell_limits(ul = c(1,1), lr = c(7, 22))))

colnames(df.CPI.oil) <- c("Case", "Unit", "Type.oil", "Variable", yearsCPI)

df.CPI.oil           <- df.CPI.oil %>% gather(Year, Value, -Case, -Unit, -Type.oil, -Variable)
df.CPI.oil$Year      <- as.numeric(df.CPI.oil$Year)
levels.Var           <- c("NPS", "SDS")
labels.Var           <- c("BAU", "2DS")
df.CPI.oil$Case      <- factor(df.CPI.oil$Case, levels=levels.Var, labels=labels.Var)
df.CPI.oil              <- df.CPI.oil[order(df.CPI.oil$Case),]

############ Graph of oil import data

df.plot         <- df.CPI.oil
#unique(df.plot$Variable)
levels.Var      <- c("Volume", "Price", "Value")
labels.Var      <- c("Imported volumes, mboe", "Import price, USD/bbl", "Import cost, million USD")

df.plot$Variable <- factor(df.plot$Variable, levels=levels.Var, labels=labels.Var)
df.plot          <- df.plot[order(df.plot$Variable),]

colours <- c("#5d6d7e", "#dc7633")
#facet.labels <- list('Export Price'="Export price, USD", 'Export Volumes'="Exported volumes, mtce", 'Value'="Revenue from coal export, USD")
#facet.labels <- c("Export price, USD", "Exported volumes, mtce", "Revenue from coal export, USD")

p <- df.plot %>% ggplot(aes(x=Year, y=Value)) +
                 geom_line(aes(color=Case), size=0.8) +
  #              geom_area(aes(fill = Case, group = Case), alpha = 0.5, position = 'identity') +
                 scale_fill_manual(values = c("#aeb6bf","#dc7633")) +
  #              scale_color_brewer(palette="Paired") +
                 ylab("") +
                 scale_colour_manual(values=colours) +
                 facet_wrap(~ Variable, nrow=1, scales="free") #, labeller=as_labeller(facet.labels))
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("Oil-Import","CPI_VolPrice","BAU_2DS", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=30, height=6, units="cm", dpi=300)

setwd(dir.ANALYSIS)
