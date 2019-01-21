
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
df.CPI.econo$Case[c(3,4)]     <- c("2DS","2DS")
df.CPI.econo          <- df.CPI.econo[c(2,4,1,3),]
df.CPI.econo          <- bind_rows(df.CPI.econo, df.CPI.econo[1:2 +dim(df.CPI.econo)[1]-2,])
df.CPI.econo$Variable[1:2 +dim(df.CPI.econo)[1]-2] <- rep("Deflator",2)

df.CPI.econo[df.CPI.econo$Variable=="Deflator" & df.CPI.econo$Case %in% c("BAU","2DS") , colnames(df.CPI.econo) %in% yearsCPI] <- 
1/(1+  df.CPI.econo[df.CPI.econo$Variable=="Inflation" & df.CPI.econo$Case %in% c("BAU","2DS") , colnames(df.CPI.econo) %in% yearsCPI])
  
vect <- c(NA,rep(1,length(yearsCPI)-1))
for(i in 1:17)
  {
  vect[i+2] <- prod(df.CPI.econo[df.CPI.econo$Variable=="Deflator" & df.CPI.econo$Case=="BAU" , 1:i +5])
  }

df.CPI.econo[df.CPI.econo$Variable=="Deflator" & df.CPI.econo$Case=="BAU"  , 1:length(vect) + 3] <- vect
df.CPI.econo[df.CPI.econo$Variable=="Deflator" & df.CPI.econo$Case=="2DS" , 1:length(vect) + 3] <- vect


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

df.CPI$X2017 <- rep(NA,dim(df.CPI)[1])
df.CPI       <- df.CPI[,c(1,2,3,dim(df.CPI)[2],seq(4,dim(df.CPI)[2]-1))]

colnames(df.CPI)[4:dim(df.CPI)[2]] <- yearsCPI
df.CPI$Case <- factor(df.CPI$Case, levels=c("BAU", "2Deg"), labels=c("BAU", "2DS"))

df.CPI                  <- bind_rows(df.CPI,df.CPI.econo)

df.priceZAR             <- data.frame(matrix(rep(0,88),ncol=22))
colnames(df.priceZAR)   <- colnames(df.CPI)
df.priceZAR$Case        <- rep(c("BAU","2DS"),2)
df.priceZAR$Variable    <- "Export Price"
df.priceZAR$Unit        <- c(rep("ZAR/t, nominal",2), rep("ZAR/t, constant2018",2))
df.priceZAR[1,1:19+3]   <- df.CPI[df.CPI$Variable=="Export Price"  & df.CPI$Case=="BAU" & df.CPI$Unit=="USD/t",1:19+3] * 
                           df.CPI[df.CPI$Variable=="Exchange rate" & df.CPI$Case=="BAU" ,1:19+3]
df.priceZAR[2,1:19+3]   <- df.CPI[df.CPI$Variable=="Export Price"  & df.CPI$Case=="2DS" & df.CPI$Unit=="USD/t",1:19+3] * 
                           df.CPI[df.CPI$Variable=="Exchange rate" & df.CPI$Case=="2DS" ,1:19+3]
df.priceZAR[3,1:19+3]   <- df.CPI[df.CPI$Variable=="Export Price"  & df.CPI$Case=="BAU" & df.CPI$Unit=="USD/t",1:19+3] * 
                           df.CPI[df.CPI$Variable=="Exchange rate" & df.CPI$Case=="BAU" ,1:19+3] *
                           df.CPI[df.CPI$Variable=="Deflator"     & df.CPI$Case=="BAU",1:19+3]
df.priceZAR[4,1:19+3]   <- df.CPI[df.CPI$Variable=="Export Price"  & df.CPI$Case=="2DS" & df.CPI$Unit=="USD/t",1:19+3] * 
                           df.CPI[df.CPI$Variable=="Exchange rate" & df.CPI$Case=="2DS" ,1:19+3] *
                           df.CPI[df.CPI$Variable=="Deflator"     & df.CPI$Case=="2DS",1:19+3]

df.CPI                   <- bind_rows(df.CPI,df.priceZAR)

rm(df.priceZAR)

df.valExport             <- data.frame(matrix(rep(0,88),ncol=22))
colnames(df.valExport)   <- colnames(df.CPI)
df.valExport$Case        <- rep(c("BAU","2DS"),2)
df.valExport$Variable    <- "Export Value"
df.valExport$Unit        <- c(rep("ZAR m, nominal",2),rep("ZAR m, constant2018",2))
df.valExport[c(1,2),1:19+3]    <- df.CPI[df.CPI$Variable=="Export Price" & df.CPI$Unit=="ZAR/t, nominal" ,1:19+3] * 
                                  df.CPI[df.CPI$Variable=="Export Volumes" & df.CPI$Unit=="mt" ,1:19+3]
df.valExport[c(3,4),1:19+3]    <- df.CPI[df.CPI$Variable=="Export Price" & df.CPI$Unit=="ZAR/t, constant2018" ,1:19+3] * 
                                  df.CPI[df.CPI$Variable=="Export Volumes" & df.CPI$Unit=="mt" ,1:19+3]

df.CPI                  <- bind_rows(df.CPI, df.valExport)

df.CPI        <- df.CPI %>% gather(Year, Value, -Case, -Variable, -Unit)
df.CPI$Year   <- as.numeric(df.CPI$Year)

df.CPI$Var      <- df.CPI$Variable
df.CPI$Var.type <- df.CPI$Variable
df.CPI$Var      <- substr(df.CPI$Var,8,8)
df.CPI[df.CPI$Var.type=="Export Value","Var"]  <- "Val"
df.CPI[df.CPI$Var.type=="Inflation","Var"]     <- "i"
df.CPI[df.CPI$Var.type=="Deflator","Var"]      <- "d"
df.CPI$Var      <- paste(df.CPI$Var,df.CPI$Case,sep="." )
df.CPI$Market   <- "Export"
df.CPI$Var.type[df.CPI$Var.type=="Export Volumes"] <- "Volume"
df.CPI$Var.type[df.CPI$Var.type=="Export Price"] <- "Price"
df.CPI$Var.type[df.CPI$Var.type=="Export Value"] <- "Value"


df.CPI[grepl("nominal" , df.CPI$Unit)=="TRUE", "Var.type"]  <- paste(df.CPI[grepl("nominal" , df.CPI$Unit)=="TRUE", "Var.type"], "n",sep=".")
df.CPI[grepl("constant" , df.CPI$Unit)=="TRUE", "Var.type"] <- paste(df.CPI[grepl("constant" , df.CPI$Unit)=="TRUE", "Var.type"], "c",sep=".")
df.CPI[grepl("nominal" , df.CPI$Unit)=="TRUE", "Var"]       <- paste(df.CPI[grepl("nominal" , df.CPI$Unit)=="TRUE", "Var"], "n",sep=".")
df.CPI[grepl("constant" , df.CPI$Unit)=="TRUE", "Var"]      <- paste(df.CPI[grepl("constant" , df.CPI$Unit)=="TRUE", "Var"], "c",sep=".")

setwd(dir.ANALYSIS)

############ Graph of coal export data

df.plot         <- df.CPI %>% filter(df.CPI$Unit %in% c("mt","ZAR/t, nominal", "ZAR m, nominal")) %>% select(-Var, -Var.type, -Unit) %>% spread(Variable, Value)
#df.plot$"Value" <- df.plot$`Export Price` * df.plot$`Export Volumes`
df.plot         <- df.plot %>% gather(Variable, Value, -Case, -Year, -Market)
#df.plot         <- df.plot %>% filter(!df.plot$Variable=="Value")
df.plot$Case    <- factor(df.plot$Case, levels =c("BAU","2DS"))
df.plot         <- df.plot[order(df.plot$Case),]

levels.Var      <- c("Export Volumes", "Export Price", "Export Value")
labels.Var      <- c("Exported volumes, mt", "Export price, ZAR/t, nominal", "Revenue from coal export, m ZAR, nominal")

df.plot$Variable <- factor(df.plot$Variable, levels=levels.Var, labels=labels.Var)
df.plot          <- df.plot[order(df.plot$Variable),]

colours <- c("#5d6d7e", "#dc7633")
#facet.labels <- list('Export Price'="Export price, USD", 'Export Volumes'="Exported volumes, mtce", 'Value'="Revenue from coal export, USD")
#facet.labels <- c("Export price, USD", "Exported volumes, mtce", "Revenue from coal export, USD")

df.plot <- df.plot[df.plot$Year %in% yearsCPI[-1],]

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
                                         range = cell_limits(ul = c(1,1), lr = c(13, 22))))
df.CPI.domestic <- df.CPI.domestic %>% gather(Year, Value, -Case, -Plants, -Variable, -Unit)

df.CPI.domestic$Year      <- as.numeric(substr(df.CPI.domestic$Year, 2, 5))

df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal costs", "Unit"]            <- "ZAR m, nominal"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal costs", "Unit"]                 <- "ZAR m, nominal"
df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal price", "Unit"]            <- "ZAR/t, nominal"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal price", "Unit"]                 <- "ZAR/t, nominal"

df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal costs", "Variable"]        <- "Total cost, ZAR m, nominal"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal costs", "Variable"]             <- "Eskom cost, ZAR m, nominal"
df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal consumption", "Variable"]  <- "Total volume consumed, mt"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal consumption", "Variable"]       <- "Eskom volume consumed, mt"
df.CPI.domestic[df.CPI.domestic$Variable=="All plants coal price", "Variable"]        <- "Price, ZAR/t, nominal"
df.CPI.domestic[df.CPI.domestic$Variable=="Eskom coal price", "Variable"]             <- "Eskom Price, ZAR/t, nominal"


# df.CPI.domestic.test <- df.CPI.domestic %>% spread(Variable, Value)
# 
# colnames(df.CPI.domestic.test)[5] <-  "Price, ZAR/t, nominal"
# colnames(df.CPI.domestic.test)[6] <-  "Total cost, ZAR m, nominal"
# df.CPI.domestic.test$`Price, ZAR/t, constant2018` <- df.CPI.domestic.test$`Price, ZAR/t, nominal`
# df.CPI.domestic.test$`Total cost, ZAR m, constant2018` <- df.CPI.domestic.test$`Total cost, ZAR m, nominal`
# 
# df.CPI.domestic.test <- df.CPI.domestic.test %>% gather(Variable, Value, -Case, -Plants, -Unit, -Year)

df.CPI.domestic           <- df.CPI.domestic[!df.CPI.domestic$Plants=="Eskom",]
df.CPI.domestic           <- droplevels(df.CPI.domestic)

df.CPI.domestic$Variable  <- factor(df.CPI.domestic$Variable)
levelsDomestic            <- c("Total volume consumed, mt", "Price, ZAR/t, nominal", "Total cost, ZAR m, nominal")
df.CPI.domestic$Variable  <- factor(df.CPI.domestic$Variable, levels=levelsDomestic)

df.CPI.domestic           <- df.CPI.domestic %>% spread(Year, Value)
df.CPI.domestic$Case      <- factor(df.CPI.domestic$Case, levels =c("BAU","2Deg"), labels=c("BAU","2DS"))
df.CPI.domestic           <- df.CPI.domestic[order(df.CPI.domestic$Case),]

df.CPI.domestic.temp      <- df.CPI.domestic[-c(1,4),]

levels(df.CPI.domestic.temp$Variable) <- c("Total volume consumed, mt", "Price, ZAR/t, constant2018", "Total cost, ZAR m, constant2018")
df.CPI.domestic.temp$Unit             <- c("ZAR/t, constant2018", "ZAR m, constant2018", "ZAR/t, constant2018", "ZAR m, constant2018")

df.CPI.domestic.temp[df.CPI.domestic.temp$Variable=="Price, ZAR/t, constant2018"  ,1:18+4] <- df.CPI.econo[df.CPI.econo$Variable=="Deflator",1:18+4] * df.CPI.domestic[df.CPI.domestic$Variable=="Price, ZAR/t, nominal",1:18+4]
df.CPI.domestic.temp[df.CPI.domestic.temp$Variable=="Total cost, ZAR m, constant2018",1:18+4] <- df.CPI.econo[df.CPI.econo$Variable=="Deflator",1:18+4] * df.CPI.domestic[df.CPI.domestic$Variable=="Total cost, ZAR m, nominal",1:18+4]

df.CPI.domestic           <- bind_rows(df.CPI.domestic, df.CPI.domestic.temp)

rm(df.CPI.domestic.temp, df.valExport)

df.CPI.domestic$Variable  <- factor(df.CPI.domestic$Variable)
df.CPI.domestic$Case      <- factor(df.CPI.domestic$Case, levels =c("BAU","2DS"))
df.CPI.domestic           <- df.CPI.domestic[order(df.CPI.domestic$Case),]

df.plot           <- df.CPI.domestic[df.CPI.domestic$Variable %in% c("Total volume consumed, mt", "Price, ZAR/t, nominal"  ,"Total cost, ZAR m, nominal"),]
df.plot           <- droplevels(df.plot)
df.plot$Variable  <- factor(df.plot$Variable, levels=c("Total volume consumed, mt" , "Price, ZAR/t, nominal", "Total cost, ZAR m, nominal"))

df.plot <- df.plot %>% gather(Year, Value, -Case, -Plants, -Variable, -Unit)
df.plot$Year <- as.integer(df.plot$Year)

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
df.CPI.total$Var.type   <- factor(df.CPI.total$Var.type, 
                                  levels=c("Price, ZAR/t, constant2018","Price, ZAR/t, nominal","Total cost, ZAR m, constant2018","Total cost, ZAR m, nominal", "Total volume consumed, mt" ),
                                  labels=c("Price.c","Price.n","Value.c", "Value.n",  "Volume"))

# df.CPI.total$Var[!df.CPI.total$Var.type=="Value"] <- substr(df.CPI.total$Var.type[!df.CPI.total$Var.type=="Value"],1,1)
# df.CPI.total$Var[df.CPI.total$Var.type=="Value"]  <- substr(df.CPI.total$Var.type[df.CPI.total$Var.type=="Value"],1,3)
df.CPI.total$Var                                  <- paste(df.CPI.total$Var.type, df.CPI.total$Case, sep=".") 
#df.CPI.total$Var <- as.factor(df.CPI.total$Var)
df.CPI.total$Var <- factor(df.CPI.total$Var,
                              levels=c("Price.c.2DS","Price.c.BAU","Price.n.2DS","Price.n.BAU","Value.c.2DS","Value.c.BAU","Value.n.2DS","Value.n.BAU","Volume.2DS","Volume.BAU"),
                              labels=c("P.2DS.c","P.BAU.c","P.2DS.n","P.BAU.n","Val.2DS.c","Val.BAU.c","Val.2DS.n","Val.BAU.n","V.2DS","V.BAU"))

df.CPI.total <- df.CPI.total %>% gather(Year, Value, -Case, -Variable, -Unit, -Market, -Var.type, -Var)

df.CPI.total$Year <- as.integer(df.CPI.total$Year)

df.CPI.temp  <- df.CPI[,colnames(df.CPI.total)]
df.CPI.total <- bind_rows(df.CPI.total, df.CPI.temp)

setwd(dir.ANALYSIS)

############################################
############ Import CPI oil data + graph
############################################

fileName.list <- c("CPI_oil.xlsx")

i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.CPI.oil <- data.frame(read_excel(data, 
                                sheet=1, 
                                range = cell_limits(ul = c(1,1), lr = c(7, 22))))

colnames(df.CPI.oil) <- c("Case", "Unit", "Type.oil", "Variable", yearsCPI[-1])

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
