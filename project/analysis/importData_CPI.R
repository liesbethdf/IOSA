
############################################
############ Import CPI coal data
############################################

fileName.list <- c("CPI_coal_power.xlsx")

############ Coal export
i <- 1
data <- paste(dir.DATA, fileName.list[i], sep = sep)

df.CPI <- data.frame(read_excel(data, 
                                    sheet=1, 
                                    range = cell_limits(ul = c(1,1), lr = c(7, 21))))

yearsCPI <- colnames(df.CPI)[4:21]
yearsCPI <- as.numeric(substr(yearsCPI, 2, 5))
colnames(df.CPI)[4:21] <- yearsCPI
df.CPI <- df.CPI %>% gather(Year, Value, -Case, -Variable, -Unit)
df.CPI$Year <- as.numeric(df.CPI$Year)
#df.CPI$Case <- as.factor(df.CPI$Case)

df.CPI$Var <- df.CPI$Variable
df.CPI$Var <- substr(df.CPI$Var,8,8)
#df.CPI$Var[df.CPI$Var=="V"] <-"Vol"
df.CPI$Var <- paste(df.CPI$Var,df.CPI$Case,sep="." )
  
setwd(dir.ANALYSIS)

############ Graph of coal export data

df.plot         <- df.CPI %>% filter(!df.CPI$Unit=="mt") %>% select(-Var, -Unit) %>% spread(Variable, Value)
df.plot$"Value" <- df.plot$`Export Price` * df.plot$`Export Volumes`
df.plot         <- df.plot %>% gather(Variable, Value, -Case, -Year)
#df.plot         <- df.plot %>% filter(!df.plot$Variable=="Value")
df.plot$Case    <- factor(df.plot$Case, levels =c("BAU","2Deg"))
df.plot          <- df.plot[order(df.plot$Case),]

colours <- 

p <- df.plot %>% ggplot(aes(x=Year, y=Value)) +
#                 geom_line(aes(color=Case, linetype=Variable)) +
                 geom_line(aes(color=Case)) +
                 scale_color_brewer(palette="Paired") +
                facet_wrap(~ Variable, nrow=1, scales="free")
#                 scale_colour_manual(values=colours)
print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("Coal-Export","CPI_VolPrice","BAU_2Deg", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=30, height=6, units="cm", dpi=300)








