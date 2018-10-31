
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