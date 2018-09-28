
############################################
############ Import IOT STAT SA, GDP 2018 release
############################################

fileName.list <- c("GDP_2018Q2.xlsx")

data <- paste(dir.DATA, "StatSA", fileName.list[1], sep = sep)

#Sheet 2 contains Industry value added and GDP at current prices in million Rand for the years 2010-2017

df.GDP.current <- data.frame(read_excel(data, 
                                sheet=2, 
                                range = cell_limits(ul = c(1,1), lr = c(20, 9))))

df.GDP.constant <- data.frame(read_excel(data, 
                                         sheet=3, 
                                         range = cell_limits(ul = c(1,1), lr = c(20, 9))))

df.GDP <- df.GDP.constant

legend <- paste(df.GDP[3,1], df.GDP[4,1], df.GDP[5,1], sep=", ")

colnames(df.GDP) <- c("Industry",df.GDP[5,2:9])

df.GDP <- df.GDP[7:19,]
rownames(df.GDP) <- seq(1:dim(df.GDP)[1])
#df.GDP$SIC <- c("1","2","3","4","5","6","7")
GDP.2018Q2 <- list(df.GDP,legend)

#write.csv(df.GDP,'GDP_2018Q2_sheet2.csv')

## Plot of mining sector VA as % of GDP

df.GDP.shares <- df.GDP
y <- dim(df.GDP.shares)[2]-1
for(i in 1:dim(df.GDP.shares)[1])
{
  df.GDP.shares[i,2:(y+1)] <-   df.GDP.shares[i,2:(y+1)]/  df.GDP.shares[dim(df.GDP.shares)[1],2:(y+1)] 
}

df.GDP.uses <- data.frame(read_excel(data, 
                                sheet=8, 
                                range = cell_limits(ul = c(1,1), lr = c(60, 20))))












