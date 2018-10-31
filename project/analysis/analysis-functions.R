
############################################
############ function for obtaining direct and indirect contributions to VA and employment per unit of final demand
######################### input : LI (one year), employees, VA, output per sector (all same year), 
######################### n is disaggregation level / number of sectors / year is year of VA,E,O
######################### output : matrices ec / va direct, indirect per sector
############################################

decomposition.direct.indirect <- function(LI, O, VAR, type, n, indList, indcodeList, year)
{
  # check if matrix and vectors correct length
  
  if (dim(LI)[1]==n & dim(LI)[2]==n & length(O)==n & length(VAR)==n)
  {
#    VAR <- df.VAR.50I.QLFS$`201406`
#    O   <- df.IOT2014$Output[1:50]
#    indList <- Industry
#    indcodeList <- SIC
#    type <- "VA"
    VAR.per.production <- VAR/O
      
    VAR.content.direct.indirect <- diag(VAR.per.production) %*% LI
    
    VAR.content.indirect <- colSums(VAR.content.direct.indirect - diag(diag(VAR.content.direct.indirect)))
    
    VAR.content.direct   <- (diag(VAR.content.direct.indirect))
    
    Order     <- seq(1:n)
    Industry  <- indList
    Code      <- indcodeList
    
    df.results                    <-  data.frame(Industry)
    df.results$code               <- Code
    df.results$order              <-  Order
    df.results$ec                 <-  colSums(VAR.content.direct.indirect)
    df.results$ec.direct          <-  VAR.content.direct
    df.results$ec.indirect        <-  VAR.content.indirect
    df.results$ec.mean            <-  mean(colSums(VAR.content.direct.indirect))
    df.results$ec.dev             <-  colSums(VAR.content.direct.indirect) - mean(colSums(VAR.content.direct.indirect))
    #df.results$ec.dev.decomp.T    <-  colSums(T.dist)
    #df.results$ec.dev.decomp.L    <-  colSums(L.dist)
    #df.results$ec.dev.decomp.N    <-  colSums(N.dist)
    #df.results$ec.dev.decomp.MF   <-  colSums(MF.dist)
    #df.results$ec.dev.decomp.MI   <-  colSums(MI.dist)
    
    #names <- c("Industry","code","order",paste(paste0(eval(parse(text = "type")),"c"),c("tot","direct","indirect","mean","dev"),sep="." ))
    names <- paste(paste0(eval(parse(text = "type")),"c"),c("tot","direct","indirect","mean","dev"),sep="." )
    
    colnames(df.results)[(dim(df.results)[2]-4):dim(df.results)[2]] <- names
  
    df.results.matrix            <- data.frame(VAR.content.direct.indirect)
    colnames(df.results.matrix)  <- Order
    rownames(df.results.matrix)  <- Order
    df.results.matrix$Industry   <- Industry
    df.results.matrix$Code       <- Code
    df.results.matrix            <- df.results.matrix[,c("Industry","Code",Order)]
    
    results <- list(df.results, df.results.matrix, year)
    
    return(results)
        
  }
  else {cat("Check dimension of input")}
}

############################################
############ function for extracting top indirect contributers
############################################

top.indirect <- function(output.list, demand, sector.nr, amount.sectors)
{
  string    <- colnames(output.list[[1]])[dim(output.list[[1]])[2]]
  VARc      <- substr(string,1,nchar(string)-4)
  VAR       <- substr(string,1,nchar(string)-5)
  yearHere  <-output.list[[3]] 
  
  df.matrix <- output.list[[2]]
  i         <- amount.sectors
  n         <- dim(df.matrix)[1]
  
  temp        <- df.matrix[,colnames(df.matrix) %in% c("Industry","Code",sector.nr)]
  temp.sorted <- temp[order(-temp[colnames(temp) %in% sector.nr]),]
  
  df.table <- temp.sorted[1:i,]
  colnames(df.table)[colnames(df.table) %in% sector.nr] <- VARc
  
  #ZZ <- diag(demand)
  #ZZZ <- df.matrix[,(dim(df.matrix)[2]-n+1):dim(df.matrix)[2]]
  
  df.VAR <- data.frame(as.matrix(df.matrix[,(dim(df.matrix)[2]-n+1):dim(df.matrix)[2]]) %*% as.matrix(diag(demand)))
  #rowSums(df.VAR) #check
  colnames(df.VAR) <- seq(1:n)
  df.VAR <- bind_cols(df.matrix[,1:2],df.VAR)
  
  temp <- df.VAR[,colnames(df.VAR) %in% c("Industry","Code",sector.nr)]
  temp.sorted <- temp[order(-temp[colnames(temp) %in% sector.nr]),]
  
  df.table.temp <- temp.sorted[1:i,]
  colnames(df.table.temp)[colnames(df.table.temp) %in% sector.nr] <- paste(VAR, yearHere, sep=", ")
  
  df.table        <- full_join(df.table, df.table.temp, by=c("Industry","Code"))
  fileName.table  <- paste(paste0("Ind", sector.nr), VAR, yearHere, sep="_")
  results         <- list(df.table, fileName.table,df.VAR)
  
  return(results)
}


add.sectorNames <- function(A)
  {
  A <- data.frame(A)
  colnames(A) <- SA.IO.codes
  rownames(A) <- SA.IO.codes
  return(A)
  }













