
list.NAcolumns <- function(df)
{ 
  na.columns <- NA
  
  for (i in 1:dim(df)[2])
  {
    if (all(is.na(df[,i]))==TRUE)
    {
      na.columns <- c(na.columns,colnames(df)[i])
    }
  }
  if (length(na.columns) > 1){na.columns <- na.columns[-1]}
  
  return(na.columns)
} 


qualifiers <- function(df)
{ 
  qualifiers.names <- "none"
  qualifiers.values <- "none"
  
  for (i in 1:dim(df)[2])
#  for (i in 1:4)
  {
    if (length(levels(df[,i]))==1)
    {
      qualifiers.values <- c(qualifiers.values,levels(df[,i]))
      qualifiers.names  <- c(qualifiers.names,colnames(df)[i])
    }
  }
  if (length(qualifiers.values) > 1){qualifiers.values <- qualifiers.values[-1]}
  if (length(qualifiers.names) > 1){qualifiers.names <- qualifiers.names[-1]}
  
  qualifiers.values <- data.frame(t(qualifiers.values))
  colnames(qualifiers.values) <- qualifiers.names
  
  return(qualifiers.values)
} 