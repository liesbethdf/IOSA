
sep = .Platform$file.sep

dir.PROJECT   <- normalizePath("..")
dir.ANALYSIS  <- paste(dir.PROJECT, "analysis", sep=sep) 
dir.DATA      <- paste(dir.PROJECT, "data", sep=sep)
dir.PLOTS     <- paste(dir.PROJECT, "graphs", sep=sep)
dir.TABLES    <- paste(dir.PROJECT, "tables", sep=sep)
dir.DESCRIPT  <- paste(dir.TABLES, "SectorDescription", sep=sep)

dir.create(dir.PLOTS, recursive=TRUE, showWarnings = FALSE)
dir.create(dir.TABLES, recursive=TRUE, showWarnings = FALSE)
dir.create(dir.DESCRIPT, recursive=TRUE, showWarnings = FALSE)
