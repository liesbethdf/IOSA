
# General configuration

source("config.R")
source("dir.R")
source("general-functions.R")
source("analysis-functions.R")

# Importing data

source("importData.R")
source("importData_CPI.R")
source("importData_OECD_IOT.R")
source("importData_Export_MIT.R")

# Fixing the model parameters

source("Domestic_Leontief.R")
source("modelParameters.R")

# Demand scenarios

source("demandScenarios_v2.R")

# Analysis

source("employmentContent.R")
#source("demandDecomposition-function-old.R")       # 2014 prices = one and constant
source("demandDecomposition-function.R") # not 2014 prices but coal in million tons
source("employmentContent_3parts.R")
source("demandDecompostion_coalExportDomestic.R")
source("demandDecompostion_coalExportDomestic_Graphs.R") 
source("impactonGDP.R") # Check : Error in LI.d %*% diag(demand.diff) : non-conformable arguments
source("Oil.R")


# Below not used

#source("sector-description.R")
#source("plots.R")
#source("scenarios.R")