
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

#source("demandScenarios.R")
source("demandScenarios_v2.R")

# Analysis

source("employmentContent.R")
#source("finaldemandDecomposition-function.R")       # 2014 prices = one and constant
source("finaldemandDecomposition-function-quant.R") # not 2014 prices but coal in million tons
source("employmentContent_3parts.R")
source("employmentCoal_ExportDomestic.R")
source("impactonGDP.R") # Check : Error in LI.d %*% diag(demand.diff) : non-conformable arguments
source("Oil.R")

# Generating graphs

#source("demandDecomposition_Stats.R")
source("employmentDecomposition_Graphs.R") 
#source("demandDecomposition_Graphs.R")     # Verification needed


# Below not used

#source("sector-description.R")
#source("plots.R")
#source("scenarios.R")