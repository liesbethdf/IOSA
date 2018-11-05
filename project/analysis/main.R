
# General configuration

source("config.R")
source("dir.R")
source("general-functions.R")
source("analysis-functions.R")

# Importing data

source("importData.R")
source("importData_CPI.R")
source("importData_OECD_IOT.R")

# Fixing the model parameters

source("Domestic_Leontief.R")
source("modelParameters.R")

# Demand scenarios

source("demandScenarios.R")

# Analysis

source("employmentContent_DomImp.R")
source("finaldemandDecomposition.R")

# Generating graphs

source("demandDecomposition_Graphs.R")
source("employmentDecomposition_Graphs.R")


# Below not sure if useful

#source("employmentContent.R") # Archived, not relevant anymore ; was based on not knowing IOT dom and imp

source("sector-description.R")
source("plots.R")
source("scenarios.R")