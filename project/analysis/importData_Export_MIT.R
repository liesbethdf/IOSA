
setwd(dir.ANALYSIS)

############################################
############ Import MIT data with export destinations of South African coal
############################################

fileName.list <- c("en_visualize_explore_line_sitc_export_zaf_show_3222_1962.2016.csv", "countryList.csv")

i <- 1
data <- paste(dir.DATA, "MIT",fileName.list[i], sep = sep)
df.MIT.export <- data.frame(read.csv(data))

i <- 2
data <- paste(dir.DATA, "MIT",fileName.list[i], sep = sep)

country.codes <- c("AFG", "AGO", "ALB", "ANT", "ARE", "ARG", "ATG", "AUS", "AUT", "BEL", "BEN", "BGD", "BGR", "BHR", "BIH", "BRA", "BRN", "BWA", "CAN", "CHE", "CHL", "CHN", "COD", "COG", "COM",
                   "CRI", "CUB", "CYP", "CZE", "DEU", "DJI", "DNK", "DZA", "EGY", "ERI", "ESP", "ETH", "FIN", "FRA", "GAB", "GBR", "GEO", "GHA", "GIB", "GIN", "GMB", "GNQ", "GRC", "GUY", "HKG",
                   "HND", "HRV", "IDN", "IND", "IRL", "IRN", "ISL", "ISR", "ITA", "JOR", "JPN", "KEN", "KOR", "KWT", "LAO", "LBN", "LBR", "LKA", "LSO", "LUX", "MAC", "MAR", "MDA", "MDG", "MDV",
                   "MEX", "MLI", "MLT", "MMR", "MNP", "MOZ", "MRT", "MUS", "MWI", "MYS", "MYT", "NAM", "NCL", "NGA", "NIC", "NLD", "NOR", "NZL", "OMN", "PAK", "PAN", "PHL", "PNG", "POL", "PRK",
                   "PRT", "QAT", "REU", "ROU", "RUS", "SAU", "SCG", "SEN", "SGP", "SHN", "SLE", "SRB", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TGO", "THA", "TKL", "TUN", "TUR", "TZA", "UGA",
                   "UKR", "URY", "USA", "UZB", "VEN", "VGB", "VNM", "WLD", "XXA", "XXB", "YEM", "ZAF", "ZMB", "ZWE")


countryCodeList <- as.vector(read.csv(data, header=FALSE, as.is = c(1)))
codeList        <- substring(countryCodeList$V1,1,3)
countryList     <- substring(countryCodeList$V1,5,100)

indices <- match(country.codes,codeList)
NAs     <- which(is.na(indices)==TRUE)
indices.notNA <- indices[-NAs]

codes.temp      <- codeList[indices.notNA]
countries.temp  <- countryList[indices.notNA]

missing.codes     <- country.codes[NAs]
missing.countries <- c("Dutch Antilles", "Serbia and Montenegro", "Rest of the World" ,"Rest of the WorldA" ,"Rest of the WorldB")

codes.all         <- c(codes.temp,missing.codes)
countries.all     <- c(countries.temp,missing.countries)
#countries.all[23] <- "Democratic Republic of the Congo"

order <- match(country.codes,codes.all)

codes.all <- codes.all[order]
countries.all <- countries.all[order]

df.MIT.export$country_names <- df.MIT.export$country_destination_id
df.MIT.export$country_names <- factor(df.MIT.export$country_names, levels = codes.all, labels = countries.all)

europe.list <-  c( "Albania", "Austria", "Belgium", "Bulgaria", "Bosnia and Herzegovina", "Switzerland", "Germany", "Cyprus", 
                   "Czech Republic", "Denmark", "Spain", "Finland", "France", "United Kingdom", "Georgia", "Gibraltar", "Greece", "Croatia", "Ireland",
                   "Iceland", "Italy", "Luxembourg", "Malta", "Moldova", "Netherlands", "Norway", "Mayotte", "Poland", "Portugal","Romania",
                   "Serbia and Montenegro", "Réunion", "Serbia", "Slovakia", "Slovenia", "Sweden",  "Ukraine" ,  " British  Virgin Islands" ,  
                   "Russian Federation", "Rest of the World", "Rest of the WorldA", "Rest of the WorldB")

africa.list <-c("Afghanistan", "Angola",  "Benin","Botswana" , " Democratic Republic of the Congo", "Congo", "Comoros", "Djibouti", "Algeria","Egypt" ,
                "Eritrea", "Ethiopia", "Gabon","Ghana", "Guinea",  "Gambia", "Equatorial Guinea", "Kenya", "Liberia", "Lesotho", "Morocco", "Mali",
                "Madagascar", "Mozambique", "Mauritania", "Malawi", "Mauritius", "Namibia", "Nigeria","Senegal", "Sierra Leone","Swaziland"  , "Seychelles",
                "Tanzania","Togo" ,"Tunisia", "Uganda"    , "South Africa", "Zambia", "Zimbabwe"     ) 

northamerica.list <- c("Canada", "United States of America")

latinamerica.list <- c("Argentina", "Brazil", "Cuba", "Dutch Antilles", "Antigua and Barbuda", "Chile", "Costa Rica", "Guyana", "Honduras", "Mexico",  
                       "Nicaragua","Panama",  "Saint Helena", "Uruguay" ,"Venezuela"     )

asia.list  <- c( "Bangladesh"   ,"China", "Brunei Darussalam", "Hong Kong", "India", "Japan", "Korea", "Lao People's Democratic Republic",  "Sri Lanka",
                 "Macao",   "Maldives", "Pakistan","Philippines", " Democratic People's Republic of Korea" , "Uzbekistan"   )

southeastasia.list  <- c("Indonesia", "Myanmar", "Malaysia",    "Viet Nam", "Singapore", "Thailand")   
middleeast.list <- c( "United Arab Emirates", "Bahrain", "Iran", "Israel", "Jordan","Kuwait", "Lebanon", "Oman", "Yemen", "Qatar","Saudi Arabia",
                      "Syrian Arab Republic", "Turkey"   )

oceania.list <-   c("Australia" ,    "Northern Mariana Islands",   "New Zealand", "New Caledonia","Papua New Guinea","Tokelau"   )                             
                                                                                                          
                                      


