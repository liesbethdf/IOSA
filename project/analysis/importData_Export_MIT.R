
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
countryCodeList <- as.vector(read.csv(data, header=FALSE, as.is = c(1)))
codeList        <- substring(countryCodeList$V1,1,3)
countryList     <- substring(countryCodeList$V1,5,100)

country.codes <-  levels(df.MIT.export$country_destination_id)
                  #c("AFG", "AGO", "ALB", "ANT", "ARE", "ARG", "ATG", "AUS", "AUT", "BEL", "BEN", "BGD", "BGR", "BHR", "BIH", "BRA", "BRN", "BWA", "CAN", "CHE", "CHL", "CHN", "COD", "COG", "COM",
                  #  "CRI", "CUB", "CYP", "CZE", "DEU", "DJI", "DNK", "DZA", "EGY", "ERI", "ESP", "ETH", "FIN", "FRA", "GAB", "GBR", "GEO", "GHA", "GIB", "GIN", "GMB", "GNQ", "GRC", "GUY", "HKG",
                  #  "HND", "HRV", "IDN", "IND", "IRL", "IRN", "ISL", "ISR", "ITA", "JOR", "JPN", "KEN", "KOR", "KWT", "LAO", "LBN", "LBR", "LKA", "LSO", "LUX", "MAC", "MAR", "MDA", "MDG", "MDV",
                  #  "MEX", "MLI", "MLT", "MMR", "MNP", "MOZ", "MRT", "MUS", "MWI", "MYS", "MYT", "NAM", "NCL", "NGA", "NIC", "NLD", "NOR", "NZL", "OMN", "PAK", "PAN", "PHL", "PNG", "POL", "PRK",
                  #  "PRT", "QAT", "REU", "ROU", "RUS", "SAU", "SCG", "SEN", "SGP", "SHN", "SLE", "SRB", "SVK", "SVN", "SWE", "SWZ", "SYC", "SYR", "TGO", "THA", "TKL", "TUN", "TUR", "TZA", "UGA",
                  #  "UKR", "URY", "USA", "UZB", "VEN", "VGB", "VNM", "WLD", "XXA", "XXB", "YEM", "ZAF", "ZMB", "ZWE")

indices <- match(country.codes,codeList)
NAs     <- which(is.na(indices)==TRUE)
indices.notNA <- indices[-NAs]

codes.temp      <- codeList[indices.notNA]
countries.temp  <- countryList[indices.notNA]

missing.codes     <- country.codes[NAs]
missing.countries <- c("Dutch Antilles", "Serbia and Montenegro", "Rest of the World" ,"XXA" ,"XXB")

codes.all         <- c(codes.temp,missing.codes)
countries.all     <- c(countries.temp,missing.countries)
#countries.all[23] <- "Democratic Republic of the Congo"

order <- match(country.codes,codes.all)

codes.all <- codes.all[order]
countries.all <- countries.all[order]

df.MIT.export$country_names <- df.MIT.export$country_destination_id
df.MIT.export$country_names <- factor(df.MIT.export$country_names, levels = codes.all, labels = countries.all)

# "Rest of the WorldA", "Rest of the WorldB"

XXA.list <- "XXA"
XXB.list <- "XXB"
WLD.list <- "Rest of the World"

europe.list <-  c( "Albania", "Austria", "Belgium", "Bulgaria", "Bosnia and Herzegovina", "Switzerland", "Germany", "Cyprus", 
                   "Czech Republic", "Denmark", "Spain", "Finland", "France", "United Kingdom", "Georgia", "Gibraltar", "Greece", "Croatia", "Ireland",
                   "Iceland", "Italy", "Luxembourg", "Malta", "Moldova", "Netherlands", "Norway", "Mayotte", "Poland", "Portugal","Romania",
                   "Serbia and Montenegro", "Réunion", "Serbia", "Slovakia", "Slovenia", "Sweden",  "Ukraine" ,  " British  Virgin Islands")

africa.list <-c("Afghanistan", "Angola",  "Benin","Botswana" , " Democratic Republic of the Congo", "Congo", "Comoros", "Djibouti", "Algeria","Egypt" ,
                "Eritrea", "Ethiopia", "Gabon","Ghana", "Guinea",  "Gambia", "Equatorial Guinea", "Kenya", "Liberia", "Lesotho", "Morocco", "Mali",
                "Madagascar", "Mozambique", "Mauritania", "Malawi", "Mauritius", "Namibia", "Nigeria","Senegal", "Sierra Leone","Swaziland"  , "Seychelles",
                "Tanzania","Togo" ,"Tunisia", "Uganda"    , "South Africa", "Zambia", "Zimbabwe") 

northamerica.list <- c("Canada", "United States of America")

latinamerica.list <- c("Argentina", "Brazil", "Cuba", "Dutch Antilles", "Antigua and Barbuda", "Chile", "Costa Rica", "Guyana", "Honduras", "Mexico",  
                       "Nicaragua","Panama",  "Saint Helena", "Uruguay" ,"Venezuela")

china.list  <- c("China")

india.list  <- c("India")

asia.list  <- c( "Bangladesh", "Brunei Darussalam", "Hong Kong", "Japan", "Korea", "Lao People's Democratic Republic",  "Sri Lanka",
                 "Macao",   "Maldives", "Pakistan","Philippines","Russian Federation", " Democratic People's Republic of Korea" , "Uzbekistan")

southeastasia.list  <- c("Indonesia", "Myanmar", "Malaysia", "Viet Nam", "Singapore", "Thailand")   
middleeast.list <- c( "United Arab Emirates", "Bahrain", "Iran", "Israel", "Jordan","Kuwait", "Lebanon", "Oman", "Yemen", "Qatar","Saudi Arabia",
                      "Syrian Arab Republic", "Turkey" )

oceania.list <-   c("Australia", "Northern Mariana Islands", "New Zealand", "New Caledonia", "Papua New Guinea", "Tokelau")                             
    
df.MIT.export$regions <- df.MIT.export$country_names

#length(europe.list) + length(africa.list) + length(northamerica.list) + length(latinamerica.list) + length(asia.list) + 
#length(southeastasia.list) + length(middleeast.list) + length(oceania.list)

levels(df.MIT.export$regions) <- list(Europe=europe.list, Africa=africa.list, NorthAmerica=northamerica.list, LatinAmerica=latinamerica.list,
                                      Asia=asia.list, China=china.list, India=india.list, SouthEastAsia=southeastasia.list, MiddleEast=middleeast.list, 
                                      Oceania=oceania.list, XXA=XXA.list, XXB=XXB.list, RestoftheWorld=WLD.list)

levels.regions <- rev(c("RestoftheWorld", "Europe", "Asia", "China", "India", "SouthEastAsia", "Africa", "MiddleEast", "NorthAmerica", "LatinAmerica", "Oceania", "XXA", "XXB"))
labels.regions <- rev(c("Unclassified", "Europe", "Asia", "China", "India", "Southeast Asia", "Africa", "Middle East", "North America", "Latin America",  "Oceania", "Stateless person", "Refugees"))

df.MIT.export$regions <- factor(df.MIT.export$regions, levels=levels.regions, labels=labels.regions)

# There is an error in Rets of the World, huge value for 2015. WLD is eliminated for 2015

df.plot <- df.MIT.export[!(df.MIT.export$year=="2015" & df.MIT.export$country_destination_id=="WLD"),]

df.plot$export_val <- df.plot$export_val / 10^6

colours <- c("#a04000", "#455a64", "#5dade2", "#3f51b5", "#80deea", "#9ccc65", "#009688", "#fdd835","#f1948a", "#c0392b", "#e74c3c", "#0277bd", "#808b96") #"#a04000")

p <- df.plot %>% ggplot(aes(x=year, y=export_val, fill=regions)) +
                       geom_bar(stat="identity") +
                       #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                       labs(x = "Year", y="million US$ (nominal)") +
                       scale_x_discrete(limits = (seq(1:21)*2)+1974) +
  #scale_y_continuous(limits = c(0, 100000)) +
                       scale_fill_manual(values=colours) +
                       labs(fill="Importing region :") 

print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ExportDestinations","Coal","Value", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=36, height=11, units="cm", dpi=300)

setwd(dir.ANALYSIS)

############################################
############ Add exports in tons for 2007 - 2017 from Facts and figures 2017, Minerals Council South Africa
############################################
yearHere <- "2008"

coal.export.MC                <- data.frame(as.matrix(as.integer(c( 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)), ncol=1))
colnames(coal.export.MC)      <- "year"
coal.export.MC$Mtons          <- c(67.675, 60.631, 60.539, 66.770, 68.807, 76.009, 74.566, 75.823, 75.376, 68.905, 70.049)
scaler                        <- sum(df.plot[df.plot$year==yearHere, "export_val"]) / coal.export.MC[coal.export.MC$year==yearHere,"Mtons"]
coal.export.MC$Mtons.plot     <- scaler* coal.export.MC$Mtons

p <-              ggplot(data=df.plot) +
                  geom_bar(aes(year, y=export_val, fill=regions), stat="identity") +
                  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  labs(x = "Year", y="million US$ (nominal)") +
                  scale_x_discrete(limits = (seq(1:21)*2)+1974) +
                  scale_y_continuous(breaks=seq(1,8)*10^3, 
                                     sec.axis = sec_axis(~./scaler, name ="million tons", breaks=c(0,20,40,60,65,70,75))) +
                  scale_fill_manual(values=colours) +
                  labs(fill="Importing region :") +
                  geom_point(data=coal.export.MC, aes(year, y=Mtons.plot)) +
                  geom_line(data=coal.export.MC, aes(year, y=Mtons.plot))
                  #sec_axis(trans=~./scaler, 
                   #        name = "million tons")#, breaks = waiver(), labels = waiver())

print(p)

setwd(dir.PLOTS)
fileName.graph <- paste("ExportDestinations","Coal","Value_tons", sep="_")
ggsave(filename = paste(fileName.graph, "pdf", sep="."), width=36, height=11, units="cm", dpi=300)

setwd(dir.ANALYSIS)

############################################
############ South African Coal reserves from BP Statistical Review
############################################
#end of the year
yearsHere <- c(2005, 2006,    2010  ,2012,2013,2014,      2016, 2017)

reserves c(48750, 48000,     30156     ,30156, 30156, 30156         9893, 9893)




