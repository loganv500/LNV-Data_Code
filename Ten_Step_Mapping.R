setwd("/Users/loganvidal/Desktop/Bayesian Work")


library(foreign)
library(ggplot2)
library(coefplot)
library(ggthemes)
library(gridExtra)
library(MASS)
library(lattice)
library(plyr)
library(randtoolbox)
library(reshape2)
library(apsrtable)
library(Matrix)
library(lme4)
library(R2WinBUGS)
library(R2jags)
library(R.utils)
library(MCMCpack)
library(coefplot)
library(scales)
library(timeSeries)
library(forecast)
library(TTR)
library(ggthemes)
library(gridExtra)
library(CARMAgeddon)
library(dplyr)
library(GGally)
library(vcd)
library(extracat)
library(caret)
library(pROC)
library(rpart)
library(partykit)
library(C50)
library(kernlab)
library(AppliedPredictiveModeling)
library(earth)
library(mda)
library(nnet)
library(scales)
library(tmap)
library(leaflet)

nhdata <- read.csv("nhdem.csv")

View(nhdata)

# creating shit to plot 
nhdata$SandersMarginVotes <- nhdata$Sanders - nhdata$Clinton
nhdata$SandersPct <- (nhdata$Sanders - nhdata$Clinton) / (nhdata$Sanders + nhdata$Clinton) # Will use formatting later to multiply by 100
nhdata$ClintonPct <- (nhdata$Clinton - nhdata$Sanders) / (nhdata$Sanders + nhdata$Clinton)
nhdata$SandersMarginPctgPoints <- nhdata$SandersPct - nhdata$ClintonPct

nhdata$SandersTPVS <- (nhdata$Sanders) / (nhdata$Sanders + nhdata$Clinton) # Will use formatting later to multiply by 100
nhdata$ClintonTPVS <- (nhdata$Clinton) / (nhdata$Sanders + nhdata$Clinton)

# Entering the shapefile, from the Census downloaded county shape files from the Census. 
# This county level data is from a shape file in R

usshapefile <- "cb_2014_us_county_5m/cb_2014_us_county_5m.shp"

usgeo <- read_shape(file=usshapefile)

# A check on whether it read the right map and read it correctly:
qtm(usgeo) # swag 

str(usgeo@data)

# Code for pulling put New Hampshire from the US county Geogrpahy shapefiles
nhgeo <- usgeo[usgeo@data$STATEFP=="33",]

# does it look right
qtm(nhgeo) # yes 

# does the shapefle have the same structure as the NH election data we've inputted:
# Shapefile structure
str(nhgeo@data$NAME)

# Election data structure 
nhdata$County <- nhdata$COUNTIES # rename counties to county for merging and matching
View(nhdata)

str(nhdata$County)

# This code turns counties to strings from factors:
nhgeo@data$NAME <- as.character(nhgeo@data$NAME)

# This code turns counties to strings from factors:
nhdata$County <- as.character(nhdata$County)

# order the data by county alphabetically 
nhgeo <- nhgeo[order(nhgeo@data$NAME),]
nhdata <- nhdata[order(nhdata$County),]

identical(nhgeo@data$NAME,nhdata$County) # this needs to be true to continue:

nhmap <- append_data(nhgeo, nhdata, key.shp = "NAME", key.data="County")

str(nhmap@data)

# Sanders county level margin maps
qtm(nhmap, "SandersMarginVotes")

# Sanders county level percentage maps
qtm(nhmap, "SandersMarginPctgPoints")


# More specified map:

tm_shape(nhmap) +
  tm_fill("SandersMarginVotes", title="Sanders Margin, Total Votes", palette = "PRGn") +
  tm_borders(alpha=.5) +
  tm_text("County", size=0.8)

# More mapping code:

tm_shape(nhmap) +
  tm_fill("SandersMarginVotes", title="Sanders Margin, Total Votes", palette = "PRGn") +
  tm_borders(alpha=.5) +
  tm_text("County", size=0.8) + 
  tm_style_classic()

nhstaticmap <- tm_shape(nhmap) +
  tm_fill("SandersMarginVotes", title="Sanders Margin, Total Votes", palette = "PRGn") +
  tm_borders(alpha=.5) +
  tm_text("County", size=0.8)
save_tmap(nhstaticmap, filename="nhdemprimary.jpg")


# Intereactive maps with Leaflet

clintonPalette <- colorNumeric(palette = "Blues", domain=nhmap$ClintonPct)


nhpopup <- paste0("County: ", nhmap@data$County,
                  "Sanders ", percent(nhmap@data$SandersPct), " - Clinton ", percent(nhmap@data$ClintonPct))

# making the map interactive:

NHPrimary <- leaflet(nhmap) %>% # creates a leaflet map project, mapped to the nhmap dataset
  addProviderTiles("CartoDB.Positron") %>% # sets the background map tiles to CartoDB's attractive Positron design.
  addPolygons(stroke=FALSE, # no border around the counties 
              smoothFactor = 0.2,
              fillOpacity = .8, # color opacity 
              popup=nhpopup,
              color= ~clintonPalette(nhmap@data$ClintonPct)
  )

# Saving that map in html for the blog:

install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(widget=NHPrimary, file="NHprimary.html")


# South Carolina Maps:

scdata <- read.csv("SC_returns.csv")
View(scdata)

# creating shit to plot 
# change the data types
scdata$Trump <- as.numeric(scdata$Trump)
scdata$Cruz <- as.numeric(scdata$Cruz)
scdata$Rubio <- as.numeric(scdata$Rubio)
scdata$Bush <- as.numeric(scdata$Bush)
scdata$Kasich <- as.numeric(scdata$Kasich)
scdata$Carson <- as.numeric(scdata$Carson)
scdata$Total <- as.numeric(scdata$Total)


scdata$TrumpVS <- (scdata$Trump) / (scdata$Total) # Will use formatting later to multiply by 100

scdata$CruzVS <- (scdata$Cruz) / (scdata$Total) # Will use formatting later to multiply by 100

scdata$RubioVS <- (scdata$Rubio) / (scdata$Total) # Will use formatting later to multiply by 100

# Get the shape file ready 


usshapefile <- "cb_2014_us_county_5m/cb_2014_us_county_5m.shp"

usgeo <- read_shape(file=usshapefile)

# A check on whether it read the right map and read it correctly:
qtm(usgeo) # swag 

str(usgeo@data)

# Code for pulling put New Hampshire from the US county Geogrpahy shapefiles
scgeo <- usgeo[usgeo@data$STATEFP=="45",]

# does it look right
qtm(scgeo) # yes 

# does the shapefle have the same structure as the NH election data we've inputted:
# Shapefile structure
str(scgeo@data$NAME)

# Election data structure 

str(scdata$County)

# This code turns counties to strings from factors:
scgeo@data$NAME <- as.character(scgeo@data$NAME)

# This code turns counties to strings from factors:
scdata$County <- as.character(scdata$County)

# order the data by county alphabetically 
scgeo <- scgeo[order(scgeo@data$NAME),]
scdata <- scdata[order(scdata$County),]

identical(scgeo@data$NAME,scdata$County) # this needs to be true to continue:

scmap <- append_data(scgeo, scdata, key.shp = "NAME", key.data="County")


# weird thing they do for the color palettes

minpct <- min(c(scmap$TrumpVS, scmap$RubioVS , scmap$CruzVS))
maxpct <- max(c(scmap$TrumpVS, scmap$RubioVS , scmap$CruzVS))

trumpPalette <- colorNumeric(palette = "Purples", domain=c(minpct, maxpct))
rubioPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))
cruzPalette <- colorNumeric(palette = "Oranges", domain = c(minpct, maxpct))
winnerPalette <- colorFactor(palette=c("#984ea3", "#e41a1c"), domain = scmap$Winner)

scpopup <- paste0("County: ", scmap@data$County,
                  "Winner: ", scmap@data$Winner,
                  "Trump: ", percent(scmap@data$TrumpVS),
                  "Rubio: ", percent(scmap@data$RubioVS),
                  "Cruz: ", percent(scmap@data$CruzVS))

leaflet(scmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=scpopup, 
              color= ~winnerPalette(scmap@data$Winner),
              group="Winners"
  ) %>%
  addLegend(position="bottomleft", colors=c("#984ea3", "#e41a1c"), labels=c("Trump", "Rubio"))

# Add multiple layers to teh same plot

scGOPmap <- leaflet(scmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=scpopup, 
              color= ~winnerPalette(scmap@data$Winner),
              group="Winners"
  ) %>% 
  addLegend(position="bottomleft", colors=c("#984ea3", "#e41a1c"), labels=c("Trump", "Rubio"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~trumpPalette(scmap@data$TrumpVS),
              group="Trump"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~rubioPalette(scmap@data$RubioVS),
              group="Rubio"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~cruzPalette(scmap@data$CruzVS),
              group="Cruz"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Winners", "Trump", "Rubio", "Cruz"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

scGOPmap

saveWidget(widget=scGOPmap, file="scGOPprimary.html")



# More Mapping:

educ <- read.csv("County_Educ.csv")
View(educ)

usshapefile <- "cb_2014_us_county_5m/cb_2014_us_county_5m.shp"

usgeo <- read_shape(file=usshapefile)

# A check on whether it read the right map and read it correctly:
qtm(usgeo) # swag 

str(usgeo@data)

str(educ$County)

# This code turns counties to strings from factors:
usgeo@data$NAME <- as.character(usgeo@data$NAME)

# This code turns counties to strings from factors:
educ$County <- as.character(educ$County)

# order the data by county alphabetically 
usgeo <- usgeo[order(usgeo@data$NAME),]
educ <- educ[order(educ$County),]

identical(usgeo@data$NAME,educ$County) # this needs to be true to continue:

usmap <- append_data(usgeo, educ, key.shp = "NAME", key.data="County", ignore.duplicates=TRUE)

qtm(usmap, "BD")

View(educ)
BachPalette <- colorNumeric(palette = "Purples", domain=c(0, 1))
HSPalette <- colorNumeric(palette = "Reds", domain = c(0, .75))
SHSPalette <- colorNumeric(palette = "Oranges", domain = c(0, .25))

summary(educ$SHSpct)
is.numeric(educ$SHSpct)

#####
uspopup <- paste0("County: ", usmap@data$County,
                  "Bachelors: ", percent(usmap@data$BDpct),
                  "High School: ", percent(usmap@data$HSpct),
                  "Some High School: ", percent(usmap@data$SHSpct))


usmapex <-   leaflet(usmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=uspopup, 
              color= ~BachPalette(usmap@data$BDpct),
              group="Bachelors"
  ) %>%
  
addPolygons(stroke=TRUE,
            weight=1,
            smoothFactor = 0.2, 
            fillOpacity = .75, 
            popup=scpopup, 
            color= ~HSPalette(usmap@data$HSpct),
            group="High School"
) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=scpopup, 
              color= ~cruzPalette(usmap@data$SHSpct),
              group="Some High School"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Bachelors", "High School", "Some High School"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 


saveWidget(widget=usmapex, file="usedu.html")


# Michigan electoral Results:

midata <- read.csv("Rep_Mich.csv")
View(midata)

# creating shit to plot 
midata$TrumpPct <- (midata$Trump) / (midata$Total) # Will use formatting later to multiply by 100
midata$KasichPct <- (midata$Kasich) / (midata$Total)
midata$CruzPct <- (midata$Cruz) / (midata$Total) 
midata$RubioPct <- (midata$Rubio) / (midata$Total) 

midata$TrumpPct <- as.numeric(midata$TrumpPct)
midata$KasichPct <- as.numeric(midata$KasichPct)
midata$CruzPct <- as.numeric(midata$CruzPct)
midata$RubioPct <- as.numeric(midata$RubioPct)





# Code for pulling put New Hampshire from the US county Geogrpahy shapefiles
migeo <- usgeo[usgeo@data$STATEFP=="26",]

# does it look right
qtm(migeo) # yes 

# does the shapefle have the same structure as the NH election data we've inputted:
# Shapefile structure
str(migeo@data$NAME)

write.csv(migeo@data$NAME, file="geo.csv")
# code to make hte counties match by putting them into a csv and coppying them over

list(migeo@data$NAME)
list(midata$County)

# Election data structure 
View(midata)

str(midata$County)

# This code turns counties to strings from factors:
migeo@data$NAME <- as.character(migeo@data$NAME)

# This code turns counties to strings from factors:
midata$County <- as.character(midata$County)

# order the data by county alphabetically 
migeo <- migeo[order(migeo@data$NAME),]
midata <- midata[order(midata$County),]

identical(migeo@data$NAME,midata$County) # this needs to be true to continue:

mimap <- append_data(migeo, midata, key.shp = "NAME", key.data="County")

qtm(mimap, "TrumpPct", fill="TrumpPct")

jpeg(file="TrumpPct.jpg", quality=100, width=1024, height=768)
qtm(mimap, "TrumpPct", title="Trump Percentage of the Vote")
dev.off()

#Interactive Michigan Map 

minpct <- min()

minpct <- min(c(mimap$TrumpPct, mimap$CruzPct, mimap$KasichPct, mimap$RubioPct))
maxpct <- max(c(mimap$TrumpPct, mimap$CruzPct, mimap$KasichPct, mimap$RubioPct))

KasichPalette <- colorNumeric(palette = "Blues", domain=c(minpct, maxpct))
RubioPalette <- colorNumeric(palette = "Purples", domain = c(minpct, maxpct))
TrumpPalette <- colorNumeric(palette = "Greens", domain=c(minpct, maxpct))
CruzPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))



#####
mipopup <- paste0("County: ", mimap@data$County,
                  "Trump: ", percent(mimap@data$TrumpPct),
                  "Kasich: ", percent(mimap@data$KasichPct),
                  "Cruz: ", percent(mimap@data$CruzPct),
                  "Rubio: ", percent(mimap@data$RubioPct))


mimapex <-   leaflet(mimap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=mipopup, 
              color= ~TrumpPalette(mimap@data$TrumpPct),
              group="Trump"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mipopup, 
              color= ~KasichPalette(mimap@data$KasichPct),
              group="Kasich"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mipopup, 
              color= ~CruzPalette(mimap@data$CruzPct),
              group="Cruz"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mipopup, 
              color= ~RubioPalette(mimap@data$RubioPct),
              group="Rubio"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Trump", "Kasich", "Cruz", "Rubio"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

mimapex
saveWidget(widget=mimapex, file="mimapGOP.html")


# Democratic Michigan Maps

midata <- read.csv("Dem_Mich.csv")
View(midata)

# creating shit to plot 
midata$SandersMarginVotes <- midata$Sanders - midata$Clinton
midata$SandersPct <- (midata$Sanders - midata$Clinton) / (midata$Sanders + midata$Clinton) # Will use formatting later to multiply by 100
midata$ClintonPct <- (midata$Clinton - midata$Sanders) / (midata$Sanders + midata$Clinton)
midata$SandersMarginPctgPoints <- midata$SandersPct - midata$ClintonPct

midata$SandersTPVS <- (midata$Sanders) / (midata$Sanders + midata$Clinton) # Will use formatting later to multiply by 100
midata$ClintonTPVS <- (midata$Clinton) / (midata$Sanders + midata$Clinton)


# Code for pulling put New Hampshire from the US county Geogrpahy shapefiles
migeo <- usgeo[usgeo@data$STATEFP=="26",]

# does it look right
qtm(migeo) # yes 

# does the shapefle have the same structure as the NH election data we've inputted:
# Shapefile structure
str(migeo@data$NAME)

write.csv(migeo@data$NAME, file="geo.csv")
# code to make hte counties match by putting them into a csv and coppying them over

list(migeo@data$NAME)
list(midata$County)

# Election data structure 
View(nhdata)

str(midata$County)

# This code turns counties to strings from factors:
migeo@data$NAME <- as.character(migeo@data$NAME)

# This code turns counties to strings from factors:
midata$County <- as.character(midata$County)

# order the data by county alphabetically 
migeo <- migeo[order(migeo@data$NAME),]
midata <- midata[order(midata$County),]

identical(migeo@data$NAME,midata$County) # this needs to be true to continue:

mimap <- append_data(migeo, midata, key.shp = "NAME", key.data="County")

qtm(mimap, "SandersMarginVotes")

jpeg(file="ClintonTPVS.jpg", quality=100, width=1024, height=768)
qtm(mimap, "ClintonTPVS", title="Clinton Vote Share")
dev.off()

#Interactive Michigan Map 

minpct <- min()

minpct <- min(c(mimap$ClintonTPVS, mimap$SandersTPVS))
maxpct <- max(c(mimap$ClintonTPVS, mimap$SandersTPVS))

ClintonPalette <- colorNumeric(palette = "Blues", domain=c(minpct, maxpct))
BerniePalette <- colorNumeric(palette = "Greens", domain = c(minpct, maxpct))


#####
mipopup <- paste0("County: ", mimap@data$County,
                  "Clinton: ", percent(mimap@data$ClintonTPVS),
                  "Sanders: ", percent(mimap@data$SandersTPVS))


mimapex <-   leaflet(mimap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=mipopup, 
              color= ~ClintonPalette(mimap@data$ClintonTPVS),
              group="Clinton"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=mipopup, 
              color= ~BerniePalette(mimap@data$SandersTPVS),
              group="Sanders"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Clinton", "Sanders"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

mimapex
saveWidget(widget=mimapex, file="mimapdems.html")

