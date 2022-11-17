###CODE for BioEco Manuscript Satterthwaite et al. 2021
###Spatial analyses & Figure 2,3,6


###3FINAL_Spatial_Mapping_BioEco
###IMPORTANT NOTE: GeoJSON.io uses projection OGC CRS84 (roughly EPSG:4326), so have kept our projection the same.


#1.1 INSTALL AND LOAD PACKAGES-----
# load libraries
# install.packages("here")
# install.packages("DT")
# install.packages("geojsonsf")
# install.packages("geojson")
# install.packages("mapview")
# install.packages("geojsonlint")
library(here) #reference files
library(readr) #read rectangular data

library(ggplot2) #visualization
require(gridExtra) #ggarrange
require(cowplot) #plot_grid

library(sf) #simple features as native R objects
library(raster) #Reading, writing, manipulating, analyzing and modeling of spatial data
library(fasterize) #Provides a drop-in replacement for rasterize() from the 'raster' package
library(gdalUtils) #mosaic rasters
library(rgdal) #bind rasters
library(rgeos) #Interface to Geometry Engine - Open Source 
require(lwgeom) #Access to selected functions found in 'liblwgeom

library(geojsonsf) #GEOJSON to sf converter
library(geojson) #representing simple geographical features
library(geojsonlint) #validate geojsons

#data manipulation
library(tidyr) #Tools to help to create tidy data, where each column is a variable, each row is an observation, and each cell contains a single value.
library(dplyr) #data manipulation

#misc
library(fuzzyjoin)
library(purrr) #complete and consistent set of tools for working with functions and vectors
library(svMisc) #progress bar for loop
library(pbapply) #progress bar to apply function
library(DT) #interface for datatables library
library(viridis) #color
library(colortools) #color
library(mapview) #interactive visualization of spatial data (NOT USED)
require(ggpubr) #to plot graphs on same plot

#1.2 CLEAR ALL-----
###clear anything in environment- spring cleaning!
rm(list=ls())

#1.3 READ IN AND FILTER DATA----
# setup variables
##csv file of cleaned survey data
newcsv <- here::here("data_Satterthwaite2021/1Final_BiologicalOceanObservingSurveyData_420_8132020_Satterthwaite2021.csv")

# read csv
newdf <- read_csv(newcsv)
dim(newdf) #371 rows/programs

# filter data to only include primary data collectors or networks of data collectors; programs systematically sampling EOVs; and long -term programs (running for >5 years)
#only include primary data collectors or networks of data collectors 
dataproviderdf <- filter(newdf, !alldataproviders ==  "NA NA")
dim(dataproviderdf) #346 primary data collectors or networks of data collectors 

##only include programs that measure EOVs (subvariables) systematically
dataproviderdfeovs <- dataproviderdf[!(is.na(dataproviderdf[,"Marine_Mammal_abundance_systematic"])) | !(is.na(dataproviderdf[,"Marine_Mammal_distribution_systematic"]))| !(is.na(dataproviderdf[,"Benthic_Inverts_abundance_systematic"])) | !(is.na(dataproviderdf[,"Benthic_Inverts_distribution_systematic"]))|!(is.na(dataproviderdf[,"Bird_abundance_systematic"])) | !(is.na(dataproviderdf[,"Bird_distribution_systematic"]))| !(is.na(dataproviderdf[,"Hard_Coral_cover_systematic"])) | !(is.na(dataproviderdf[,"Hard_Coral_composition_systematic"]))|!(is.na(dataproviderdf[,"Fish_abundance_systematic"])) | !(is.na(dataproviderdf[,"Fish_distribution_systematic"])) | !(is.na(dataproviderdf[,"Macroalgal_cover_systematic"])) | !(is.na(dataproviderdf[,"Macroalgal_composition_systematic"]))|!(is.na(dataproviderdf[,"Mangrove_cover_systematic"])) | !(is.na(dataproviderdf[,"Mangrove_composition_systematic"]))| !(is.na(dataproviderdf[,"Microbe_biomass_systematic"])) | !(is.na(dataproviderdf[,"Microbe_diversity_systematic"]))| !(is.na(dataproviderdf[,"Phytoplankton_biomass_systematic"])) | !(is.na(dataproviderdf[,"Phytoplankton_diversity_systematic"]))| !(is.na(dataproviderdf[,"Seagrass_cover_systematic"])) | !(is.na(dataproviderdf[,"Seagrass_composition_systematic"]))|!(is.na(dataproviderdf[,"Turtle_abundance_systematic"])) | !(is.na(dataproviderdf[,"Turtle_distribution_systematic"]))| !(is.na(dataproviderdf[,"Zooplankton_biomass_systematic"])) | !(is.na(dataproviderdf[,"Zooplankton_diversity_systematic"]))| !(is.na(dataproviderdf[,"OceanSound_systematic"])),]
dim(dataproviderdfeovs) #256 were primary data collectors or networks of data collectors that systematically sampled EOVs

#only include programs that are active/recently active
dataproviderdfeovsactive <- subset(dataproviderdfeovs, duration_end_year == "active" | duration_end_year == "2020" | duration_end_year == "2019") #include programs that are active or included the current year as end year
dim(dataproviderdfeovsactive) #232 were primary data collectors or networks of data collectors that systematically sampled EOVs and are recently active

###data providers that have been sampling for the last 3 or more years #try more (3years 2016, 5year 2014, 6 years )
dataproviderdfeovsactive5yrs <- subset(dataproviderdfeovsactive, duration_start_year < 2015) #subset for only those who have been sampling for 5 or more years (survey was conducted in 2019-2020)
dim(dataproviderdfeovsactive5yrs) #203 were primary data collectors or networks of data collectors that systematically sampled EOVs, are recently active, and have been running for more than 5 years

mappingdfproviders <- dataproviderdfeovsactive5yrs
dim(mappingdfproviders)
sum(is.na(mappingdfproviders$spatial_geojson_final)) #25 programs are missing 

#1.4 INPUT ADDITIONAL SPATIAL DATA TO CREATE FINAL DF----
####LARGE CSV GEOJSONS
egeoj <- here::here("data_Satterthwaite2021/2ProgramSpatialInformation_Satterthwaite2021.csv")

# read csv
egeoj1 <- read_csv(egeoj)

##get number without data providers providing info (131 didn't provide spatial information)
sum(unique(as.character(egeoj1$prog_name)) %in% unique(as.character(mappingdfproviders$prog_name)))
length(intersect(as.character(egeoj1$prog_name), as.character(mappingdfproviders$prog_name)))

##get number of those from pics (10 programs were drawn from photos found online)
piccsvfiles <- subset(egeoj1, picCSV == "Y")
length(intersect(as.character(piccsvfiles$prog_name), as.character(mappingdfproviders$prog_name)))

##get number of those from larvae CSV files (10 from large CSV files)
listcsvfiles <- subset(egeoj1, largeCSV == "Y")
length(intersect(as.character(piccsvfiles$prog_name), as.character(mappingdfproviders$prog_name)))

#Directly input GEOJSON spatial information for spatial data that was too large to store in a .csv
dim(subset(egeoj1, largeCSV == "Y" | smallCSV == "Y"))

#change names so that they match exactly
listcsvfiles$prog_name[grepl("Fotobiologia Playa", listcsvfiles$prog_name, ignore.case = T)] <- "Estacion de Fotobiologia Playa Union"
mappingdfproviders$prog_name[grepl("Fotobiologia Playa", mappingdfproviders$prog_name, ignore.case = T)] <- "Estacion de Fotobiologia Playa Union"

listcsvfiles$prog_name[grepl("Mammals as Ocean Samplers", listcsvfiles$prog_name, ignore.case = T)] <- "National Observatory System- Mammals as Ocean Samplers"
mappingdfproviders$prog_name[grepl("Mammals as Ocean Samplers", mappingdfproviders$prog_name, ignore.case = T)] <- "National Observatory System- Mammals as Ocean Samplers"

list <- unique(listcsvfiles$prog_name) #list of programs with spatial information that needs to be put directly in
length(list) #20 programs have spatial data to put directly in

# read in .csv files of coordinates, convert multiple points into single geojson string
geos <- function(i) {
  prog_name <- list[i]
  gj <- here::here(paste0("Data_Satterthwaite2021/largeCSVsites_final/",prog_name,".csv")) %>% 
    read_csv() %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    st_geometry() %>% st_geometrycollection("POINT") %>% 
    st_sfc() %>% st_sf() %>% sf_geojson() %>% as.character()
}

listofgeos2 <- lapply(1:length(list), geos)
unlistofgeos2 <- unlist(listofgeos2)
finalgeos2 <- as.data.frame(cbind(unlistofgeos2, list))
dim(finalgeos2) #20 programs with spatial info

##insert spatial data into dataframe for programs that have missing data
mappingdfproviders$spatial_geojsonLARGECSVGEO <- finalgeos2$unlistofgeos2[match(mappingdfproviders$prog_name,finalgeos2$list)]
mappingdfproviders$spatial_geojsonLARGECSVGEO<- as.character(mappingdfproviders$spatial_geojsonLARGECSVGEO)

###replace nas in with values from spatial_geojson
mappingdfprovidersfinal <- mappingdfproviders %>% mutate(spatial_geojson_final = coalesce(spatial_geojsonLARGECSVGEO, spatial_geojson_final))
sum(is.na(mappingdfprovidersfinal$spatial_geojson_final)) #13 programs have missing data

##read in the geojsons for final programs (because geojson has too many characters to store in .csv)
mystring <- read_file(here::here("Data_Satterthwaite2021/largeCSVsites_final/ORCAGEOJSON.txt"))

mappingdfprovidersfinal$spatial_geojson_final[which(mappingdfprovidersfinal$prog_name=="ORCA")] <- mystring
sum(is.na(mappingdfprovidersfinal$spatial_geojson_final)) #only 12 now with missing spatial data

mystring2 <- read_file(here::here("Data_Satterthwaite2021/largeCSVsites_final/WCSGEOJSON.txt"))

mappingdfprovidersfinal$spatial_geojson_final[which(mappingdfprovidersfinal$prog_name=="WCS global coral reef monitoring")] <- mystring2
sum(is.na(mappingdfprovidersfinal$spatial_geojson_final)) #only 11 now with missing spatial data

#create spatial df
dataproviderdfspatial <- mappingdfprovidersfinal[!is.na(mappingdfprovidersfinal$spatial_geojson_final),]

nrow(dataproviderdfspatial) #192 with spatial data (dataframe with spatial data only)
nrow(mappingdfprovidersfinal) #203 programs which leaves 11 missing spatial data (203-192) (data frame with all programs -- whether they have spatial data or not)

##this is the list of programs that have missing spatial data
i <- which(! mappingdfprovidersfinal$prog_name %in% dataproviderdfspatial$prog_name)
mappingdfprovidersfinal$prog_name[i] #names of programs with missing data

## Taking the first row and turning it into a simple feature object and then running a for loop on the rest.
## Split out the first one to create a simple feature object to then bind to the forloop output. 

#turn character into simple feature object
sf_obj <- geojson_sf(dataproviderdfspatial[1,]$spatial_geojson_final, expand_geometries = T) %>%
  mutate(Resp_id = dataproviderdfspatial[1,]$Resp_id) #use the unique identifier to join back to dataset

#add rest of datacolumns to the sf object 
out <- sf_obj %>%
  left_join(dataproviderdfspatial, by = "Resp_id")

###for sustematic ONLY c(2:nrow(dataproviderdfspatial)
dim(dataproviderdfspatial)
for(i in c(2:nrow(dataproviderdfspatial))){
  
  print(i)
  
  #turn character into simple feature object
  sf_obj <- geojson_sf(dataproviderdfspatial[i,]$spatial_geojson_final, expand_geometries = T) %>%
    mutate(Resp_id = dataproviderdfspatial[i,]$Resp_id) #use the unique identifier to join back to dataset
  
  sf_obj$Resp_id <- as.numeric(sf_obj$Resp_id)
  dataproviderdfspatial$Resp_id <- as.numeric(dataproviderdfspatial$Resp_id)
  
  #add rest of datacolumns to the sf object and removing the spatial geojson columns
  df <- sf_obj %>%
    left_join(dataproviderdfspatial, by = "Resp_id")
  
  out <- rbind(out, df)
  
}


out2 <- out |> 
  dplyr::group_by(Resp_id) |>
  dplyr::group_map(
    function(tbl, key){
      # tbl may have one or more geometries
      # key is empty since we kept the grouping variable
      
      # check that the individual geometries are valid
      # if not try to make them so
      valid <- if (!any(sf::st_is_valid(tbl))) {
        try(sf::st_make_valid(tbl))
      } else {
        tbl
      }
      
      # make_valid might have failed, 
      if (inherits(valid, "try_error") || (!all(sf::st_is_valid(tbl))) ){
        return(NULL)
      }
      # combine into one and bind with join with just the first row
      # since all but geometry column is replicated
      dplyr::bind_cols(
        dplyr::slice(tbl,1) |> sf::st_drop_geometry(),
        sf::st_union(valid) |>
          sf::st_as_sf(sf_column_name = "x") |>
          sf::st_set_geometry("geometry"))
    }, .keep = TRUE) |>
  dplyr::bind_rows() |>
  sf::st_as_sf(sf_column_name = "geometry") |>
  sf::write_sf(dsn = here::here("final-out.geojson"), delete_dsn = TRUE)


#save out as a spatial object if needed
#write_sf(df, "name.shp") 
##check spatial scope of programs
out$area <- st_area(out$geometry) #error message is ok, just that some lat and lons have some out of range values
length(unique(out$prog_name))
out$aream2 <- as.numeric(sub("\\s+\\D+$", "", out$area))
#View(aream2)
out$aream2[out$aream2 < 250000] <- 250000 #create some value for points 5*5km (10 km * 10km == 10000 * 10000 == 100000000 ; 5km * 5km == 5000m * 5000m == 25000000) (1km * 1km == 1000m * 1000m == 1000000)(0.5km * 0.5km == 500m * 500m == )
length(unique(out$prog_name))
finalsptim <- aggregate(aream2 ~ prog_name, data=out, FUN=sum)
length(unique(finalsptim$prog_name))
finalsptim$areakm2 <- finalsptim$aream2/1000000
finalsptim$length_m2 <- sqrt(finalsptim$aream2)
#m2 to km2
finalsptim$length_km2 <- finalsptim$length_m2 / 1000
#View(finalsptim)
dim(finalsptim)
summary(finalsptim$areakm2)
sum(finalsptim$areakm2)
8225674/361900000 * 100
summary(finalsptim$length_km2)



mappingdfproviders <- out
dim(mappingdfproviders)


dim(mappingdfprovidersfinal) 
length(unique(mappingdfproviders$prog_name))#203-192 == missing ~11 programs
View(unique(mappingdfproviders$prog_name))
class(mappingdfproviders)




#1.5 FIND AREA OF TOTAL GLOBAL OCEAN ACCORDING TO OUR RASTERS----
####GOOD resource: https://caucasus-spiders.info/r-spatial/raster-basics-3/
raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #55km resolution

##read in ocean data
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

##read in land data
##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)
landrast <- rasterize(land, raster, field= 1)
plot(landrast)

###OCEAN
oceanrast1 <- rasterize(ocean, raster, field= 1)
plot(oceanrast1)
#remove points on land
oceanmask <- mask(x = landrast, mask = oceanrast1)
oceanmaskfinal <- crop(x = landrast, y = extent(oceanmask))
plot(oceanmaskfinal)
oceanmaskfinal[is.na(oceanmaskfinal[])] <- 2
values(oceanmaskfinal)[values(oceanmaskfinal) < 2] = NA

plot(oceanmaskfinal2)
oceanmaskfinal2

oceanrast <- oceanmaskfinal
plot(oceanrast)


#tapply(area(oceanrast), oceanrast[], sum) 
cell_size<-area(oceanrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
length(cell_size)
median(cell_size)
#compute area [km2] of all cells in geo_raster
length(cell_size)*median(cell_size) ###USE THIS!!
#####OCEAN: 411550230 km2 (roughly according to our raster) which is somewhat similar to 361.9 million sq km



tapply(area(landrast), landrast[], sum) 
cell_size<-area(landrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
length(cell_size)*median(cell_size) ###LAND: 151410073
411550230 + 151410073
##size of entire raster ==  562960303


#2.1 MAPS: MAP OF ALL ACTIVE, EOV MONITORING PROGRAMS: NOT A FIGURE IN MS----
##Create raster of early system with a 0.5km/500m resolution
raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfproviders$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- mappingdfproviders %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- mappingdfproviders %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- mappingdfproviders %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
rast_all_active <- raster::merge(comb_rast, pointraster)

##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

#ocean <- gDifference(as(extent(final_rast), 'SpatialPolygons'), land) #remove 
#plot(ocean)
rast_all_active_mask <- mask(x = rast_all_active, mask = ocean)
rnew2ALL <- crop(x = rast_all_active_mask, y = extent(ocean))
rnew2ALL
plot(rnew2ALL)
plot(ocean)
ocean

rnew2ALL
prop.table(table(as.vector(rnew2ALL)))["1"]

##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

## plot the monitoring with the world map
#plot(rnew2ALL, legend = F, col = "slateblue")
#plot(land, col = "grey", add = TRUE)

##CALCULATE AREA OF SYSTEMATIC, ACTIVE
#tapply(area(rnew2ALL), rnew2ALL[], sum) #AREA OF RASTER (maybe calculate with smaller resolution/grid cells)
cell_size <- area(rnew2ALL, na.rm=T, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size <- cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
totalarea <- length(cell_size)*median(cell_size) ###151410073
totalarea #24518636
totalarea/411550230 * 100 ##5.96% of global surface ocean with systematic only
#totalarea/361900000 * 100 
#24000000/361900000 * 100
#totalarea/1335000000 * 100
###HEXAGON MAP OF ALL ACTIVE PROGRAMS
##convert raster back to points
#test <- as.data.frame(rasterToPoints(rnew2ALL))

#seal_sf <- st_as_sf(test, coords = c("x","y")) %>% 
#st_set_crs(leaflet:::epsg4326)
#seal_coords <- do.call(rbind, st_geometry(seal_sf)) %>% 
#as_tibble() %>% setNames(c("homelon","homelat"))

# Get the world polygon
world <- map_data("world")
#get the EEZ
#eezs <- st_read(here('final_data_BioEco/World_EEZ_v11_20191118/eez_v11.shp'))
#eez200nm <- eezs %>% filter(POL_TYPE == '200NM')
#class(eez200nm)
#class(world)
#colnames(eez200nm)

# FINAL PLOT OF ALL ACTIVE SURVEYS
#ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.4)  + geom_hex(data= seal_coords, aes(x=homelon, y=homelat, fill = 1), bins=c(180,360), show.legend = FALSE) + theme_void()


###TESTING FOR THE RANDOMNESS OF SAMPLING IN ALL OF THE OCEAN
require(rasterVis)
#raster of all samples
#NOTE THIS COMES FROM LINE 608
alldatatypesraster
values(alldatatypesraster)[values(alldatatypesraster) < 1] = NA #convert 0s to NAs
plot(alldatatypesraster)
#convert raster to points to be able to plot lat and lon histograms
spts <- rasterToPoints(alldatatypesraster, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts <- spTransform(spts, CRS(llprj))
print(llpts)
sampledx <- as.data.frame(llpts)
head(sampledx)

#histograms of the sampling
class(x)
hist(sampledx$x)
hist(sampledx$y)

plot(oceanrast)
oceanrast
oceanrast2 <- oceanrast
oceanrast2
plot(oceanrast2)
#Take a random sample from the cell values of a Raster* object- use 0.06 to get 6% of the cells 
oceanrastrandom <- sampleRandom(oceanrast2, ncell(oceanrast2)*.06, asRaster=TRUE)
plot(oceanrastrandom)
#convert raster to points to be able to plot lat and lon histograms
sptsrand <- rasterToPoints(oceanrastrandom, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llptsrand <- spTransform(sptsrand, CRS(llprj))
print(llptsrand)
x <- as.data.frame(llptsrand)
head(x)
#histograms of RANDOM sampling
#hist(x$x)
hist(x$y)

#Kolmogorov-Smirnov test for distributions compare randomly sampled ocean to samples
#ks.test(sampledx$x, x$x) # Kolmogorov-Smirnov test statistic = D & significance level
ks.test(sampledx$y, x$y)

##LONGITUDE
x$e <- 'entire ocean'
sampledx$e <- 'sampled areas'

combo <- rbind(x, samplex)
head(combo)

class(combo)

require(scales)

#histogram of longitude 
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
scales::show_col(safe_colorblind_palette)
###Also see https://personal.sron.nl/~pault/ muted qualitative color scheme with color-blind safe colors


longhist <- ggplot(combo, aes(x, fill=e, colour=e)) +
  geom_histogram(aes(y=2*(..density..)/sum(..density..)),  alpha=0.2, 
                 position="identity", lwd=0.2) + scale_y_continuous(labels = scales::percent_format()) + ggtitle("") + xlab("Longitude") + ylab("Percent") + theme(legend.title = element_blank(), panel.background=element_blank()) + scale_fill_manual(values=c("#44AA99","#DDCC77")) + scale_color_manual(values=c("#44AA99","#DDCC77"))
longhist
##colors pulled from colorblind safe palette

###LATITUDE
combo <- rbind(x, sampledx)
head(combo)

class(depthall)

require(scales)
#histogram of latitude 
lathist <- ggplot(combo, aes(y, fill=e, colour=e)) +
  geom_histogram(aes(y=2*(..density..)/sum(..density..)),  alpha=0.2, 
                 position="identity", lwd=0.2) + scale_y_continuous(labels = scales::percent_format()) + ggtitle("") + xlab("Latitude") + ylab("Percent") + theme(legend.title = element_blank(), panel.background=element_blank()) + scale_fill_manual(values=c("#44AA99","#DDCC77")) + scale_color_manual(values=c("#44AA99","#DDCC77"))
lathist

##compare for land
plot(landrast)
landrast
landrast2 <- landrast
landrast2
plot(landrast2)
#Take a random sample from the cell values of a Raster* object- use 0.06 to get 6% of the cells 
#landrastrandom <- sampleRandom(landrast2, ncell(landrast2)*.06, asRaster=TRUE)
#convert raster to points to be able to plot lat and lon histograms
sptsland <- rasterToPoints(landrast2, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llptsland <- spTransform(sptsland, CRS(llprj))
print(llptsland)
xland <- as.data.frame(llptsland)
head(xland)
#histograms of RANDOM sampling
histogram(xland$x)
histogram(xland$y)

#Kolmogorov-Smirnov test for distributions
ks.test(xland$x, x$x) # Kolmogorov-Smirnov test statistic = D & significance level
ks.test(xland$y, x$y)



###RIPLEY TEST TO EXAMINE SPATIAL RANDOMNESS
# simple test examples Ripley's function
# First generate a uniform random distribution and test via Ripley's K,L
# Second generate a normal random clustered distribution center midfiedl
# and test via Ripley's K,L

install.packages('spatstat')
library(spatstat)
require(spatstat)

# Part 1 - Generate a uniform randomly distributed X,Y pairs and test using Ripleys.
# The cloer the data to the theoretical the more evenly disbursed (closeness to Poisson Dist)

# Example latitude and longitude of data points
# calculates min and max for window constraints
# should convert to UTM coordinates or similar to determine distances
# Generate uniform random data (substitute for lat,long data)
NumSamples = 250
MinLat = min(x$y)
MaxLat = max(x$y)
MinLong = min(x$x)
MaxLong = max(x$x)
set.seed(123)
Long <- runif(NumSamples, min=MinLong, max=MaxLong)
set.seed(234)
Lat <- runif(NumSamples, min=MinLat, max=MaxLat) 

# Plot Lat, Long Uniform random distribution
plot(Long,Lat, main="Random Lat,Long coordinates ~Uniform distribution") 

# Define ppp object based on Liencres point locations
pppObject <- ppp(x = x$x, y = x$y,  c(MinLong, MaxLong), c(MinLat, MaxLat))
plot(pppObject) # should look like our observed
summary(pppObject)

# calculate Ripley's function
K <- Kest(pppObject, mode='ripley', correction="isotropic")

# plot Ripley's K, Ripley's L
plot(K, main="Ripley's K function for cells")
plot(K, sqrt(./pi) ~ r, ylab="L(r)", main="Ripley's L function for cells")

# Define ppp object based on Liencres point locations: NOW FOR RANDOMLY SAMPLED
NumSamples = 250
MinLat = min(samplex$y)
MaxLat = max(samplex$y)
MinLong = min(samplex$x)
MaxLong = max(samplex$x)
set.seed(123)
Long <- runif(NumSamples, min=MinLong, max=MaxLong)
set.seed(234)
Lat <- runif(NumSamples, min=MinLat, max=MaxLat) 

pppObjectrand <- ppp(x = samplex$x, y = samplex$y,  c(MinLong, MaxLong), c(MinLat, MaxLat))
plot(pppObjectrand) # should look like our observed
summary(pppObjectrand)

# calculate Ripley's function
K <- Kest(pppObjectrand, mode='ripley', correction="isotropic")

# plot Ripley's K, Ripley's L
plot(K, main="Ripley's K function for cells")
plot(K, sqrt(./pi) ~ r, ylab="L(r)", main="Ripley's L function for cells")

# Part 2 - Next generate a Normal distribution with some clustering and perform the 
# same test

# Generate Normal random data (substitute for lat,long data)
NumSamples = 250
set.seed(123)
Long <- rnorm(NumSamples, mean = 50, sd = 20) 
set.seed(234)
Lat <- rnorm(NumSamples, mean = 50, sd = 20)  

# Plot Lat, Long Normal random distribution
plot(Long,Lat, main="Random Lat,Long coordinates ~Normal distribution") 

# Define ppp object based on Liencres point locations
pppObject <- ppp(x = x$x, y = x$y,  c(MinLong, MaxLong), c(MinLat, MaxLat))
plot(pppObject)

# calculate Ripley's function
K <- Kest(pppObject, mode='ripley', correction="isotropic")

# plot Ripley's K, Ripley's L
plot(K, main="Ripley's K function for cells")
plot(K, sqrt(./pi) ~ r, ylab="L(r)", main="Ripley's L function for cells")



#2.2* MAPS: MAP OF OBIS/GBIF DATA & ALL SAMPLING WITH EEZs: FIGURE 5a----
rnew2ALL #raster of our sampling programs
rnew2OBISGBIF #raster for OBIS/GBIF GET FROM CODE 4.3!!!!
raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

rnew2ALL[rnew2ALL == 1] <- 1
rnew2OBISGBIF[rnew2OBISGBIF == 1] <- 2

alldatatypesraster <- mosaic(rnew2ALL, rnew2OBISGBIF, fun=sum)
#plot(alldatatypesraster)

##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

alldatatypesrasterr2new <- mask(alldatatypesraster, ocean)
surveygbifobisraster_crop_final <- crop(x = alldatatypesrasterr2new, y = extent(ocean))

plot(surveygbifobisraster_crop_final)

prop.table(table(as.vector(surveygbifobisraster_crop_final))) #this is another way to look at the area covered by the different pixels

##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

## plot the monitoring with the world map
#cuts=c(1,2,3) #set breaks
#pal <- colorRampPalette(c("lightblue","yellow","red"))
#plot(surveygbifobisraster_crop_final, legend = T, breaks=cuts, col = pal(3))
#plot(land, col = "grey", add = TRUE) ##FINAL FIGURE? This output is Figure combining eovs

###GBIF/OBIS & SURVEY SAMPLING HEXAGON MAP
##convert raster back to points
surveygbifobisdf <- as.data.frame(rasterToPoints(surveygbifobisraster_crop_final))
colnames(surveygbifobisdf)
unique(surveygbifobisdf$layer)

ogs_sf <- st_as_sf(surveygbifobisdf, coords = c("x","y")) %>% 
  st_set_crs(leaflet:::epsg4326)
ogs_coords <- do.call(rbind, st_geometry(ogs_sf)) %>% 
  as_tibble() %>% setNames(c("x","y"))

surveygbifobisdfmerge <- merge(ogs_coords, surveygbifobisdf)
dim(ogs_coords)
dim(surveygbifobisdf)
dim(surveygbifobisdfmerge)
colnames(surveygbifobisdfmerge)
unique(surveygbifobisdfmerge$layer)
surveygbifobisdfmerge12 <- subset(surveygbifobisdfmerge, layer > 0) #remove 0 cells from raster
unique(surveygbifobisdfmerge$layer)
unique(surveygbifobisdfmerge12$layer)
dim(surveygbifobisdfmerge12)




# Get the world polygon
world <- map_data("world")
# plot
head(surveygbifobisdfmerge12)
#surveygbifobisdfmerge12[surveygbifobisdfmerge12 == 1] <- "Survey Only"  #SURVEY ONLY
#surveygbifobisdfmerge12[surveygbifobisdfmerge12 == 2] <- "OBIS/GBIF Only" #OBIS/GBIF ONLY
#surveygbifobisdfmerge12[surveygbifobisdfmerge12 == 3] <- "Both: Survey & OBIS/GBIF" #OBIS/GBIF & Survey

###OBIS/GBIF/SURVEY FIGURE WITHOUT EEZS
#colorfun <- colortools::splitComp("lightsalmon", plot=F, bg= "white")
#colorfun2 <- c("#7A97FF", "cadetblue3", "#FFA07A")
#ggplot() + 
#geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#stat_summary_hex(data= surveygbifobisdfmerge12, aes(x=x, y=y, z=as.integer(layer)), show.legend = T, fun = "max", bins = c(180,360)) +
#theme_void() + labs(fill = " ") + scale_fill_gradientn(colors = colorfun2 , breaks=c(1,2,3),labels=c("Survey Only","OBIS/GBIF Only","Both")) + guides(fill = guide_legend()) #use this so that labels are in correct location

###FINAL FIGURE OF OBIS/GBIF & SURVEY SITES WITH EEZS: FINAL FIGURE 5a
# FINAL PLOT OF OVERLAPPING SURVEYS OBIS/GBIF/Survey with EEZs
# Load EEZ polygons
eezs <- st_read(here::here('final_data_BioEco/World_EEZ_v11_20191118/eez_v11.shp'))
colnames(eezs)
eez200nm <- eezs %>% filter(POL_TYPE == '200NM') # select the 200 nautical mile polygon layer
class(eez200nm)
dim(eez200nm)
colnames(eez200nm)
#View(unique(eez200nm$GEONAME))

surveygbifobisdfmerge12test <- surveygbifobisdfmerge12
coordinates(surveygbifobisdfmerge12test) <- ~ x + y
class(surveygbifobisdfmerge12test)

eez200nmsp <- as(eez200nm, 'Spatial')
class(eez200nmsp)

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
class(surveygbifobisdfmerge12test)
proj4string(surveygbifobisdfmerge12test) <- proj4string(eez200nmsp)
class(eez200nmsp)
overlappinglayers <- over(eez200nmsp, surveygbifobisdfmerge12test)
#View(overlappinglayers)
finaloverlap <- cbind(overlappinglayers, eez200nm$GEONAME)

finaloverlapno <- finaloverlap[is.na(finaloverlap$layer),]
dim(finaloverlapno)
test <- subset(eez200nm, GEONAME %in% finaloverlapno$`eez200nm$GEONAME`)

#View(test$GEONAME)

testbound <- st_boundary(test)
numwithout <- dim(test)
numtot <- dim(eez200nm)
numwithout/numtot #22% of EEZs do not have any known biological sampling




#2.3* HDI INDEX & COUNTRIES WITH SAMPLING V. NO SAMPLING----
##Use column SOVERIGN to understand which countries don't have sampling since soverign is a better label to classify countries
finaloverlapsov <- cbind(overlappinglayers, eez200nm$SOVEREIGN1) #for countries
finaloverlapsov
finaloverlapsovcons <- finaloverlapsov %>% 
  group_by(`eez200nm$SOVEREIGN1`) %>% 
  summarise_all(funs(first(na.omit(.))))
View(finaloverlapsovcons)

finaloverlapsovconsNA <- finaloverlapsovcons[is.na(finaloverlapsovcons$layer),] #these are all the countries without sampling
finaloverlapsovconsNONA <- finaloverlapsovcons[!is.na(finaloverlapsovcons$layer),] #these are all the countries with sampling
dim(finaloverlapsovconsNA) #34 countries without sampling
finaloverlapsovconsNA$Sampling <- "No Sampling"
dim(finaloverlapsovconsNONA) #121 countries with sampling
finaloverlapsovconsNONA$Sampling <- "Sampling"
nosampcount <- length(unique(finaloverlapsovconsNA$`eez200nm$SOVEREIGN1`))
View(finaloverlapsovconsNA$`eez200nm$SOVEREIGN1`) #list of countries without sampling
allcount <- length(unique(eez200nm$SOVEREIGN1))
finalwwosampling <- rbind(finaloverlapsovconsNONA,finaloverlapsovconsNA)
View(finalwwosampling) #count with sampling
nosampcount/allcount #22% of countries do not have any known biological sampling


##csv file comes from 1InitialSurveyCleanandSpatialData
hdicsv <- here::here("final_data_BioEco/HumanDevelopmentIndex2018_UNEP.csv")
#hdicsvkey <- here::here("final_data_BioEco/HumanDevelopmentIndex_Key_Countries_EEZ.csv") #likely don't need this
# read csv
hdidf <- read_csv(hdicsv)
#hdikey <- read_csv(hdicsvkey)

##MATCH WITH SOVERIGN
colnames(hdidf)
#View(hdidf)
colnames(finalwwosampling)
finalwwosampling$Country <- as.character(finalwwosampling$`eez200nm$SOVEREIGN1`)

hdifinal <- merge(finalwwosampling, hdidf)
#View(hdifinal)
colnames(hdifinal)


#m1 <- lm(HDI_2018  ~ Sampling,data=hdifinal)
#summary(m1) #linear model suggests that Sampling and No sampling sig. different

nosmp <- subset(hdifinal, Sampling == "No Sampling")
hist(nosmp$HDI_2018)
smp <- subset(hdifinal, Sampling == "Sampling")
hist(smp$HDI_2018)

# independent 2-group Mann-Whitney U Test
#wilcox.test(y~A)
# where y is numeric and A is A binary factor
wilcox.test(HDI_2018 ~ Sampling, data=hdifinal) #use wilcox test in the paper

#samp_summary <- hdifinal %>% # the names of the new data frame and the data frame to be summarised
group_by(Sampling) %>%   # the grouping variable
  summarise(mean_PL = mean(HDI_2018, na.rm=T),  # calculates the mean of each group
            sd_PL = sd(HDI_2018, na.rm=T), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(HDI_2018, na.rm=T)/sqrt(n())) # calculates the standard error of each group

#samp_summary
#mean_sdl
#dim(hdifinal)
#testfundorder <- hdifinal %>% mutate(Sampling = fct_reorder(Sampling, HDI_2018, .fun='mean'))

###VIOLIN PLOT
#ggplot(testfundorder, aes(y = testfundorder$HDI_2018 , x= testfundorder$Sampling, fill= testfundorder$Sampling)) + geom_violin(trim=FALSE) + geom_point(color="grey", cex = 0.2) + stat_summary(fun.data=mean_se, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2) + stat_summary(fun.y=mean, geom="point", color="black") + labs(x="", y="Human Development Index (HDI)") + theme_bw() + theme(legend.position = "none") + scale_fill_manual(values = c("aliceblue", "azure"))


#SCATTERPLOT
#ggplot(samp_summary, aes(Sampling, mean_PL)) + geom_point() +labs(y="Human Development Index (HDI)", x= "") + theme_bw() + geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2) 
#require(plotly)
#fig <- plot_ly(data = hdifinal, x = ~Sampling, y = ~HDI_2018, text=~Country, type = 'scatter')
#fig



###RAINCLOUD PLOT: SELECTED AS FINAL PLOT
##https://micahallen.org/2018/03/15/introducing-raincloud-plots/
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

install.packages("see")
require(see)

dim(hdifinal)
testfundorder <- hdifinal %>%
  mutate(Sampling = fct_reorder(Sampling, HDI_2018, .fun='mean'))

###FINAL HDI to Sampling v. No Sampling PLOT: FIGURE 5b
hdiplot <- ggplot(data = testfundorder, aes(y = HDI_2018, x = Sampling, fill = Sampling)) +
  see::geom_violinhalf(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HDI_2018), position = position_jitter(width = .15), size = .5, alpha = 0.8, color="lightgrey")+
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  labs(x="", y="Human Development Index (HDI)") + theme(legend.position = "none", plot.margin = units(c(0, 0, 0, 0))) + theme_bw() + scale_fill_manual(values = c("slategray3", "skyblue3"), guide=FALSE)  +
  coord_flip() +
  raincloud_theme
hdiplot


#2.4 AREAS OF ACTIVE SAMPLING IN EEZ v. ABNJ: NO FIGURE BUT INFO FOR MS----
# coerce to raster
rasterSURV <- rasterize(surveygbifobisdfmerge12test, raster, layer= 1)
rasterSURV
rasterEEZ <- rasterize(eez200nmsp, raster)
plot(rasterEEZ)
rasterEEZ
class(rasterEEZ)

rasterEEZ2 <- rasterEEZ
rasterEEZ2[rasterEEZ2 > 1] <- 1 #find area of the EEZ
rasterEEZ2[rasterEEZ2 < 1] <- NA
rasterEEZ2
plot(rasterEEZ2)
class(rasterEEZ2)

rasterSURV2 <- rasterSURV
rasterSURV2[rasterSURV2 > 1] <- 1 #find area of the SURV
rasterSURV2[rasterSURV2 < 1] <- NA
rasterSURV3 <- subset(rasterSURV2, 1)
plot(rasterSURV3)
class(rasterSURV3)

##GET AREA WITHIN EEZ
cell_size<-area(rasterEEZ2, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
eez200nm_rast_area <- length(cell_size)*median(cell_size)
eez200nm_rast_area #183781652 is area within EEZs
rast_all_active_mask1 <- raster::mask(rasterSURV3, mask = rasterEEZ2)
rast_all_active_mask1
allactrastcrop <- crop(x = rast_all_active_mask1, y = rasterEEZ2)
allactrastcrop[allactrastcrop < 1] <- NA #find area of the EEZ
allactrastcrop[allactrastcrop > 1] <- NA
plot(allactrastcrop)
cell_size<-area(allactrastcrop, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells 
allactrastcrop_area200 <-length(cell_size)*median(cell_size)
allactrastcrop_area200 #19010452
(allactrastcrop_area200/411550230) * 100 #4.6% of known biological sampling within EEZs
allactrastcrop_area200/eez200nm_rast_area * 100 #10.3% of EEZs is covered by biological sampling
(eez200nm_rast_area/411550230) * 100 #44.7% of global ocean is EEZs

##GET AREA IN ABNJ
#convert EEZ raster to have all values not part of EEZs
rasterEEZ2[rasterEEZ2 > 0] <- 2
plot(rasterEEZ2)
rasterEEZ2[is.na(rasterEEZ2)] <- 1
rasterEEZ2[rasterEEZ2 > 1] <- NA
plot(rasterEEZ2)
rasterSURV3

##Get land and crop out so only have 
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

landrast <- rasterize(land, raster)
plot(landrast)
landrast[is.na(landrast[])] <- 0 
landrast[landrast > 0] <- NA
landrast[landrast = 0] <- 1
plot(landrast)
landrast

EEZmask <- raster::mask(rasterEEZ2, ocean)
plot(EEZmask)
EEZmask_crop_final <- raster::crop(x = EEZmask, y = extent(ocean))
plot(EEZmask_crop_final)
EEZmask2 <- raster::mask(rasterEEZ2, landrast)
EEZmask_crop_final2 <- raster::crop(x = EEZmask2, y = extent(landrast))
plot(EEZmask_crop_final2)



##GET AREA IN ABNJ (outside EEZs)
cell_size<-area(EEZmask_crop_final2, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
eez200nm_rast_noarea<-length(cell_size)*median(cell_size)
eez200nm_rast_noarea #237599836
extent(EEZmask_crop_final2)

EEZmask_NO_final<- projectRaster(EEZmask_crop_final2, rasterSURV3, method = 'ngb') 

rast_all_active_mask1noeez <- raster::mask(x = rasterSURV3, mask = EEZmask_NO_final)
rast_all_active_mask1noeez
allactrastnoncrop <- crop(x = rast_all_active_mask1noeez, y = EEZmask_NO_final)
allactrastnoncrop[allactrastnoncrop < 1] <- NA #find area of the EEZ
allactrastnoncrop[allactrastnoncrop > 1] <- NA
plot(allactrastnoncrop)
cell_size<-area(allactrastnoncrop, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
allactrastcrop_area200 <-length(cell_size)*median(cell_size)
allactrastcrop_area200 #AREA OF SAMPLING WITHIN eez: 10534070
(allactrastcrop_area200/411550230)*100 #2.56 of total known biological sampling is within EEZ waters 
(eez200nm_rast_noarea/411550230) * 100 

(19010452 + 10534070)/ (244129600 + 183781652) * 100 #about 7% area
(244129600)/ (244129600 + 183781652) #57% is ABNJ
(183781652)/ (244129600 + 183781652) #43% is EEZ


###FINAL FIGURE FOR OBIS/GBIF, SURVEY, AND ALL
require(RColorBrewer)
#see <- brewer.pal(n = 8, name = 'Dark2')
#see
head(surveygbifobisdfmerge12)
#colorfun <- colortools::splitComp("cadetblue3", plot=F, bg= "white")
colorfun2 <- c("#6699CC", "#44AA99","#117733")
OBISGBIFSurveyPlot <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), color= "grey82", fill="grey89", alpha=1)  +
  stat_summary_hex(data= surveygbifobisdfmerge12, aes(x=x, y=y, z=as.integer(layer)), show.legend = T, fun = "max", bins = c(360,720)) +
  theme_void() + labs(fill = " ") + scale_fill_gradientn(colors = colorfun2 , breaks=c(1,2,3),labels=c("Survey Only","OBIS & GBIF Only","Both")) + guides(fill = guide_legend()) + geom_sf(data = testbound, fill = "transparent", color = "darkgray", size = 0.25, alpha=0.95) + theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"), legend.position="bottom", legend.margin=margin(-5,0,0,0), legend.text=element_text(size=12))
# ("left", "right", "bottom", "top")

OBISGBIFSurveyPlot
ggsave(filename = "OBISGBIFSurveyPlot.jpeg", width = 8, height = 6, device='jpeg', dpi=700)
# insert ggplot codes
dev.off()

#write.csv(finaloverlap, here::here("output_data_BioEco/EEZsOBISGBIFSURVEY.csv"))

####MAP & PLOT: COMBINED PLOT FOR MAP OF SAMPLING & HDI INDEX: FIGURE 5------
###INTEGRATED FIGURE FOR 5a,b
#grid.arrange(OBISGBIFSurveyPlot, hdiplot, ncol = 1, heights=c(4,1), labels=c("a","b"))
plot_grid(OBISGBIFSurveyPlot, hdiplot, ncol = 1, scale = c(1,0.8), labels = c('a)', 'b)'))




####2.5 METRICS FOR AREA COVERED BY ALL, OBIS/GBIF, SURVEY, BOTH-------
###ALL COVERAGE: AREA
allrast <- surveygbifobisraster_crop_final
allrast
allrast[allrast < 1] <- NA #remove 0s
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(allrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
all_area<-length(cell_size)*median(cell_size)
all_area/411550230 * 100 #6.96


###AREA FOR EACH: OBIS/GBIF
obisrast <- surveygbifobisraster_crop_final
obisrast[obisrast < 2] <- NA #remove 0/1s
obisrast[obisrast > 2] <- NA #remove 2,3s
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(obisrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
obis_area<-length(cell_size)*median(cell_size)
obis_area/411550230 * 100 #1.04


###AREA FOR EACH: Survey
survrast <- surveygbifobisraster_crop_final
survrast[survrast < 1] <- NA #remove 0s
survrast[survrast > 1] <- NA #remove 2,3s
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(survrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
sur_area<-length(cell_size)*median(cell_size)
sur_area/411550230 * 100 #5.29%


###AREA BOTH COMBINED
bothrast <- surveygbifobisraster_crop_final
bothrast[bothrast < 3] <- NA #remove 0,1,2
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(bothrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
both_area<-length(cell_size)*median(cell_size)
both_area/411550230 * 100 #0.69%






####2.6 PREP: DATA ACCESS DATA FOR MAP: PREP FIGURE 4b-------

####MAP OF ACCESSIBLE DATA: OPEN DATA & REQUEST
##Create raster of early system with a 0.5km/500m resolution
dim(mappingdfproviders)


mappingdfprovidersaccessible <- subset(mappingdfproviders, dataaccess_AnyRawBioDataRestrictions == "No, the raw data are publicly accessible/open." | dataaccess_restriction_level == "Access by request only- anyone can get access but need to formally ask")
dim(mappingdfprovidersaccessible)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- mappingdfprovidersaccessible %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- mappingdfprovidersaccessible %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- mappingdfprovidersaccessible %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
rast_accessible <- raster::merge(comb_rast, pointraster)
#before being cropped, includes all areas on land

##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)
plot(ocean)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326


###remove areas that do not fall in the ocean so as to get the most accurate spatial estimate
rast_acc_mask <- raster::mask(x = rast_accessible, mask = ocean)
rast_acc_crop_final <- raster::crop(x = rast_acc_mask, y = extent(ocean))

##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

## plot the monitoring with the world map
#plot(rast_open_crop_final, legend = F, breaks=cuts, col = "slateblue")
#plot(land, col = "grey", add = TRUE)


####MAP OF ALL ACTIVE, EOV, OPEN OR WANT TO BE OPEN DATA MONITORING PROGRAMS
##Create raster of early system with a 0.5km/500m resolution
dim(mappingdfproviders)
mappingdfprovidersopenortrying <- subset(mappingdfproviders, dataaccess_AnyRawBioDataRestrictions == "No, the raw data are publicly accessible/open." | dataaccess_restriction_level == "Access by request only- anyone can get access but need to formally ask"| dataaccess_WorkingToMakeOpenInFuture == "Yes")
dim(mappingdfprovidersopenortrying)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopenortrying$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondfopentry <- mappingdfprovidersopenortrying %>% filter(grepl("POLYGON", st_geometry_type(geometry)))

polygonrasteropentry <- fasterize(polygondfopentry, raster)
#plot(polygonrasteropentry)

##filter linestring to convert to raster
linedfopentry <- mappingdfprovidersopenortrying %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
linerasteropentry <- rasterize(linedfopentry, raster, field=1)
#plot(linerasteropentry)

##filter points to convert to raster
pointdfopentry <- mappingdfprovidersopenortrying %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coordsopentry <- do.call(rbind, st_geometry(pointdfopentry)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointrasteropentry <- rasterize(seal_coordsopentry, raster, field=1)
#plot(pointrasteropentry)

##combine all rasters
comb_rastopentry <- raster::merge(polygonrasteropentry, linerasteropentry)
rast_opentry <- raster::merge(comb_rastopentry, pointrasteropentry)
#before being cropped, includes all areas on land
rast_opentry

##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

#ocean <- gDifference(as(extent(final_rast), 'SpatialPolygons'), land) #remove 
#plot(ocean)
rast_opentry_mask <- raster::mask(x = rast_opentry, mask = ocean)
rast_opentry_crop_final <- raster::crop(x = rast_opentry_mask, y = extent(ocean))


##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

###combine data to get integrated map
datarast <- mosaic(rnew2ALL, rast_acc_crop_final, rast_opentry_crop_final, fun = sum)
plot(datarast)



####2.7 AREAS OF DATA ACCESS-------
##AREA OF ALL SAMPLING!
alldatarast2 <- datarast
alldatarast2[alldatarast2 < 0.9] <- NA
alldatarast2

#get sizes of all cells in raster [km2]
cell_size<-area(alldatarast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
alldatarast2_area<-length(cell_size)*median(cell_size)
alldatarast2_area/411550230 * 100 #6% CHECKS OUT!

##ACCESSIBLE DATA: AREA
datarastopen <- datarast
datarastopen[datarastopen < 3] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-raster::area(datarastopen, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
opendata_area<-length(cell_size)*median(cell_size)
opendata_area/411550230 * 100 #5.29 ACCESSIBLE
####opendata_area over ocean area

##OPEN/TRYING DATA: AREA
datarastopentry <- datarast
datarastopentry[datarastopentry < 2] <- NA #remove values other than 2 because that is for open/try
datarastopentry[datarastopentry > 2] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-raster::area(datarastopentry, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
opentrydata_area<-length(cell_size)*median(cell_size)
opentrydata_area/411550230 * 100 #0.47% TRYING TO MAKE OPEN
####opendata_area over ocean area

##Non Accessible DATA: AREA
nonacrast <- datarast
nonacrast[nonacrast < 1] <- NA #remove values other than 1 because that is for restricted
nonacrast[nonacrast > 1] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-raster::area(nonacrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
resdata_area<-length(cell_size)*median(cell_size)
resdata_area/411550230 * 100 #0.15% RESTRICTED?


####2.8 MAP: DATA ACCESS DATA: FIGURE 4b-------
###INTEGRATED DATA ACCESS MAP: HEXAGON: FINAL FIGURE
##convert raster back to points
dataaccess <- as.data.frame(rasterToPoints(datarast))
colnames(dataaccess)
unique(dataaccess$layer)
#TO DO: PROBABLY SHOULD REMOVE ANY POINTS OVER LAND
dataaccess_sf <- st_as_sf(dataaccess, coords = c("x","y")) %>% 
  st_set_crs(leaflet:::epsg4326)
dataaccess_coords <- do.call(rbind, st_geometry(dataaccess_sf)) %>% 
  as_tibble() %>% setNames(c("x","y"))

dataaccessmerge <- merge(dataaccess_coords, dataaccess)
dim(dataaccess_coords)
dim(dataaccess)
dim(dataaccessmerge)
colnames(dataaccessmerge)
unique(dataaccessmerge$layer)
dataaccessmerge12 <- subset(dataaccessmerge, layer > 0)
unique(dataaccessmerge$layer)
unique(dataaccessmerge12$layer)

head(dataaccessmerge12)
# CONVERT NUMBERS IN RASTER TO APPROPRIATE DESIGNATION
#dataaccessmerge12[dataaccessmerge12 == 1] <- "Data exists, but inaccessible"  #SURVEY ONLY
#dataaccessmerge12[dataaccessmerge12 == 2] <- "Attempting to make data open" #OBIS/GBIF ONLY
#dataaccessmerge12[dataaccessmerge12 == 3] <- "Accessible data" #OBIS/GBIF & Survey

#coloranal <- colortools::splitComp("cadetblue", plot=F, bg= "white")
coloranal2 <- c("#44AA99","#882255", "#CC6677")
#colorblind palette - https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

###FINAL FIGURE 3: Data Access
dataplot <- ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), color= "grey82", fill="grey89", alpha=1) +
  stat_summary_hex(data= dataaccessmerge12, aes(x=x, y=y, z=as.integer(layer)), show.legend = T, fun = "max", bins = c(360,720)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "transparent"), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + labs(fill = " ") + scale_fill_gradientn(colors = coloranal2, breaks=c(1,2,3),labels=c("Restricted data access","Attempting to make data accessible","Accessible data")) + guides(fill = guide_legend(reverse = TRUE)) + theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"), legend.position="bottom", legend.margin=margin(-5,0,0,0), legend.text=element_text(size=12)) #use this so that labels are in correct location 
dataplot

dataplot
ggsave(filename = "DataAccessMap.jpeg", width = 11, height = 6, device='jpeg', dpi=700)
# insert ggplot codes
dev.off()

#for pacific centered map but need to work on; https://github.com/valentinitnelav/valentinitnelav.github.io/blob/master/gallery/Pacific%20centered%20world%20map%20with%20ggplot.R


####2.8 MAP: FUNDING/ MANDATED & NON MANDATED: NO FIGURE-----
##Create raster of early system with a 0.5km/500m resolution
dim(mappingdfproviders)
colnames(mappingdfproviders)

#create mandated and non-mandated column
unique(mappingdfproviders$funder_sector_notmandatedgov)
mappingdfproviders$funder_sector_notmandatedgov_use <- ifelse(grepl("mandated", mappingdfproviders$funder_sector_notmandatedgov), "Policy/government: non", "NA")
unique(mappingdfproviders$funder_sector_notmandatedgov_use)

mappingdfproviders$funder_comb <- paste(mappingdfproviders$funder_sector_academia, mappingdfproviders$funder_sector_civilsociety,mappingdfproviders$funder_sector_industry,mappingdfproviders$funder_sector_notmandatedgov_use, mappingdfproviders$funder_sector_other, mappingdfproviders$funder_sector_mandatedgov) 
unique(mappingdfproviders$funder_comb)

mappingdfproviders$funder_mandated_nonmandated <- ifelse(grepl("mandated", mappingdfproviders$funder_comb), "mandated", "not mandated")
unique(mappingdfproviders$funder_mandated_nonmandated)
#table(mappingdfproviders$funder_mandated_nonmandated)
#View(mappingdfproviders$funder_mandated_nonmandated)

#mandated
mappingdfprovidersmandated <- subset(mappingdfproviders, funder_mandated_nonmandated == "mandated")


raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopenortrying$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondfman <- mappingdfprovidersmandated %>% filter(grepl("POLYGON", st_geometry_type(geometry)))

polygonrasterman<- fasterize(polygondfman, raster)
#plot(polygonrasteropentry)

##filter linestring to convert to raster
linedfman <- mappingdfprovidersmandated %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
linerasterman <- rasterize(linedfman, raster, field=1)
#plot(linerasteropentry)

##filter points to convert to raster
pointdfman <- mappingdfprovidersmandated %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coordsman <- do.call(rbind, st_geometry(pointdfman)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointrasterman <- rasterize(seal_coordsman, raster, field=1)
#plot(pointrasteropentry)

##combine all rasters
comb_rastman <- raster::merge(polygonrasterman, linerasterman)
rast_man <- raster::merge(comb_rastman, pointrasterman)
#before being cropped, includes all areas on land
rast_man

##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

#ocean <- gDifference(as(extent(final_rast), 'SpatialPolygons'), land) #remove 
#plot(ocean)
rast_man_mask <- raster::mask(x = rast_man, mask = ocean)
rast_man_crop_final <- raster::crop(x = rast_man_mask, y = extent(ocean))


##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

#nonmandated
mappingdfprovidersnotmandated <- subset(mappingdfproviders, funder_mandated_nonmandated == "not mandated")


raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopenortrying$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondfman <- mappingdfprovidersnotmandated %>% filter(grepl("POLYGON", st_geometry_type(geometry)))

polygonrasterman<- fasterize(polygondfman, raster)
#plot(polygonrasteropentry)

##filter linestring to convert to raster
linedfman <- mappingdfprovidersnotmandated %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
linerasterman <- rasterize(linedfman, raster, field=1)
#plot(linerasteropentry)

##filter points to convert to raster
pointdfman <- mappingdfprovidersnotmandated %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coordsman <- do.call(rbind, st_geometry(pointdfman)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointrasterman <- rasterize(seal_coordsman, raster, field=1)
#plot(pointrasteropentry)

##combine all rasters
comb_rastman <- raster::merge(polygonrasterman, linerasterman)
rast_nonman <- raster::merge(comb_rastman, pointrasterman)
#before being cropped, includes all areas on land
rast_nonman

##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

#ocean <- gDifference(as(extent(final_rast), 'SpatialPolygons'), land) #remove 
#plot(ocean)
rast_nonman_mask <- raster::mask(x = rast_nonman, mask = ocean)
rast_nonman_crop_final <- raster::crop(x = rast_nonman_mask, y = extent(ocean))


##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)


###combine data to get integrated map
rast_man_crop_final[rast_man_crop_final == 1] <- 1 #mandated = 1
rast_nonman_crop_final[rast_nonman_crop_final == 1] <- 2 #non-mandated = 2, #3 = both

unique(rast_man_crop_final$layer)
unique(rast_nonman_crop_final$layer)

datamannonmanrast <- raster::mosaic(rast_man_crop_final, rast_nonman_crop_final, fun = sum)
unique(datamannonmanrast$layer)

####AREA: MANDATED V. NOT: PREP FOR FIGURE 4a-----
##AREA OF ALL SAMPLING!
datamannonmanrast2 <- datamannonmanrast
datamannonmanrast2[datamannonmanrast2 < 1] <- NA

#get sizes of all cells in raster [km2]
cell_size<-area(datamannonmanrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
datamannonmanrast2_area<-length(cell_size)*median(cell_size)
datamannonmanrast2_area/411550230 * 100 #6% CHECKS OUT!


####2.9 AREA OF FUNDING/ MANDATED & NON MANDATED------
datamannonmanrasttest <- datamannonmanrast
#datamannonmanrasttest[datamannonmanrasttest > 0] <- NA
unique(datamannonmanrasttest$layer)
#tapply(area(datamannonmanrasttest), datamannonmanrasttest[], sum)
#482045029 + 3964559 + 19417185 + 2143081 
#482045029/507569854 * 100 #95 % none
#3964559/507569854 * 100 #0.8 man
#19417185/507569854 * 100 #3.8 non mand
#2143081/507569854 * 100 #0.4 both
#get sizes of all cells in raster [km2]
cell_size<-area(datamannonmanrasttest, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
datamannonmanrast2_area<-length(cell_size)*median(cell_size)
datamannonmanrast2_area/411550230 * 100 #6% CHECKS OUT!

##MANDATED: AREA
datamannonmanrast2 <- datamannonmanrast
datamannonmanrast2[datamannonmanrast2 < 1] <- NA
datamannonmanrast2[datamannonmanrast2 > 1] <- NA
#calculate area of regions under 0 m as
#get sizes of all cells under 0 m
cell_size<-area(datamannonmanrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
opendata_area<-length(cell_size)*median(cell_size)
opendata_area/411550230 * 100 #0.9% Mandated
####opendata_area over ocean area

##NON MANDATED DATA: AREA
datamannonmanrast2 <- datamannonmanrast
datamannonmanrast2[datamannonmanrast2 < 2] <- NA #remove values other than 2 because that is for open/try
datamannonmanrast2[datamannonmanrast2 > 2] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(datamannonmanrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
opentrydata_area<-length(cell_size)*median(cell_size)
opentrydata_area/411550230 * 100 #4.6% NOT MANDATED
####opendata_area over ocean area

##NON MANDATED OR MANDATED: BOTH
datamannonmanrast2 <- datamannonmanrast
datamannonmanrast2[datamannonmanrast2 < 3] <- NA #remove values other than 2 because that is for open/try
datamannonmanrast2[datamannonmanrast2 > 3] <- NA
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size<-area(datamannonmanrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in datarastopen
opentrydata_area<-length(cell_size)*median(cell_size)
opentrydata_area/411550230 * 100 #0.5% NOT MANDATED
####opendata_area over ocean area




###MAP: MANDATED & NON MANDATED MAP: FIGURE 4a-----
##convert raster back to points
datamannonman <- as.data.frame(rasterToPoints(datamannonmanrast))
colnames(datamannonman)
unique(datamannonman$layer)
#TO DO: PROBABLY SHOULD REMOVE ANY POINTS OVER LAND
dataman_sf <- st_as_sf(datamannonman, coords = c("x","y")) %>% 
  st_set_crs(leaflet:::epsg4326)
dataman_coords <- do.call(rbind, st_geometry(dataman_sf)) %>% 
  as_tibble() %>% setNames(c("x","y"))

datamanmerge <- merge(dataman_coords, datamannonman)
dim(dataman_coords)
dim(datamannonman)
dim(datamanmerge)
colnames(datamanmerge)
unique(datamanmerge$layer)
datamanmerge12 <- subset(datamanmerge, layer > 0)
unique(datamanmerge$layer)
unique(datamanmerge12$layer)

head(datamanmerge12)
# CONVERT NUMBERS IN RASTER TO APPROPRIATE DESIGNATION
#dataaccessmerge12[dataaccessmerge12 == 1] <- "mandated only" 
#dataaccessmerge12[dataaccessmerge12 == 2] <- "not mandated only " 
#dataaccessmerge12[dataaccessmerge12 == 3] <- "both" 

#coloranal <- colortools::splitComp("lightskyblue3", plot=F, bg= "white")
#coloranal
coloranal2 <- c("chocolate","darkseagreen", "darkslategrey")
###FINAL FIGURE: MANDATED V NOT
plotman <- ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)  +
  stat_summary_hex(data= datamanmerge12, aes(x=x, y=y, z=as.integer(layer)), show.legend = T, fun = "max", bins = c(180,360)) +
  theme_void() + labs(fill = " ") + scale_fill_gradientn(colors = coloranal2, breaks=c(1,2,3),labels=c("Mandated only","Non mandated only","Both: Mandated & Not Mandated ")) + guides(fill = guide_legend()) + theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"), legend.position="bottom", legend.margin=margin(-5,0,0,0), legend.text=element_text(size=12)) #use this so that labels are in correct location
plotman




####FINAL MAP & MAP: MANDATED & DATA ACCESS; FIGURE 4-------
plot_grid(plotman, dataplot, ncol = 1, scale = c(1,1), labels = c('a)', 'b)'))



###MAP OF ALL EOVs
##LAND
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

###MARINE MAMMALS RASTER
marinemammaldf <- subset(mappingdfproviders, Marine_Mammal_abundance_systematic == "Variable is/was collected systematically" | Marine_Mammal_distribution_systematic == "Variable is/was collected systematically")
#marinemammaldf <- subset(mappingdfproviders, Marine_Mammal_abundance_systematic == "Variable is/was collected systematically" | Marine_Mammal_distribution_systematic == "Variable is/was collected systematically" | Marine_Mammal_abundance_opportunistic == "Variable is/was collected opportunistically" | Marine_Mammal_distribution_opportunistic == "Variable is/was collected opportunistically")
dim(marinemammaldf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- marinemammaldf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- marinemammaldf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- marinemammaldf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_marinemammal <- raster::merge(comb_rast, pointraster)
plot(final_rast_marinemammal) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 


###BENTHIC INVERTS RASTER
benthicinvertdf <- subset(mappingdfproviders, Benthic_Inverts_abundance_systematic == "Variable is/was collected systematically" | Benthic_Inverts_distribution_systematic == "Variable is/was collected systematically" )
#benthicinvertdf <- subset(mappingdfproviders, Benthic_Inverts_abundance_systematic == "Variable is/was collected systematically" | Benthic_Inverts_distribution_systematic == "Variable is/was collected systematically" | Benthic_Inverts_abundance_opportunistic == "Variable is/was collected opportunistically" | Benthic_Inverts_distribution_opportunistic == "Variable is/was collected opportunistically")
dim(benthicinvertdf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- benthicinvertdf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- benthicinvertdf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- benthicinvertdf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_benthicinvert <- raster::merge(comb_rast, pointraster)
plot(final_rast_benthicinvert) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE)

###BIRD RASTER
birddf <- subset(mappingdfproviders, Bird_abundance_systematic == "Variable is/was collected systematically" | Bird_distribution_systematic == "Variable is/was collected systematically")
#birddf <- subset(mappingdfproviders, Bird_abundance_systematic == "Variable is/was collected systematically" | Bird_distribution_systematic == "Variable is/was collected systematically" | Bird_abundance_opportunistic == "Variable is/was collected opportunistically" | Bird_distribution_opportunistic == "Variable is/was collected opportunistically")
dim(birddf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- birddf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster- NO LINES FOR BIRDS
#linedf <- birddf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
#lineraster <- rasterize(linedf, raster, field=1)
#plot(lineraster)

##filter points to convert to raster
pointdf <- birddf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, pointraster)
final_rast_bird <- raster::merge(polygonraster, pointraster)
plot(final_rast_bird) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

###HARD CORAL RASTER
hardcoraldf <- subset(mappingdfproviders, Hard_Coral_cover_systematic == "Variable is/was collected systematically" | Hard_Coral_composition_systematic == "Variable is/was collected systematically")
#hardcoraldf <- subset(mappingdfproviders, Hard_Coral_cover_systematic == "Variable is/was collected systematically" | Hard_Coral_composition_systematic == "Variable is/was collected systematically" | Hard_Coral_cover_opportunistic == "Variable is/was collected opportunistically" | Hard_Coral_composition_opportunistic == "Variable is/was collected opportunistically")
dim(hardcoraldf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- hardcoraldf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- hardcoraldf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- hardcoraldf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_coral <- raster::merge(comb_rast, pointraster)
plot(final_rast_coral) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 


#READ IN CORAL DATASET & CONVERT TO RASTER- NOT WORKING! 
#final_rast_coral
#coraldist <- st_read(here::here("final_data_BioEco/Spatial files for habitats/Hard Coral/14_001_WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Pt_v4.shp"))
#head(coraldist)
#plot(coraldist)


#ext <- floor(extent(coraldist))
#rr <- raster(ext, res=0.5)
#coraldistrast <- rasterize(coraldist, rr, field=1)
#coraldistrast
#plot(coraldistrast)

###SAMPLING CORAL
#final_rast_coral; coraldistrastfinal
#final_rast_coral[final_rast_coral < 1] <- NA

#coral_mask <- raster::mask(x = coraldistrastfinal, mask = final_rast_coral)

#coralsamplingextrast <- crop(x = coral_mask, coraldistrastfinal)
#coralsamplingextrast
#plot(coralsamplingextrast)

#coralsamplingextrast2 <- coralsamplingextrast
#coralsamplingextrast2[coralsamplingextrast2 < 1] <- 1
#plot(coralsamplingextrast2)

#area sampled within the coral distribution
#cell_size<-area(coralsamplingextrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
#cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
#coralsampled_area<-length(cell_size)*median(cell_size)
#coralsampled_area #266139.8


#total area of the coral dist
#coraldistrastfinal
#plot(coraldistrastfinal)
#get sizes of all cells in raster [km2]- AREA OF sampled in 200m
#cell_size<-area(coraldistrastfinal, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
#cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
#coraldist_area<-length(cell_size)*median(cell_size)
#coraldist_area #5238377

#coralsampled_area/coraldist_area * 100 #IMPORTANT: 5% of coral distribution is sampled



###FISH RASTER
fishdf <- subset(mappingdfproviders, Fish_abundance_systematic == "Variable is/was collected systematically" | Fish_distribution_systematic == "Variable is/was collected systematically")
#fishdf <- subset(mappingdfproviders, Fish_abundance_systematic == "Variable is/was collected systematically" | Fish_distribution_systematic == "Variable is/was collected systematically" | Fish_abundance_opportunistic == "Variable is/was collected opportunistically" | Fish_distribution_opportunistic == "Variable is/was collected opportunistically")
dim(fishdf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- fishdf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- fishdf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- fishdf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_fish <- raster::merge(comb_rast, pointraster)
plot(final_rast_fish) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

###MACROALGAL RASTER
macroalgaldf <- subset(mappingdfproviders, Macroalgal_cover_systematic == "Variable is/was collected systematically" | Macroalgal_composition_systematic == "Variable is/was collected systematically")
#macroalgaldf <- subset(mappingdfproviders, Macroalgal_cover_systematic == "Variable is/was collected systematically" | Macroalgal_composition_systematic == "Variable is/was collected systematically" | Macroalgal_cover_opportunistic == "Variable is/was collected opportunistically" | Macroalgal_composition_opportunistic == "Variable is/was collected opportunistically")
dim(macroalgaldf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- macroalgaldf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- macroalgaldf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- macroalgaldf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_macroalgal <- raster::merge(comb_rast, pointraster)
plot(final_rast_macroalgal) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

###MANGROVE RASTER
mangrovedf <- subset(mappingdfproviders, Mangrove_cover_systematic == "Variable is/was collected systematically" | Mangrove_composition_systematic == "Variable is/was collected systematically")
#mangrovedf <- subset(mappingdfproviders, Mangrove_cover_systematic == "Variable is/was collected systematically" | Mangrove_composition_systematic == "Variable is/was collected systematically" | Mangrove_cover_opportunistic == "Variable is/was collected opportunistically" | Mangrove_composition_opportunistic == "Variable is/was collected opportunistically")
dim(mangrovedf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- mangrovedf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster- No LINES FOR MANGROVES
#linedf <- mangrovedf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
#lineraster <- rasterize(linedf, raster, field=1)
#plot(lineraster)

##filter points to convert to raster
pointdf <- mangrovedf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, pointraster)
final_rast_mangrove <- raster::merge(polygonraster, pointraster)
plot(final_rast_mangrove) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 



##READ IN MANGROVE DATASET & CONVERT TO RASTER- NOT WORKING! 
#final_rast_mangrove
#mangrovedist <- st_read(here::here("final_data_BioEco/Spatial files for habitats/Mangroves/GMW_001_GlobalMangroveWatch_2016/01_Data/GMW_2016_v2.shp"))
#head(mangrovedist)
#plot(mangrovedist)


#ext <- floor(extent(mangrovedist))
#rr <- raster(ext, res=0.5)
#mangrovedistrast <- rasterize(mangrovedist, rr, field=1)
#mangrovedistrast
#plot(mangrovedistrast)

#final_rast_mangrove: mangroves sampled
#mangrovedistrast: total mangrove distribution
#mangrovedistrast[mangrovedistrast > 1] <- NA
#plot(mangrovedistrast)
#crs(mangrovedistrast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#plot(final_rast_mangrove)
#final_rast_mangrove

#mangrovedistrastfinal <- projectRaster(mangrovedistrast,final_rast_mangrove, method = 'ngb') #neither method (eg ngb and bilinear) changes by much at all
#mangrovedistrastfinal
#plot(mangrovedistrastfinal)


###SAMPLING MANGROVE
#final_rast_mangrove; mangrovedistrastfinal
#final_rast_mangrove[final_rast_mangrove < 1] <- NA

#mangrove_mask <- raster::mask(x = mangrovedistrastfinal, mask = final_rast_mangrove)

#mangrovesamplingextrast <- crop(x = mangrove_mask, mangrovedistrastfinal)
#mangrovesamplingextrast
#plot(mangrovesamplingextrast)

#mangrovesamplingextrast2 <- mangrovesamplingextrast
#mangrovesamplingextrast2[mangrovesamplingextrast2 < 1] <- 1
#plot(mangrovesamplingextrast2)

#area sampled within the mangrove distribution
#cell_size<-area(mangrovesamplingextrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
#cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
#mangrovesampled_area<-length(cell_size)*median(cell_size)
#mangrovesampled_area #266139.8


#total area of the mangrove dist
#mangrovedistrastfinal
#plot(mangrovedistrastfinal)
#get sizes of all cells in raster [km2]- AREA OF sampled in 200m
#cell_size<-area(mangrovedistrastfinal, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
#cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
#mangrovedist_area<-length(cell_size)*median(cell_size)
#mangrovedist_area #5238377

#mangrovesampled_area/mangrovedist_area * 100 #IMPORTANT: 5% of mangrove distribution is sampled


###MICROBE RASTER
microbedf <- subset(mappingdfproviders, Microbe_biomass_systematic == "Variable is/was collected systematically" | Microbe_diversity_systematic == "Variable is/was collected systematically" )
#microbedf <- subset(mappingdfproviders, Microbe_biomass_systematic == "Variable is/was collected systematically" | Microbe_diversity_systematic == "Variable is/was collected systematically" | Microbe_biomass_opportunistic == "Variable is/was collected opportunistically" | Microbe_diversity_opportunistic == "Variable is/was collected opportunistically")
dim(microbedf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- microbedf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- microbedf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- microbedf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_microbe <- raster::merge(comb_rast, pointraster)
plot(final_rast_microbe) #before being cropped, includes all areas on land


###PHYTOPLANKTON RASTER
phytodf <- subset(mappingdfproviders, Phytoplankton_biomass_systematic == "Variable is/was collected systematically" | Phytoplankton_diversity_systematic == "Variable is/was collected systematically")
#phytodf <- subset(mappingdfproviders, Phytoplankton_biomass_systematic == "Variable is/was collected systematically" | Phytoplankton_diversity_systematic == "Variable is/was collected systematically" | Phytoplankton_biomass_opportunistic == "Variable is/was collected opportunistically" | Phytoplankton_diversity_opportunistic == "Variable is/was collected opportunistically")
dim(phytodf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- phytodf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- phytodf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- phytodf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_phyto <- raster::merge(comb_rast, pointraster)
plot(final_rast_phyto) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

###SEAGRASS RASTER
seagrassdf <- subset(mappingdfproviders, Seagrass_cover_systematic == "Variable is/was collected systematically" | Seagrass_composition_systematic == "Variable is/was collected systematically")
#seagrassdf <- subset(mappingdfproviders, Seagrass_cover_systematic == "Variable is/was collected systematically" | Seagrass_composition_systematic == "Variable is/was collected systematically" | Seagrass_cover_opportunistic == "Variable is/was collected opportunistically" | Seagrass_composition_opportunistic == "Variable is/was collected opportunistically")
dim(seagrassdf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- seagrassdf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster: NO LINESTRINGS FOR SEAGRASS
#linedf <- seagrassdf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
#lineraster <- rasterize(linedf, raster, field=1)
#plot(lineraster)

##filter points to convert to raster
pointdf <- seagrassdf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, pointraster)
final_rast_seagrass <- raster::merge(polygonraster, pointraster)
plot(final_rast_seagrass) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

##READ IN SEAGRASS DATASET & CONVERT TO RASTER
#seagrassdist <- st_read(here::here("final_data_BioEco/Spatial files for habitats/Seagrass/WCMC013_014_SeagrassPtPy2020_v7/014_001_WCMC013-014_SeagrassPtPy2020_v7/01_Data/WCMC_013_014_SeagrassesPt_v7.shp"))
#head(seagrassdist)


#ext <- floor(extent(seagrassdist))
#rr <- raster(ext, res=0.5)
#seagrassdistrast <- rasterize(seagrassdist, rr, field=1)
#plot(seagrassdistrast)

#final_rast_seagrass: seagrasss sampled
#seagrassdistrast: total seagrass distribution
#seagrassdistrast[seagrassdistrast > 1] <- NA
#plot(seagrassdistrast)
#crs(seagrassdistrast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#plot(final_rast_seagrass)
#final_rast_seagrass

#seagrassdistrastfinal <- projectRaster(seagrassdistrast,final_rast_seagrass, method = 'ngb') #neither method (eg ngb and bilinear) changes by much at all
#seagrassdistrastfinal
#plot(seagrassdistrastfinal)


###SAMPLING SEAGRASSS
#final_rast_seagrass; seagrassdistrastfinal
#final_rast_seagrass[final_rast_seagrass < 1] <- NA

#seagrass_mask <- raster::mask(x = seagrassdistrastfinal, mask = final_rast_seagrass)

#seagrasssamplingextrast <- crop(x = seagrass_mask, seagrassdistrastfinal)
#seagrasssamplingextrast
#plot(seagrasssamplingextrast)

#seagrasssamplingextrast2 <- seagrasssamplingextrast
#seagrasssamplingextrast2[seagrasssamplingextrast2 < 1] <- 1
#plot(seagrasssamplingextrast2)

#area sampled within the seagrass distribution
#cell_size<-area(seagrasssamplingextrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
#cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
#seagrasssampled_area<-length(cell_size)*median(cell_size)
#seagrasssampled_area #266139.8


#total area of the 200m globally
#seagrassdistrastfinal
#plot(seagrassdistrastfinal)
#get sizes of all cells in raster [km2]- AREA OF sampled in 200m
#cell_size<-area(seagrassdistrastfinal, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
#cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
#seagrassdist_area<-length(cell_size)*median(cell_size)
#seagrassdist_area #5238377

#seagrasssampled_area/seagrassdist_area * 100 #IMPORTANT: 5% of seagrass distribution is sampled




###SOUND RASTER
sounddf <- subset(mappingdfproviders, OceanSound_systematic == "Variable is/was collected systematically")
dim(sounddf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- sounddf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster: NO LINESTRINGS FOR SEAGRASS
#linedf <- sounddf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
#lineraster <- rasterize(linedf, raster, field=1)
#plot(lineraster)

##filter points to convert to raster
pointdf <- sounddf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, pointraster)
final_rast_sound <- raster::merge(polygonraster, pointraster)
plot(final_rast_sound) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

###TURTLE RASTER
turtledf <- subset(mappingdfproviders, Turtle_abundance_systematic == "Variable is/was collected systematically" | Turtle_distribution_systematic == "Variable is/was collected systematically")
#turtledf <- subset(mappingdfproviders, Turtle_abundance_systematic == "Variable is/was collected systematically" | Turtle_distribution_systematic == "Variable is/was collected systematically" | Turtle_abundance_opportunistic == "Variable is/was collected opportunistically" | Turtle_distribution_opportunistic == "Variable is/was collected opportunistically")
dim(turtledf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- turtledf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- turtledf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- turtledf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_turtle <- raster::merge(comb_rast, pointraster)
plot(final_rast_turtle) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

###ZOOPLANKTON RASTER
zoodf <- subset(mappingdfproviders, Zooplankton_biomass_systematic == "Variable is/was collected systematically" | Zooplankton_diversity_systematic == "Variable is/was collected systematically")
#zoodf <- subset(mappingdfproviders, Zooplankton_biomass_systematic == "Variable is/was collected systematically" | Zooplankton_diversity_systematic == "Variable is/was collected systematically" | Zooplankton_biomass_opportunistic == "Variable is/was collected opportunistically" | Zooplankton_diversity_opportunistic == "Variable is/was collected opportunistically")
dim(zoodf)
head(zoodf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- zoodf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- zoodf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- zoodf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_zoo <- raster::merge(comb_rast, pointraster)
plot(final_rast_zoo) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 

#COMBINE ALL EOV RASTERS
##crop all EOV rasters for ocean only
alleovraster <- mosaic(final_rast_marinemammal, final_rast_benthicinvert, final_rast_bird, final_rast_coral, final_rast_fish, final_rast_macroalgal, final_rast_mangrove, final_rast_microbe, final_rast_phyto, final_rast_seagrass,final_rast_sound, final_rast_turtle, final_rast_zoo, fun=sum)
#plot(alleovraster)

##AT THE END OF ALL EOVS
##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

#ocean <- gDifference(as(extent(final_rast), 'SpatialPolygons'), land) #remove 
#plot(ocean)
alleovrastermask <- mask(alleovraster, ocean)
alleovraster_crop_final <- crop(x = alleovrastermask, y = extent(ocean))

##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)

## plot the monitoring with the world map
#cuts=c(1,2,3,4,5,6,7,8,9,10,11,12) #set breaks
#pal <- colorRampPalette(c("lightblue","yellow","red"))

#plot(alleovraster_crop_final, legend = T, breaks=cuts, col = pal(12))
#plot(land, col = "grey", add = TRUE) 



##CALCULATE AREAS FOR DIFFERENT # of EOVS
areaeov <- areaeov[0,0] #clear DF
areaeov <- NULL
for (i in 1:11){
  progress(i, max.value = 10, progress.bar = T)
  alleovrast <- alleovraster_crop_final
  alleovrast[alleovrast < i] <- NA #remove values other than 1 because that is for restricted
  alleovrast[alleovrast > i] <- NA
  #calculate area of regions under 0 m asl
  #get sizes of all cells under 0 m
  cell_size<-area(alleovrast, na.rm=TRUE, weights=FALSE)
  #delete NAs from vector of all raster cells
  ##NAs lie outside of the rastered region, can thus be omitted
  cell_size<-cell_size[!is.na(cell_size)]
  #compute area [km2] of all cells in datarastopen
  eov_area<-length(cell_size)*median(cell_size)
  perofocean <- eov_area/411550230 * 100 #1.08% RESTRICTED?
  areaeov <- rbind(areaeov, data.frame(i, perofocean))
}
View(areaeov)
sum(areaeov$perofocean)
sum(areaeov$perofocean[c(3:13)]) #sum of 3 or more EOVS
sum(areaeov$perofocean[c(5:13)]) #sum of 5 or more EOVS
sum(areaeov$perofocean[c(7:13)]) #sum of 7 or more EOVS


##add zeros to data frame for other EOVs
areaeov[12,1] <- 12 
areaeov[12,2] <- 0 
areaeov[13,1] <- 13
areaeov[13,2] <- 0 

###plot the number of EOVs measured by the surface area % of ocean covered
ggplot(data =areaeov, aes(y= areaeov$perofocean, x= areaeov$i)) + labs(y = "percent of surface ocean covered (%)", x = "Number of EOVs sampled") +geom_point()+geom_smooth(color="black") + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13)) + theme_classic()

###EOV MAP: HEXAGON
alleovraster_crop_final2 <- alleovraster_crop_final
alleovraster_crop_final2[alleovraster_crop_final2 < 1] <- NA #remove 0/1s

unique(alleovraster_crop_final2$layer)
class(alleovraster_crop_final2$layer)

eovall12 <- as.data.frame(rasterToPoints(alleovraster_crop_final2))

# Get the world polygon
world <- map_data("world")

head(eovall12)

eovall12$layer <- as.numeric(eovall12$layer) #use as.character instead of as.factor because for some reason it is scaling the 10s very large when using as.factor??

#colortools::pals("fish") #get colors from color palette
#get colors from color palette
library(scales)
show_col(viridis_pal()(10))

#cols <- c("1" = "#69D2E7","3" = "#A7DBD8", "5" = "#E0E4CC" , "7" = "#F38630", "9" = "#FA6900")
cols <- c("1" = "#440154FF","3" = "#3E4A89FF", "5" = "#31688EFF" , "7" = "#1F9E89FF", "9" = "#FDE725FF")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  stat_summary_hex(data= eovall12, aes(x=x, y=y, z=as.integer(layer)), show.legend = T, fun = "max", bins = c(180,360)) +
  theme_void() + labs(fill = "Biological EOVs measured") + scale_fill_gradientn(colors = cols ,limits = c(0, 13), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13),labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13)) + guides(fill = guide_legend()) #use this so that labels are in correct location


###BATHYMETRIC DATA: GEBCO
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2)

depth_raster <- raster(here::here("final_data_BioEco/gebco_2020_netcdf/GEBCO_2020.nc"),  varname = "elevation")
proj4string(depth_raster)=CRS("+init=EPSG:4326")
depth_raster

fasterAgg.Fun <- function(x,...) {
  myRle.Alt <- function (x1) {
    n1 <- length(x1)
    y1 <- x1[-1L] != x1[-n1]
    i <- c(which(y1), n1)
    which.max(diff(c(0L, i)))
  }
  
  if (sum(x)==0) {
    return(NA)
  } else {
    myRle.Alt(sort(x, method="quick"))
  }
}


#depth_ag_rast <- aggregate(depth_raster, fact=100, fun= fasterAgg.Fun, na.rm=TRUE) #mmaybe faster?
#depth_ag_rast <- aggregate(depth_raster, fact=100, fun= mean, na.rm=TRUE) #NOTE: This line of code takes an exceptionally long time so possibly write raster and import once data finalized
#export raster
#writeRaster(depth_ag_rast, "depth_aggregated_raster")
#import raster
depth_ag_rast2 <- raster("depth_aggregated_raster.grd")
plot(depth_ag_rast2)
depth_ag_rast2

depth_ag_0m <- depth_ag_rast2
depth_ag_0m[depth_ag_0m > 0] <- NA



test <- rasterToPoints(depth_ag_0m)
head(test)
test2 <- as.data.frame(test)
class(test2)
plot(test2$Elevation.relative.to.sea.level ~ test2$x)

#ggplot(test2, aes(x=test2$x, y=test2$Elevation.relative.to.sea.level)) + geom_point()
ggplot(test2, aes(x, Elevation.relative.to.sea.level)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") 

ggplot(test2, aes(y, Elevation.relative.to.sea.level)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") 


finaldepthrast0m <- projectRaster(depth_ag_0m, alldatatypesraster, method = 'ngb') #neither method (eg ngb and bilinear) changes by much at all
finaldepthrast0m
plot(finaldepthrast0m)

alldatatypesraster[alldatatypesraster < 1] <- NA

depth_mask1 <- raster::mask(x = finaldepthrast0m, mask = alldatatypesraster)
finaldepthextrast <- crop(x = depth_mask1, alldatatypesraster)
finaldepthextrast
plot(finaldepthextrast)

histsamp <- hist(finaldepthextrast)
histocean <-hist(finaldepthrast0m)

plot(finaldepthextrast)
depthpts <- rasterToPoints(finaldepthextrast, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
depthpts2 <- spTransform(depthpts, CRS(llprj))
print(depthpts2)
depthsamp <- as.data.frame(depthpts2)
head(depthsamp)
#histograms of RANDOM sampling


plot(finaldepthrast0m)
depthptsall <- rasterToPoints(finaldepthrast0m, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
depthptsall2 <- spTransform(depthptsall, CRS(llprj))
print(depthptsall2)
depthall <- as.data.frame(depthptsall2)
head(depthall)

hist(depthall$Elevation.relative.to.sea.level)
hist(depthsamp$Elevation.relative.to.sea.level)

ks.test(depthall$Elevation.relative.to.sea.level, depthsamp$Elevation.relative.to.sea.level)

depthall$e <- 'entire ocean'
depthsamp$e <- 'sampled'

combo <- rbind(depthall, depthsamp)
head(combo)

class(depthall)

require(scales)
#histogram of depth 
combo$Depth <- combo$Elevation.relative.to.sea.level * -1
depthhist <- ggplot(combo, aes(Depth, fill=e, colour=e)) +
  geom_histogram(aes(y=2*(..density..)/sum(..density..)),  alpha=0.2, 
                 position="identity", lwd=0.2) + scale_y_continuous(labels = scales::percent_format()) + ggtitle("") + xlab("Depth (m)") + ylab("Percent") + theme(legend.title = element_blank(), panel.background=element_blank()) + scale_fill_manual(values=c("#44AA99","#DDCC77")) + scale_color_manual(values=c("#44AA99","#DDCC77"))
depthhist

###FINAL FIGURE Create combined distribution plot
allhistplots <- ggarrange(lathist, depthhist, labels = c("a", "b"), common.legend = TRUE, legend = "bottom", nrow=1, ncol=2)
allhistplots

ggsave(filename = "FINALFIGURE_allhistplots.jpeg", width = 8, height = 4, device='jpeg', dpi=700)
# insert ggplot codes
dev.off()


###CREATE NERITIC RASTER (0m to 200m)
depth_ag_200m <- depth_ag_rast2
depth_ag_200m[depth_ag_200m < -200] <- NA #remove all cells deeper than 200m depth (-200m)
depth_ag_200m[depth_ag_200m > 1] <- NA #remove all cells higher than sea level (+1m)
plot(depth_ag_200m)

#change rast_all_active for 
finaldepthrast <- projectRaster(depth_ag_rast2, rast_all_active, method = 'ngb') #neither method (eg ngb and bilinear) changes by much at all
finaldepthrast
plot(finaldepthrast)

finaldepth200mrast <- projectRaster(depth_ag_200m, rast_all_active, method = 'ngb') #neither method (eg ngb and bilinear) changes by much at all
finaldepth200mrast
plot(finaldepth200mrast)

#total area of the 200m globally
finaldepth200mrast
#get sizes of all cells in raster [km2]- AREA OF sampled in 200m
cell_size<-raster::area(finaldepth200mrast, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
finaldepth200mrast2_area<-length(cell_size)*median(cell_size)
finaldepth200mrast2_area #22238165 area of the 200m zone

###RASTER OF HABITATS ONLY 
colnames(mappingdfproviders)

###HABITAT RASTER (Coral, Macroalgae, Mangrove, Seagrass)
habitatnearshoredf <- subset(mappingdfproviders, Hard_Coral_cover_systematic == "Variable is/was collected systematically" | Hard_Coral_composition_systematic == "Variable is/was collected systematically" | Macroalgal_cover_systematic == "Variable is/was collected systematically" | Macroalgal_composition_systematic == "Variable is/was collected systematically" | Mangrove_cover_systematic == "Variable is/was collected systematically" | Mangrove_composition_systematic == "Variable is/was collected systematically" | Seagrass_cover_systematic == "Variable is/was collected systematically" | Seagrass_composition_systematic == "Variable is/was collected systematically")
dim(habitatnearshoredf)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326)

##figure out what geometries exist
#View(st_geometry_type(mappingdfprovidersopen$geometry)) #POINTS, POLYGONS, LINESTRINGS

##filter polygons to convert to raster
polygondf <- habitatnearshoredf %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
polygonraster <- fasterize(polygondf, raster)
plot(polygonraster)

##filter linestring to convert to raster
linedf <- habitatnearshoredf %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
#View(st_geometry_type(linedf$geometry))
lineraster <- rasterize(linedf, raster, field=1)
plot(lineraster)

##filter points to convert to raster
pointdf <- habitatnearshoredf %>% filter(grepl("POINT", st_geometry_type(geometry)))
#View(st_geometry_type(pointdf$geometry))
seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
pointraster <- rasterize(seal_coords, raster, field=1)
plot(pointraster)

##combine all rasters
all_my_rasts <- c(polygonraster, lineraster, pointraster)
comb_rast <- raster::merge(polygonraster, lineraster)
final_rast_habitat <- raster::merge(comb_rast, pointraster)
plot(final_rast_habitat) #before being cropped, includes all areas on land
plot(land, col = "grey", add = TRUE) 


###TESTING FOR THE REPRESENTATIVENESS OF SAMPLING IN HABITATS
require(rasterVis)
#raster of all samples
final_rast_habitat
plot(final_rast_habitat)
#convert raster to points to be able to plot lat and lon histograms
spts <- rasterToPoints(final_rast_habitat, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts <- spTransform(spts, CRS(llprj))
print(llpts)
habx <- as.data.frame(llpts)
head(habx)

#histograms of the sampling
histogram(habx$x)
histogram(habx$y)

plot(finaldepth200mrast)
finaldepth200mrast
finaldepth200mrast2 <- finaldepth200mrast
finaldepth200mrast2
plot(finaldepth200mrast2)

values(finaldepth200mrast2)[values(finaldepth200mrast2) > -1] = NA
values(finaldepth200mrast2)[values(finaldepth200mrast2) < -1] = 1

plot(finaldepth200mrast2)
finaldepth200mrast2


#Take a random sample from the cell values of a Raster* object- use 0.06 to get 6% of the cells 
nearshorerastrandom <- sampleRandom(finaldepth200mrast2, ncell(finaldepth200mrast2)*.006, asRaster=TRUE)
nearshorerastrandom
plot(nearshorerastrandom)
#convert raster to points to be able to plot lat and lon histograms
spts <- rasterToPoints(nearshorerastrandom, spatial = TRUE)
library(rgdal)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
llpts <- spTransform(spts, CRS(llprj))
print(llpts)
samplex <- as.data.frame(llpts)
head(samplex)
#histograms of RANDOM sampling
histogram(samplex$x)
histogram(samplex$y) 

#Kolmogorov-Smirnov test for distributions
ks.test(samplex$x, habx$x) # Kolmogorov-Smirnov test statistic = D & significance level
ks.test(samplex$y, habx$y)







###GET PERCENTAGE OF **HABITAT ONLY SAMPLING** IN 200m depth (shelf)
##final_rast_habitat (just Habitat EOVs) ; rast_all_active (all EOVs)
final_rast_habitat[final_rast_habitat < 1] <- NA
plot(final_rast_habitat)


depth_mask200 <- raster::mask(x = finaldepth200mrast, mask = final_rast_habitat)
finaldepthextrast <- crop(x = depth_mask200, final_rast_habitat)
finaldepthextrast
plot(finaldepthextrast)

finaldepthextrast2 <- finaldepthextrast
finaldepthextrast2[finaldepthextrast2 < 1] <- 1
plot(finaldepthextrast2)

#get sizes of all cells in raster [km2]- AREA OF sampled in 200m
cell_size<-area(finaldepthextrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
finaldepthextrast2_area<-length(cell_size)*median(cell_size)
finaldepthextrast2_area #1402839 is the area of the sampled area of HABITAT EOVS within the 200m zone
#4506000 is the area of the sampled area of ALL EOVS within the 200m zone
finaldepthextrast2_area/finaldepth200mrast2_area * 100 #IMPORTANT: 6% of the shelf area is sampled by long-term biological observing programs sampling HABITAT EOVs

###GET PERCENTAGE OF *ALL EOV SAMPLING* IN 200m depth (shelf)
##final_rast_habitat (just Habitat EOVs) ; rast_all_active (all EOVs)
rast_all_active[rast_all_active < 1] <- NA

depth_mask200 <- raster::mask(x = finaldepth200mrast, mask = rast_all_active)
finaldepthextrast <- crop(x = depth_mask200, rast_all_active)
finaldepthextrast
plot(finaldepthextrast)

finaldepthextrast2 <- finaldepthextrast
finaldepthextrast2[finaldepthextrast2 < 1] <- 1
plot(finaldepthextrast2)

#get sizes of all cells in raster [km2]- AREA OF sampled in 200m
cell_size<-area(finaldepthextrast2, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster
finaldepthextrast2_area<-length(cell_size)*median(cell_size)
finaldepthextrast2_area #1402839 is the area of the sampled area of HABITAT EOVS within the 200m zone
#4506000 is the area of the sampled area of ALL EOVS within the 200m zone
finaldepthextrast2_area/finaldepth200mrast2_area * 100 #IMPORTANT: 20% of the shelf area is sampled by long-term biological observing programs sampling ALL EOVs


###GET AVERAGE DEPTH OF SAMPLED AREAS
rast_all_active[rast_all_active < 1] <- NA

depth_mask1 <- mask(x = finaldepthrast, mask = rast_all_active)
finaldepthextrast <- crop(x = depth_mask1, rast_all_active)
finaldepthextrast
plot(finaldepthextrast)

hist(finaldepthextrast)

##crop raster to ocean
actocean <- mask(x = rast_all_active, mask = ocean)
actrastcrop <- crop(x = actocean, y = ocean)
plot(actrastcrop)
##since there are some points still on land take out land
class(land)
landrast <- rasterize(land, actrastcrop)
yesrastfinal <- overlay(actrastcrop, landrast, fun = function(x, y) {
  x[!is.na(y[])] <- NA
  return(x)
})

plot(yesrastfinal)

finaldepthextrast2 <- projectRaster(finaldepthextrast,yesrastfinal, method = 'bilinear')
depth_mask2 <- mask(x = finaldepthextrast2, mask = yesrastfinal)
finaldepthextrast3<- crop(x = depth_mask2, yesrastfinal)
finaldepthextrast3
plot(finaldepthextrast3) #this should look like sampling areas with depths

depthdf <- as.data.frame(finaldepthextrast3, na.rm=T)
colnames(depthdf)
#summary(depthdf$Elevation.relative.to.sea.level)

###GET AVERAGE DEPTH OF NON-SAMPLED AREAS
finaldepthextrast <- crop(x = finaldepthrast, extent(rast_all_active))
finaldepthextrast

rast_all_active_non <- rast_all_active
rast_all_active_non[is.na(rast_all_active_non[])] <- 2 
rast_all_active_non[rast_all_active_non < 2] <- NA
plot(rast_all_active_non)

##crop raster to ocean
nonocean <- mask(x = rast_all_active_non, mask = ocean)
nonrastcrop <- crop(x = nonocean, y = ocean)
plot(nonrastcrop)
##since there are some points still on land take out land
class(land)
landrast <- rasterize(land, nonrastcrop)
nonrastfinal <- overlay(nonrastcrop, landrast, fun = function(x, y) {
  x[!is.na(y[])] <- NA
  return(x)
})

plot(nonrastfinal)
nonrastfinal

nonrastfinal2 <- projectRaster(nonrastfinal, finaldepthextrast)

depth_mask1 <- mask(x = finaldepthextrast, mask = nonrastfinal2)
nonactrastcrop <- crop(x = depth_mask1, y = nonrastfinal2)
plot(nonactrastcrop)

nodepthdf <- as.data.frame(nonactrastcrop, na.rm=T)
colnames(nodepthdf)
summary(nodepthdf$Elevation.relative.to.sea.level)

nodepthdf$condition <- "Not sampled"
depthdf$condition <- "Sampled"

dfcomb <- rbind(depthdf, nodepthdf)
colnames(dfcomb)
dfcomb1 <- subset(dfcomb, Elevation.relative.to.sea.level < 0) # keep only depths, not elevations
library(reshape2)
dfcomb2 <- melt(dfcomb1, id.var="condition")
colnames(dfcomb2)
#ggplot(dfcomb2, aes(x=condition, y=value)) + geom_boxplot(aes(fill=variable))


###PLOT: 
head(dfcomb2)
library(plyr)
mdf <- plyr::ddply(dfcomb2, .(condition), summarise, se = sd( value )/sqrt(length(value)), depth = mean(value)) 
View(mdf)
colnames(mdf)

summary(lm(dfcomb2$value ~ dfcomb2$condition))
t.test(dfcomb2$value ~ dfcomb2$condition)

#FINAL PLOT FOR DEPTH SAMPLED: MEAN + SE pplot
condition2 <- factor(mdf$condition, levels = c("Sampled", "Not sampled"))
ggplot(mdf, aes(x=condition2, y=depth, colour=condition2)) + 
  geom_errorbar(aes(ymin=depth-se, ymax=depth+se), width=.1) +
  geom_line() +
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "none") +
  labs(x= "", y= "depth (m)") +
  ylim(-4000, 0) +
  scale_color_manual(values=c("darkorange3", "dodgerblue3"))



#dfcomb2$condition2 <- factor(dfcomb2$condition, levels = c("Sampled", "Not sampled"))

#mu <- ddply(dfcomb2, "condition2", summarise, grp.mean=mean(value))
#ggplot(dfcomb2, aes(x=value, color=condition2)) +
#geom_histogram(fill="white", position="dodge", binwidth = 500)+
#geom_vline(data=mu, aes(xintercept=grp.mean, color=condition2), linetype="dashed") +
#labs(x= "", y= "depth (m)") +
# theme(legend.position="top") 

##Histograms
hist(dfcomb2$value)

dfsamp <- subset(dfcomb2, condition == "Sampled")
hist(dfsamp$value)

dfnotsamp <- subset(dfcomb2, condition == "Not sampled")
hist(dfnotsamp$value)



###BIOLOGY AND CONCURRENT PHYSICAL SAMPLING MAP (Na omit not working!)
outvalidmappingdf <- st_make_valid(mappingdfproviders)
outvalidmappingdfNOSF <- outvalidmappingdf
st_geometry(outvalidmappingdfNOSF) <- NULL

ET_stack <- list()

for(i in names(outvalidmappingdfNOSF[,c(223:242)])){
  mappingdfprovidersphys <- outvalidmappingdf %>% drop_na(i)
  dim(outvalidmappingdf)
  dim(mappingdfprovidersphys)
  raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                   resolution=0.5, crs=leaflet:::epsg4326)
  
  ##filter polygons to convert to raster
  polygondf <- mappingdfprovidersphys %>% filter(grepl("POLYGON", st_geometry_type(geometry)))
  polygonraster <- rasterize(polygondf, raster)
  
  ##filter linestring to convert to raster
  linedf <- mappingdfprovidersphys %>% filter(grepl("LINESTRING", st_geometry_type(geometry)))
  #View(st_geometry_type(linedf$geometry))
  lineraster <- rasterize(linedf, raster)
  
  ##filter points to convert to raster
  pointdf <- mappingdfprovidersphys %>% filter(grepl("POINT", st_geometry_type(geometry)))
  #View(st_geometry_type(pointdf$geometry))
  seal_coords <- do.call(rbind, st_geometry(pointdf)) %>% 
    as_tibble() %>% setNames(c("lon","lat"))
  pointraster <- rasterize(seal_coords, raster)
  
  ##combine all rasters to get locations with concurrent physical + biological data only
  all_my_rasts <- c(polygonraster, lineraster, pointraster)
  comb_rast <- raster::merge(polygonraster, lineraster)
  final_rast_physical <- raster::merge(comb_rast, pointraster)
  final_rast_physical[final_rast_physical > 0] <- 1
  final_rast_physical[final_rast_physical < 1] <- 0
  ET_stack[[i]] <- final_rast_physical
}
##error message: Error in st_cast_sfc_default(x) : list item(s) not of class sfg but looks like still works?
ET_stack
ET_stackfinal <- brick(ET_stack)
ET_stackfinal
class(ET_stackfinal)
rs1 <- calc(ET_stackfinal, sum, na.rm= T)
rs1
class(rs1)
plot(rs1)


##CROP RASTER TO ONLY KEEP POINTS IN THE OCEAN
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
ocean <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_ocean",
                 stringsAsFactors=FALSE, verbose=FALSE)

raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

#ocean <- gDifference(as(extent(final_rast), 'SpatialPolygons'), land) #remove 
#plot(ocean)
physicalrastermask <- mask(rs1, ocean)
physicalraster_crop_final <- crop(x = physicalrastermask, y = extent(ocean))

##Get land
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)
fils <- unzip(fil)
land <- readOGR(grep("shp$", fils, value=TRUE), "ne_110m_land",
                stringsAsFactors=FALSE, verbose=FALSE)


###PHYSICAL AND BIOLOGICAL MAP: HEXAGON
physicalraster_crop_final2 <- physicalraster_crop_final
physicalraster_crop_final2[physicalraster_crop_final2 < 1] <- NA #remove 0s

unique(physicalraster_crop_final2$layer)
class(physicalraster_crop_final2$layer)

physical12 <- as.data.frame(rasterToPoints(physicalraster_crop_final2))

# Get the world polygon
world <- map_data("world")

head(physical12)



###FINAL FIGURE: Concurrent Physical data
#cols <- c("1" = "#440154FF","3" = "#3E4A89FF", "5" = "#31688EFF" , "7" = "#1F9E89FF", "9" = "#FDE725FF")
#viridis(option="plasma", n=14)
cols <- c("1" = "#000004FF","3" = "#5801A4FF", "5" = "#930FA3FF" , "7" = "#C23C81FF", "9" = "#E4695EFF")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  stat_summary_hex(data= physical12, aes(x=x, y=y, z=as.integer(layer)), show.legend = T, fun = "max", bins = c(180,360)) + theme_void() + labs(fill = "Physical EOVs measured") + scale_fill_gradientn(colors = cols ,limits = c(0, 14), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) + guides(fill = guide_legend()) #use this so that labels are in correct location



##### EEZ by Human Development Index
#### EEZ

###FIGURE OUT ANY SURVEY SAMPLING (1 or more EOVs)
#To cite this product: Flanders Marine Institute (2018). The intersect of the Exclusive Economic Zones and IHO sea areas, version 3. Available online at https://www.marineregions.org/. https://doi.org/10.14284/324
##from: https://www.marineregions.org/downloads.php version 3
# Load EEZ polygons
eezs <- st_read(here::here('final_data_BioEco/World_EEZ_v11_20191118/eez_v11.shp'))
colnames(eezs)
eez200nm <- eezs %>% filter(POL_TYPE == '200NM') # select the 200 nautical mile polygon layer
class(eez200nm)
dim(eez200nm)
colnames(eez200nm)
length(unique(eez200nm$SOVEREIGN1))
dim(eez200nm)
##figure out what geometries exist
View(st_geometry_type(eez200nm$geometry))

##filter polygons to convert to raster: EEZ
raster <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                 resolution=0.5, crs=leaflet:::epsg4326) #epsg4326

class(eez200nm)
dim(eez200nm)
colnames(eez200nm)

###FIND AREA OF EEZS
list <- eez200nm$SOVEREIGN1
testfun <- function (i) {
  #progress(i, max.value = 230, progress.bar = T)
  test <- eez200nm[i,]
  eez200nmrast <- rasterize(test, raster, field = 1)
  eez200nmrast2 <- eez200nmrast
  eez200nmrast2[eez200nmrast2 < 1] <- NA #find area of the EEZ
  eez200nmrast2[eez200nmrast2 > 1] <- NA
  cell_size<-area(eez200nmrast2, na.rm=TRUE, weights=FALSE)
  #delete NAs from vector of all raster cells
  ##NAs lie outside of the rastered region, can thus be omitted
  cell_size <- cell_size[!is.na(cell_size)]
  #compute area [km2] of all cells in datarastopen
  eez200nmrast2_rast_area <- length(cell_size) * median(cell_size)
  eez200nmrast2_rast_area ###OUTPUT AS AREA OF EEZ
}
EEZ200arearast <- pblapply(1:length(list), testfun)
EEZ200arearast[sapply(EEZ200arearast, function(x) length(x)==0)] <- NA #convert integer 0s to NAs in R
EEZ200arealist <- unlist(EEZ200arearast)
View(EEZ200arealist)

EEZ200areadf <- as.data.frame(cbind(EEZ200arealist, as.character(list)))
View(EEZ200areadf)


###FIND AREA OF ACTIVE SURVEY SAMPLING IN SURVEY + OBIS & GBIF
list <- eez200nm$SOVEREIGN1
raster1 <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                  resolution=0.5, crs=leaflet:::epsg4326)

rast_all_active_obis_gbif_reproj <- projectRaster(surveygbifobisraster_crop_final, raster1)
testfun2 <- function (i) {
  #progress(i, max.value = 230, progress.bar = T)
  test <- eez200nm[i,]
  eez200nmrast <- rasterize(test, raster1, field = 1)
  eez200nmrast2 <- eez200nmrast
  eez200nmrast2[eez200nmrast2 < 1] <- NA #find area of the EEZ
  eez200nmrast2[eez200nmrast2 > 1] <- NA
  cell_size<-area(eez200nmrast2, na.rm=TRUE, weights=FALSE)
  #delete NAs from vector of all raster cells
  ##NAs lie outside of the rastered region, can thus be omitted
  cell_size<-cell_size[!is.na(cell_size)]
  #compute area [km2] of all cells in datarastopen
  eez200nm_rast_area<-length(cell_size)*median(cell_size)
  rast_all_active_mask1 <- mask(x = rast_all_active_obis_gbif_reproj, mask = eez200nmrast)
  allactrastcrop <- crop(x = rast_all_active_mask1, y = eez200nmrast)
  allactrastcrop[allactrastcrop < 1] <- NA #find area of the EEZ
  allactrastcrop[allactrastcrop > 1] <- NA
  cell_size<-area(allactrastcrop, na.rm=TRUE, weights=FALSE)
  #delete NAs from vector of all raster cells
  ##NAs lie outside of the rastered region, can thus be omitted
  cell_size<-cell_size[!is.na(cell_size)]
  #compute area [km2] of all cells in datarastopen
  allactrastcrop_area200 <-length(cell_size)*median(cell_size)
  allactrastcrop_area200 ###OUTPUT AS AREA OF ACTIVE RASTER IN EEZ
}
activearea200inrast <- pblapply(1:length(list), testfun2)
activearea200inrast[sapply(activearea200inrast, function(x) length(x)==0)] <- NA #convert integer 0s to NAs in R
SurveyOBISGBIFOutput <- unlist(activearea200inrast)
#View(SurveyOutput)
surveyobisgbifoutputdf <- as.data.frame(cbind(SurveyOBISGBIFOutput, as.character(list)))
#View(surveyoutputdf)
View(surveyobisgbifoutputdf)

colnames(EEZ200areadf)
View(EEZ200areadf)
EEZ200areadfNA <- EEZ200areadf[!is.na(EEZ200areadf$EEZ200arealist),]
View(EEZ200areadfNA)
EEZ200areadf$EEZ200arealist <- as.numeric(as.character(EEZ200areadf$EEZ200arealist))
EEZ200areadfNAag <- aggregate(EEZ200arealist~V2,data=EEZ200areadf,FUN=sum) #aggregate all territories
View(EEZ200areadfNAag) 

colnames(surveyobisgbifoutputdf)
surveyobisgbifoutputdf$SurveyOBISGBIFOutput <- as.numeric(as.character(surveyobisgbifoutputdf$SurveyOBISGBIFOutput))
surveyobisgbifoutputdf[is.na(surveyobisgbifoutputdf)] <- 0
View(surveyobisgbifoutputdf)
surveyobisgbifoutputdfag <- aggregate(SurveyOBISGBIFOutput~V2,data=surveyobisgbifoutputdf,FUN=sum) #aggregate all territories
View(surveyobisgbifoutputdfag)

areaeez200 <- merge(EEZ200areadfNAag,surveyobisgbifoutputdfag) ###dataframe of areas of EEZs and active survey & obis/gbif sampling from survey
colnames(areaeez200)
View(areaeez200)
zerotest <- subset(areaeez200, SurveyOBISGBIFOutput == 0)
length(unique(zerotest$V2))
View(unique(zerotest$V2))

###HUMAN DEVELOPMENT INDEX
###Accessed on Aug 31 2020; https://data.un.org/DocumentData.aspx?q=human+development+index&id=418

###read in HDI
##csv file comes from 1InitialSurveyCleanandSpatialData
hdicsv <- here::here("final_data_BioEco/HumanDevelopmentIndex2018_UNEP.csv")
#hdicsvkey <- here::here("final_data_BioEco/HumanDevelopmentIndex_Key_Countries_EEZ.csv") #likely don't need this
# read csv
hdidf <- read_csv(hdicsv)
#hdikey <- read_csv(hdicsvkey)

##MATCH WITH SOVERIGN
colnames(hdidf)
View(hdidf)
View(areaeez200)
areaeez200$V2 <- as.character(areaeez200$V2)
areaeez200$Country <- areaeez200$V2
hdifinal <- merge(areaeez200, hdidf)
View(hdifinal)
colnames(hdifinal)

hdifinal$ratio <- (as.numeric(as.character(hdifinal$SurveyOBISGBIFOutput)) + 0.00000000000000000000000000000000001)/as.numeric(as.character(hdifinal$EEZ200arealist)) * 100 
as.numeric(as.character(hdifinal$ratio))
View(hdifinal)
#hdifinal$ratio[hdifinal$ratio < 0.000000000001 ] <- 0
#View(hdifinal)

m1 <- glm(ratio  ~ HDI_2018, family= "Gamma", data=hdifinal)
summary(m1)


ggplot(hdifinal, aes(hdifinal$HDI_2018, hdifinal$ratio)) + geom_point() +labs(x="Human Development Index (HDI)", y= "Percentage of EEZ sampled by biological observing programs for at least one EOV") + theme_bw() + geom_smooth(aes(HDI_2018, ratio), method = "glm", method.args = list(family = "Gamma"), se = F, colour = "black", size = 0.8)

require(plotly)
fig <- plot_ly(data = hdifinal, x = ~HDI_2018, y = ~ratio, text=~Country)
fig


##MATCH WITH KEY
#dim(hdikey)
dim(hdidf)
colnames(hdidf)
#colnames(hdikey)

#combine 
hdifinal <- merge(hdikey, hdidf, all= T)
View(hdifinal)
hdifinalNA<- hdifinal[!is.na(hdifinal$GEONAME),]
colnames(surobisgbifdf)
colnames(hdifinalNA)
View(hdifinalNA)
surobisgbifdf$V2 <- as.character(surobisgbifdf$V2)
surobisgbifdf$GEONAME <- surobisgbifdf$V2
hdifinalNA$GEONAME<- as.character(hdifinalNA$GEONAME)
class(hdifinalNA$GEONAME)
class(surobisgbifdf$GEONAME)
unique(hdifinalNA$GEONAME)
unique(surobisgbifdf$GEONAME)
dim(hdifinalNA)
dim(surobisgbifdf)

##since EEZs seem like they must have a space or something slightly different conduct a fuzzy join
fuzzytest <- stringdist_join(surobisgbifdf, hdifinalNA, 
                             by = "GEONAME",
                             mode = "left",
                             ignore_case = T, 
                             method = "jw", 
                             max_dist = 0.21, 
                             distance_col = "dist") %>% group_by(GEONAME.x) %>% top_n(1, -dist)
View(fuzzytest)


fuzzytest$ratio <- (as.numeric(as.character(fuzzytest$SurveyOutput)) + 0.00000000000000000000000000000000001)/as.numeric(as.character(fuzzytest$EEZ200arealist)) * 100 
as.numeric(as.character(fuzzytest$ratio))
View(fuzzytest)
fuzzytest$ratio[fuzzytest$ratio < 0.000000000001 ] <- 0
fuzzytest$ratio[fuzzytest$ratio > 100 ] <- 100 #change any that are very low to 0
View(fuzzytest)

finaleezrat <- fuzzytest[!is.na(fuzzytest$Country),]
finaleezrat2 <- fuzzytest[!is.na(fuzzytest$HDI_2018),]
View(finaleezrat2)

m1 <- glm(ratio  ~ HDI_2018, family= "Gamma", data=finaleezrat2)
summary(m1)


ggplot(finaleezrat2, aes(finaleezrat2$HDI_2018, finaleezrat2$ratio)) + geom_point() +labs(x="Human Development Index (HDI)", y= "Percentage of EEZ sampled by biological observing programs for at least one EOV") + theme_bw() + geom_smooth(aes(HDI_2018, ratio), method = "glm", method.args = list(family = "Gamma"), se = F, colour = "black", size = 0.8) 

require(plotly)
fig <- plot_ly(data = finaleezrat2, x = ~HDI_2018, y = ~ratio, text=~Country)
fig
##STILL NEED TO MAKE SURE TO GET OTHER MATCHES + ALSO COMBINE LIKE ENTRIES!!!

#average ratios for each country (so for those with multiple territories)?




###MATCH WITH FUZZY MATCHING
areaeez200$V3 <- gsub("\\s*\\([^\\)]+\\)", "", as.character(areaeez200$V2))
areaeez200$V4 <- gsub("Exclusive Economic Zone", "", areaeez200$V3)

View(areaeez200$V4)

areaeez200$Country <- areaeez200$V4
areaeez200$SurveyOutput <- as.numeric(as.character(areaeez200$SurveyOutput))
areaeez200$SurveyOutput[is.na(areaeez200$SurveyOutput)] <- 0

##since countries and EEZs have different names conduct a fuzzy join
fuzzytest <- stringdist_join(areaeez200, hdidf, 
                             by = "Country",
                             mode = "left",
                             ignore_case = T, 
                             method = "jw", 
                             max_dist = 0.21, 
                             distance_col = "dist") %>% group_by(Country.x) %>% top_n(1, -dist)
dim(fuzzytest)
View(fuzzytest)
colnames(fuzzytest)
fuzzytest$ratio <- as.numeric(as.character(fuzzytest$SurveyOutput))/as.numeric(as.character(fuzzytest$EEZ200arealist)) * 100 + 0.00000000000000000001

m1 <- glm(ratio  ~ HDI_2018, family= "Gamma", data=fuzzytest)
summary(m1)

ggplot(fuzzytest, aes(fuzzytest$HDI_2018, fuzzytest$ratio)) + geom_point() +labs(x="Human Development Index (HDI)", y= "Percentage of EEZ sampled by biological observing programs for at least one EOV") + theme_bw() + geom_smooth( aes(HDI_2018, ratio), method = "glm", method.args = list(family = "Gamma"), se = FALSE, colour = "black", size = 0.8)
##STILL NEED TO MAKE SURE TO GET OTHER MATCHES + ALSO COMBINE LIKE ENTRIES!!!


