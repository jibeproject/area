#SET UP
rm(list = ls())

library(sf)
library(dplyr)

####################
#PART 1: read files
####################
#get postcode centroids
setwd("D:/JIBE")
#read Greater Manchester region boundaries
gm_bound <- st_read(file.path("01_DataInput/Cityreg_bounds/GreaterManchester/bounds.geojson"))
#read pois
poi <- st_read(file.path("01_DataInput/poi/Download_poi_UK_1803987/poi_4138552/poi_4138552.gpkg"))
#clip POIs to GM
gm_poi <- st_intersection(poi, gm_bound)
rm(poi)

####################
#PART 2: filter POIs based on relevance to the UK urban livability
####################
hosp_code <- utils::read.csv(file = file.path("01_DataInput/poi/ULI/Hospitals.csv"), header = TRUE, colClasses='character')
colnames(hosp_code)[1] <- "pointx_class"

sch_code <- utils::read.csv(file = file.path("01_DataInput/poi/ULI/SecondaryandFurtherEdu.csv"), header = TRUE, colClasses='character')
colnames(sch_code)[1] <- "pointx_class"

#keep only relevant POIs
gm_poi <- gm_poi[gm_poi$pointx_class %in% c(hosp_code$pointx_class, sch_code$pointx_class),]

#read UKBuildings dataset to get area size and volume for POIs
uk_build <- st_read(file.path("01_DataInput/poi/Geomni/ukbuildings_4188614/UKBuildings.shp"))

#keep only building polygones that contain POIs
gm_build_poi <- st_join(gm_poi[,c('ref_no', 'name', 'pointx_class', 'groupname', 'categoryname',
                                  'classname', 'feature_easting', 'feature_northing')],
                        uk_build[, c('bui_id', 'prop_id', 'prop_area', 'height', 'use')])

#set units for area size and height
gm_build_poi$prop_area <- units::set_units(gm_build_poi$prop_area, m^2)
gm_build_poi$height <- units::set_units(gm_build_poi$height, m)

#get buildings with missing area/height
gm_build_poi_missvol <- gm_build_poi[is.na(gm_build_poi$prop_area),]
#find nearest neighbour with area/height info
uk_build_missvol <- st_join(gm_build_poi_missvol[,-12], uk_build[,"prop_id"], join=nngeo::st_nn, maxdist = 250, k = 1)
colnames(uk_build_missvol)[13] <- "prop_id"#second prop_id is of the neighbouring building with info on area/height

#find the areas from the non-missing
gm_build_poi_df <- gm_build_poi %>% st_drop_geometry()
areas <- gm_build_poi_df[gm_build_poi_df$prop_id %in% uk_build_missvol$prop_id,] %>% select(prop_id, prop_area, height)
areas <- unique(areas)
areas <- na.omit(areas)

#add prop_id from nearest polygon area and hight to missing
uk_build_areas <- left_join(uk_build_missvol[ ,c("ref_no", "pointx_class", "prop_id")], areas, by = "prop_id") %>% select(ref_no, pointx_class, prop_id, prop_area, height) %>% st_drop_geometry()
uk_build_areas <- na.omit(uk_build_areas) #652 with values for area and height

#fill missing volume, prop_id and area values
gm_build_poi$height[is.na(gm_build_poi$height)] <- uk_build_areas$height[match(gm_build_poi$ref_no,uk_build_areas$ref_no)][which(is.na(gm_build_poi$height))]
gm_build_poi$prop_id[is.na(gm_build_poi$prop_id)] <- uk_build_areas$prop_id[match(gm_build_poi$ref_no,uk_build_areas$ref_no)][which(is.na(gm_build_poi$prop_id))]
gm_build_poi$prop_area[is.na(gm_build_poi$prop_area)] <- uk_build_areas$prop_area[match(gm_build_poi$prop_id,uk_build_areas$prop_id)][which(is.na(gm_build_poi$prop_area))]

#remove not needed
rm(areas, exclude, gm_build_poi_df, gm_build_poi_missvol, gm_poi, playgrounds, uk_build_areas, uk_build_missvol, poi, uk_build)

#dataframe with average people employed per industry as calculated found in bigdata/postcode-pois "POI Taxonomy - UK.xlsx" Sheet SELECTED
work_pop <- data.frame(c(0.099867734, 0.060546405), row.names = c(
                         "hospital", "school"))
colnames(work_pop)[1] <- "pop_per_m2"
#get mean values to fill NAs
summary(st_drop_geometry(gm_build_poi[gm_build_poi$categoryname == "Health Practitioners and Establishments","prop_area"]), na.rm = TRUE)
#mean is 6013.5
gm_build_poi$prop_area <- ifelse(gm_build_poi$categoryname == "Health Practitioners and Establishments" & is.na(gm_build_poi$prop_area),
                       as.numeric(6013.5),
                       gm_build_poi$prop_area)#took the mean area of the hospital category for the NAs

summary(st_drop_geometry(gm_build_poi[gm_build_poi$categoryname == "Primary, Secondary and Tertiary Education","prop_area"]), na.rm = TRUE)
#mean is 1606.8
gm_build_poi$prop_area <- ifelse(gm_build_poi$categoryname == "Primary, Secondary and Tertiary Education" & is.na(gm_build_poi$prop_area),
                                 as.numeric(1606.8),
                                 gm_build_poi$prop_area)#took the mean area of the hospital category for the NAs

gm_build_poi$weight <- ifelse(gm_build_poi$categoryname == "Health Practitioners and Establishments", gm_build_poi$prop_area*work_pop$pop_per_m2[1], gm_build_poi$weight)
gm_build_poi$weight <- ifelse(gm_build_poi$categoryname == "Primary, Secondary and Tertiary Education", gm_build_poi$prop_area*work_pop$pop_per_m2[2], gm_build_poi$weight)

gm_build_poi <- dplyr::mutate(gm_build_poi, ID = row_number()) %>% select(ID, everything())
gm_build_poi$X <- NA %>% as.numeric()
gm_build_poi$Y <- NA %>% as.numeric()
for (i in 1:nrow(gm_build_poi)){
  gm_build_poi[i,'X'] <- unlist(gm_build_poi$geom[i])[1]
  gm_build_poi[i,'Y'] <- unlist(gm_build_poi$geom[i])[2]
}

gm_build_poi <- gm_build_poi %>% st_drop_geometry()
colnames(gm_build_poi)[15] <- "WEIGHT"
gm_build_poi <- gm_build_poi  %>% select(ID, X, Y, WEIGHT, ref_no, name) #select only needed columns

utils::write.table(gm_build_poi, file.path("02_DataOutput/area/13_hospitals_and_edu.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
