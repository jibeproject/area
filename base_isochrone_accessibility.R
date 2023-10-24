#SET UP
rm(list = ls())

library(sf)
library(dplyr)

if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)

####################
#PART 1: read files
####################
#get postcode centroids
setwd("../bigdata/postcode-pois/")
#read Greater Manchester region boundaries
gm_bound <- st_read(file.path("bounds.geojson"))

#read pois
poi <- st_read(file.path("poi_4138552.gpkg")) #this file is proprietary and therefore not uploaded on GitHub
#clip POIs to GM
gm_poi <- st_intersection(poi, gm_bound)
rm(poi)
#read concave hull of postcode service areas for JIBE (walk speed) disutility
conc_hull_all <- st_read("conc_hull.gpkg")
conc_hull_all <- st_zm(conc_hull_all) #drop Z and M values

####################
#PART 2: filter POIs based on relevance to the UK urban livability
####################
poi_code <- utils::read.csv(file = file.path("ULIUK_POI_codes.csv"), header = TRUE, colClasses='character')
colnames(poi_code)[1] <- "pointx_class"

#keep only relevant POIs
gm_poi <- gm_poi[gm_poi$pointx_class %in% poi_code$pointx_class,]

#read UKBuildings dataset to get area size and volume for POIs
uk_build <- st_read(file.path("UKBuildings.shp"))

#keep only building polygones that contain POIs
gm_build_poi <- st_join(gm_poi[,c('ref_no', 'name', 'pointx_class', 'groupname', 'categoryname',
                                     'classname', 'feature_easting', 'feature_northing')],
                        uk_build[, c('bui_id', 'prop_id', 'prop_area', 'height', 'use')])

#set units for area size and height
gm_build_poi$prop_area <- units::set_units(gm_build_poi$prop_area, m^2)
gm_build_poi$height <- units::set_units(gm_build_poi$height, m)

#get buildings with missing area/height
gm_build_poi_missvol <- gm_build_poi[is.na(gm_build_poi$prop_area),]
unique(gm_build_poi_missvol$groupname)
unique(gm_build_poi_missvol$classname)

#find transport facilities that don't have a building
exclude <- gm_build_poi_missvol[stringr::str_detect(gm_build_poi_missvol$classname, "Hail and Ride Zones") == TRUE |
                                  stringr::str_detect(gm_build_poi_missvol$classname, "Bus Stops") == TRUE |
                                  stringr::str_detect(gm_build_poi_missvol$classname, "Taxi Ranks") == TRUE |
                                  stringr::str_detect(gm_build_poi_missvol$classname, "Cash Machines") == TRUE |
                                  stringr::str_detect(gm_build_poi_missvol$classname, "Railway Stations, Junctions and Halts") == TRUE |
                                  stringr::str_detect(gm_build_poi_missvol$classname, "Tram, Metro and Light Railway Stations and Stops") == TRUE,]
#find playgrounds that don't have a building
playgrounds <- gm_build_poi_missvol[stringr::str_detect(gm_build_poi_missvol$classname, "Picnic Areas") == TRUE |
                                   stringr::str_detect(gm_build_poi_missvol$classname, "Playgrounds") == TRUE, ]
#find greenspace that don't have a building
greenspace <- gm_build_poi_missvol[stringr::str_detect(gm_build_poi_missvol$classname, "Commons") == TRUE |
                                   stringr::str_detect(gm_build_poi_missvol$classname, "Country and National Parks") == TRUE |
                                   stringr::str_detect(gm_build_poi_missvol$classname, "Municipal Parks and Gardens") == TRUE, ]


#remove transport facilities, playgrounds and greenspaces that don't have a building
gm_build_poi_missvol <- gm_build_poi_missvol[!(gm_build_poi_missvol$classname %in% exclude$classname),]
gm_build_poi_missvol <- gm_build_poi_missvol[!(gm_build_poi_missvol$classname %in% greenspace$classname),]
gm_build_poi_missvol <- gm_build_poi_missvol[!(gm_build_poi_missvol$classname %in% playgrounds$classname),]

#find nearest neighbour with area/height info to add to missing values (some points of interest due to precision issue fall outside the building footprint, thus searching for nearest neighbours)
uk_build_missvol <- st_join(gm_build_poi_missvol[,-12], uk_build[,"prop_id"], join=nngeo::st_nn, maxdist = 250, k = 1)
colnames(uk_build_missvol)[13] <- "prop_id"#second prop_id is of the neighbouring building with info on area/height

#find the areas from the non-missing
gm_build_poi_df <- gm_build_poi %>% st_drop_geometry()
areas <- gm_build_poi_df[gm_build_poi_df$prop_id %in% uk_build_missvol$prop_id,] %>% select(prop_id, prop_area, height)
areas <- unique(areas) #3053 plygons with prop_id as nearest neighbour
areas <- na.omit(areas)

#add prop_id from nearest polygon area and height to missing
uk_build_areas <- left_join(uk_build_missvol[ ,c("ref_no", "pointx_class", "prop_id")], areas, by = "prop_id") %>% select(ref_no, pointx_class, prop_id, prop_area, height) %>% st_drop_geometry()
uk_build_areas <- na.omit(uk_build_areas) #652 with values for area and height

#fill missing volume, prop_id and area values
gm_build_poi$height[is.na(gm_build_poi$height)] <- uk_build_areas$height[match(gm_build_poi$ref_no,uk_build_areas$ref_no)][which(is.na(gm_build_poi$height))]
gm_build_poi$prop_id[is.na(gm_build_poi$prop_id)] <- uk_build_areas$prop_id[match(gm_build_poi$ref_no,uk_build_areas$ref_no)][which(is.na(gm_build_poi$prop_id))]
gm_build_poi$prop_area[is.na(gm_build_poi$prop_area)] <- uk_build_areas$prop_area[match(gm_build_poi$prop_id,uk_build_areas$prop_id)][which(is.na(gm_build_poi$prop_area))]

#remove not needed
rm(areas, exclude, gm_build_poi_df, gm_build_poi_missvol, gm_poi, playgrounds, uk_build_areas, uk_build_missvol, poi, uk_build)

#remove parks, separate dataset with access points
gm_build_poi <- gm_build_poi[!(gm_build_poi$classname %in% greenspace$classname),]
rm(greenspace)

#read parks access points
parks <- st_read("GB_AccessPoint.shp")
parks_area <- st_read("GB_GreenspaceSite.shp") #get area
parks <- parks[parks$accessType != "Motor Vehicle", ] #remove 'car only' access

gm_parks <- st_intersection(parks, gm_bound) #restrict to GM
parks_area$prop_area <- st_area(parks_area) #add area size
colnames(parks_area)[1] <- "refToGSite"

#join park area to access
gm_parks <- left_join(gm_parks, st_drop_geometry(parks_area[,c("refToGSite", "prop_area")]), by = "refToGSite") #merge access pnts with area size info
colnames(gm_parks)[1] <- "ref_no" #rename columns to match poi dataset
gm_parks$pointx_class <- "03180814" #add POI code class for parks

#assign parks to various service areas
serv_area_parks <- st_intersection(conc_hull_all, gm_parks[,c("ref_no", "pointx_class", "prop_area")])

#assign POIs to various service areas
serv_area_pois <- st_intersection(conc_hull_all, gm_build_poi[,c("ref_no", "pointx_class", "prop_area")])

#remove not needed
rm(parks, parks_area)

#join together
serv_area <- rbind(serv_area_parks, serv_area_pois, greenspace) #not sure if greenspace is needed here

#count diverse pois per postcode area
serv_area_count <- serv_area %>% group_by(serv_area, pointx_class) %>% tally() %>% mutate(poi_cnt = n) %>% select (-n) %>% ungroup()

#read public transport
bus_tram <- read.csv("12_cat/12_bus_tram.csv", sep = ",") %>% st_as_sf(., coords = c("x", "y"), crs = 27700)
train <- read.csv("12_cat/12.1_rail.csv", sep = ",") %>% st_as_sf(., coords = c("x", "y"), crs = 27700)
pt <- rbind(bus_tram, train)

#assign PT to various service areas
serv_area_pt <- st_intersection(conc_hull_all, pt)

#remove not needed
rm(bus_tram, train, pt)

#join together
serv_area_pt_count <- serv_area_pt %>% group_by(serv_area) %>% tally() %>% mutate(poi_cnt = n) %>% select (-n) %>% ungroup()
serv_area_pt_count$pointx_class <- "10590732" #all PT stops are assigned bus code
####################
#PART 3: create postcode/poi dataset
####################
#create postcode/poi matrix
serv_area_pois_matrix <- as.data.frame.matrix(xtabs(poi_cnt ~ serv_area + pointx_class, serv_area_count), responseName = "serv_area")
serv_area_pt_matrix <- as.data.frame.matrix(xtabs(poi_cnt ~ serv_area + pointx_class, serv_area_pt_count), responseName = "serv_area")

#merge the two matrices
serv_area_pois_matrix <- serv_area_pois_matrix %>% select(-'10570738', -'10570756', -'10590732', -'10590759') #remove PT from POI dataset
serv_area_pois_matrix <- merge(serv_area_pois_matrix, serv_area_pt_matrix, by = "row.names", all.x = TRUE)
serv_area_pois_matrix[,"03180814"]
#add overall variety count
serv_area_pois_matrix$various <- rowSums(serv_area_pois_matrix[,2:58] != 0) #!!!double check exactly which columns are being summarized
colnames(serv_area_pois_matrix)[1] <- "serv_area"

#check for consistency
serv_area_pois_matrix[serv_area_pois_matrix$serv_area =="BL09FR",]
serv_area_pois_matrix[serv_area_pois_matrix$serv_area =="BL00AA",]

#rename pc_cent column
pc_cent <- st_read("gm_pccents.shp")
colnames(pc_cent)[2] <- "serv_area"
pc_cent$serv_area <- gsub(" ", "", pc_cent$serv_area, fixed = TRUE)
pc_cent$serv_area <- gsub("  ", "", pc_cent$serv_area, fixed = TRUE)

#join serv_cents_counts with postcode centroids
postcode_cent_poi <- left_join(pc_cent[,2], serv_area_pois_matrix, by = "serv_area")
postcode_cent_poi <- postcode_cent_poi %>% select(serv_area, various, everything())
postcode_cent_poi[is.na(postcode_cent_poi)] <- 0

#rename pc_area column
pc_area <- st_read("gm_pcarea.shp")
colnames(pc_area)[1] <- "serv_area"
pc_area$serv_area <- gsub(" ", "", pc_area$serv_area, fixed = TRUE)
pc_area$serv_area <- gsub("  ", "", pc_area$serv_area, fixed = TRUE)
pc_area <- st_intersection(pc_area, gm_bound) #clip area postcodes to GM only

#join serv_area_counts with postcode centroids
postcode_area_poi <- left_join(pc_area[,1], serv_area_pois_matrix, by = "serv_area")
postcode_area_poi <- postcode_area_poi %>% select(serv_area, various, everything())
postcode_area_poi[is.na(postcode_area_poi)] <- 0

####################
#PART 4: add population counts
####################
pop_counts_pc <- utils::read.csv(file = file.path(IMD/Postcode_Estimates_Table_1.csv"), header = TRUE)
pc_a_f <- utils::read.csv(file = file.path("IMD/rft-headcounts-and-household-estimates/R2_5_postcode_estimates_revised_17_10_2013/Postcode_Estimates_1_A_F.csv"), header = TRUE)
pc_g_l <- utils::read.csv(file = file.path("IMD/rft-headcounts-and-household-estimates/R2_5_postcode_estimates_revised_17_10_2013/Postcode_Estimates_1_G_L.csv"), header = TRUE)
pc_m_r <- utils::read.csv(file = file.path("IMD/rft-headcounts-and-household-estimates/R2_5_postcode_estimates_revised_17_10_2013/Postcode_Estimates_1_M_R.csv"), header = TRUE)
pc_s_z <- utils::read.csv(file = file.path("IMD/rft-headcounts-and-household-estimates/R2_5_postcode_estimates_revised_17_10_2013/Postcode_Estimates_1_S_Z.csv"), header = TRUE)

#cleanup columns
colnames(pop_counts_pc)[1] <- "serv_area"
pop_counts_pc$serv_area <- gsub(" ", "", pop_counts_pc$serv_area, fixed = TRUE)
pop_counts_pc$serv_area <- gsub("  ", "", pop_counts_pc$serv_area, fixed = TRUE)
#for centroids
postcode_cent_poi <- left_join(postcode_cent_poi, pop_counts_pc, by = "serv_area")
postcode_cent_poi <- postcode_cent_poi %>% select(serv_area, various, Total, Males, Females, Occupied_Households, everything())
postcode_cent_poi[is.na(postcode_cent_poi)] <- 0
postcode_cent_poi$serv_area <- as.character(postcode_cent_poi$serv_area)
#for areas
postcode_area_poi <- left_join(postcode_area_poi, pop_counts_pc, by = "serv_area")
postcode_area_poi <- postcode_area_poi %>% select(serv_area, various, Total, Males, Females, Occupied_Households, everything())
postcode_area_poi[is.na(postcode_area_poi)] <- 0
postcode_area_poi$serv_area <- as.character(postcode_area_poi$serv_area)

st_write(postcode_cent_poi, file.path(paste0("../postcode_poi_pop.gpkg")), "centroid", driver="GPKG", delete_layer = TRUE)
st_write(postcode_area_poi, file.path(paste0("../postcode_poi_pop.gpkg")), "area", driver="GPKG", delete_layer = TRUE)

####################
#PART 5: Tally OA to postcode population counts
####################
rm(list=setdiff(ls(), c("pc_cent", "pop_counts_pc", "postcode_cent_poi"))) #keep few objects and remove everything else from postcode_dataset.RData environment

oa <- utils::read.csv(file = file.path("..//KS102EW.csv"), header = TRUE)
colnames(oa)[3] <- "OA11CD"
oa_bounds <- st_read(file.path("Output_Areas__December_2011__Boundaries_EW_BFC.shp"))
gm_oa_bounds <- oa_bounds[oa_bounds$LAD16NM == "Tameside" |
                            oa_bounds$LAD16NM == "Stockport" |
                            oa_bounds$LAD16NM == "Rochdale" |
                            oa_bounds$LAD16NM == "Manchester" |
                            oa_bounds$LAD16NM == "Bolton" |
                            oa_bounds$LAD16NM == "Bury" |
                            oa_bounds$LAD16NM == "Trafford" |
                            oa_bounds$LAD16NM == "Wigan" |
                            oa_bounds$LAD16NM == "Salford" |
                            oa_bounds$LAD16NM == "Oldham", ] %>% na.omit()

oa_pop <- left_join(gm_oa_bounds, oa, by = "OA11CD")

#OA=2682528 to PC=2682556 delta = 28 people more to the PC level
sum(oa_pop$Age..All.usual.residents..measures..Value) - sum(postcode_cent_poi$Total)

####################
#PART 6: Add the binary metric for the 12 categories
####################
iso_area <- st_read("../postcode_poi_pop.gpkg", layer = "area")
iso_voro <- st_read("../postcode_poi_pop.gpkg", "Voronoi_isochrone")
iso_cent <- st_read("../postcode_poi_pop.gpkg", "centroid")

#add 12 categories columns
cat <- c("soc_cul", "edu", "prim_hlth", "hlth_res", "rec_sport", "early_yr",
                                "food_retail", "eat_est", "fin", "serv", "green", "pub_trans")
iso_area[ , cat] <- NA
iso_area <- iso_area %>% select(everything(), geom)
iso_voro[ , cat] <- NA
iso_voro <- iso_voro %>% select(everything(), geom)
iso_cent[ , cat] <- NA
iso_cent <- iso_cent %>% select(everything(), geom)
#remove leading X from column names
colnames(iso_area)[7:63] <-  sub("X", "", colnames(iso_area)[7:66])
colnames(iso_voro)[7:63] <-  sub("X", "", colnames(iso_voro)[7:66])
colnames(iso_cent)[7:63] <-  sub("X", "", colnames(iso_cent)[7:66])

#read poi codes
#01_social_cultural
soc_01 <- utils::read.csv(file.path("12_cat/01_social_cultural.csv"), header = FALSE, colClasses='character')
soc_01[1,] <- "03170244"
colnames(soc_01) <- "pointx_class"
#02_edu
edu_02 <- utils::read.csv(file.path("12_cat/02_edu.csv"), header = FALSE, colClasses='character')
edu_02[1,] <- "05310375"
colnames(edu_02) <- "pointx_class"
#03_prim_health
prim_03 <- utils::read.csv(file.path("12_cat/03_prim_health.csv"), header = FALSE, colClasses='character')
prim_03[1,] <- "05280815"
colnames(prim_03) <- "pointx_class"
#04_health_res
hlth_re_04 <- utils::read.csv(file.path("12_cat/04_health_res.csv"), header = FALSE, colClasses='character')
hlth_re_04[1,] <- "05280364"
colnames(hlth_re_04) <- "pointx_class"
#05_recreation_sports
sport_05 <- utils::read.csv(file.path("12_cat/05_recreation_sports.csv"), header = FALSE, colClasses='character')
sport_05[1,] <- "04240304"
colnames(sport_05) <- "pointx_class"
#06_early_yr
early_06 <- utils::read.csv(file.path("12_cat/06_early_yr.csv"), header = FALSE, colClasses='character')
early_06[1,] <- "05280809"
colnames(early_06) <- "pointx_class"
#07_food_retail
food_07 <- utils::read.csv(file.path("12_cat/07_food_retail.csv"), header = FALSE, colClasses='character')
food_07[1,] <- "09470819"
colnames(food_07) <- "pointx_class"
#08_eating_est
eating_est_08 <- utils::read.csv(file.path("12_cat/08_eating_est.csv"), header = FALSE, colClasses='character')
eating_est_08[1,] <- "09470661"
colnames(eating_est_08) <- "pointx_class"
#09_finance
fin_09 <- utils::read.csv(file.path("12_cat/09_finance.csv"), header = FALSE, colClasses='character')
fin_09[1,] <- "02090141"
colnames(fin_09) <- "pointx_class"
#10_services
srvcs_10 <- utils::read.csv(file.path("12_cat/10_services.csv"), header = FALSE, colClasses='character')
srvcs_10[1,] <- "09480716"
colnames(srvcs_10) <- "pointx_class"
#11_green_space
green_11 <- utils::read.csv(file.path("12_cat/11_green_space.csv"), header = FALSE, colClasses='character')
green_11[1,] <- "03180814"
colnames(green_11) <- "pointx_class"
#12_public_trans
pt_12 <- as.data.frame("10590732")
colnames(pt_12) <- "pointx_class"

#fill in 1 or 0 if even 1 poi from the same category present
#ISO_AREA
for (i in 1:nrow(iso_area)){
  iso_area$soc_cul[i] <-     ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% soc_01$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$edu[i] <-         ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% edu_02$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$prim_hlth[i] <-   ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% prim_03$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$hlth_res[i] <-    ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% hlth_re_04$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$rec_sport[i] <-   ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% sport_05$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$early_yr[i] <-    ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% early_06$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$food_retail[i] <- ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% food_07$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$eat_est[i] <-     ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% eating_est_08$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$fin[i] <-         ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% fin_09$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$serv[i] <-        ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% srvcs_10$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$green[i] <-       ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% green_11$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_area$pub_trans[i] <-   ifelse((sum(st_drop_geometry(iso_area[i,(colnames(iso_area) %in% pt_12$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
}
iso_area$score <- rowSums(st_drop_geometry(iso_area[,64:75]))
iso_area <- iso_area %>% select(everything(), geom)

#ISO_CENT
for (i in 1:nrow(iso_cent)){
  iso_cent$soc_cul[i] <-     ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% soc_01$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$edu[i] <-         ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% edu_02$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$prim_hlth[i] <-   ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% prim_03$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$hlth_res[i] <-    ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% hlth_re_04$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$rec_sport[i] <-   ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% sport_05$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$early_yr[i] <-    ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% early_06$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$food_retail[i] <- ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% food_07$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$eat_est[i] <-     ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% eating_est_08$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$fin[i] <-         ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% fin_09$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$serv[i] <-        ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% srvcs_10$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$green[i] <-       ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% green_11$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_cent$pub_trans[i] <-   ifelse((sum(st_drop_geometry(iso_cent[i,(colnames(iso_cent) %in% pt_12$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
}
iso_cent$score <- rowSums(st_drop_geometry(iso_cent[,64:75]))
iso_cent <- iso_cent %>% select(everything(), geom)

#ISO_VORO
for (i in 1:nrow(iso_voro)){
  iso_voro$soc_cul[i] <-     ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% soc_01$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$edu[i] <-         ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% edu_02$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$prim_hlth[i] <-   ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% prim_03$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$hlth_res[i] <-    ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% hlth_re_04$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$rec_sport[i] <-   ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% sport_05$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$early_yr[i] <-    ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% early_06$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$food_retail[i] <- ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% food_07$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$eat_est[i] <-     ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% eating_est_08$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$fin[i] <-         ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% fin_09$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$serv[i] <-        ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% srvcs_10$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$green[i] <-       ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% green_11$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
  iso_voro$pub_trans[i] <-   ifelse((sum(st_drop_geometry(iso_voro[i,(colnames(iso_voro) %in% pt_12$pointx_class) == TRUE]))>=1)==TRUE, as.numeric(1), as.numeric(0))
}
iso_voro$score <- rowSums(st_drop_geometry(iso_voro[,64:75]))
iso_voro <- iso_voro %>% select(everything(), geom)

st_write(iso_area, file.path(paste0("../iso_area.gpkg")), "area", driver="GPKG", delete_layer = TRUE)
st_write(iso_cent, file.path(paste0("../iso_cent.gpkg")), "cent", driver="GPKG", delete_layer = TRUE)
st_write(iso_voro, file.path(paste0("../iso_voro.gpkg")), "voronoi", driver="GPKG", delete_layer = TRUE)
