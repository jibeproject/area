#SET UP
#SET UP
rm(list=setdiff(ls(), c("gm_build_poi", "gm_parks"))) #datasets are outputs form base_isochrone_accessibility script

library(sf)
library(dplyr)

if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)

####################
#PART 1: read files
####################
#read Greater Manchester region boundaries
gm_bound <- st_read(file.path("01_DataInput/Cityreg_bounds/GreaterManchester/bounds.geojson"))

#read workplace population centroids
wp_cents <- st_read(file.path("bigdata/WorkPlaceZones/Workplace_Zones_(December_2011)_Population_Weighted_Centroids/Workplace_Zones_(December_2011)_Population_Weighted_Centroids.shp"))
wp_bounds <- st_read(file.path("bigdata/WorkPlaceZones/Workplace_Zones_(December_2011)_Boundaries/Workplace_Zones_(December_2011)_Boundaries.shp"))
wp_industry <- utils::read.csv(file.path("bigdata/WorkPlaceZones/WP605EW - Industry (Workplace population).csv"), header = TRUE)

#clip to GM
gm_wp_cents <- st_intersection(wp_cents, gm_bound)
gm_wp_bound <- wp_bounds[wp_bounds$wz11cd %in% gm_wp_cents$wz11cd,]
gm_wp_industry <- wp_industry[wp_industry$geography.code %in% gm_wp_cents$wz11cd,]
rm(wp_cents, wp_bounds, wp_industry) #remove not needed
st_write(gm_wp_bound, file.path(paste0("02_DataOutput/area/gm_workplace.gpkg")), "boundary", driver="GPKG")
st_write(gm_wp_cents, file.path(paste0("02_DataOutput/area/gm_workplace.gpkg")), "centroid", driver="GPKG")
#keep and rename only G,I,K,P,Q and R/S industry categories
colnames(gm_wp_industry)[18] <- "G:Whole sale and retail"
colnames(gm_wp_industry)[20] <- "I:Acommodation and food service activities"
colnames(gm_wp_industry)[22] <- "K:Financial and insurance acctivities"
colnames(gm_wp_industry)[27] <- "P:Education"
colnames(gm_wp_industry)[28] <- "Q:Human health and social work activities"
colnames(gm_wp_industry)[29] <- "R/S:Arts,entertainment and recreation"
gm_wp_industry <- gm_wp_industry[,c("geography", "G:Whole sale and retail",
                                    "I:Acommodation and food service activities",
                                    "K:Financial and insurance acctivities",
                                    "P:Education",
                                    "Q:Human health and social work activities",
                                    "R/S:Arts,entertainment and recreation")]
####################
#PART 2: assign Workplace zone code to POI dataset
####################
#add workzone code to POI building footprints
gm_build_poi_wp <- st_intersection(gm_build_poi, gm_wp_bound[,"wz11cd"])
write.table(gm_wp_industry, file.path("02_DataOutput/area/gm_workplaceIndustry.csv"), col.names = TRUE, sep = ";")

#read-in POI codes for workplace population grouped by industry category
k <- utils::read.csv(file.path("01_DataInput/poi/ULI/IndustryK_Financial and insurance acctivities.csv"), header = FALSE, colClasses='character')
k[1,] <- "02090141"
i <- utils::read.csv(file.path("01_DataInput/poi/ULI/IndustryI_Acommodation and food service activities.csv"), header = FALSE, colClasses='character')
i[1,] <- "09470661"
g <- utils::read.csv(file.path("01_DataInput/poi/ULI/IndustryG_Whole sale and retail.csv"), header = FALSE, colClasses='character')
g[1,] <- "09470819"
q <- utils::read.csv(file.path("01_DataInput/poi/ULI/Industry_Q Human health and social work activities.csv"), header = FALSE, colClasses='character')
q[1,] <- "05280809"
p <- utils::read.csv(file.path("01_DataInput/poi/ULI/Industry_P Education.csv"), header = FALSE, colClasses='character')
p[1,] <- "05310375"
r_s <- utils::read.csv(file.path("01_DataInput/poi/ULI/Industry_R,S Arts, entertainment and recreation.csv"), header = FALSE, colClasses='character')
r_s[1,] <- "03170244"

#split POI building footprint dataset to six industry classes
k_poi <- gm_build_poi_wp[gm_build_poi_wp$pointx_class %in% k$V1,]
i_poi <- gm_build_poi_wp[gm_build_poi_wp$pointx_class %in% i$V1,]
g_poi <- gm_build_poi_wp[gm_build_poi_wp$pointx_class %in% g$V1,]
q_poi <- gm_build_poi_wp[gm_build_poi_wp$pointx_class %in% q$V1,]
p_poi <- gm_build_poi_wp[gm_build_poi_wp$pointx_class %in% p$V1,]
r_s_poi <- gm_build_poi_wp[gm_build_poi_wp$pointx_class %in% r_s$V1,]

#clean NAs
k_poi$prop_area <- ifelse((is.na(k_poi$prop_area) & k_poi$classname != "Cash Machines") == TRUE, median(k_poi$prop_area, na.rm = TRUE), k_poi$prop_area)
k_poi$prop_area <- ifelse((k_poi$classname == "Cash Machines" & !is.na(k_poi$prop_area)) == TRUE, as.numeric(0), k_poi$prop_area)
k_poi$prop_area <- ifelse((k_poi$classname == "Cash Machines" & is.na(k_poi$prop_area)) == TRUE, as.numeric(0), k_poi$prop_area)

i_poi$prop_area <- ifelse(is.na(i_poi$prop_area) == TRUE, median(i_poi$prop_area, na.rm = TRUE), i_poi$prop_area)

g_poi$prop_area <- ifelse(is.na(g_poi$prop_area) == TRUE, median(g_poi$prop_area, na.rm = TRUE), g_poi$prop_area)

q_poi$prop_area <- ifelse(is.na(q_poi$prop_area) == TRUE, median(q_poi$prop_area, na.rm = TRUE), q_poi$prop_area)

p_poi$prop_area <- ifelse(is.na(p_poi$prop_area) == TRUE, median(p_poi$prop_area, na.rm = TRUE), p_poi$prop_area)

r_s_poi$prop_area <- ifelse((is.na(r_s_poi$prop_area) & r_s_poi$classname != "Hair and Beauty Services") == TRUE, mean(r_s_poi$prop_area, na.rm = TRUE), r_s_poi$prop_area)

r_s_poi$prop_area <- ifelse((is.na(r_s_poi$prop_area) & r_s_poi$classname == "Hair and Beauty Services") == TRUE, median(r_s_poi$prop_area, na.rm = TRUE), r_s_poi$prop_area)

####################
#PART 3: create weights
#####################
#create dataframe with work population counts per industry class and its work population density
work_pop <- data.frame(c(1553274, 13308345, 41258877, 2283280, 2728816, 10248837),
                       c(0.042878462, 0.006869825, 0.006999027, 0.099867734, 0.060546405, 0.007433136), row.names = c(
                         "sum_K", "sum_I", "sum_G", "sum_Q", "sum_P", "sum_R/S"))
colnames(work_pop)[1] <- "area_sum"
colnames(work_pop)[2] <- "pop_per_m2"

#create weights by multiplying POI building footprint areas with work population density per industry class
k_poi$weight <- k_poi$prop_area*work_pop$pop_per_m2[1]
i_poi$weight <- i_poi$prop_area*work_pop$pop_per_m2[2]
g_poi$weight <- g_poi$prop_area*work_pop$pop_per_m2[3]
q_poi$weight <- q_poi$prop_area*work_pop$pop_per_m2[4]
p_poi$weight <- p_poi$prop_area*work_pop$pop_per_m2[5]
r_s_poi$weight <- r_s_poi$prop_area*work_pop$pop_per_m2[6]
#join POI building footprint areas together
poi <- rbind(k_poi, i_poi, g_poi, q_poi, p_poi, r_s_poi)
poi$classname <- tolower(poi$classname)

#add summary of areas per class
areas <- poi %>% group_by(classname) %>% mutate(area_sum=sum(prop_area))
#produce dataframe with POI grouped by classes containing info on area summary
areas_clean <- as.data.frame(cbind(unique(areas$classname), unique(areas$area_sum)))
colnames(areas_clean)[1] <- "classname"
colnames(areas_clean)[2] <- "sum_area_m2"
areas_clean$classname <- tolower(areas_clean$classname)
sum(as.numeric(areas_clean$work_pop))
sum(as.numeric(areas_clean$sum_area_m2))

#read in dataset from Excel file to add info on work population summary and population density per m2
areas_clean1 <- utils::read.csv(file.path("01_DataInput/poi/ULI/work_pop_area_size.csv"), header = TRUE, colClasses='character') #created in Excel
colnames(areas_clean1)[5] <- "classname"
areas_clean1$classname <- tolower(areas_clean1$classname)
areas_clean1$classname[16] <- "walk-in centre"
areas_clean1$classname[44] <- "baby and nursery equipment and children's clothes"
areas_clean1$classname[27] <- "nursery schools and pre and after school care"

areas_clean <- left_join(areas_clean, areas_clean1[,c("classname", "work_pop", "workpop_per_m2")], by = "classname")
#add additional info to POI building footprint areas dataset
poi <- left_join(poi, areas_clean[,c("classname", "workpop_per_m2")], by = "classname")

#divide area based on the number of POIs found in the same building
prop_id <- poi %>% st_drop_geometry() %>% group_by(prop_id) %>% tally()
prop_id[is.na(prop_id$prop_id),2] <- as.numeric(1)
poi <- prop_id %>% ungroup() %>% left_join(., poi) %>% mutate(ind_area = prop_area/n)

#add weights
poi$wght <- poi$ind_area*as.numeric(poi$workpop_per_m2) #gives the weight as the est. number of ppl working in the location

sum(as.numeric(poi$wght)) #crosschecked with code line 116, has to have the same value as total 517156 people working
poi <- poi %>% select(-n, -weight) #remove not needed

qqnorm(log(poi[(poi$prop_area > 0) == TRUE,]$prop_area))#check distribution of theoretical quintailes-- if the data is log-normally distributed, do the log-transformation
qex <- function(x) qexp((rank(x)-.375)/(length(x)+.25))
plot(qex(poi[(poi$prop_area > 0) == TRUE,]$prop_area),log(poi[(poi$prop_area > 0) == TRUE,]$prop_area))

####################
#PART 4: test some standardization techniques
####################
############### LOG TRANSFORMATION
#log1p(x) computes log(1+x) accurately also for |x| << 1.
#log(x+1) transformation is often used for transforming data that are right-skewed, but also include zero values.
#The shape of the resulting distribution will depend on how big x is compared to the constant 1. Therefore the shape of the
#resulting distribution depends on the units in which x was measured.

poi$log_wght <- log1p(poi$wght) #best fit for the dataset as the weights have few extreme values that skew the distribution
summary(poi$log_wght)

############### STANDARDIZATION
poi$minmax <-(poi$wght - min(poi$wght)) / (max(poi$wght) - min(poi$wght))
poi$featurescl <- poi$wght / max(poi$wght)
poi$per <-(poi$wght - min(poi$wght)) / (max(poi$wght) - min(poi$wght)) * 100
poi_scl <- poi %>% mutate_at(c('wght'), ~(scale(.) %>% as.vector))
sd(poi_scl$wght)

summary(poi$wght)
summary(poi_scl$wght)

####################
#PART 5: add greenspace
####################
#add population to parks
pop <- st_read(file.path("02_DataOutput/area/postcode_poi_pop.gpkg"), layer = "centroid")
pop <- pop[,c("serv_area", "Total", "Males", "Females")]

#get nearest access point for each postcode centroid and sttach population counts
nn_parkaccess <- st_join(pop, gm_parks[,c("ref_no")], join=nngeo::st_nn, maxdist = 2000, k = 1) #based on the standard for greenspace found pg. 12 from https://www.eastsuffolk.gov.uk/assets/Planning/Rendlesham/Folder-9/9.13-Nature-Nearby-Accessible-Natural-Greenspace-Guidance-Natural-England.pdf

#count total people that have same access point as nearest neighbour
gm_parks <- left_join(gm_parks, st_drop_geometry(nn_parkaccess[,c("Total", "ref_no")]), by = "ref_no")
#add 0 to NAs Totals
gm_parks$Total <- ifelse(is.na(gm_parks$Total), as.numeric(0), gm_parks$Total)
#count the population with access to the same park regardless of entrance
gm_parks <- gm_parks %>% group_by(ref_no, refToGSite) %>% mutate(pop_count = sum(Total)) %>% ungroup() %>% select(-Total)
gm_parks <- unique(gm_parks)#remove duplicates

#create park weights
gm_parks_norm <- gm_parks %>% group_by(refToGSite) %>% mutate(tot = sum(pop_count)) %>% ungroup() %>% select(-pop_count)
park_wghts <- gm_parks_norm %>% select(refToGSite, prop_area, tot) %>% st_drop_geometry() %>% unique() #create weights for parks, size normalized by people density
park_wghts$pp_pm2 <- park_wghts$tot/park_wghts$prop_area #get population density per m2

#read in the green space area including info on how many other activity areas are inside
gm_parks_area <- st_read("02_DataOutput/area/parks_multi_activ.gpkg") #file generated in QGIS with 'Join Attributes by Location' algorithm
colnames(gm_parks_area)[1] <- "refToGSite"
park_wghts <- left_join(park_wghts, gm_parks_area[,c("refToGSite", "various")], by = "refToGSite")
park_wghts$various <- ifelse(is.na(park_wghts$various), as.numeric(1), park_wghts$various)#fill in NAs with 1
park_wghts <- park_wghts %>% select(-geom) #remove geometry column
park_wghts$wght <- park_wghts$various #transfer weights from variety of activities
park_wghts <- park_wghts %>% select(-wght)

park_wghts$wght <- park_wghts$various / (1 + units::drop_units(park_wghts$pp_pm2)) %>% as.numeric()

median(park_wghts$wght)
mean(park_wghts$wght)

#add weights normalized for population density per m2 park area
gm_parks <- left_join(gm_parks, park_wghts[,c('refToGSite', 'pp_pm2', 'wght')], by = 'refToGSite')

#add X/Y coords
for (i in 1:nrow(gm_parks)){
  gm_parks$feature_easting[i] <- gm_parks$geometry[[i]][1]
  gm_parks$feature_northing[i] <- gm_parks$geometry[[i]][2]
}

#assign mean weight for playgrounds based on parks
gm_play <- gm_build_poi_wp[gm_build_poi_wp$classname == "Picnic Areas" | gm_build_poi_wp$classname == "Playgrounds",]
gm_play$wght <- "1.093642"#took the mean of all parks

####################
#PART 6: combine POIs and Green Space datasets in one
####################

poi_all <- rbind(st_drop_geometry(gm_parks[,c("feature_easting", "feature_northing", "ref_no", "pointx_class", "prop_area", "wght")]),
                 st_drop_geometry(poi[,c("feature_easting", "feature_northing", "ref_no", "pointx_class", "prop_area", "wght")]),
                 st_drop_geometry(gm_play[,c("feature_easting", "feature_northing", "ref_no", "pointx_class", "prop_area", "wght")]))


poi_all$wght <- ifelse(poi_all$pointx_class == "02090141",
                       as.character(with(poi, mean(wght[classname == "banks and building societies"]))),
                       poi_all$wght)#took the median wght of all banks

poi_all$wght <- as.numeric(poi_all$wght)
plot(density(poi_all$wght))

#cross-checking/do double log transformation
poi_all$log_wght <- log1p(poi_all$wght) #log transformation
plot(density(poi_all$wght))
plot(density(poi_all$log_wght))
summary(poi_all)

#add textual description to combined dataset
poi$ref_no <- as.character(poi$ref_no)
poi_all <- left_join(poi_all, poi[,c("ref_no", "name")])
poi_all$name <- ifelse(is.na(poi_all$name), paste0("green space"), poi_all$name)

#write dataset for visual check in QGIS
write.table(poi_all, file.path(paste0("02_DataOutput/area/poi_all_wght_final.csv")), col.names = TRUE, quote = FALSE, row.names = FALSE, sep = ";")

#add values for 0 weights for ATMs
poi$wght <- ifelse(poi$pointx_class == "02090141",
                       as.character(with(poi, mean(wght[classname == "banks and building societies"]))),
                       poi$wght)#took the median wght of all banks

poi$log_wght <- ifelse(poi$pointx_class == "02090141",
                   as.character(with(poi, mean(log_wght[classname == "banks and building societies"]))),
                   poi$log_wght)#took the median wght of all banks

write.table(poi, file.path(paste0("02_DataOutput/area/poi_logwght_names.csv")), col.names = TRUE, quote = FALSE, row.names = FALSE, sep = ";")

####################
#PART 7: split by 12 categories
####################
#01_social_cultural
#read poi codes
soc_01 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/01_social_cultural.csv"), header = FALSE, colClasses='character')
soc_01[1,] <- "03170244"
colnames(soc_01) <- "pointx_class"
poi_soc <- poi_all[poi_all$pointx_class %in% soc_01$pointx_class,]
poi_soc <- dplyr::mutate(poi_soc, ID = row_number()) %>% select(ID, everything())
colnames(poi_soc)[2] <- "X"
colnames(poi_soc)[3] <- "Y"
colnames(poi_soc)[7] <- "WEIGHT"
poi_soc <- poi_soc %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_soc, file.path("02_DataOutput/area/Social_and_culture_locations.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#02_edu
edu_02 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/02_edu.csv"), header = FALSE, colClasses='character')
edu_02[1,] <- "05310375"
colnames(edu_02) <- "pointx_class"
poi_edu <- poi_all[poi_all$pointx_class %in% edu_02$pointx_class,]
poi_edu <- dplyr::mutate(poi_edu, ID = row_number()) %>% select(ID, everything())
colnames(poi_edu)[2] <- "X"
colnames(poi_edu)[3] <- "Y"
colnames(poi_edu)[7] <- "WEIGHT"
poi_edu <- poi_edu %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_edu, file.path("02_DataOutput/area/Education.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#03_prim_health
prim_03 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/03_prim_health.csv"), header = FALSE, colClasses='character')
prim_03[1,] <- "05280815"
colnames(prim_03) <- "pointx_class"
poi_prim <- poi_all[poi_all$pointx_class %in% prim_03$pointx_class,]
poi_prim <- dplyr::mutate(poi_prim, ID = row_number()) %>% select(ID, everything())
colnames(poi_prim)[2] <- "X"
colnames(poi_prim)[3] <- "Y"
colnames(poi_prim)[7] <- "WEIGHT"
poi_prim <- poi_prim %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_prim, file.path("02_DataOutput/area/Primary_health_care.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#04_health_res
hlth_re_04 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/04_health_res.csv"), header = FALSE, colClasses='character')
hlth_re_04[1,] <- "05280364"
colnames(hlth_re_04) <- "pointx_class"
poi_hlthres <- poi_all[poi_all$pointx_class %in% hlth_re_04$pointx_class,]
poi_hlthres <- dplyr::mutate(poi_hlthres, ID = row_number()) %>% select(ID, everything())
colnames(poi_hlthres)[2] <- "X"
colnames(poi_hlthres)[3] <- "Y"
colnames(poi_hlthres)[7] <- "WEIGHT"
poi_hlthres <- poi_hlthres %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_hlthres, file.path("02_DataOutput/area/Community_health_resources.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#05_recreation_sports
sport_05 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/05_recreation_sports.csv"), header = FALSE, colClasses='character')
sport_05[1,] <- "04240304"
colnames(sport_05) <- "pointx_class"
poi_sport <- poi_all[poi_all$pointx_class %in% sport_05$pointx_class,]
poi_sport <- dplyr::mutate(poi_sport, ID = row_number()) %>% select(ID, everything())
colnames(poi_sport)[2] <- "X"
colnames(poi_sport)[3] <- "Y"
colnames(poi_sport)[7] <- "WEIGHT"
poi_sport <- poi_sport %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_sport, file.path("02_DataOutput/area/Recreational_sports_pitches_and_facilities.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#06_early_yr
early_06 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/06_early_yr.csv"), header = FALSE, colClasses='character')
early_06[1,] <- "05280809"
colnames(early_06) <- "pointx_class"
poi_early <- poi_all[poi_all$pointx_class %in% early_06$pointx_class,]
poi_early <- dplyr::mutate(poi_early, ID = row_number()) %>% select(ID, everything())
colnames(poi_early)[2] <- "X"
colnames(poi_early)[3] <- "Y"
colnames(poi_early)[7] <- "WEIGHT"
poi_early <- poi_early %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_early, file.path("02_DataOutput/area/Early_year_access.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#07_food_retail
food_07 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/07_food_retail.csv"), header = FALSE, colClasses='character')
food_07[1,] <- "09470819"
colnames(food_07) <- "pointx_class"
poi_food <- poi_all[poi_all$pointx_class %in% food_07$pointx_class,]
poi_food <- dplyr::mutate(poi_food, ID = row_number()) %>% select(ID, everything())
colnames(poi_food)[2] <- "X"
colnames(poi_food)[3] <- "Y"
colnames(poi_food)[7] <- "WEIGHT"
poi_food <- poi_food %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_food, file.path("02_DataOutput/area/Food_retail.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#08_eating_est
eating_est_08 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/08_eating_est.csv"), header = FALSE, colClasses='character')
eating_est_08[1,] <- "09470661"
colnames(eating_est_08) <- "pointx_class"
poi_eatest <- poi_all[poi_all$pointx_class %in% eating_est_08$pointx_class,]
poi_eatest <- dplyr::mutate(poi_eatest, ID = row_number()) %>% select(ID, everything())
colnames(poi_eatest)[2] <- "X"
colnames(poi_eatest)[3] <- "Y"
colnames(poi_eatest)[7] <- "WEIGHT"
poi_eatest <- poi_eatest %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_eatest, file.path("02_DataOutput/area/Eating_establishments.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#09_finance
fin_09 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/09_finance.csv"), header = FALSE, colClasses='character')
fin_09[1,] <- "02090141"
colnames(fin_09) <- "pointx_class"
poi_fin <- poi_all[poi_all$pointx_class %in% fin_09$pointx_class,]
poi_fin <- dplyr::mutate(poi_fin, ID = row_number()) %>% select(ID, everything())
colnames(poi_fin)[2] <- "X"
colnames(poi_fin)[3] <- "Y"
colnames(poi_fin)[7] <- "WEIGHT"
poi_fin <- poi_fin %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_fin, file.path("02_DataOutput/area/Financial.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#10_services
srvcs_10 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/10_services.csv"), header = FALSE, colClasses='character')
srvcs_10[1,] <- "09480716"
colnames(srvcs_10) <- "pointx_class"
poi_srvcs <- poi_all[poi_all$pointx_class %in% srvcs_10$pointx_class,]
poi_srvcs <- dplyr::mutate(poi_srvcs, ID = row_number()) %>% select(ID, everything())
colnames(poi_srvcs)[2] <- "X"
colnames(poi_srvcs)[3] <- "Y"
colnames(poi_srvcs)[7] <- "WEIGHT"
poi_srvcs <- poi_srvcs %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_srvcs, file.path("02_DataOutput/area/Services.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#11_green_space
green_11 <- utils::read.csv(file.path("01_DataInput/poi/ULI/12_cat/11_green_space.csv"), header = FALSE, colClasses='character')
green_11[1,] <- "03180814"
colnames(green_11) <- "pointx_class"
poi_green <- poi_all[poi_all$pointx_class %in% green_11$pointx_class,]
poi_green <- dplyr::mutate(poi_green, ID = row_number()) %>% select(ID, everything())
colnames(poi_green)[2] <- "X"
colnames(poi_green)[3] <- "Y"
colnames(poi_green)[7] <- "WEIGHT"
poi_green <- poi_green %>% select(ID, X, Y, WEIGHT, ref_no, name)
write.table(poi_green, file.path("02_DataOutput/area/Public_open_space.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
#12_public_trans (dataset generated by Ismail via GTFS.org)
bus_tram <- read.csv("D:/JIBE/01_DataInput/poi/ULI/12_cat/12_bus_tram.csv", sep = ",") %>% st_as_sf(., coords = c("x", "y"), crs = 27700)
train <- read.csv("D:/JIBE/01_DataInput/poi/ULI/12_cat/12.1_rail.csv", sep = ",") %>% st_as_sf(., coords = c("x", "y"), crs = 27700)
poi_pt <- rbind(bus_tram, train)
rm(bus_tram, train)
poi_pt <- dplyr::mutate(poi_pt, ID = row_number()) %>% select(ID, everything()) %>% select(-id)
poi_pt$X <- NA %>% as.numeric()
poi_pt$Y <- NA %>% as.numeric()
for (i in 1:nrow(poi_pt)){
  poi_pt[i,'X'] <- unlist(poi_pt$geometry[i])[1]
  poi_pt[i,'Y'] <- unlist(poi_pt$geometry[i])[2]
}

colnames(poi_pt)[2] <- "WEIGHT"
poi_pt <- st_intersection(poi_pt, gm_bound)
poi_pt <- poi_pt  %>% select(ID, X, Y, -region, everything()) %>% st_drop_geometry()
poi_pt <- poi_pt %>% select(-region)
write.table(poi_pt, file.path("02_DataOutput/area/Public_transport.csv"), col.names = TRUE, sep = ";", quote = FALSE, row.names = FALSE)
