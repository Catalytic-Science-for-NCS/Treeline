library(terra)
library(tidyverse)
library(ggmap)
library(sf)


### MAKE DEPARTMENT/STATE SCALE OPPORTUNITY MAPS ##


huila <- vect("Colombia/Departamentos_202112_shp/departamento.shp") 
huila <- huila[huila$DeNombre=="Huila",] #1,813,707 ha total
recs <- vect("TIA/expertRecs_lessComplex.shp")
recs_huila <- subset(recs, recs$COUNTRY=="Colombia") %>% project(huila) %>% crop(huila)


#load in crop, grazing, and forest cover
crop <- rast("../../Downloads/crop_presence_huila.tif") %>% project(y=crs(huila), method="near")
graze <- rast("../../Downloads/grazing_presence_huila.tif")%>% project(y=crs(huila), method="near")
ag <- terra::merge(crop, graze)
fc <- rast("../../Downloads/forest2015_huila.tif")%>% project(y=crs(huila), method="bilinear") #253,204.1 ha forest cover

#get rid of everything over 25% tree cover
s <-c(fc, crop, graze)
s$b1 <- ifel(s$b1==1 & fc<=25, 1, NA)
s$remapped <- ifel(s$remapped==1 & fc<=25, 1, NA)
s$ag <- terra::merge(s$b1, s$remapped)
s$tree_canopy_cover  <- ifel(s$tree_canopy_cover<=25 & !is.na(s$ag), s$tree_canopy_cover, NA)

s <- crop(s, huila) %>% mask(huila)

#rasterize expert Recs for huila
recs_r_tic <- rasterize(recs_huila, s$b1, field="TIA1")
recs_r_tip <- rasterize(recs_huila, s$remapped, field="TIA2")
recs <- c(recs_r_tic, recs_r_tip)

s2 <- c(s, recs)
s2$TIA1 <- ifel(is.na(s2$b1), NA, s2$TIA1)
s2$TIA2 <- ifel(is.na(s2$remapped), NA ,s2$TIA2)
s2$TIA_both <- merge(s2$TIA1, s2$TIA2)

delta <- s2$TIA_both - s2$tree_canopy_cover


#current ha of tree cover in ag lands
fc_ha <- global(s2$tree_canopy_cover, sum, na.rm=T)/100*(res(s2$tree_canopy_cover)[[1]]*res(s2$tree_canopy_cover)[[1]]/10000)
#42360.32 ha

#potential addition for ag lands
delta_ha <- global(delta, sum, na.rm=T)/100*(res(delta)[[1]]*res(delta)[[1]]/10000)
#18410.5 ha



#LETS MAKE-A DA MAPS #
library(tidyterra)

myMap <- get_stamenmap(bbox = c(left = -76.7,
                                bottom = 1.56,
                                right = -74.52,
                                top = 3.8),
                       maptype = "terrain", 
                       crop = FALSE,
                       zoom = 10)


ra <- aggregate(delta, fact=3, fun=mean, na.rm=T) %>% as.polygons()
ra <- ra[ra$TIA_both>0,]

delta_map <- ggmap(myMap)+
  #scale_fill_brewer(palette = "OrRd") +
  geom_sf(data = st_as_sf(ra), 
               aes(fill = TIA_both),col=NA, 
               inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(huila), col="black", size=0.5,fill=NA, inherit.aes = FALSE)+
  coord_sf(crs = st_crs(4326))+
  scale_fill_viridis_c(option="inferno", direction = -1)+
  ylab("")+
  xlab("")
  
delta_map


ggsave("ag_delta_huila.png", delta_map, dpi=600, height=6, width=8)



# 
# ggplot(huila) +
#   geom_spatraster(data = ra) +
#   geom_spatvector(fill = NA) +
#   scale_fill_whitebox_c(
#     palette = "deep", direction = -1,
#     #labels = scales::label_number(suffix = "º")
#   ) +
#   theme_minimal() +
#   coord_sf(crs = 9377) +
#   labs(
#     fill = "",
#     title = "Delta Tree Cover (%)",
#     subtitle = "TIA1"
#   )


### MATO GROSSO ###

mg <- vect("../../Downloads/MatoGrosso_boundary_shp/mt656st7052.shp") %>% terra::project('epsg:5880')

recs <- vect("TIA/expertRecs_lessComplex.shp")
recs_mg <- subset(recs, recs$COUNTRY=="Brazil") %>% terra::project('epsg:5880') %>% crop(mg)


#load in crop, grazing, and forest cover
crop <- rast("../../Downloads/drive-download-20230310T190337Z-001/crop_presence_matogrosso.tif") %>% terra::project('epsg:5880')
graze <- rast("../../Downloads/drive-download-20230310T190337Z-001/grazing_presence_matogrosso.tif") %>% terra::project('epsg:5880')
fls <- list.files("../../Downloads/drive-download-20230310T190337Z-001/", "forest", full.names=TRUE) 
fc_rs <- sprc(lapply(fls, rast))
fc <- mosaic(fc_rs)  %>% terra::project('epsg:5880', method="bilinear")


#get rid of everything over 25% tree cover
s <-c(fc, crop, graze)
s$b1 <- ifel(s$b1==1 & fc<=25, 1, NA)
s$remapped <- ifel(s$remapped==1 & fc<=25, 1, NA)
s$ag <- terra::merge(s$b1, s$remapped)
s$tree_canopy_cover  <- ifel(s$tree_canopy_cover<=25 & !is.na(s$ag), s$tree_canopy_cover, NA)

s <- crop(s, mg) %>% mask(mg)

#rasterize expert Recs for mato grosso
recs_r_tic <- rasterize(recs_mg, s$b1, field="TIA1")
recs_r_tip <- rasterize(recs_mg, s$remapped, field="TIA2")
recs <- c(recs_r_tic, recs_r_tip)

s2 <- c(s, recs)
s2$TIA1 <- ifel(is.na(s2$b1), NA, s2$TIA1)
s2$TIA2 <- ifel(is.na(s2$remapped), NA ,s2$TIA2)
s2$TIA_both <- merge(s2$TIA1, s2$TIA2)

delta <- s2$TIA_both - s2$tree_canopy_cover


#current ha of forest cover
#test_tree_cover <- project(s$tree_canopy_cover, 'epsg:5880')
fc_ha <- global(s2$tree_canopy_cover, sum, na.rm=T)/100*(res(s2$tree_canopy_cover)[[1]]*res(s2$tree_canopy_cover)[[1]]/10000)


#potential addition for TIA (both)
#delta <- project(delta, 'epsg:5880')
delta_ha <- global(delta, sum, na.rm=T)/100*(res(delta)[[1]]*res(delta)[[1]]/10000)


wkt <- "PROJCS[“unknown”,GEOGCS[“unknown”,DATUM[“Unknown based on GRS80 ellipsoid”,SPHEROID[“GRS 1980”,6378137,298.257222101,AUTHORITY[“EPSG”,”7019”]]],PRIMEM[“Greenwich”,0,AUTHORITY[“EPSG”,”8901”]],UNIT[“degree”,0.0174532925199433,AUTHORITY[“EPSG”,”9122”]]],PROJECTION[“Albers_Conic_Equal_Area”],PARAMETER[“latitude_of_center”,-12],PARAMETER[“longitude_of_center”,-54],PARAMETER[“standard_parallel_1”,-2],PARAMETER[“standard_parallel_2”,-22],PARAMETER[“false_easting”,5000000],PARAMETER[“false_northing”,10000000],UNIT[“metre”,1,AUTHORITY[“EPSG”,”9001”]],AXIS[“Easting”,EAST],AXIS[“Northing”,NORTH]]"



#LETS MAKE-A DA MAPS #
library(tidyterra)

myMap <- get_stamenmap(bbox = c(left = -63,
                                bottom = -19,
                                right = -50,
                                top = -7),
                       maptype = "terrain", 
                       crop = FALSE,
                       zoom = 7)


ra <- aggregate(delta, fact=10, fun=mean, na.rm=T) %>% as.polygons()
ra <- ra[ra$TIA_both>0,]

delta_map <- ggmap(myMap)+
  #scale_fill_brewer(palette = "OrRd") +
  geom_sf(data = st_as_sf(ra), 
          aes(fill = TIA_both),col=NA, 
          inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(mg), col="black", size=0.5,fill=NA, inherit.aes = FALSE)+
  coord_sf(crs = st_crs(4326))+
  scale_fill_viridis_c(option="inferno", direction = -1)+
  ylab("")+
  xlab("")

#delta_map


ggsave("ag_delta_mg.png", delta_map, dpi=600, height=8, width=8)


