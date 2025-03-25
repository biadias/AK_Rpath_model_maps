#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL AUTHORS: Grant Adams and Alberto Rovelinni
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#

# WARNING: RUN THE TWO LINES BELOW FIRST it is to install rgdal that is deprecated
#url <- "https://download.r-forge.r-project.org/bin/windows/contrib/4.4/rgdal_1.6-7.zip"
#install.packages(url, type="source", repos=NULL)

library(rgdal)
library(rbgm)
library(sf)
library(terra)
library(ggplot2)
library(here)
library(tidyverse)
library(ggrepel)
library(ggpattern)
library(magick)

#library("googledrive")
#googledrive::drive_auth(email = "enter your email here!") #RUN THIS with your e-mail
#READ https://lter.github.io/scicomp/tutorial_googledrive-pkg.html connect googledrive to R


#Alaska polygon
AKpoly <- read_sf("shapefiles/AKpoly.shp")
AKpoly <- st_transform(x = AKpoly, "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")


fgdb <- "AK_Crab_Management_Areas.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

fc <- rgdal::readOGR(dsn=fgdb, layer="Alaska_Marine_Areas_AK_prj")
fc2 <- spTransform(x = fc, CRS("+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"))

AKmngtArea <- st_as_sf(fc)
AKmngtA <- st_transform(AKmngtArea, crs = "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

#All the information from ESR

#Chukchi <- read_sf("shapefiles_code/Chukchi_Conner2019.shp")
#Chukchi <- st_transform(x = Chukchi, "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

NBS <- read_sf("shapefiles/nebs_strata.shp")
NBS <- st_transform(x = NBS, "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

EBS <- read_sf("shapefiles/mips.shp")
EBS <- st_transform(x = EBS, "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

EGOA <- read_sf("shapefiles/EGOA.shp")
EGOA <- st_transform(x = EGOA, "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

WGOA <- read_sf("shapefiles/WGOA.shp")
WGOA <- st_transform(x = WGOA, "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")


nmfs <- AKmngtA %>% filter(NMFS_REP_AREA %in% c(610,620,630,640, 650)) %>% # subset to 610-650
  dplyr::select(NMFS_REP_AREA, geometry)


akplots_v2 <- AKmngtA %>% 
  drop_na(Ecosystem_Area) %>% 
  ggplot()+
  geom_sf(data= drop_na(nmfs),aes(color= NMFS_REP_AREA, geometry= geometry), fill=NA, color= "#999999", alpha=0.8)+
  #geom_sf(data=Chukchi, aes(fill= "Chukchi"), color= NA, alpha=0.5)+
  geom_sf(data=EBS, aes(fill= "EBS"), color= NA, alpha=0.75)+
  geom_sf(data=WGOA, aes(fill= "WGOA"), color= NA, alpha=0.75)+
  geom_sf(data=EGOA, aes(fill= "EGOA"), color= NA, alpha=0.75)+
  geom_sf(data=NBS, aes(fill= "NBS"), color= NA, alpha=0.75)+
  #geom_sf_pattern(data=WGOA, aes(pattern_type="WGOA"), pattern = 'magick',pattern_fill = "#E69F00",  pattern_alpha=0.6, color="#E69F00", fill=NA)+
  #geom_sf_pattern(data=EGOA, aes(pattern_type="EGOA"), pattern = 'magick',pattern_density= 0.01, pattern_fill = "#E69F00", pattern_alpha=0.6, color="#E69F00", fill=NA)+
  geom_sf(data=AKpoly,  fill= "#999999", color= NA, alpha=0.5)+
  geom_sf_text(data= drop_na(nmfs, NMFS_REP_AREA),
               aes(label = NMFS_REP_AREA),
               size = 2,
               hjust = 0.5, 
               color= "#333333"
  )+
  theme_bw() +
  theme(panel.grid.major = element_line(color = "#cccccc", 
                                        linetype = "dashed", linewidth = 0.5))+
  theme_bw() +
  theme(panel.grid.major = element_line(color = "#cccccc", 
                                        linetype = "dashed", linewidth = 0.5))+
  xlab("") + 
  ylab("") +
  scale_fill_manual(values= c( "WGOA"= "#E69F00",
                               "EGOA"= "#009E73",
                               #"Chukchi"="#0072B2", 
                               "NBS"="#0072B2", 
                               "EBS"="#56B4E9" 
  ))+
  #scale_pattern_type_manual(values = c("WGOA"="left30", 
  #                                     "EGOA"="right45")) +
  labs( x = "", y = "", fill = " ", pattern_type=" ") +
  
  coord_sf()


akplots_v2
ggsave( "figures/EWE_ak_plots.png", akplots_v2, height=4, width=8, dpi = 600)
