library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(rgdal)
library(tidyverse)
library(stringr)
library(dplyr)
library(spdep)
library(tmap)

#1.1 Read File
London_Borough_Shape <- st_read("https://raw.github.com/supernovajon/gis/main/London_Borough_Excluding_MHW.gpkg")

London_Borough_Shape %>%
  st_geometry() %>%
  plot()
tmap_mode("plot")

London_Borough <- London_Borough_Shape %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)
#st_write(London_Borough,"London_Borough_Excluding_MHW.gpkg")

st_layers("https://raw.github.com/supernovajon/gis/main/Rapid_charging_points.gpkg")

Rapid_charging <- st_read(("https://raw.github.com/supernovajon/gis/main/Rapid_charging_points.gpkg"),
                          layer='Rapid_charging_points')
st_crs(Rapid_charging)$proj4string
st_crs(Rapid_charging) <- 27700 #st_transform(., 27700) #st transform becomes nan

#1.2 Data Cleaning (inside borough)
Rapid_charging_in <- Rapid_charging[London_Borough,]
tm_shape(London_Borough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Rapid_charging_in) +
  tm_dots(col = "blue")

#filter by use
Taxi <- Rapid_charging_in %>%
  filter(taxipublicuses == 'Taxi')

Public <- Rapid_charging_in %>%
  filter(taxipublicuses == 'Public use'|taxipublicuses == 'Public Use')
#Capital Public Use

#2.1 Summarising Data
#for Public
tm_shape(London_Borough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Public) +
  tm_dots(col = "purple")+
  tm_format("World", title="Public Points")
#for Taxi
tm_shape(London_Borough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Taxi) +
  tm_dots(col = "red")+
  tm_format("World", title="Taxi Points")

#2.2 data manipulation
#for Public
#change the name of the borough to match
London_Borough[20,1]='Richmond'
London_Borough[23,1]='Hammersmith & Fulham'
#outside london is part of hillingdon
Public[77,1]='Hillingdon'
#Ickenham is an area centred on an old village in Greater London, forming the eastern part of Uxbridge and within the London Borough of Hillingdon

PublicSF <- Public %>%
  st_drop_geometry()

Public_group <- London_Borough %>%
  full_join(.,
            PublicSF,
            by = c("NAME" = "borough"))

#replaces the NA values with 0
Public_group[is.na(Public_group)]<-0

#count
Public_group$numberrcpoints <- as.numeric(Public_group$numberrcpoints)
Public_group <- Public_group %>%
  group_by(NAME) %>%
  summarise(.,
            area_points = sum(numberrcpoints))

Public_group <- Public_group%>%
  mutate(area=st_area(.))%>%
  mutate(DensityP=area_points/area*100000000)

Public_group$DensityP <- as.numeric(Public_group$DensityP)

#for Taxi
#one space in name needs matching
Taxi[6,1]='City of London'

TaxiSF <- Taxi %>%
  st_drop_geometry()

Taxi_group <- London_Borough %>%
  full_join(.,
            TaxiSF,
            by = c("NAME" = "borough"))

Taxi_group[is.na(Taxi_group)]<-0

Taxi$numberrcpoints <- as.numeric(Taxi$numberrcpoints)
Taxi_group$numberrcpoints <- as.numeric(Taxi_group$numberrcpoints)
Taxi_group <- Taxi_group %>%
  group_by(NAME) %>%
  summarise(.,
            area_points = sum(numberrcpoints))

Taxi_group <- Taxi_group%>%
  mutate(area=st_area(.))%>%
  mutate(DensityT=area_points/area*100000000)

Taxi_group$DensityT <- as.numeric(Taxi_group$DensityT)

#3.1 pattern points analysis
tm_shape(Public_group) +
  tm_polygons("DensityP",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("NAME", "DensityP"),
              title="Public Points Density")+
  tm_scale_bar(position = c("left", "bottom"), text.size = .75)+
  tm_layout(legend.position = c("right","top"),
            legend.text.size=.75,
            legend.title.size = 1.1,
            frame=FALSE)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))+
  tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.05))

tm_shape(Taxi_group) +
  tm_polygons("DensityT",
              style="jenks",
              palette="BuPu",
              midpoint=NA,
              popup.vars=c("NAME", "DensityT"),
              title="Taxi Points Density")+
  tm_scale_bar(position = c("left", "bottom"), text.size = .75)+
  tm_layout(legend.position = c("right","top"),
            legend.text.size=.75,
            legend.title.size = 1.1,
            frame=FALSE)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))+
  tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.05))

#for Public
window <- as.owin(London_Borough)
plot(window)

PublicSP <- Public %>%
  as(., "Spatial")
PublicSP.ppp <- ppp(x=PublicSP@coords[,1],
                    y=PublicSP@coords[,2],
                    window=window)
TaxiSP <- Taxi %>%
  as(., "Spatial")
TaxiSP.ppp <- ppp(x=TaxiSP@coords[,1],
                  y=TaxiSP@coords[,2],
                  window=window)

PublicSP.ppp %>%
  plot(.,pch=16,cex=0.5,
       main="Public_use London")
TaxiSP.ppp %>%
  plot(.,pch=16,cex=0.5,
       main="Taxi_use London")
PublicSP.ppp %>%
  density(., sigma=1000) %>%
  plot()
TaxiSP.ppp %>%
  density(., sigma=2000) %>%
  plot()
#K
KP <- PublicSP.ppp %>%
  Kest(., correction="border") %>%
  plot()
KT <- TaxiSP.ppp %>%
  Kest(., correction="border") %>%
  plot()
#G
GP <- PublicSP.ppp %>%
  Gest(., correction="rs") %>%
  plot()
GT <- TaxiSP.ppp %>%
  Gest(., correction="rs") %>%
  plot()

library(raster)
library(fpc)
library(plyr)
library(OpenStreetMap)
library(dbscan)
library(cluster)
library(factoextra)

#for Public
#extract the points from the spatial points data frame
PublicSPDB <- data.frame(PublicSP@coords[,1:2])
#compute the avg of k-nearest neighbor distances
PublicSPDB%>%
  dbscan::kNNdistplot(.,k=4)
db <- fpc::dbscan(PublicSPDB, eps = 3600, MinPts = 4)
plot(db, PublicSPDB, main = "DBSCAN Output", frame = T)
plot(London_Borough$geom, add=T)
print(db)
#Noise/outlier observations are coded as 0 (there are 15 noise points)
fviz_cluster(db, PublicSPDB, geom = "point", main = "Public_use Cluster plot",show.clust.cent = TRUE)
#noise is represented by black circles

#for Taxi
TaxiSPDB <- data.frame(TaxiSP@coords[,1:2])
TaxiSPDB%>%
  dbscan::kNNdistplot(.,k=4)
db <- fpc::dbscan(TaxiSPDB, eps = 3600, MinPts = 4)
plot(db, TaxiSPDB, main = "DBSCAN Output", frame = T)
plot(London_Borough$geom, add=T)
print(db)
fviz_cluster(db, TaxiSPDB, geom = "point", main = "Taxi_use Cluster plot",show.clust.cent = TRUE)

#3.3 Spatial autocorrelation is a measure of similarity between nearby data
#for Public
library(purrr)
library(geodaData)
coords <- cbind(PublicSPDB$coords.x1,PublicSPDB$coords.x2)
knn1 <- knearneigh(coords)
str(knn1)
k1 <- knn2nb(knn1)

#KNN k=4， 4 closest point
coords <- cbind(PublicSPDB$coords.x1,PublicSPDB$coords.x2)
knn4 <- knearneigh(coords, k=4)
str(knn4)
k4 <- knn2nb(knn4)
plot(k4, coords, lwd=.3, col="red", cex = 1)
plot(Public_group$geom, add=T)

coords2 <- cbind(TaxiSPDB$coords.x1,TaxiSPDB$coords.x2)
knn2 <- knearneigh(coords2)
str(knn2)
k2 <- knn2nb(knn2)

critical.threshold <- max(unlist(nbdists(k1,coords)))
critical.threshold

critical.threshold2 <- max(unlist(nbdists(k2,coords2)))
critical.threshold2
#distance based neighbor
nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
dist.band.card <- card(nb.dist.band)
dist.band.card

nb.dist.band2 <- dnearneigh(coords2, 0, critical.threshold2)
dist.band.card2 <- card(nb.dist.band2)
dist.band.card2

ggplot() +
  geom_histogram(aes(x=dist.band.card)) +
  stat_bin(bins = 30)
xlab("Number of Neighbors")
ggplot() +
  geom_histogram(aes(x=dist.band.card2)) +
  stat_bin(bins = 30)
xlab("Number of Neighbors2")

#Connectivity graph
plot(nb.dist.band, coords, lwd=.15, col="red", cex =1)
plot(Public_group$geom, add=T)

plot(nb.dist.band2, coords2, lwd=.15, col="red", cex =1)
plot(Taxi_group$geom, add=T)

#Isolates
#as the critical threhold is 7100, an upper distance bound 3500 is used instead
dist.band.iso <- dnearneigh(coords, 0, 3500)
iso.card <- card(dist.band.iso)
ggplot() +
  geom_histogram(aes(x=iso.card)) +
  xlab("Number of Neighbors")
plot(dist.band.iso, coords, lwd=.8, col="blue", cex =1)
plot(Public_group$geom, add=T)

dist.band.iso2 <- dnearneigh(coords2, 0, 2900)
iso.card2 <- card(dist.band.iso2)
ggplot() +
  geom_histogram(aes(x=iso.card2)) +
  xlab("Number of Neighbors2")
plot(dist.band.iso2, coords2, lwd=.8, col="blue", cex =1)
plot(Taxi_group$geom, add=T)

#3.4 First calculate the centroids (borough level)
Public$numberrcpoints <- as.numeric(Public$numberrcpoints)
points_sf_joined <- London_Borough%>%
  st_join(Public)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=numberrcpoints/area*100000000)
points_sf_joined[is.na(points_sf_joined)]<-0

CentroidP <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()
plot(CentroidP,axes=TRUE)
#create a neighbours list
knn1 <- knearneigh(CentroidP)
str(knn1)
k1 <- knn2nb(knn1)
critical.threshold <- max(unlist(nbdists(k1,CentroidP)))
critical.threshold
nb.dist.band <- dnearneigh(CentroidP, 0, critical.threshold)
plot(nb.dist.band, st_geometry(CentroidP), col="red")
plot(points_sf_joined$geom, add=T)

#create a spatial weights object from these weights
DPublic.lw <- nb.dist.band %>%
  nb2listw(., style="C")
head(DPublic.lw$neighbours)

#calculate the Moran’s I and other associated statistics
I_LPublic_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., DPublic.lw)
I_LPublic_Global_Density

#Geary’s C tells us whether similar values or dissimilar values are clustering
C_LPublic_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., DPublic.lw)
C_LPublic_Global_Density

#Getis Ord General G tells us whether high or low values are clustering. If G > Expected = High values clustering; if G < expected = low values clustering
G_LPublic_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., DPublic.lw)
G_LPublic_Global_Density

#local versions of the Moran’s I statistic and a Getis Ord statistic to see where we have hot-spots
Gi_LPublic_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., DPublic.lw)
points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LPublic_Local_Density))
GIColours<- rev(brewer.pal(8, "RdBu"))
#set breaks manually based on 99%, 95% 90% significant level and min/max value.
breaks1<-c(-7,-2.58,-1.96,-1.65,1.65,1.96,2.58,7)
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Public uses in London")+
  tm_scale_bar(position = c("left", "bottom"), text.size = .75)+
  tm_layout(legend.position = c("right","top"),
            legend.text.size=.75,
            legend.title.size = 1.1,
            frame=FALSE)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))+
  tm_text(text = "name", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.05))

#for Taxi
Taxi$numberrcpoints <- as.numeric(Taxi$numberrcpoints)
points_sf_joinedT <- London_Borough%>%
  st_join(Taxi)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density=numberrcpoints/area*100000000)
points_sf_joinedT[is.na(points_sf_joinedT)]<-0
CentroidT <- points_sf_joinedT%>%
  st_centroid()%>%
  st_geometry()
plot(CentroidT,axes=TRUE)

knn2 <- knearneigh(CentroidT)
str(knn2)
k2 <- knn2nb(knn2)
critical.threshold <- max(unlist(nbdists(k2,CentroidT)))
critical.threshold
nb.dist.band2 <- dnearneigh(CentroidT, 0, critical.threshold)
plot(nb.dist.band2, st_geometry(CentroidT), col="red")
plot(points_sf_joinedT$geom, add=T)

DTaxi.lw <- nb.dist.band2 %>%
  nb2listw(., style="C")
head(DTaxi.lw$neighbours)
I_LTaxi_Global_Density <- points_sf_joinedT %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., DTaxi.lw)
I_LTaxi_Global_Density

C_LTaxi_Global_Density <- points_sf_joinedT %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., DTaxi.lw)
C_LTaxi_Global_Density

G_LTaxi_Global_Density <- points_sf_joinedT %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., DTaxi.lw)
G_LTaxi_Global_Density

Gi_LTaxi_Local_Density <- points_sf_joinedT %>%
  pull(density) %>%
  as.vector()%>%
  localG(., DTaxi.lw)
points_sf_joinedT <- points_sf_joinedT %>%
  mutate(density_GT = as.numeric(Gi_LTaxi_Local_Density))
GIColours<- rev(brewer.pal(8, "RdBu"))
breaks1<-c(-7,-2.58,-1.96,-1.65,1.65,1.96,2.58,7)
tm_shape(points_sf_joinedT) +
  tm_polygons("density_GT",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Taxi uses in London")+
  tm_scale_bar(position = c("left", "bottom"), text.size = .75)+
  tm_layout(legend.position = c("right","top"),
            legend.text.size=.75,
            legend.title.size = 1.1,
            frame=FALSE)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))+
  tm_text(text = "name", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.05))

#4. other variables (as with the results that taxi are too clustered hence, use public to assess)
co2_emissions_London <- read.csv("https://raw.github.com/supernovajon/gis/main/co2-emissions.csv",
                                 header = TRUE,
                                 sep = ",",
                                 encoding = "latin1")
#change the name of the borough to match
co2_emissions_London[21,1]='Kingston upon Thames'
co2_emissions_London[13,1]='Hammersmith & Fulham'
co2_emissions_London[7,1]='City of London'
points_sf_joined_co2 <- points_sf_joined %>%
  full_join(.,
            co2_emissions_London,
            by = c("name" = "co2"))
I_LPublic_Local_co2 <- points_sf_joined_co2 %>%
  arrange(name)%>%
  pull(X2017) %>%
  as.vector()%>%
  localmoran(., DPublic.lw)%>%
  as_tibble()
points_sf_joined_co2 <- points_sf_joined_co2 %>%
  arrange(name)%>%
  mutate(co2_LocIz = as.numeric(I_LPublic_Local_co2$Z.Ii))
breaks2<-c(-6,-2.58,-1.96,-1.65,1.65,1.96,2.58,6)
tm_shape(points_sf_joined_co2) +
  tm_polygons("co2_LocIz",
              style="fixed",
              breaks=breaks2,
              palette=GIColours,
              midpoint=NA,
              title="Local Moran's I, co2 emission")

#Now the Gi* statistic to look at clusters of high and low scores...
G_LPublic_Local_co2 <- points_sf_joined_co2 %>%
  dplyr::arrange(name)%>%
  dplyr::pull(X2017) %>%
  as.vector()%>%
  localG(., DPublic.lw)
points_sf_joined_co2 <- points_sf_joined_co2 %>%
  dplyr::arrange(name)%>%
  dplyr::mutate(co2_LocGiz = as.numeric(G_LPublic_Local_co2))
tm_shape(points_sf_joined_co2) +
  tm_polygons("co2_LocGiz",
              style="fixed",
              breaks=breaks2,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, co2 emission")+
  tm_credits("(c) OpenStreetMap contrbutors", position=c(0.0,0.0))+
  tm_text(text = "name", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.05))

#save plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="GIS assessment")
