########PRIMER TO SPATIAL DATA
#L HEDGE UNSW 9/9/2014



######Introduction to Spatial DATA

data(volcano)


####LINES, POLYGONS, GRIDS
contour(volcano, color.palette = terrain.colors, asp = 1) #lines and polygons (sort of)
filled.contour(volcano, color.palette = terrain.colors, asp = 1)   #GRIDS

###CLASSES

y<-runif(100, min=6,max=10)
y
class(y)

x<-c('James','Lavender','needs','a','hair','cut')
x
class(x)

model.formula<-formula(x~y)
model.formula
class(model.formula)


###Spatial CLASSES
install.packages('sp')
library(sp)

getClass('Spatial')
getClass('CRS')


###Spatial Class doesnt mean much. Just a bounding box and a PROJ4 string. Sub classes 
#are where the real fun is.

getClass('SpatialPoints')
getClass('SpatialPolygons')
getClass('SpatialPixels')


###it gets even funner with the subclasses of theme_segment
getClass('SpatialPointsDataFrame')
getClass('SpatialPolygonsDataFrame')


####Creating a simple spatial dataset

#read in the data
CRAN_df<-read.table(paste0(getwd(),'/Data/CRAN051001a.txt'), header=T, stringsAsFactors=F)
head(CRAN_df)

#we only want the coordinates....now. (we'll come to data a bit laterz)
CRAN_mat<-cbind(CRAN_df$long, CRAN_df$lat)
row.names(CRAN_mat)<-1:nrow(CRAN_mat)
str(CRAN_mat)

#remember what things we need for a SpatialPoints
getClass('SpatialPoints')

#remember we need a CRS to tell us how to interpret the coordinates
CRAN_CRS<-CRS("+proj=longlat +ellps=WGS84")

#MAKE YOUR FIRST SPATIAL OBJECT
CRAN_sp<-SpatialPoints(coords=CRAN_mat, proj4string=CRAN_CRS)


install.packages('maps')
install.packages('maptools')
install.packages('mapdata')

library(maps)
library(maptools)
library(mapdata)

plot(CRAN_sp, col='red', pch=16)
map('worldHires', add=T)
mtext("Cran mirrors throughout the world",side=3,line=1, adj=0)


#Remember the Bounding Box? sp will make one automatically based on the data
bbox(CRAN_sp)

#also check the projection information
proj4string(CRAN_sp)

#summary methods
summary(CRAN_sp)


#getting tricky now. Say we want the coordinates of the CRAN Mirrors in Australia? Just use your normal R style 
#subsetting methods.
aus<-which(CRAN_df$loc=='Australia')
aus
coordinates(CRAN_sp)[aus,]

germany<-which(CRAN_df$loc=='Germany')
germany
coordinates(CRAN_sp)[germany,]

#you can subset the whole spatialdata in this way too
germany_sp<-CRAN_sp[germany,]
summary(germany_sp)
plot(germany_sp)


########ADDING DATA TO POINTS############

#use SpatialPointsDataFrame

#will match the row names of the data to the row names of the matrix of the sp coordinate matrix
str(row.names(CRAN_df))

#not really important. They match. 

CRAN_spdf<-SpatialPointsDataFrame(CRAN_mat, CRAN_df, proj4string=CRAN_CRS)
plot(CRAN_spdf)


###LETS LOOK AT SLOTS
str(CRAN_spdf)


###SPATIAL LINES


#get a list of points around australia using the 'maps' package
australia<-map("world","australia",plot=FALSE)

aus_CRS<-CRS(("+proj=longlat +ellps=WGS84"))

aus_lines<-map2SpatialLines(australia, proj4string=aus_CRS)
plot(aus_lines)

str(aus_lines,2)


####SPATIAL POLYGONS

#spatial polygons are just closed lines. Where the coordinate of the end of the line is the same as the start of the line
getClass('SpatialPolygons')

#the 'polygons' slot is where all the action is.
getClass('Polygon')

#labpt- centroid of the polygon
#area- area of the polygon in the units of the coordinates
#hole- whether the polygon is a hole or not
#ringDir- the direction of the ring
#coords- the coordinates of the vertices of the outer ring of the polygon. The thing that 'makes' the polygon.


###SPATIALPOLYGONDATAFRAME
#Just add data tto the mix 
getClass('SpatialPolygonsDataFrame')


###BUT WE DON"T REALLY JUST 'MAKE' A SPATIALPOLYGON OR SPATIALLINES DATASET. WE USUALLY IMPORT THEM.

# this is where the OGR rgdal framework comes into play
library(rgdal)

#pull up what sorts of files rgdal can deal with. Note that the main one is the ESRI Shapefile. 
#these ware what ArcMap uses.

ogrDrivers()

#pull in a spatialpolygondataframe from file

sydney<-readOGR(dsn=paste0(getwd(),"/Data"), layer='SH_est_poly_clipped')
plot(sydney)

#Katherine wants to deploy 20 BRUVS into Sydney Harbour over 3 time periods (60 drops) and wants to include a map
#of the locations of each BRUV drop for each time.


time1<-spsample(sydney, n=20, type='random')
time2<-spsample(sydney, n=20, type='random')
time3<-spsample(sydney, n=20, type='random')

####OR#####
times<-1:3
sites<-list()
	for (i in 1:length(times)){
		sites[[i]]<-spsample(sydney, n=20, type='random')
	}	
sites
##########

####PLOT THE SITES
class(time1) ##spsample returns a SpatialPoints object

cols<-c('aquamarine','firebrick','dodgerblue')

plot(sydney, lwd=0.6)
points(time1, pch=16, cex=0.5, col=cols[1])
points(time2, pch=16, cex=0.5, col=cols[2])
points(time3, pch=16, cex=0.5, col=cols[3])
title('Sampling sites in Sydney Harbour', cex=0.8)
legend( "bottomleft", legend=c("Time One","Time Two","Time Three"), col=cols, pch=16,bty="n")
box()


###MAKING MAPS IN GGPLOT

#We have to present a series of maps to Transport for NSW outlining where we'll be sampling 
#in our upcoming project looking at recreational boating in Sydney Harbour. These maps are 
#part of larger document outlining our 'Scope of Work'. This code has been embedded in a 
#knitr document so that it can easily be updated if things change
install.packages('ggmap')
install.packages('grid')
install.packages('rgeos')

library(ggplot2)
library(ggmap)
library(grid)
library(rgeos)

#mooring field locations
names<-c('Clontarf', 'Hunters Bay','North Harbour','Watsons Bay', 'Camp Cove','Quarantine Bay')
type<-c('Mooring/Marina', 'Mooring', 'Mooring/Marina','Mooring','None','None')
lat<-c(-33.804360,-33.826925,-33.799713,-33.842970, -33.839139, -33.814582)
lon<-c(151.250773,151.254970,151.269397,151.279814,151.277849,151.285352)
locations<-data.frame(Area=as.character(names), Lat=lat, Lon=lon, Type=type)

#get a background map of sydney
sydney_map<-get_map(location=c(151.248,-33.835), zoom=13, source='stamen', maptype='toner')


map1<-ggmap(sydney_map)+
	  geom_point(data=locations, aes(x=lon, y=lat, col=Type), size=5)+
	  scale_colour_brewer(palette="Dark2")+
	  labs(x = "Longitude", y= 'Latitude')+
	  theme(legend.position=c(0.15,0.85),legend.key.size = unit(0.7, "cm"),
        legend.direction="vertical",plot.title = element_text(size = rel(1.1),hjust=0),
        axis.title=element_blank(),legend.text=element_text(size=7),plot.title = element_text(size = rel(2), colour = "blue"))+
	  geom_text(data=locations, aes(x=lon, y=lat, label=Area, col=Type, hjust=1.1, vjust=0))
map1

#####OR TRY IT WITH ANOTHER BACKGROUND MAP

sydney_map_2<-get_map(location=c(151.248,-33.835), zoom=13)


map1<-ggmap(sydney_map_2)+
	  geom_point(data=locations, aes(x=lon, y=lat, col=Type), size=5)+
	  scale_colour_brewer(palette="Dark2")+
	  labs(x = "Longitude", y= 'Latitude')+
	  theme(legend.position=c(0.15,0.85),legend.key.size = unit(0.7, "cm"),
        legend.direction="vertical",plot.title = element_text(size = rel(1.1),hjust=0),
        axis.title=element_blank(),legend.text=element_text(size=7),plot.title = element_text(size = rel(2), colour = "blue"))+
	  geom_text(data=locations, aes(x=lon, y=lat, label=Area, col=Type, hjust=1.1, vjust=0))
map1


####A MORE COMPLEX EXAMPLE WITH SIMPLE DATA MANIPULATION

#at each of those sites above, we want to get the location of moorings and then plan a sampling regime 
#that samples randomly from different distances away from each mooring.

###PLOT ONE- The Mooring Locations
mooring_list_sp<-readRDS(file=paste0(getwd(),'/Data/sydney_moorings'))
clontarf<-mooring_list_sp[[2]]

plot(clontarf, sub='One: Map mooring locations', pch=16, cex=0.5)
title(main='One: Map mooring locations')

convex<-gConvexHull(clontarf, byid=F)
plot(convex,add=T)

inner_zone<-gBuffer(convex, width=-60, byid=F)
plot(inner_zone, add=T)

sample_point<-spsample(inner_zone, n=1, type='random')
plot(sample_point, add=T, cex=1, col='red')

sampling_radius<-gBuffer(sample_point, width=60, byid=F)
plot(sampling_radius,add=T, lty=3)

included_points<-over(clontarf, sampling_radius)
included_points<-clontarf[!is.na(included_points)]
plot(included_points, cex=1.5, col='dodgerblue', add=T)


library(raster)
getClass('Raster')
rast<-raster()
projection(rast)<-proj4string(clontarf)
extent(rast)<-extent(clontarf)
res(rast)<-1

clontarf_rast<-rasterize(clontarf,rast,field=1)
clontarf_dist_rast<-distance(clontarf_rast)
plot(clontarf_dist_rast)

sampling_zone<-mask(clontarf_dist_rast,sampling_radius)
plot(sampling_zone)


plot(clontarf, sub='One: Map mooring locations', pch=16, cex=0.5)
title(main='One: Map mooring locations')
plot(convex,add=T)
plot(inner_zone, add=T)
plot(sample_point, add=T, cex=1, col='red')
plot(sampling_radius,add=T, lty=3)
plot(sampling_zone, add=T)
plot(included_points, cex=1.5, col='dodgerblue', add=T)


#####RASTERS####
#moving on to raster layers. The basics.

r<-raster(ncol=10, nrow=10)
getClass('Raster')
getClass('RasterLayer')

values(r)<-1:100
plot(r)

values(r)<-runif(100)
plot(r)


##raster arithmetic

p<-raster(ncol=10,nrow=10)
values(p)<-runif(length(p)) #notice we are moving away from hard coding

s<-r+p
plot(s)

a<-mean(p,q,r)
plot(a)


stack1<-stack(r,p,s,a)
names(stack1)<-c('Layer_R','Layer_Q','Layer_S','The_Mean')
plot(stack1)


#####A real world example
####Kingsley and I needed to find sites to survey that encompassed rocky reef, but was less than 10m in depth. We had
#a raster layer from DPI for sub-tidal macrophytes and a rasterlayer from OEH from the depthy

bathy<-raster(paste0(getwd(),'/Data/bathymetry.grd'))
plot(bathy)
bathy


####Subsetting to only depths less than 
bathy.10m<-bathy
bathy.10m[bathy<=-10]<-NA
plot(bathy.10m)


benthos<-raster(paste0(getwd(),'/Data/benthos.grd'))
plot(benthos)
benthos

rocky_reef<-benthos
rocky_reef[benthos==1 | benthos==3]<-NA
plot(rocky_reef, col='black')

####but lets do it all in one step. Where are the areas
#with rocky reef, less than 10 m

shallow_reef<-raster()
extent(shallow_reef)<-extent(bathy)
res(shallow_reef)<-res(bathy)
proj4string(shallow_reef)<-proj4string(bathy)

shallow_reef[benthos==2 & bathy>=-10]<-1
shallow_reef
plot(shallow_reef,col='red', legend=F)

plot(sydney,add=T)
##WHAT HAPPENED?

proj4string(shallow_reef)
proj4string(sydney)
CRS(sydney)

##they're not the same projections

#lucky theres a wrapper function to transfrom and project different spatial projections

sydney_utm<-spTransform(sydney, CRS=CRS("+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs"))
plot(sydney_utm, add=T, lwd=0.5)



