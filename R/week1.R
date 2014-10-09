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
class(model.formulat)


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
plot(CRAN_sp)


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
install.packages('maps')
install.packages('maptools')


library(maps)
library(maptools)

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









