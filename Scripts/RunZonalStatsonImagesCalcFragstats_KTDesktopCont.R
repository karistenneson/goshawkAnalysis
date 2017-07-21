library(devtools)
require(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(SDMTools)
library(sp)
library(tools)
library(velox)
#install_github("hunzikp/velox")

# Prepare workspace.
removeTmpFiles(h=0)
WorkingDirectory <-'C:\\Users\\krtenneson\\Desktop\\Goshawk_Phase_2\\PatchMetrics\\grass'
setwd(WorkingDirectory)

## Old Code.
#pdfPath<-"\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\Goshawk_Phase_2\\Data\\TreeSegmentation\\patchMorph\\ZonalStats"
#clusterPath<-"\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\Goshawk_Phase_2\\Data\\TreeClusters\\rasters"

# Load habitat raster file (grass habitat).
#GrassRasterPath<-"\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\Goshawk_Phase_2\\Data\\Grass"
setwd('C:\\Users\\krtenneson\\Desktop\\Goshawk_Phase_2\\PatchMetrics')
#grassPatches <- raster('Grassraster_CHbelow3.tif')
grassPatches <- raster('Grass_raster_withRoads.tif')
plot(grassPatches) 

# Read nest sites.
#nests <- readShapeSpatial("\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\Goshawk_Phase_2\\Data\\Shapefiles\\NestSites\\NestSites_KT_1.shp.shp")
nests <- readOGR("C:\\Users\\krtenneson\\Desktop\\Goshawk_Phase_2\\PatchMetrics\\NestSites\\NestSites_KT_2.shp")
plot(nests, add = T)

# Change working directory.
setwd(WorkingDirectory)

## Create a variable storing plot names.
testSitesTable <- data.frame(nests)
testSitesTable[37,1:5]
colnames(testSitesTable)
plotID<-      testSitesTable$NAME
coordinates<- testSitesTable[ ,c('coords.x1', 'coords.x2')]

# Set buffer widths
bufferFile <- c(2780, 735, 196, 180)
bufferFileName<- NULL
buffer <- max(bufferFile)
# Clip raster extent to match nest sites extent.
NestExtent<-extent(nests)
NestExtent[c(2,4)]<-NestExtent[c(2,4)]+buffer
NestExtent[c(1,3)]<-NestExtent[c(1,3)]-buffer
grassPatches <- crop(grassPatches, NestExtent)

# Test cropped data.
plot(grassPatches) 
plot(nests, add = T)

# create a blank table.
SummaryTable<- NULL
#FullSummaryTable<-read.csv("FragstatsGrassRds_KTDesktopv2.csv")
# Create a vector of colors.
colorRamp <- rainbow(100)
 
###################################################################
###################################################################

for (i in 38:length(plotID)){
  extent <- c(testSitesTable$coords.x1[i]-buffer, testSitesTable$coords.x1[i]+buffer, 
              testSitesTable$coords.x2[i]-buffer, testSitesTable$coords.x2[i]+buffer)
  tile <- crop(grassPatches, extent)
  #plot(tile)
  
  nestSites <- SpatialPoints(testSitesTable[i,c('coords.x1','coords.x2')])
  #plot(nestSites, add = T, pch = 17, cex = 2)
  
  for (b in 1:length(bufferFile)){
    spol <- gBuffer(nestSites, width = bufferFile[b], byid=TRUE)
    tile.sub <- mask(tile, spol)
    #plot(tile.sub, col = c('white','red','blue',colorRamp[b]), add = T)
    #plot(spol, add = T)
    
    # Calculate metrics...
    #patch<-PatchStat(tile.sub, cellsize = 1, latlon = FALSE)
    class <- ClassStat(tile.sub, cellsize = 1, bkgd = NA, latlon = FALSE)
    class <- cbind(OBJECTID = rep(testSitesTable$OBJECTID[i], times = length(class[,1])), 
                   PlotSite = rep(testSitesTable$NAME[i], times = length(class[,1])),
                   NestSite = rep(testSitesTable$NEST[i], times = length(class[,1])),
                   SystemTime = rep(Sys.time(), times = length(class[,1])),
                   col = rep(colorRamp[i], times = length(class[,1])),
                   radius = rep(bufferFile[b], times = length(class[,1])), 
                   class) 
    # add statement to print system time as a column
    if(i == 1 & b == 1) {
      FullSummaryTable <- class 
    } else {
        FullSummaryTable<-rbind(FullSummaryTable, class)}
    
    }
  write.csv(FullSummaryTable, file = "FragstatsGrassRds_KTDesktopv2.csv", row.names = F)
}

# add in a save polygon statement - to check the output regions.
# add in a system time column

######################################################################
## Ignore below this line

######################################################################
    ######################################################################

tail(FullSummaryTable)
colnames(FullSummaryTable)
dim(FullSummaryTable)


name<- paste0(folderName,'lineplot.pdf')
#pdf(name)


xlabel <- 'radius, in m'
ylabel <- 'Number of forest clusters in the region around plot'
titleMainInput <- paste0('Number of forest clusters around nest with a VSS = ')
columnOfInterest<- "n.patches" 
maxy <- max(FullSummaryTable[,columnOfInterest])
#FullSummaryTable[FullSummaryTable$class == uniqueClass[c]& FullSummaryTable$site == uniqueSite[i], c(1: 4)]

for (c in 1:length(uniqueClass)){
  name<- paste0('\\\\166.2.126.25\\rseat\\Programs\\Reimbursibles\\fy2016\\Goshawk_Phase_2\\Data\\TreeClusters\\graphics\\',
                  columnOfInterest,'class_',uniqueClass[c],'lineplot.pdf')
  pdf(name)
  titleMain<- paste0(titleMainInput, uniqueClass[c])
  plot(c(0,max(bufferFile)),c(0,maxy), 
       xlab = xlabel, ylab = ylabel, 
       main = titleMain,
       col = "white")
  uniqueSite <- unique(FullSummaryTable$site)
  uniqueClass<- unique(FullSummaryTable$class)
  
  for (i in 1:length(uniqueSite)){
    value <-  FullSummaryTable[FullSummaryTable$site == uniqueSite[i] & FullSummaryTable$class == uniqueClass[c],columnOfInterest] 
    radiusValue <- FullSummaryTable[FullSummaryTable$site == uniqueSite[i] & FullSummaryTable$class == uniqueClass[c],'radius'] 
    #area <- 3.14*(radiusValue^2)
    LineColor <- FullSummaryTable$col[FullSummaryTable$site == uniqueSite[i]]
    lines(radiusValue, value, col = LineColor[1], lwd = 2)
  }
  legend("topleft", legend = uniqueSite, 
         fill = unique(FullSummaryTable$col), bg = 'white')
  dev.off()
  }  


