library(raster)
library(rgdal)
library(randomForest)
library(foreign)

################################################################################
# Random forest predictor.
# ------------------------
# Once you've created the random forest class object using various predictors,
# use this function to create a raster surface predicting the class or coverage
# results over any landscape.  The input raster "imagefile" is expected to be
# a GeoTIFF only comprising bands representing each of the predictors.
#
# "bandnames" must simply be the character list of band names corresponding to
# the predictors, IN THE ORDER THEY APPEAR IN THE GEOTIFF FILE.  These names
# must be same as those used to create the rando forest object (obviously).
#
# Needless to say, "rf" is the random forst object previously created.
# The function should create a single band GeoTIFF file with the same name as
# "imagefile", but prefixed by "p_".
#
# Damien - 27 August 2014
#
################################################################################

rfPredict <- function(rf, imagefile, bandnames, imageout=paste0("p_",imagefile)) {
  o <- raster(imagefile,band=1) #Import the first raster band as prototype
  nbands <- length(bandnames)
  
  # Now put all data into a data frame
  data_all<-data.frame(values(raster(imagefile,band=1)))
  
  for(i in 2:nbands){
    data_all<-cbind(data_all,values(raster(imagefile,band=i))) # 
  }
  bandnames->names(data_all)
  
  # Now get rid of any rows that have a null value in any of the columns
#   for(i in 1:nbands){
#     data_all<-data_all[!is.na(data_all[,i]),]
#   }
      
  p<-predict(rf,newdata=data_all)  # Now classify the whole

  values(o)<-as.numeric(p)
  writeRaster(o,imageout,'GTiff')
  return(p)
}