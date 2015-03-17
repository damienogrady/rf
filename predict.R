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
# must be same as those used to create the random forest object (obviously).
#
# Needless to say, "rf" is the random forst object previously created.
# The function should create a single band GeoTIFF file with the same name as
# "imagefile", but prefixed by "p_".
#
# Damien - 27 August 2014
# 18 March 2015 - changed to work on batches of 100000 cells at a time so tgat
# memory can manage it.
#
################################################################################

rfPredict <- function(rf, imagefile, bandnames, imageout=paste0("p_",imagefile)) {
  o <- raster(imagefile,band=1) #Import the first raster band as prototype
  nbands <- length(bandnames)
  out<-matrix()
  s<-stack(imagefile)
  names(s)<-bandnames

  batches<-ceiling(ncell(s)/100000)
  
  ptm <- proc.time()
  for (i in 1:batches) {
    ulimit <- i * 100000
    llimit <- (ulimit - 100000) + 1
    ulimit <- ifelse(ulimit > ncell(s), ncell(s), ulimit)
    out[llimit:ulimit]<-predict(rf,newdata=s[llimit:ulimit])
  }
  print(proc.time() - ptm)

  values(o)<-out
  writeRaster(o,imageout,'GTiff',datatype='INT1U')
  return(1)
}