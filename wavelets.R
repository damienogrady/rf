library(raster)
library(rgdal)
library(waveslim)


r<-raster("ndvi14.tif")
rm<-as.matrix(r) # Get the values
rm[is.na(rm)]<-0 # Replace NA values with 0

o<-r #for output raster
tm<-as.matrix(o)

ntilecols<-as.integer(ncol(rm)/500)
ntilerows<-as.integer(nrow(rm)/500)

for (tilerow in 1:ntilerows) {
  fromrow <- 500 * (tilerow - 1) + 1
  if (tilerow == ntilerows) {
    torow<-nrow(rm)
  } else {
    torow<-fromrow + 499
  }
  for (tilecol in 1:ntilecols) {
    fromcol <- 500 * (tilecol - 1) + 1
    if (tilecol == ntilecols) {
      tocol<-ncol(rm)
    } else {
      tocol<-fromcol + 499
    }
    w<-mra.2d(rm[fromrow:torow,fromcol:tocol])
    for(name in names(w)) {
      if(!exists(name)) {assign(name,tm)}
      get(name)[fromrow:torow,fromcol:tocol]<-w[[name]]
    }
  }  
}
# w<-mra.2d(rm)

for(name in names(w)) {
  #outfile<-paste(name,"png",sep='.')
  values(o)<-w[[name]]
  writeRaster(o,paste0('Wavelet_tifs/',name,".tif"))
}