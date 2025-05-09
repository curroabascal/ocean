#' This is a function to plot downloaded copernicus products.
#' @param yy year
#' @param mm month
#' @param dd day
#' @param product product
#' @param folder folder
#' @param level level
#' @param rangelon range of longitudes. The default is the whole area in the NetCDF file
#' @param rangelat range of latitudes. The default is the whole area in the NetCDF file
#' @param main Plot title
#' @param xlab x-axis title
#' @param ylab y-axis title
#' @param log Logical indicating whether to log transform (chl)
#' @param interpolate Logical indicating whether to interpolate or show raw data
#' 
#' @keywords plot,copernicus
#' @export
#' @examples
#' See temperature at around 15m deep.
#' dir.create('./test')
#' download_copernicus(yearstart=2014,monthstart=1,yearend=2014,
#' monthend=1,minlon=-15,maxlon=5,minlat=35,maxlat=45,mindepth=10,maxdepth=20,c('mtemp'),'./test',username='username',
#' password='mypassword')
#' test<-load_copernicus(2014,1,'mtemp','./test')
#' test$depth
#' plot_copernicus(yy=2014,mm=1,dd=12,product='mtemp',folder='./test',level=3,main='Prueba temperatura a 15 m 12/01/2014',xlab='Longitud',ylab='Latitud',log=T,interpolate = T)

plot_copernicus<-function(yy,mm,dd,product,folder,level=1,rangelon=NULL,rangelat=NULL,rangevar=NULL,main='',xlab='',ylab='',log=F,interpolate=T){
  myloadpackage(c('akima','maps'))
  dat<-load_copernicus(yy,mm,product,folder)
  
  if (length(dim(dat$z))==4) dat$z<-array(dat$z[,,level,],dim=dim(dat$z)[c(1,2,4)]) 
  index<-which.min(abs(as.Date(paste(yy,mm,dd,sep='-'))-dat$date+.5))
  dat$z<-dat$z[,,index]
  if(log) dat$z=log(dat$z)
  
  if(is.null(rangelon)) rangelon<-range(dat$lon)
  if(is.null(rangelat)) rangelat<-range(dat$lat)
  if(is.null(rangevar)){rangevar2<-c(max(min(dat$z,na.rm=T),mean(dat$z,na.rm=T)-3*sd(dat$z,na.rm=T)),
                                     min(max(dat$z,na.rm=T),mean(dat$z,na.rm=T)+3*sd(dat$z,na.rm=T)))} else {rangevar2=rangevar}
  if(is.null(main)) main<-paste(product,dat$date[index])
  
  
  dat$z[!is.na(dat$z) & (dat$z<rangevar2[1])]<-rangevar2[1]
  dat$z[!is.na(dat$z) & (dat$z>rangevar2[2])]<-rangevar2[2]
  
  pal <- colorRampPalette(rev(c("red","yellow","#8BFFFF","#3EF0F5","#7D9DEC","#2F3AF8","purple")))
  
  layout(matrix(2:1, nrow=1),widths=c(8,1.5))
  par(mai=c(2,0,2,0.8))
  
  if (interpolate){
    dat$z[is.na(dat$z)]=mean(dat$z,na.rm=T)
    tt<-bicubic.grid(dat$lon,dat$lat,dat$z,xlim=rangelon,ylim=rangelat,dx=.05,dy=.05)
    if(is.null(rangevar)) rangevar=range(tt$z)
    zbreaks <- seq(rangevar[1],rangevar[2],diff(rangevar)/100)
    cols <-pal(length(zbreaks)-1)
    image(x=.01, y=zbreaks, z=matrix(zbreaks,1), col=cols, breaks=zbreaks, useRaster=TRUE, xlab="", ylab="", axes=FALSE)
    axis(4, at=pretty(zbreaks,5), las=2)
    
    par(mai=c(1.02,0.82,0.82,0.42))
    image(tt$x,tt$y,z=tt$z, col=cols, breaks=zbreaks, useRaster=TRUE, xlim=rangelon, ylim=rangelat,
          xlab=xlab,ylab=ylab,main=main)
  } else {
    if(is.null(rangevar)) rangevar=rangevar2
    zbreaks <- seq(rangevar[1],rangevar[2],diff(rangevar)/100)
    cols <-pal(length(zbreaks)-1)
    image(x=.01, y=zbreaks, z=matrix(zbreaks,1), col=cols, breaks=zbreaks, useRaster=TRUE, xlab="", ylab="", axes=FALSE)
    axis(4, at=pretty(zbreaks,5), las=2)
    
    par(mai=c(1.02,0.82,0.82,0.42))
    
    image(dat$lon,dat$lat,z=dat$z, col=cols, breaks=zbreaks, useRaster=TRUE, xlim=rangelon, ylim=rangelat,
          xlab=xlab,ylab=ylab,main=main)
  }
  map(add=T,col='black',fill=T)
  box()
  
}
