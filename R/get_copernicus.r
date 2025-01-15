#' This is a function to interpolate copernicus data, given the date, longitude and latitude.
#'
#' @param yy vector with the year component of the date
#' @param mm vector with the month component of the date
#' @param dd vector with the day component of the date
#' @param lon vector of longitudes
#' @param lat vector of latitudes
#' @param folder path to folder with copernicus data
#' @param level depth level
#' @keywords get,copernicus
#' @export
#' @examples
#' See temperature at around 15m deep in several locations in January
#' dir.create('./test')
#' download_copernicus(yearstart=2017,monthstart=1,yearend=2017,
#' monthend=1,minlon=-15,maxlon=5,minlat=35,maxlat=45,mindepth=10,maxdepth=20,c('mtemp'),'./test',username='username',
#' password='mypassword')
#' test<-load_copernicus(2017,1,'mtemp','./test')
#' test$depth
#' plot_copernicus(yy=2017,mm=1,dd=12,product='mtemp',folder='./test',level=3,main='Prueba temperatura a 15 m 12/01/2014',xlab='Longitud',ylab='Latitud',log=F,interpolate = T)
#' testdata<-data.frame(year=rep(2017,5),month=rep(1,5),day=c(1,2,13,20,4),lon=c(-10,-11,-12,-5,3),lat=c(36,38,40,38,38))
#' testdata$temp15<-get_copernicus(yy=testdata$year,mm=testdata$month,dd=testdata$day,
#'                                 lon=testdata$lon,lat=testdata$lat,product='mtemp',folder='./test/',level=3)
#' testdata

get_copernicus <-function(yy,mm,dd,lon,lat,product,folder,level=1) {
  myloadpackage(c('fields'))
  
  #Cambiar por data(productlist)
  # productlist<-read.csv('P:/Pacifico/Rwork/Rprojects/ocean/ocean_new/mercatorproducts.csv',stringsAsFactors = F)
  data(productlist)
  if (!product %in% productlist$name){stop(paste("El producto debe ser uno de los siguientes:",paste(productlist$name,collapse=',')))}  
  # if (productlist$islevel[match(product,productlist$name)]==0 & level>1){print(paste('El producto ',product,' tiene valores a un s?lo nivel'));level=1}  

  # lon[lon>180]<-lon[lon>180]-360
  locdat<-data.frame(yy,mm,dd,lon,lat)
  locdat$date<-as.Date(with(locdat,paste(yy,mm,dd,sep='-')))
  yymm<-paste(yy,formatC(mm,width=2,flag=0),sep='-')
  res<-rep(NA,length(yymm))
  
  
  for (i in 1:length(unique(yymm))){
    print(paste("row ",i," out of ",length(unique(paste(yy,mm))),sep=""))    
    dat<-load_copernicus(substr(unique(yymm)[i],1,4),substr(unique(yymm)[i],6,7),product,folder)
    if(class(dat)!='character'){
      #si la variable tienes m?ltiples niveles, quedarme s?lo con el que me interesa...
      if(length(dim(dat$z))>3) dat$z<-array(dat$z[,,level,],dim=dim(dat$z)[c(1,2,4)])
      
      locdat2<-locdat[paste(locdat$yy,formatC(locdat$mm,width=2,flag=0),sep='-')==unique(yymm)[i],]
      locdat2$index<-sapply(locdat2$date,function(x){which.min(abs(x-trunc(dat$date)))}) #Add trunc because if not x was at 0:00 and dat$date at 12:00, it caused some biases.
      
      jnk=matrix(rep(NA,nrow(locdat2)),ncol=1)
      dummy=tapply(1:nrow(locdat2),INDEX=locdat2$index,FUN=function(k){jnk[k]<<-interp.surface(list(x=dat$lon,y=dat$lat,z=dat$z[,,unique(locdat2$index[k])]),
                                                                                               cbind(locdat2$lon[k],locdat2$lat[k]))})
      
      res[paste(locdat$yy,formatC(locdat$mm,width=2,flag=0),sep='-')==unique(yymm)[i]]<-jnk
      
      if(any(min(locdat2$lon)<min(dat$lon),max(locdat2$lon)>max(dat$lon),
             min(locdat2$lat)<min(dat$lat),min(locdat2$lat)>max(dat$lat))) cat('HAY VALORES FUERA DEL RANGO DE LOS ARCHIVOS DESCARGADOS')
    }
  }
  res
}
