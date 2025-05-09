#' This is a function to load daily products, downloaded as monthly files from Copernicus by using the function download_copernicus()
#' The biogeochemical model product before 2012 has several issues and do not work well...(varying long and lat)
#' @param yy year
#' @param mm month
#' @param product product name (from data(productlist))
#' @param folder folder with copernicus data
#' 
#' @keywords load,copernicus
#' @export
#' @examples
#' dir.create('./test')
#' download_copernicus(yearstart=2017,monthstart=1,yearend=2017,
#' monthend=1,minlon=-15,maxlon=5,minlat=35,maxlat=45,mindepth=0,maxdepth=4,c('chl'),'./test',username='username',
#' password='password')
#' test<-load_copernicus(2017,1,'chl','./test')
#' Cholorophylle at the surface (log transformed)
#' image(test$lon,test$lat,log(test$z[,,1,12]))


load_copernicus<-function(yy,mm,product,folder){
  myloadpackage(c('RNetCDF'))
  # #Cambiar por data(productlist)
  # productlist<-read.csv('P:/Pacifico/Rwork/Rprojects/ocean/ocean_new/mercatorproducts.csv',stringsAsFactors = F)
  data(productlist)
  productlist$Date.start<-as.Date(productlist$Date.start,'%d/%m/%Y')
  productlist$Date.end<-as.Date(productlist$Date.end,'%d/%m/%Y')
  productlist$ref.date<-as.Date(productlist$ref.date,'%d/%m/%Y')
  
  if (!product %in% productlist$name){stop(paste("El producto debe ser uno de los siguientes:",paste(productlist$name,collapse=',')))}  
  if(substr(folder,nchar(folder),nchar(folder))!='/') folder=paste(folder,'/',sep='')
  
  foldername<-paste(folder,product,sep='')
  filelist<-dir(foldername,full.names=T)
  yymm<-paste(yy,formatC(mm,width=2,flag=0),sep='-')
  sel=grep(yymm,filelist)
  
  product2<-productlist[productlist$names==product & as.Date(ISOdate(yy,mm,1))>=productlist$Date.start & as.Date(ISOdate(yy,mm,1))<productlist$Date.end,]

  if (length(sel)>0) {
    nc<-open.nc(filelist[sel])
    # print.nc(nc)
    varnames<-sapply(0:file.inq.nc(nc)$ndims,function(x){var.inq.nc(nc,x)$name})
    lonname<-varnames[grep('lon',varnames)]
    latname<-varnames[grep('lat',varnames)]
    timename<-varnames[grep('time',varnames)]
    depthname<-varnames[grep('depth',varnames)]
    
    
    lons<-var.get.nc(nc,lonname)
    # lons[lons>180]<-lons[lons>180]-360
    lats<-var.get.nc(nc,latname)
    times<-var.get.nc(nc,timename)
    date=as.Date(utcal.nc(att.get.nc(nc,'time','units'),times,type='c'))
    # if(length(depthname)==0){
    #   depths<-0
    # } else {
    #     depths<-var.get.nc(nc,depthname)
    #     }
    ifelse(length(depthname)==0,depths<-0,depths<-var.get.nc(nc,depthname))
 
    # date<-product2$ref.date+times*product2$convertsecs/86400
    
    # options(show.error.messages = FALSE)
    # var.inq.nc(nc,product2$varname)
    # attributesnames<-sapply(0:(var.inq.nc(nc,product2$varname)$natts-1),function(x){att.inq.nc(nc,product2$varname,x)$name})
    # scalefactorname<-attributesnames[grep('scale',attributesnames)]
    # offsetname<-attributesnames[grep('offset',attributesnames)]
    # fillvalname<-attributesnames[grep('FillValue',attributesnames)]
    # 
    # scale_factor<-ifelse(length(scalefactorname)==0,1,att.get.nc(nc,product2$varname,scalefactorname))
    # offset<-ifelse(length(offsetname)==0,0,att.get.nc(nc,product2$varname,offsetname))
    # fillval<-ifelse(length(fillvalname)==0,-99999999,att.get.nc(nc,product2$varname,fillvalname))
    dat<-var.get.nc(nc,product2$varname,unpack=T)
    # dat[dat==fillval]<-NaN
    # dat<-dat*scale_factor+offset
    # if (product=='sst'){dat<-dat-273.15}
    close.nc(nc)
    
    #The chl product has inverse lat coordinates!
    res<-list(lon=lons,lat=lats,depth=depths,date=date,z=array(dat,dim=c(length(lons),length(lats),length(depths),length(date))))
    if (product=='chl'){
      res$lat<-rev(res$lat)
      res$z<-array(res$z[,dim(res$z)[2]:1,,],dim=c(length(lons),length(lats),length(depths),length(date)))
    }
    res
  } else print(paste('No se encuentra el archivo copernicusglobal_',productlist$filename[match(product,productlist$name)],'_',yymm,'....nc',sep=''))
}
