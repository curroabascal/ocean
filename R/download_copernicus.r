#' This is a function to download data from Copernicus.
#'
#' @param yearstart First year of time series
#' @param monthstart First month of time series
#' @param yearend Last year of time series
#' @param monthend Last month on fime series
#' @param minlon minimum longitude
#' @param maxlon maximum longitude
#' @param minlat minimum latitude
#' @param maxlat maximum latitude
#' @param mindepth mimimum depth
#' @param maxdepth maximum depth
#' @param product product name (from data(productlist))
#' @param outputdir path to directore to save data.
#' @param username copernicus username
#' @param password copernicus password
#' 
#' @keywords download,copernicus
#' @export
#' @examples
#' dir.create('./test')
#' yearstart=2010
#' monthstart=1
#' yearend=2010
#' monthend=1
#' minlon=-25
#' maxlon=-20
#' minlat=35
#' maxlat=40
#' mindepth=0
#' maxdepth=4
#' product=c('mchl')
#' outputdir='./test'
#' productlist<-read.csv('P:/Pacifico/Rwork/Rprojects/ocean/mercatorproducts.csv')
#' download_copernicus(yearstart,monthstart,
#'                     yearend,monthend,
#'                    minlon,maxlon,minlat,maxlat,mindepth,maxdepth, 
#'                     product,outputdir,username,
#'                     password)
  download_copernicus<-function(yearstart,monthstart,
                              yearend,monthend,
                              minlon,maxlon,minlat,maxlat,mindepth=0 ,maxdepth=4, 
                              product=NULL,outputdir=NULL){
  
  # #Mercador products tiene que ser un rdata del paquete...
  # productlist<-read.csv('P:/Pacifico/Rwork/Rprojects/ocean/ocean_new/mercatorproducts.csv',stringsAsFactors = F)
  data(productlist)
  # data(mercatorproducts)
  myloadpackage('lubridate') #Tiene una funci?n que me interesa para calcular los d?as del mes...
  check_credentials()
  mambapath=Sys.getenv("mambapath")
  USERNAME=Sys.getenv("copernicus_user")
  PASSWORD=Sys.getenv("copernicus_pass")
  path_copernicus_marine_toolbox<-gsub("condabin\\\\mamba.bat","envs\\\\cmc\\\\Scripts\\\\copernicusmarine",mambapath)
  
  spatialrange<-paste('-x',minlon,'-X',maxlon,
                      '-y',minlat,'-Y',maxlat,
                      '-z',mindepth,'-Z',maxdepth)
  

  # biovars<-c('mchl','mphyc','mpp','mo2','mfe','msi','mno3','mpo4') #Las variables del modelo biogeoqu?mico no 
  #son diarias, y necesito coger 3 d?as antes y depu?s
  #para despu?s asociarla a cada d?a.
  
  #Control de errores en productos o carpeta de destino
  
  if(is.null(product)){print('Debe indicarse un producto'); break()}
  if(length(product)>length(product[product %in% productlist$names])){
    stop(paste("No se reconocen los productos:",product[!product %in% productlist$names],sep=" "))
  }
  
  product<-productlist[productlist$names %in% product,]
  
  product$Date.start<-as.POSIXct(product$Date.start,format='%d/%m/%Y')
  product$Date.end<-as.POSIXct(product$Date.end,format='%d/%m/%Y')
  #Control de errores en carpeta de destino
  if(is.null(outputdir)) stop('outputdir no especificado')    
  if(!dir.exists(outputdir)) stop('La carpeta especificada no existe')  
  if(substr(outputdir,nchar(outputdir),nchar(outputdir))!='/') outputdir=paste(outputdir,'/',sep='')
  datesstart<-trunc(seq.POSIXt(ISOdate(yearstart,monthstart,1),
                         ISOdate(yearend,monthend,1),by='month'),unit='days')
  
  datesend<-trunc(datesstart+(days_in_month(datesstart))*86400,unit='days')
  
  for (i in 1:nrow(product)){
    product2<-product[i,]
    # print(i)
    for (j in 1:length(datesstart)){
      # print(j)
      datestart<-as.character(datesstart[j])
      dateend<-as.character(datesend[j])
      temporalrange<-paste('-t',datestart,'-T',dateend)
      
      # filename<-paste('download_',product2$data_type,format(datesstart[j],'%Y'),
      #                 '-',format(datesstart[j],'%m'),'.nc',sep='')
      #Comprobar si existe la carpeta. Si no, crearla:
      if(!dir.exists(paste(outputdir,product2$names,'/',sep='')->dirname)) dir.create(dirname)
      filename<-paste('copernicusglobal_',product2$data_type,format(datesstart[j],'%Y'),
                      '-',format(datesstart[j],'%m'),'_',gsub(' ','',spatialrange),'.nc',sep='')
      if(datesstart[j]>=product2$Date.start & datesstart[j]<product2$Date.end){
        pytext <- paste (path_copernicus_marine_toolbox, "subset -i", product2$dt_id,                    
                          spatialrange,temporalrange,"--variable",
                          product2$var_cmd, "-o", dirname,"--username",USERNAME,"--password",PASSWORD,        
                          "-f", filename, "--force-download",
                          sep = " ")
   print(filename)
        #Para que no me machaque archivos
        if (!file.exists(paste(dirname,filename,sep=''))){
          system(pytext)
          Sys.sleep(1)
         
      }
 
      }
      
    }
    
    # if (i<nrow(product)) Sys.sleep(5)
  }
}
