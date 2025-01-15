#' Ésta es una función para chequear si las credenciales de observe están en el sistema. Si no, las pide y da la opción de escribirlas.
#' Las escribirá en la ruta del usuario
#' @export
#' @examples
#' check_credentials()
#' 
#' 
check_credentials <-function() {
  credexist<-file.exists(paste(file.path(Sys.getenv("USERPROFILE")),'copernicuscredentials.txt',sep="\\")->copernicuscredentialsfile)
  
  if (credexist){
    print('Leyendo credenciales almacenadas')
    jnk=readLines(copernicuscredentialsfile)
    Sys.setenv(mambapath=jnk[1],copernicus_user=jnk[2],copernicus_pass=jnk[3])
  } else {
    print('No existen credenciales para acceder a copernicus. ¿Quiere introducirlas?')
    cat("1. Si\n")
    cat("2. No")
    opcion <- readline("Elige una opcion (1-2): ")
    
    if (opcion==1){
      print('Ruta al archivo mamba.bat')
      mambapath <- file.choose()
      copernicus_user <- readline("Usuario copernicus: ")
      copernicus_pass <- readline("contraseña: ")
      Sys.setenv(mambapath=mambapath,copernicus_user=copernicus_user,copernicus_pass=copernicus_pass)
      
      opcion2 <- readline("¿Quieres guardar las credenciales en el ordenador (y/n):")
      if (opcion2=='y'){
        writeLines(paste(mambapath,copernicus_user,copernicus_pass,sep='\n'),con=copernicuscredentialsfile)
      }
    } else stop('No hay credenciales para acceder a copernicus',call.=F)
  }
}
  
 