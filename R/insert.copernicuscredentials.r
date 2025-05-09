#' Ésta es una función para orzar la entrada de nuevas credenciales
#' incluyendo la ruta a mamba
#' @export
#' @examples
#' insert.copernicuscredentials()
#' 
#' 
insert.copernicuscredentials<-function(){
  print('Ruta al archivo mamba.bat')
  mambapath <- file.choose()
  copernicus_user <- readline("Usuario copernicus: ")
  copernicus_pass <- readline("contraseña: ")
  Sys.setenv(mambapath=mambapath,copernicus_user=copernicus_user,copernicus_pass=copernicus_pass)
  
  opcion2 <- readline("¿Quieres guardar las credenciales en el ordenador (y/n):")
  if (opcion2=='y'){
    writeLines(paste(mambapath,copernicus_user,copernicus_pass,sep='\n'),con=copernicuscredentialsfile)
  }
}
  
 