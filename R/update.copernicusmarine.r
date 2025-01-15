#' Ésta es una función para actualizar el módulo de copernicusmarine
#' @export
#' @examples
#' update.copernicusmarine()
#' 
#' 
update.copernicusmarine<-function(){
  check_credentials()
  #No sÃ© si es neceario activarlo, pero bueno, da un poco igual...
  mambapath=Sys.getenv('mambapath')
  pythonpath=gsub("condabin\\\\mamba.bat","python.exe",mambapath)
  # mambapath=paste0("C:\\Users\\",Sys.info()['user'],"\\AppData\\Local\\miniforge3\\condabin\\mamba.bat")
  # pythonpath=paste0("C:\\Users\\",Sys.info()['user'],"\\AppData\\Local\\miniforge3\\python.exe")
  system(paste(paste0(mambapath," activate cmc &&"),paste0(pythonpath," -m pip install copernicusmarine --upgrade"),sep=" "),invisible = F)
  
}
  
 