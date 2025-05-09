#' Ésta es una función para instalar el modulo de copernicus marine
#' @export
#' @examples
#' create.environment()
#' 
#' 
create.environment<-function(){
  check_credentials()
  #Create the cmc environment
  cmctext="name: cmc
channels:
  - conda-forge
dependencies:
  - python>=3.9,<3.13
  - pip
  - pip:
    - copernicusmarine"
  tempdir = tempdir()
  writeLines(cmctext,con=paste(tempdir, "copernicusmarine-env.yml", sep = "\\")->>envfile)
  
  mambapath=Sys.getenv('mambapath')
  system(paste0(mambapath," env create -f ",envfile),invisible = F)
  
  cat('Installed environments:\n')
  system(paste0(mambapath," env list"),invisible = F)
  
  #Lo activo
  system(paste0(mambapath," activate cmc"))
  # 
  # # #Para ver la ayuda...
  # # copernicusmarinepath=paste0("C:\\Users\\",Sys.info()['user'],"\\AppData\\Local\\miniforge3\\envs\\cmc\\Scripts\\")
  # # system(paste0(copernicusmarinepath,"copernicusmarine --help"))
}
