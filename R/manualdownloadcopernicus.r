#' Function to download data from copernicus, just pasting the text obtained when doing a manual selection and clicking on automate and then copying the commmand line interface tex .
#'
#' @param CLItext Text as obtained from marine copernicus query tool
#' @param outputdir Dir where file will be stored
#' @param filename filename
#' 
#' @keywords download,copernicus
#' @export
#' @examples
#' textfromweb="subset --dataset-id cmems_mod_glo_phy_my_0.083deg_P1D-m --variable thetao --start-datetime 1993-01-01T00:00:00 --end-datetime 2021-06-30T00:00:00 --minimum-longitude -5.3 --maximum-longitude 2.18 --minimum-latitude 35.07 --maximum-latitude 36.73 --minimum-depth 0.49402499198913574 --maximum-depth 0.49402499198913574"
#' manualdownloadcopernicus(textfromweb,'test','temp.nc')

manualdownloadcopernicus<-function(CLItext,outputdir,filename){
  dir.create(outputdir)
  require(ocean)
  check_credentials()
  mambapath = Sys.getenv("mambapath")
  USERNAME = Sys.getenv("copernicus_user")
  PASSWORD = Sys.getenv("copernicus_pass")
  path_copernicus_marine_toolbox <- gsub("condabin\\\\mamba.bat", 
                                         "envs\\\\cmc\\\\Scripts\\\\copernicusmarine", mambapath)
  
  pytext <- paste(path_copernicus_marine_toolbox,CLItext,  
                  "-o", outputdir, "--username", USERNAME, "--password", 
                  PASSWORD, "-f", filename, "--force-download", 
                  sep = " ")
  
  system(pytext)
  
}