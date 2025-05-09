#' This is a script to try to make the whole installation...
#'
#' @keywords download,copernicus
#' @export
#' @examples
#' install.copernicus()

install.copernicus<-function(){
  install.mamba()
  check_credentials()
  create.environment()
  update.copernicusmarine()
}
