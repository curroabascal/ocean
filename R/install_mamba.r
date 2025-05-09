#' Ésta es una función para instalar mamba
#' @export
#' @examples
#' .install.mamba()
#' 
#' 
install.mamba<-function () {
  cat("This will install mamba in your PC and create the environment. It will be done at your \nown responsibility.\n")
  q <- readline(prompt = "Do you want to install mamba? (y/n)")
  if (q == "y") {
    tempdir = tempdir()
    filename <- paste(tempdir, "miniforge.exe", sep = "\\")
    mambainstallfile <- download.file("https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Windows-x86_64.exe", 
                                      destfile = filename, mode = "wb")
    system2(filename,invisible = F)
  }
}
  
 