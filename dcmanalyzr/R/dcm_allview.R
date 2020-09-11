#' DICOM Allview
#'
#' Generates images for all DICOM files in the directory
#' 
#' @name dcm_allview
#'
#' @param directory path for directory of interest. Defaults to working directory
#' @examples dcm_allview()
#'
#' @export

library(oro.dicom)

dcm_allview <- function(directory = ".") {
  
  print("This can be slow. Please wait")
  
  files <- list.files(path = directory, pattern = "\\.dcm$")
  
  if (length(files) == 0) {
    print("No DICOM files present")
    return()
  }
  
  fig.axis <- ceiling(sqrt(length(files)))
  graphics::par(mfrow=c(fig.axis,fig.axis))
  
  for (file in as.list(files)) {
    filename <- R.utils::filePath(directory,file)
    dcm <- oro.dicom::readDICOMFile(filename)
    graphics::image(dcm$img, col = grDevices::rainbow(12), main=file)

  }
  
}
