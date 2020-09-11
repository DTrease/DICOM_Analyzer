#' DICOM Metadata
#'
#' Extracts DICOM metadata in data frame for all .dcm files in directory
#' 
#' @name dcm_metadata
#'
#' @param directory path for directory of interest. Defaults to working directory
#' @return hdr.data dataframe containing all DICOM header metadata
#' @examples dcm_metadata()
#'
#' @export

dcm_metadata <- function(directory = ".") {

  first <- 0 #initialize the counter
  files <- list.files(path = directory, pattern = "\\.dcm$")

  if (length(files) == 0) {
    hdr.data <- NULL
  }
  
  for (file in as.list(files)) {
    filename <- R.utils::filePath(directory,file)
    dcm <- oro.dicom::readDICOMFile(filename)
    observations <- as.list(dcm$hdr$value)
    observations <- append(file,observations)
    
    if (first == 0) {
      hdr.data <- data.frame(observations,stringsAsFactors = FALSE)
      variables <- as.list(dcm$hdr$name)
      variables <- append("Filename",variables)
      colnames(hdr.data) <- variables
      first <- 1
    } else {
      hdr.data <- rbind(hdr.data,observations,stringsAsFactors = FALSE)
    }
  
  }

  #print(paste("Extracted ",length(hdr.data[,1])," files"))
  return(hdr.data)

}
