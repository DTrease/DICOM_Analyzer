#' DICOM Select
#'
#' Displays images and metrics for all .dcm images in the target directory for user evaluation
#'
#' @name dcm_select
#' 
#' @importFrom magrittr %>%
#' @param directory path for directory of interest. Defaults to working directory
#' @return hdr.data dataframe containing all DICOM header metadata and user evaluations
#' @examples dcm_select()
#'
#' @export

dcm_select <- function(directory = ".") {

  first <- 0 #initialize the counter
  files <- list.files(path = directory, pattern=".dcm") #pattern = "\\.dcm$")

  if (length(files) == 0) {
    print("No DICOM files present")
    return()
  }

  print("Presenting all DICOMs found in file")
  print("For each image,")
  print("Hit return if image quality is acceptable")
  print("Type n to mark as unacceptable")
  print("Type q to exit")

  first <- 0 

  for (file in as.list(files)) {
    filename <- R.utils::filePath(directory,file)
    dcm <- oro.dicom::readDICOMFile(filename)
    
    # Render dcm as cimg object
    subject <- imager::as.cimg(dcm$img)
    
    # Display image and metrics to user
    graphics::layout(matrix(c(1,1,4,1,1,5,2,3,6), 3, 3, byrow = TRUE))
    graphics::plot(subject, main=file, asp=1)
    graphics::hist(subject,main="Grey Level Histogram",xlab="Signal (counts)")
    subj.fft <- imager::FFT(subject)
    power<- sqrt(subj.fft$real^2+subj.fft$imag^2)
    graphics::plot(power, main="FFT Power Spectrum")
    imager::imrow(power,1) %>% graphics::plot(main="FFT, ky=1",type="l")
    imager::imrow(power,5) %>% graphics::plot(main="FFT, ky=5",type="l")
    imager::imrow(power,120) %>% graphics::plot(main="FFT, ky=120",type="l")
    
    # User direct input
    input <-readline()
    if (input == "q") {
      print("Selection Canceled By User")
      return() 
    } else if (input == "n") {
      print("Image Marked as Poor")
      metric <- "Poor"
    } else {
      print("Image Marked as OK")
      metric <- "Good"
    }
    
    # Write metadata to dataframe
    observations <- as.list(dcm$hdr$value)
    observations <- append(file,observations)
    observations <- append(observations,metric)
    
    if (first == 0) {
      hdr.data <- data.frame(observations,stringsAsFactors = FALSE)
      variables <- as.list(dcm$hdr$name)
      variables <- append("Filename",variables)
      variables <- append(variables,"ImageQuality")
      colnames(hdr.data) <- variables
      first <- 1
    } else {
      hdr.data <- rbind(hdr.data,observations,stringsAsFactors = FALSE)
    }
    
  }

return(hdr.data)

}