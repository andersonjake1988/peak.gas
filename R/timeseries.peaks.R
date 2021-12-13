#' A function to combine and name timeseries text files
#'
#' This function will loop through a folder containing text file outputs to combine and assign sample names, outputing a single timeseries
#'
#' @keywords LiCor, Peak, CO2, Li-Cor, gas, plot
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @examples
#' setwd(path.package("peak.gas"))
#' output <- timeseries.peaks()
#' @export

timeseries.peaks <- function(directory = choose.dir(), verbose = T){
  setwd(directory)
  if(verbose == T){
    message("Working directory set to:")
    message(getwd())
  }
  filelist <- list.files(pattern = c(".txt", ".TXT"))
  output.raw <- data.frame()
  print("Looping through Folder:")
  progress_bar <- txtProgressBar(min = 0, max = length(filelist), style = 3)
  for(i in 1:length(filelist)){
    setTxtProgressBar(progress_bar, i)
    b <- read.table(filelist[i], header = T, sep = "\t", fill = T, strip.white = T, check.names = F)
    data.1 <- mutate(b, Sample = NA, .before = 1)
    names(data.1) <- c("Sample", "Test", "Time", "CO2")
    data.1 <- cbind(data.1, File = filelist[i])
    data.2 <- filter(data.1, Test != "--------------------------------------------------------------")
    data.3 <- data.2
    for(i in 1:nrow(data.2)){
      if(is.na(data.2[i,4])==T){
        data.3[i,1] <- as.character(data.2[i,2])
      } else {
        next
      }
    }
    data.4 <- na.omit(fill(data.3, Sample, .direction = "down"))
    output.raw <- rbind(output.raw, data.4)
  }
  output.raw$Time <- as_datetime(output.raw$Time)
  output_final <- output.raw[, c(5,1,3,4)]
  class(output_final) <- c("timeseries", class(output_final))
  return(output_final)
}
