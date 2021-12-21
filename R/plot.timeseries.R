#' A Plotting function used to plot peaks in a time series
#'
#' This function provides a plotting framework for exploring the data output from the timeseries.peaks() function
#'
#' @keywords LiCor, Peak, CO2, Li-Cor, gas, plot
#' @param data output from the timeseries.peaks() function
#' @param file the name of the file the user wishes to plot
#' @param sample the specific sample the user wishes to plot
#' @param samp.div A logical call specifying whether or not to divide plot by sample dividers or not. Defaults to TRUE
#' @param time.start option to specify when you want the plot to start
#' @param time.stop option to specify when you want the plot to stop
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @examples
#' ts.output <- timeseries.peaks(directory = path.package("peak.gas"))
#' # File wide examples
#' plot(ts.output, file = "vn_veg_07292021.txt")
#' plot(ts.output, file = "vn_veg_07292021.txt", time.start = "2021-08-02 13:30:00")
#' plot(ts.output, file = "vn_veg_07292021.txt", time.stop = "2021-08-02 12:40:00")
#' plot(ts.output, file = "vn_veg_07292021.txt", time.start = "2021-08-02 12:45:00", time.stop = "2021-08-02 13:00:00")
#' # Specific Sample examples
#' plot(ts.output, file = "vn_veg_07292021.txt", sample = "NS_Veg")
#' plot(ts.output, file = "vn_veg_07292021.txt", sample = "NS_Veg", time.start = "2021-08-02 13:01:00")
#' plot(ts.output, file = "vn_veg_07292021.txt", sample = "NS_Veg", time.stop = "2021-08-02 12:54:00")
#' plot(ts.output, file = "vn_veg_07292021.txt", sample = "NS_Veg", time.start = "2021-08-02 12:56:00", time.stop = "2021-08-02 13:01:00")
#' @export

plot.timeseries <- function(data,
                            file,
                            sample = NULL,
                            samp.div = TRUE,
                            time.start = min(data$Time),
                            time.stop = max(data$Time)){
  UNR <- function(){
    theme(text = element_text(color = "black", size = 15),
                   plot.title = element_text(face = "bold", color = "darkblue", margin = margin(b = 15)),
                   plot.subtitle = element_text(size = 10),
                   axis.ticks = element_line(size = 1.5),
                   axis.title = element_text(face = "bold", line = 2),
                   axis.title.x = element_text(margin = margin(t = 10), color = "darkblue", size = 15),
                   axis.title.y = element_text(margin = margin(r = 10), color = "darkblue", size = 15),
                   axis.title.y.right = element_text(margin = margin(l = 15),color = "grey50", size = 15),
                   axis.text = element_text(color = "black"),
                   axis.text.x = element_text(margin = margin(t = 15)),
                   axis.text.y = element_text(margin = margin(r = 10)),
                   axis.line = element_line(colour = "black"),
                   axis.ticks.length = unit(2, "mm"),
                   plot.caption = element_text(color = "black"),
                   plot.background = element_rect(fill = "white"),
                   plot.margin = margin(t = 10, r = 50, b = 10, l = 10),
                   panel.background = element_rect(fill = "white"),
                   panel.border = element_rect(fill = "NA", color = "darkblue", size = 1.5),
                   legend.background = element_rect(color = "black", fill = "white"),
                   legend.key =  element_rect(fill = "white"),
                   legend.text = element_text(color = "black"),
                   legend.position = "bottom",
                   strip.background = element_rect(color = "blue", fill = "grey75", size = 2),
                   strip.text.y = element_text(size = 13, face = "bold"),
                   panel.grid = element_blank())
  }
  if(is.null(sample) & samp.div == FALSE){
    file.plot <- filter(data, File == file)
    ggplot(file.plot, aes(x = Time, y = CO2)) +
      geom_line(color = "blue") +
      ggtitle(paste0("Time Series Plot for:", " ", file)) +
      UNR()
  } else if(!is.null(sample)){
    file.plot <- filter(data, File == file)
    sample.plot <- filter(file.plot, Sample == sample)
    start.stop <- filter(sample.plot, Time >= as_datetime(time.start) & Time <= as_datetime(time.stop))
    ggplot(start.stop, aes(x = Time, y = CO2))+
      geom_line(color = "blue")+
      ggtitle(paste0("File: ", " ", file, "\nSample: ", sample))+
      UNR()
  } else if(is.null(sample) & samp.div == TRUE){
    file.plot <- filter(data, File == file)
    start.stop <- filter(file.plot, Time >= as_datetime(time.start) & Time <= as_datetime(time.stop))
    vline <- as_datetime(min(start.stop$Time))
    txt <- vector()
    for(i in 1:(nrow(start.stop)-1)){
      if(start.stop$Sample[i] != start.stop$Sample[i+1]){
        vline <- c(vline, start.stop$Time[i])
        txt <- c(txt, start.stop$Sample[i])
      } else{
        next
      }
    }
    txt <- c(txt, filter(start.stop, Time == max(Time))$Sample)
    vline <- c(vline, max(start.stop$Time))
    pos <- diff.POSIXt(as_datetime(vline))/2
    text.pos <- vector()
    for(i in 1:length(pos)){
      text.pos[i] <- vline[i] + pos[i]
    }
    samples <- paste0(unique(start.stop$Sample), collapse = ', ')
    ggplot()+
      geom_line(data = start.stop, aes(x = Time, y = CO2), color = "blue")+
      geom_vline(xintercept = vline, color = "firebrick")+
      ggtitle(paste0("File: ", " ", file))+
      expand_limits(y = max(start.stop$CO2)/.75)+
      geom_text(label = txt, show.legend = F,
                aes(x = as_datetime(text.pos), y = max(start.stop$CO2)/.85, angle = 90, color = txt),
                size = 3.5)+
      UNR()
  }
}
