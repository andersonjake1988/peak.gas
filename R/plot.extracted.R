#' A Plotting function used with output from CO2 Peak finding function
#'
#' This function provides a plotting framework for exploring the data output from the extract.peaks() function
#'
#' @param data output from the extract.peaks() function
#' @param file the name of the file the user wishes to look at
#' @param sample the specific sample the user wishes to look at
#' @param std.curve a logical argument that states whether the user wishes to look at the standard curve or not
#' @param method Specify whether you want to plot the "linear" relationship or the "log" transformed relationship
#' @keywords LiCor, Peak, CO2, Li-Cor, gas, plot
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @examples
#' setwd(path.package("peak.gas"))
#' output <- extract.peaks()
#' plot.extracted(output, std.curve = T)
#' plot.extracted(output, file = "vn_clear_071621.txt", std.curve = T)
#' plot.extracted(output, file = "vn_clear_07292021.txt", std.curve = T)
#' plot.extracted(output, file = "vn_veg_07292021.txt", std.curve = T)
#' plot.extracted(output, file = "vn_darkveg_071621.txt")
#' plot.extracted(output, sample = "NN_DARKVEG")
#' plot.extracted(output, file = "vn_darkveg_071621.txt", sample = "NC_DARKVEG")
#' @export

plot.extracted <- function(data, file = NULL, sample = NULL, std.curve = F, method = "linear"){
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
  numextract <- function(string){
    as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
  }
  ghost <- filter(data, !str_detect(toupper(Sample), "CURVE") & !str_detect(toupper(Sample), "CHECK"))
  ghost$Replicate <- as.numeric(ghost$Replicate)
  if(is.null(sample) & is.null(file) & std.curve == F){
    rando.group <- sample(unique(data$File_Name), size = 1)
    samp.run <- sample(unique(filter(ghost, File_Name == rando.group)$Order_Run), size = 1)
    verify <- filter(ghost, File_Name == rando.group, Order_Run == samp.run)
    mean.ppm <- mean(verify$AUC_ppm)
    ggplot(verify, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify$File_Name, "\nSample:  ", tolower(verify$Sample), "\nOrder Run:  ", verify$Order_Run)) +
      geom_text(label = verify$Replicate, nudge_y = (mean.ppm/log(verify$AUC_ppm)*.40))+
      UNR()
  } else if(!is.null(file) & is.null(sample) & std.curve == F){
    filt <- filter(ghost, File_Name == file)
    samp.run <- sample(unique(filt$Order_Run), size = 1)
    verify.1 <- filter(filt, Order_Run == samp.run)
    mean.ppm <- mean(verify.1$AUC_ppm)
    ggplot(verify.1, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify.1$File_Name, "\nSample:  ", tolower(verify.1$Sample), "\nOrder Run:  ", verify.1$Order_Run)) +
      geom_text(label = verify.1$Replicate, nudge_y = (mean.ppm/log(verify.1$AUC_ppm)*.40))+
      UNR()
  } else if(is.null(file) & !is.null(sample) & std.curve == F){
    samp <- filter(data, toupper(Sample) == toupper(sample))
    samp2 <- filter(samp, File_Name == sample(unique(File_Name), 1))
    samp.run <- sample(unique(samp2$Order_Run), size = 1)
    verify.2 <- filter(samp2, Order_Run == samp.run)
    mean.ppm <- mean(verify.2$AUC_ppm)
    ggplot(verify.2, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify.2$File_Name, "\nSample:  ", tolower(verify.2$Sample), "\nOrder Run:  ", verify.2$Order_Run)) +
      geom_text(label = verify.2$Replicate, nudge_y = (mean.ppm/log(verify.2$AUC_ppm)*.40))+
      UNR()
  } else if(!is.null(file) & !is.null(sample) & std.curve == F){
    verify.3 <- filter(ghost, File_Name == file, toupper(Sample) == toupper(sample))
    mean.ppm <- mean(verify.3$AUC_ppm)
    ggplot(verify.3, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify.3$File_Name, "\nSample:  ", tolower(verify.3$Sample), "\nOrder Run:  ", verify.3$Order_Run)) +
      geom_text(label = verify.3$Replicate, nudge_y = (mean.ppm/log(verify.3$AUC_ppm)*.40))+
      UNR()
  } else if(is.null(file) & is.null(sample) & std.curve == T & method == "linear"){
    curv <- filter(data, str_detect(toupper(Sample), "CURVE")) %>%
      mutate(standard = numextract(Sample), .before = 3)
    check <- filter(data, str_detect(toupper(Sample), "CHECK") )%>%
      mutate(standard = numextract(Sample), .before = 3)
    asdf <- summary(lm(AUC ~ standard, data = curv))
    Y <- as.numeric(asdf$coefficients[1])
    M <- as.numeric(asdf$coefficients[2])
    R <- as.numeric(asdf$adj.r.squared)
    form <- data.frame(Y = Y, M = M, R = R)
    form2 <- paste0("Y = ", round(form$M, digits = 2), " * x", " + ", round(form$Y, digits = 2))
    r.squared <- paste0("adjusted R^2 = ", round(form$R, digits = 4))
    if(max(curv$standard) > max(check$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv, aes(x = standard, y = AUC_ppm),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = curv, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                                     y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                                     label = paste(form2, r.squared, sep = "\n"))) +
        ggtitle("Linear Standard Curve for All Samples") +
        xlab("Standard ppm") +
        UNR()
    } else if(max(curv$standard) <= max(check$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv, aes(x = standard, y = AUC_ppm),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = check, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                                      y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                                      label = paste(form2, r.squared, sep = "\n"))) +
        ggtitle("Linear Standard Curve for All Samples") +
        xlab("Standard ppm") +
        UNR()
    }
  } else if(!is.null(file) & is.null(sample) & std.curve == T & method == "linear"){
    curv.2 <- filter(data, str_detect(toupper(Sample), "CURVE"), File_Name == file) %>%
      mutate(standard = numextract(Sample), .before = 3)
    check.2 <- filter(data, str_detect(toupper(Sample), "CHECK"), File_Name == file)%>%
      mutate(standard = numextract(Sample), .before = 3)
    asdf.2 <- summary(lm(AUC ~ standard, data = curv.2))
    Y.2 <- as.numeric(asdf.2$coefficients[1])
    M.2 <- as.numeric(asdf.2$coefficients[2])
    R.2 <- as.numeric(asdf.2$adj.r.squared)
    form.2 <- data.frame(Y = Y.2, M = M.2, R = R.2)
    form2.2 <- paste0("Y = ", round(form.2$M, digits = 2), " * x", " + ", round(form.2$Y, digits = 2))
    r.squared.2 <- paste0("adjusted R^2 = ", round(form.2$R, digits = 4))
    if(max(curv.2$standard) > max(check.2$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv.2, aes(x = standard, y = AUC_ppm),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check.2, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv.2, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = curv.2, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                                       y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                                       label = paste(form2.2, r.squared.2, sep = "\n"))) +
        ggtitle(paste0("Linear Standard Curve for ", curv.2$File_Name)) +
        xlab("Standard ppm") +
        UNR()
    } else if(max(curv.2$standard) <= max(check.2$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv.2, aes(x = standard, y = AUC_ppm),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check.2, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv.2, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = check.2, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                                        y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                                        label = paste(form2.2, r.squared.2, sep = "\n"))) +
        ggtitle(paste0("Linear Standard Curve for ", curv.2$File_Name)) +
        xlab("Standard ppm") +
        UNR()
    }
  }else if(is.null(file) & is.null(sample) & std.curve == T & method == "log"){
    curv <- filter(data, str_detect(toupper(Sample), "CURVE")) %>%
      mutate(standard = numextract(Sample), .before = 3)
    check <- filter(data, str_detect(toupper(Sample), "CHECK") )%>%
      mutate(standard = numextract(Sample), .before = 3)
    asdf <- summary(lm(log(AUC) ~ log(standard)^2, data = curv))
    Y <- as.numeric(asdf$coefficients[1])
    M <- as.numeric(asdf$coefficients[2])
    R <- as.numeric(asdf$adj.r.squared)
    form <- data.frame(Y = Y, M = M, R = R)
    form1 <- paste(round(form$M, digits = 2), "* log(x) +", round(form$Y, digits = 2))
    form2 <- paste0("Y = ", "e^(", form1, ")")
    r.squared <- paste0("adjusted R^2 = ", round(form$R, digits = 4))
    if(max(curv$standard) > max(check$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv, aes(x = log(standard)^2, y = log(AUC_ppm)),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = curv, aes(x = (((max(log(standard)^2)-min(log(standard)^2))*.25)+min(log(standard)^2)),
                                                     y = (((max(log(AUC_ppm))-min(log(AUC_ppm)))*.75)+min(log(AUC_ppm))),
                                                     label = paste(form2, r.squared, sep = "\n"))) +
        ggtitle("Log Standard Curve for All Samples") +
        xlab("log(Standard ppm)^2") +
        UNR()
    } else if(max(curv$standard) <= max(check$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv, aes(x = log(standard)^2, y = log(AUC_ppm)),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = check, aes(x = (((max(log(standard)^2)-min(log(standard)^2))*.25)+min(log(standard)^2)),
                                                      y = (((max(log(AUC_ppm))-min(log(AUC_ppm)))*.75)+min(log(AUC_ppm))),
                                                      label = paste(form2, r.squared, sep = "\n"))) +
        ggtitle("Log Standard Curve for All Samples") +
        xlab("log(Standard ppm)^2") +
        UNR()
    }
  } else if(!is.null(file) & is.null(sample) & std.curve == T & method == "log"){
    curv.2 <- filter(data, str_detect(toupper(Sample), "CURVE"), File_Name == file) %>%
      mutate(standard = numextract(Sample), .before = 3)
    check.2 <- filter(data, str_detect(toupper(Sample), "CHECK"), File_Name == file)%>%
      mutate(standard = numextract(Sample), .before = 3)
    asdf.2 <- summary(lm(log(AUC) ~ log(standard)^2, data = curv.2))
    Y.2 <- as.numeric(asdf.2$coefficients[1])
    M.2 <- as.numeric(asdf.2$coefficients[2])
    R.2 <- as.numeric(asdf.2$adj.r.squared)
    form.2 <- data.frame(Y = Y.2, M = M.2, R = R.2)
    form2.1 <- paste(round(form.2$M, digits = 2), "* log(x) +", round(form.2$Y, digits = 2))
    form2.2 <- paste0("Y = ", "e^(", form2.1, ")")
    r.squared.2 <- paste0("adjusted R^2 = ", round(form.2$R, digits = 4))
    if(max(curv.2$standard) > max(check.2$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv.2, aes(x = log(standard)^2, y = log(AUC_ppm)),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check.2, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv.2, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = curv.2, aes(x = (((max(log(standard)^2)-min(log(standard)^2))*.25)+min(log(standard)^2)),
                                                       y = (((max(log(AUC_ppm))-min(log(AUC_ppm)))*.75)+min(log(AUC_ppm))),
                                                       label = paste(form2.2, r.squared.2, sep = "\n"))) +
        ggtitle(paste0("Log Standard Curve for ", curv.2$File_Name)) +
        xlab("log(Standard ppm)^2") +
        UNR()
    } else if(max(curv.2$standard) <= max(check.2$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv.2, aes(x = log(standard)^2, y = log(AUC_ppm)),
                             method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check.2, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv.2, aes(x = log(standard)^2, y = log(AUC_ppm), fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
         geom_text(data = check.2, aes(x = (((max(log(standard)^2)-min(log(standard)^2))*.25)+min(log(standard)^2)),
                                                         y = (((max(log(AUC_ppm))-min(log(AUC_ppm)))*.75)+min(log(AUC_ppm))),
                                                         label = paste(form2.2, r.squared.2, sep = "\n"))) +
        ggtitle(paste0("Log Standard Curve for ", curv.2$File_Name)) +
        xlab("log(Standard ppm)^2") +
        UNR()
    }
  }
}
