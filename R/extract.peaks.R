#' A CO2 peak finding function designed for Li-Cor LI-8100A
#'
#' This function provides user's of a LI-8100A Li-Cor Analyzer a simple way to extract and compile peak results from CO2 peak data.
#' It will loop through a folder containing the text files that the Li-Cor Analyzer outputs, and compiles the results into a single R dataframe.
#'
#'
#' @param cut.off Cut off value used to define when the peak should start and end. Defaults to 2.
#' @param method select regression method used for the standard curve, either "linear", or "log".
#' @param standard.sum Specifies whether or not the user wants to output a summary of the standard curve statistics.
#' @param check.stand A logical argument stating whether or not to compare "check" standards to the standard curve.
#' @param check.alpha The value (0-1) you want to accept for check standards deviation, lower numbers indicate the confidence interval increases.
#' @param ci.meth Argument to specify whether to compare average ("avg") check standard AUC values, or individual ("indiv") check standard AUC values
#' @keywords LiCor, Peak, CO2, gas, Li-Cor, extract
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @export
#' @examples
#' setwd(path.package("peak.gas"))
#' output <- extract.peaks()

extract.peaks <- function(cut.off = 2, method = "linear", standard.sum = F,
                          check.stand = F, check.alpha = .05, ci.meth = "avg",
                          verbose = F){
  if(method != "linear" & method != "log"){
    stop(c("method argument must be either 'linear' or 'log' not ", method))
  }
  if(standard.sum != T & standard.sum != F){
    stop(c("standard.sum argument must be either 'TRUE' or 'FALSE' not ", standard.sum))
  }
  if(check.stand != T & check.stand != F){
    stop(c("check.stand argument must be either 'TRUE' or 'FALSE' not ", check.stand))
  }
  if(check.alpha > 1 | check.alpha <= 0){
    stop(c("check.alpha must be within the range 0 to 1 not ", check.alpha))
  }
  if(ci.meth != "avg" & ci.meth != "indiv"){
    stop(c("ci.meth must be either 'avg' or 'indiv' not ", ci.meth))
  }
  if(verbose != T & check.stand != F){
    stop(c("verbose argument must be either 'TRUE' or 'FALSE' not ", verbose))
  }
  Peaks <- function(x){
    output <- vector()
    for(i in 1:length(x)){
      ifelse(x[i] >= cut.off, output[i] <- x[i], output[i] <- NA)
    }
    output
  }
  numextract <- function(string){
    as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
  }
  filelist <- list.files(pattern = c(".txt", ".TXT"))
  output.raw <- data.frame()
  print("Looping through Folder:")
  progress_bar <- txtProgressBar(min = 0, max = length(filelist), style = 3)
  for(a in 1:length(filelist)){
    setTxtProgressBar(progress_bar, a)
    b <- read.table(filelist[a], header = T, sep = "\t", fill = T, strip.white = T, check.names = F)
    if(length(b) != 3){
      print(' ')
      stop(c(filelist[a], ' is not formatted properly, 3 columns are required.'))
    }
    data.1 <- mutate(b, Sample = rep(NA, nrow(b)), .before = 1)
    names(data.1) <- c("Sample", "Test", "Time", "CO2")
    data.2 <- filter(data.1, Test != "--------------------------------------------------------------")
    data.3 <- data.2
    for(i in 1:nrow(data.2)){
      if(is.na(data.2[i,4])==T){
        data.3[i,1] <- as.character(data.2[i,2])
      } else {
        next
      }
    }
    file.annot <- filter(data.3, is.na(CO2)) %>%
      mutate("temp" = c(1:length(Sample))) %>%
      mutate("Sample" = paste0(Sample, "/", temp))
    for(i in 1:nrow(data.3)){
      if(is.na(data.3$CO2[i])){
        data.3$Sample[i] <- file.annot$Sample[1]
        file.annot <- file.annot[-1, ]
      } else {
        next
      }
    }
    data.4 <- na.omit(fill(data.3, Sample, .direction = "down"))
    Preserve.order <- unique(data.4$Sample)
    options(dplyr.summarise.inform = FALSE)
    test.2 <- data.4 %>%
      group_by(Sample) %>%
      mutate("Sample" = factor(Sample, levels = Preserve.order)) %>%
      summarise("Peaks" = Peaks(CO2)) %>%
      arrange(Sample) %>%
      mutate("Value" = !is.na(Peaks), "Replicate" = NA)
    test.2 <- cbind(test.2, "Time" = as_datetime(data.4$Time))
    test.2 <- arrange(test.2, Time)
    r <- 0
    for(i in 1:(length(test.2$Value)-1)){
      if(test.2$Value[i] == T & test.2$Value[i+1] == T){
        test.2$Replicate[i] <- r
      } else if(test.2$Value[i] == F & test.2$Value[i+1] == T){
        r <- r + 1
      } else if(test.2$Value[i] == F & test.2$Value[i+1] == F){
        test.2$Replicate[i] <- NA
      } else {
        test.2$Replicate[i] <- r
      }
    }
    for(i in 2:(length(test.2$Value)-1)){
      if(test.2$Value[i] == F & test.2$Value[i+1] == T){
        test.2$Peaks[i] <- cut.off
        test.2$Replicate[i] <- test.2$Replicate[i+1]
      } else if(test.2$Value[i] == F & test.2$Value[i-1] == T){
        test.2$Peaks[i] <- cut.off
        test.2$Replicate[i] <- test.2$Replicate[i-1]
      } else {
        next
      }
    }
    test.3 <- na.omit(test.2)
    test.3 <- mutate(test.3, "Area" = 0)
    test.3$diff <- c(0, diff(test.3$Peaks))
    peaks <- test.3$Peaks-cut.off
    diff <- c(0, diff(peaks))
    for(i in 1:(length(test.3$Peaks)-1)){
      if(test.3$Replicate[i] == test.3$Replicate[i+1]){
        time <- as.numeric(difftime(test.3$Time[i+1], test.3$Time[i]))
        if(test.3$Value[i] == F & test.3$Value[i+1] == T){
          test.3$Area[i] <- (time * diff[i+1])/2
        } else if(test.3$Value[i] == F & test.3$Value[i+1] == F){
          test.3$Area[i] <- (time * diff[i])/2
        } else if(test.3$Value[i] == T){
          test.3$Area[i] <- (peaks[i] * time) + ((time * diff[i+1])/2)
        }
      } else{
        next
      }
    }
    test.4 <- test.3 %>%
      group_by(Sample, Replicate)%>%
      summarize("AUC" = sum(Area), "Peak" = max(Peaks), "Time_Peak_Start" = min(Time), "Time_Peak_End" = max(Time)) %>%
      mutate("Subsample" = c(1:length(Sample))) %>%
      unite(col = "Sample", Sample, Subsample, sep = "/") %>%
      arrange(Time_Peak_Start)
    test.5 <- cbind(File_Name = filelist[a], test.4)
    test.5 <- separate(test.5, Sample, c("Sample", "Annotation", "Replicate"), sep = "/")
    test.5 <- mutate(test.5, "Order_Run" = rep(1, n()), .before = 3)
    g <- 1
    for(i in 2:nrow(test.5)){
      if(test.5$Sample[i] == test.5$Sample[i-1]){
        test.5$Order_Run[i] <- g
      } else {
        g <- g + 1
        test.5$Order_Run[i] <- g
      }
    }
    output.raw <- rbind(output.raw, test.5)
  }
  print("done")
  output <- output.raw %>%
    group_by(Sample, Order_Run) %>%
    unite("Sample", Sample, Replicate, sep = ". ") %>%
    select(-Annotation) %>%
    arrange(Time_Peak_Start)
  preserve.order <- unique(output$File_Name)
  output <- mutate(output, "Timespan_(s)" = as.numeric(difftime(Time_Peak_End, Time_Peak_Start)))
  output.1 <- filter(output, str_detect(toupper(Sample), "CURVE"))
  output.1 <- mutate(output.1, "Standard" = numextract(Sample))
  output.2 <- filter(output, str_detect(toupper(Sample), "CHECK"))
  output.2 <- mutate(output.2, "Standard" = numextract(Sample))
  curve <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(curve) <- c("File_Name", "Y.intercept", "Slope", "R.squared")
  if(dim(output.1)[1] != 0){
    if(method == "linear"){
      q <- 0
      output.1 <- mutate(output.1, low = NA, high = NA)
      for(i in 1:length(unique(output.1$File_Name))){
        dat1 <- filter(output.1, File_Name == unique(output.1$File_Name)[i])
        linmod <- lm(AUC ~ Standard, data = dat1)
        for(j in 1:nrow(dat1)){
          q <- q+1
          new.dat <- dat1[j,]
          output.1$low[q] <- predict(linmod, newdata = new.dat, interval = 'confidence', level = 1-check.alpha)[2]
          output.1$high[q] <- predict(linmod, newdata = new.dat, interval = 'confidence', level = 1-check.alpha)[3]
        }
        asdf <- summary(linmod)
        Y <- asdf$coefficients[1]
        M <- asdf$coefficients[2]
        R <- asdf$r.squared
        curve[i,] <- c(unique(as.character(output.1$File_Name))[i], Y, M, R)
      }
      if(check.stand == T){
        ot1 <- output.1 %>%
          group_by(File_Name, Standard) %>%
          summarize(ci.low = mean(low), ci.high = mean(high))
        if(ci.meth == "avg"){
          ot2 <- output.2 %>%
            group_by(File_Name, Standard) %>%
            summarize(mean.AUC = mean(AUC))
          ot3 <- full_join(ot1, ot2, by = c("File_Name", "Standard"))
          ot3$File_Name <- factor(ot3$File_Name, levels = preserve.order)
          ot3 <- arrange(ot3, File_Name)
          ot3$File_Name <- as.character(ot3$File_Name)
          ot4 <- mutate(ot3, checkci = mean.AUC > ci.low & mean.AUC < ci.high)
          cierr <- data.frame(File_Name = NA, Standard = NA, ci.low = NA, ci.high = NA, mean.AUC = NA)
          naerr <- data.frame(File_Name = NA, Standard = NA, mean.AUC = NA)
        } else if(ci.meth == "indiv"){
          ot2 <- select(output.2, File_Name, Sample, Order_Run, Standard, AUC)
          ot3 <- full_join(ot1, ot2, by = c("File_Name", "Standard"))
          ot3$File_Name <- factor(ot3$File_Name, levels = preserve.order)
          ot3 <- arrange(ot3, File_Name, Order_Run)
          ot3$File_Name <- as.character(ot3$File_Name)
          ot4 <- mutate(ot3, checkci = AUC > ci.low & AUC < ci.high)
          cierr <- data.frame(File_Name = NA, Sample = NA, Order_Run = NA, ci.low = NA, ci.high = NA, AUC = NA)
          naerr <- data.frame(File_Name = NA, Sample = NA, Order_Run = NA, AUC = NA)
        }
        if(any(ot4$checkci == FALSE) | any(is.na(ot4$checkci))){
          for(i in 1:nrow(ot4)){
            if(ci.meth == "avg"){
              if(is.na(ot4['checkci'][i,])){
                naerr[i,] <- select(ot4[i,], File_Name, Standard, mean.AUC)
              } else if(ot4['checkci'][i,] == FALSE){
                cierr[i,] <- select(ot4[i,], File_Name, Standard, ci.low, ci.high, mean.AUC)
              } else if(ot4['checkci'][i,] == TRUE){
                next
              }
            } else if(ci.meth == "indiv"){
              if(is.na(ot4['checkci'][i,])){
                naerr[i,] <- select(ot4[i,], File_Name, Sample, Order_Run, AUC)
              } else if(ot4['checkci'][i,] == FALSE){
                cierr[i,] <- select(ot4[i,], File_Name, Sample, Order_Run, ci.low, ci.high, AUC)
              } else if(ot4['checkci'][i,] == TRUE){
                next
              }
            }
          }
          cierr <- na.omit(cierr)
          naerr <- na.omit(naerr)
          if(nrow(cierr) != 0){
            warning(call. = F, c("\n\nCheck standards deviate from the ", 100*(1 - check.alpha), "%", " confidence interval in the following Samples:\n"))
            for(i in 1:nrow(cierr)){
              if(ci.meth == "avg"){
                warning(call. = F, c("File: ", cierr$File_Name[i], "\tStandard: ", cierr$Standard[i], "check",
                                     "\tCI range: ", round(cierr$ci.low[i]), " to ", round(cierr$ci.high[i]), "\tAUC: ", round(cierr$mean.AUC[i], 2)))
              }
              if(ci.meth == "indiv"){
                warning(call. = F, c("File: ", cierr$File_Name[i], "\tSample: ", cierr$Sample[i], "\tOrder_Run: ", cierr$Order_Run[i],
                                     "\tCI range: ", round(cierr$ci.low[i]), " to ", round(cierr$ci.high[i]), "\tAUC: ", round(cierr$AUC[i], 2)))
              }
            }
          }
          if(nrow(naerr) != 0){
            warning(call. = F, "\n\nNA values for confidence interval due to missing standard 'curves' in the following Samples:\n")
            if(ci.meth == "avg"){
              for(i in 1:nrow(naerr)){
                warning(call. = F, c("File: ", naerr$File_Name[i], "\tStandard: ", naerr$Standard[i], "check", "\tAUC: ", round(naerr$mean.AUC[i], 2)))
              }
            }
            if(ci.meth == "indiv"){
              for(i in 1:nrow(naerr)){
                warning(call. = F, c("File: ", naerr$File_Name[i], "\tSample: ", naerr$Sample[i], "\tOrder_Run:", naerr$Order_Run[i], "\tAUC: ", round(naerr$AUC[i], 2)))
              }
            }
          }
        } else{
          next
        }
      }
      curve[, 2:4] <- sapply(curve[, 2:4], as.numeric)
      output.0 <- filter(output, !str_detect(toupper(Sample), "CURVE"))
      inwork <- output %>%
        group_by(File_Name) %>%
        summarize(n = n())
      inwork2 <- output.0 %>%
        group_by(File_Name) %>%
        summarize(n = n())
      if(dim(inwork2)[1]==0){
        curve.2 <- mutate(inwork, n2 = 0, "curve" = (n != n2))
      } else {
        curve.2 <- mutate(inwork, n2 = inwork2$n, "curve" = (n != n2))
      }
      yes.curve <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(yes.curve) <- c("File_Name", "Y.intercept", "Slope", "R.squared")
      for(i in 1:length(curve.2$File_Name)){
        for(j in 1:length(curve$File_Name)){
          if(curve.2$File_Name[i] == curve$File_Name[j] & curve.2$curve[i]==T){
            yes.curve[i,] <- curve[j,]
          } else if(curve.2$curve[i]==F){
            yes.curve[i,] <- c(curve.2$File_Name[i], rep(NA,3))
          }
        }
      }
      yes.curve$File_Name <- factor(yes.curve$File_Name, levels = preserve.order)
      yes.curve <- arrange(yes.curve, File_Name)
      curve.3 <- fill(yes.curve, "File_Name", "Y.intercept", "Slope", "R.squared", .direction = "down")
      curve.3[, 2:4] <- sapply(curve.3[, 2:4], as.numeric)
      stand <- output.1 %>%
        group_by(File_Name, Standard) %>%
        summarise(Mean = mean(AUC), std.dev = sd(AUC))%>%
        mutate(COV = std.dev/Mean)
      sum.stat <- left_join(stand, curve, by = "File_Name")
      output$AUC_ppm <- 0
      for(i in 1:nrow(output)){
        for(j in 1:nrow(curve.3)){
          if(output$File_Name[i] == curve.3$File_Name[j]){
            output$AUC_ppm[i] <- (output$AUC[i] - curve.3$Y.intercept[j])/(curve.3$Slope[j])
          }
        }
      }
    } else if(method == "log"){
      q <- 0
      output.1 <- mutate(output.1, low = NA, high = NA)
      for(i in 1:length(unique(output.1$File_Name))){
        dat1 <- filter(output.1, File_Name == unique(output.1$File_Name)[i])
        linmod <- lm(log(AUC) ~ log(Standard), data = dat1)
        for(j in 1:nrow(dat1)){
          q <- q+1
          new.dat <- dat1[j,]
          output.1$low[q] <- predict(linmod, newdata = new.dat, interval = 'confidence', level = 1-check.var)[2]
          output.1$high[q] <- predict(linmod, newdata = new.dat, interval = 'confidence', level = 1-check.var)[3]
        }
        asdf <- summary(linmod)
        Y <- asdf$coefficients[1]
        M <- asdf$coefficients[2]
        R <- asdf$r.squared
        curve[i,] <- c(unique(as.character(output.1$File_Name))[i], Y, M, R)
      }
      if(check.stand == T){
        ot1 <- output.1 %>%
          group_by(File_Name, Standard) %>%
          summarize(ci.low = mean(low), ci.high = mean(high))
        if(ci.meth == "avg"){
          ot2 <- output.2 %>%
            group_by(File_Name, Standard) %>%
            summarize(mean.AUC = mean(log(AUC)))
          ot3 <- full_join(ot1, ot2, by = c("File_Name", "Standard"))
          ot3$File_Name <- factor(ot3$File_Name, levels = preserve.order)
          ot3 <- arrange(ot3, File_Name)
          ot3$File_Name <- as.character(ot3$File_Name)
          ot4 <- mutate(ot3, checkci = mean.AUC > ci.low & mean.AUC < ci.high)
          cierr <- data.frame(File_Name = NA, Standard = NA, ci.low = NA, ci.high = NA, mean.AUC = NA)
          naerr <- data.frame(File_Name = NA, Standard = NA, mean.AUC = NA)
        } else if(ci.meth == "indiv"){
          ot2 <- select(output.2, File_Name, Sample, Order_Run, Standard, AUC)
          ot2$AUC <- log(ot2$AUC)
          ot3 <- full_join(ot1, ot2, by = c("File_Name", "Standard"))
          ot3$File_Name <- factor(ot3$File_Name, levels = preserve.order)
          ot3 <- arrange(ot3, File_Name, Order_Run)
          ot3$File_Name <- as.character(ot3$File_Name)
          ot4 <- mutate(ot3, checkci = AUC > ci.low & AUC < ci.high)
          cierr <- data.frame(File_Name = NA, Sample = NA, Order_Run = NA, ci.low = NA, ci.high = NA, AUC = NA)
          naerr <- data.frame(File_Name = NA, Sample = NA, Order_Run = NA, AUC = NA)
        }
        if(any(ot4$checkci == FALSE) | any(is.na(ot4$checkci))){
          for(i in 1:nrow(ot4)){
            if(ci.meth == "avg"){
              if(is.na(ot4['checkci'][i,])){
                naerr[i,] <- select(ot4[i,], File_Name, Standard, mean.AUC)
              } else if(ot4['checkci'][i,] == FALSE){
                cierr[i,] <- select(ot4[i,], File_Name, Standard, ci.low, ci.high, mean.AUC)
              } else if(ot4['checkci'][i,] == TRUE){
                next
              }
            } else if(ci.meth == "indiv"){
              if(is.na(ot4['checkci'][i,])){
                naerr[i,] <- select(ot4[i,], File_Name, Sample, Order_Run, AUC)
              } else if(ot4['checkci'][i,] == FALSE){
                cierr[i,] <- select(ot4[i,], File_Name, Sample, Order_Run, ci.low, ci.high, AUC)
              } else if(ot4['checkci'][i,] == TRUE){
                next
              }
            }
          }
          cierr <- na.omit(cierr)
          naerr <- na.omit(naerr)
          if(nrow(cierr) != 0){
            warning(call. = F, c("\n\nCheck standards deviate from the ", 100*(1 - check.alpha), "%", " confidence interval in the following Samples:\n"))
            for(i in 1:nrow(cierr)){
              if(ci.meth == "avg"){
                warning(call. = F, c("File: ", cierr$File_Name[i], "\tStandard: ", cierr$Standard[i], "check",
                                     "\tCI range: ", round(cierr$ci.low[i],2), " to ", round(cierr$ci.high[i],2), "\tAUC: ", round(cierr$mean.AUC[i], 2)))
              }
              if(ci.meth == "indiv"){
                warning(call. = F, c("File: ", cierr$File_Name[i], "\tSample: ", cierr$Sample[i], "\tOrder_Run: ", cierr$Order_Run[i],
                                     "\tCI range: ", round(cierr$ci.low[i],2), " to ", round(cierr$ci.high[i],2), "\tAUC: ", round(cierr$AUC[i], 2)))
              }
            }
          }
          if(nrow(naerr) != 0){
            warning(call. = F, "\n\nNA values for confidence interval due to missing standard 'curves' in the following Samples:\n")
            if(ci.meth == "avg"){
              for(i in 1:nrow(naerr)){
                warning(call. = F, c("File: ", naerr$File_Name[i], "\tStandard: ", naerr$Standard[i], "check", "\tAUC: ", round(naerr$mean.AUC[i], 2)))
              }
            }
            if(ci.meth == "indiv"){
              for(i in 1:nrow(naerr)){
                warning(call. = F, c("File: ", naerr$File_Name[i], "\tSample: ", naerr$Sample[i], "\tOrder_Run:", naerr$Order_Run[i], "\tAUC: ", round(naerr$AUC[i], 2)))
              }
            }
          }
        } else{
          next
        }
      }
      curve[, 2:4] <- sapply(curve[, 2:4], as.numeric)
      output.0 <- filter(output, !str_detect(toupper(Sample), "CURVE"))
      inwork <- output %>%
        group_by(File_Name) %>%
        summarize(n = n())
      inwork2 <- output.0 %>%
        group_by(File_Name) %>%
        summarize(n = n())
      curve.2 <- mutate(inwork, n2 = inwork2$n, "curve" = (n != n2))
      yes.curve <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(yes.curve) <- c("File_Name", "Y.intercept", "Slope", "R.squared")
      for(i in 1:length(curve.2$File_Name)){
        for(j in 1:length(curve$File_Name)){
          if(curve.2$File_Name[i] == curve$File_Name[j] & curve.2$curve[i]==T){
            yes.curve[i,] <- curve[j,]
          } else if(curve.2$curve[i]==F){
            yes.curve[i,] <- c(curve.2$File_Name[i], rep(NA,3))
          }
        }
      }
      yes.curve$File_Name <- factor(yes.curve$File_Name, levels = preserve.order)
      yes.curve <- arrange(yes.curve, File_Name)
      curve.3 <- fill(yes.curve, "File_Name", "Y.intercept", "Slope", "R.squared", .direction = "down")
      curve.3[, 2:4] <- sapply(curve.3[, 2:4], as.numeric)
      stand <- output.1 %>%
        group_by(File_Name, Standard) %>%
        summarise(Mean = mean(AUC), std.dev = sd(AUC))%>%
        mutate(COV = std.dev/Mean)
      sum.stat <- left_join(stand, curve, by = "File_Name")
      output$AUC_ppm <- 0
      for(i in 1:nrow(output)){
        for(j in 1:nrow(curve.3)){
          if(output$File_Name[i] == curve.3$File_Name[j]){
            output$AUC_ppm[i] <- exp((log(output$AUC[i]) - curve.3$Y.intercept[j])/(curve.3$Slope[j]))
          }
        }
      }
    }
    sum.stat2 <- output %>%
      filter(str_detect(toupper(Sample), "CURVE")) %>%
      mutate(Standard = numextract(Sample)) %>%
      group_by(Standard, File_Name) %>%
      summarise(Mean_ppm = mean(AUC_ppm))
    standard.summary.stats <- left_join(sum.stat, sum.stat2, by = c("File_Name", "Standard"))
    output <- separate(output, Sample, c("Sample", "Replicate"), sep = ". ")
    if(standard.sum == T){
      View(standard.summary.stats)
    }
    return(output)
  } else {
    print(' ')
    warning('No standard curve data found, could not compute concentration')
    output$AUC_ppm <- 0
    output <- separate(output, Sample, c("Sample", "Replicate"), sep = ". ")
    output_final <- as_tibble(output)
    class(output_final) <- c("extracted", class(output_final))
    return(output_final)
  }
}
