### Authors: Jessica E. Kosie and Shahar B. Shirtz
### converting Dwell-Time R code into functions. There are two (three) types of functions here.
### First, perpare data for analysis: remove outliers, calculate residuals etc.
### Second, analysis and visualization of the cleaned data.


Load_My_Libraries <- function(){
  library(nortest)
  library(doBy)
  library(ggplot2)
  
}

Load_My_Data <- function(filename, sep=",", firstslide, lastslide){
  my_data = read.csv(filename, header=TRUE, sep=sep)
  my_data <- subset(my_data, SlideNumber >= firstslide & SlideNumber <= lastslide)
  return(my_data)
}

Is_Data_Normal <- function(my_data){
  library(nortest)
  lillie.test(my_data$DwellTime) #significant p-value means data is skewed (sig. differs from normal)
}

Get_Outliers <- function(my_data, sds=3, add=FALSE){
  ## sds is the number of stadard divs one wants; defaults at 3
  ## add specifies whether to add the sd data to my_data; defaults at FALSE
  
  MeanDT <- mean(my_data$DwellTime, na.rm=TRUE) # the mean
  SdDT <- sd(my_data$DwellTime, na.rm=TRUE) # the sd
  
  MplusSD <- MeanDT + sds*SdDT # the mean plus the number of specified sd's
  Result <- ifelse(my_data$DwellTime >= MplusSD, 1, 0)
  if(add){
    my_data$SDAbove <- Result
    return(my_data)
  }
  return(ifelse(my_data$DwellTime >= MplusSD,1,0))
}

Get_Number_Of_Outliers <- function(my_data, sds=3){
  return(sum(Get_Outliers(my_data,sds), na.rm=TRUE)/nrow(my_data))
}
Get_Outlier_Participants <- function(my_data, sds=3){
  participants <- summaryBy(SDAbove ~ SubID, data=Get_Outliers(my_data,sds,TRUE), FUN=sum,na.rm=TRUE)
  #return(as.vector(participants[which(participants[2]>0),1]))
}
Calculate_Outlier_Precent <- function(my_data,sds=3,slides=345){
  #this should remove the participants who have more than 10% outliers
  my_outliers <- summaryBy(SDAbove ~ SubID, data=Get_Outliers(my_data,add=TRUE), FUN=sum, na.rm=TRUE)
  # I'm using 345 here because that's the number of slide in the test dataset; 
  # Need a reliable way for getting the number of slides (dif parts have dif #'s)
  my_outliers$precentoutlier <- my_outliers$SDAbove.sum/slides
  return(my_outliers)
}
Remove_Outlier_Participant <- function(my_data, sds=3, slides=345,precent=0.1){
  my_outliers <- Calculate_Outlier_Precent(my_data,sds,slides)
  slowpokes <- my_outliers[my_outliers$precentoutlier>=precent, "SubID"]
  return(Remove_Participant(my_data,slowpokes))
}
Remove_Participant <- function(my_data, PartID){
  for(i in 1:length(PartID)){
    my_data <- subset(my_data, SubID != PartID[i])  
  }
  return(my_data)
}


Winsorize_My_Data <- function(my_data, sds=3){
  MplusSD <- mean(my_data$DwellTime, na.rm=TRUE) + sds*sd(my_data$DwellTime, na.rm=TRUE)
  my_data$DwellTime[my_data$DwellTime >MplusSD] <- MplusSD
  return(my_data)
}

Nullify_High_Dwelltime <- function(my_data,sds=3){
  MplusSD <- mean(my_data$DwellTime, na.rm=TRUE) + sds*sd(my_data$DwellTime, na.rm=TRUE)
  my_data$DwellTime[my_data$DwellTime >=MplusSD] <- NA
  return(my_data)
}

Logtransform_My_Data <- function(my_data){
  my_data$logDT <- log10(my_data$DwellTime*1000)
  return(my_data)
}

Residualize_My_Data <- function(my_data){
  ## For each participant, take all the slideshows they watched
  ## and residualize for each of them. This function assumes my_data has a log transformed column.
  
  my_subjects <- unique(my_data$SubID)
  residual.data <- as.data.frame(setNames(replicate(ncol(my_data), numeric(0), simplify = F), colnames(my_data)))
  for (i in my_subjects){
    subjects_slideshows <- unique(my_data$Slideshow[my_data$SubID==i])
    print(subjects_slideshows)
    for (j in subjects_slideshows){
      ## now one can residualize?
      my_temp <- subset(my_data, SubID==i)
      my_temp <- subset(my_temp, Slideshow == j)
      # now my temp has the result of a specific subject and a specific slideshow type
      # if there is no good fit to a power curve, the residuals are calculated from the mean.
      model <- nls(logDT ~ a*t^b, data=my_temp, start=list(a=1,b=1), control=nls.control(warnOnly=TRUE), na.action= na.exclude)
      my_temp$ResidDT <- resid(model)
      residual.data <- rbind(residual.data,my_temp)
      rm(my_temp)  
    }
  }
}

## further function for analysis of dwell time data based on residuals are needed