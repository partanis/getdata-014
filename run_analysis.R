gdebug <<- FALSE # This switch can be used to display more information for debugging by supplying TRUE in main.
meanStdDF <<- data.frame() ## this data frame will contain the cleaned up tidy data (test + train + subject + activity)
subj <<- as.vector(1:30) ## Assumption is that the subj vector is 1:30, if this something else can be supplied as input paratmeter

## The o/p of the following function is the tidy data set that is ready for analysis, i.e. ready for calculating 
## the mean by activity and by subject.
## The function does the following
## 1. Loads all the files required 
## 2. Identify the mean columns
## 3. Identify the std columns (standard deviation)
## 4. Combines the indexes from 2 and 3. Should have 66 columns
## 5. Narrow down the Training data set to only Mean and STD columns
## 6. Narrow down the Test data set to only Mean and STD columns
## 7. Add the activity and the subject columns to the Training and Test data sets
## 8. Merge the modified (narrowed, and containing subject and activity) data sets.
## The final dataframe should be of dimensions 10299x68
##
createTidyData <- function(path="") {
 path <- paste(path,"UCI HAR Dataset/",sep="")
 if (gdebug) {
   message("in funtion tidydata()"); message("path:",path); message("files:")
   cat(getwd(),"\n")
   print(list.files(path,all.files=T))
 }
 
 ## load all the files required 
 activities <- read.table(paste(path,"activity_labels.txt",sep=""))
 trnCols <- read.table(paste(path,"features.txt",sep=""))
 Xtrain <- read.table(paste(path,"/train/X_train.txt",sep=""),col.names=trnCols[,2])
 Xtest <- read.table(paste(path,"/test/X_test.txt",sep=""),col.names=trnCols[,2])
 
 YTrain <- read.table(paste(path,"/train/y_train.txt",sep=""),col.names="Activity")
 YTest <- read.table(paste(path,"/test/y_test.txt",sep=""),col.names="Activity")
 
 subjectTrain <- read.table(paste(path,"/train/subject_train.txt",sep=""),col.names="Activity")
 subjectTest <- read.table(paste(path,"/test/subject_test.txt",sep=""),col.names="Activity")
  
 ## Get the columns that have mean
 meanCols <- grep("mean()",trnCols[,2],value=TRUE, fixed=TRUE) 
 meanColsIdx <- grep("mean()",trnCols[,2], fixed=TRUE) ## This eliminated the columns such as meanFreq()
 
 stdColsIdx <- grep("std()",trnCols[,2])
 stdCols <- grep("std()",trnCols[,2],value=TRUE)
 
 ##Combine mean and std
 meanStdColIdx <- c(meanColsIdx, stdColsIdx)
 
 ## Get the data from the frame where only those columns are selected which has mean and std in it.
 meanStdTrainDF <- Xtrain[meanStdColIdx]
 meanStdTestDF <- Xtest[meanStdColIdx]
  
 ##Add Activity columns to both the above DFs
 meanStdTestDF$Activity <- as.numeric(YTest[,1]) 
 meanStdTrainDF$Activity <- as.numeric(YTrain[,1])
 
 ##Add Subject columns to both the above DFs
 meanStdTestDF$Subject <- as.numeric(subjectTest[,1]) 
 meanStdTrainDF$Subject <- as.numeric(subjectTrain[,1])
  
 ##Combine the two dataframes by combining data vertically
 meanStdDF <- rbind(meanStdTestDF, meanStdTrainDF) 
  
 if(gdebug) { ## display the debug content
   cat("\nactivities",dim(activities))
   cat("\ntrnCols",dim(trnCols))
   cat("\nXTrain",dim(Xtrain))
   cat("\nXTest",dim(Xtest))
   cat("\nYTrain",dim(YTrain))
   cat("\nYTest",dim(YTest))
   cat("\nsubjectTrain",dim(subjectTrain))
   cat("\nsubjectTest",dim(subjectTest))
   cat("\nmeanColsIdx",length(meanColsIdx))
   cat("\nstddColsIdx",length(stdColsIdx))
   cat("\nmeanStdColIdx",length(meanStdColIdx))
   cat("\nmeanStdTrainDF",dim(meanStdTrainDF))
   cat("\nmeanStdTestDF",dim(meanStdTestDF))
   cat("\nmeanStdDF",dim(meanStdDF))
 }
 meanStdDF
}

## This function does analysis on the tidy data set created by createTidyData function.
## 1. Sorts the data by Subject and Activity so the dataset can be grouped by Subject and its activities.
## 2. Creates an new output data frame meanDF, which will contain the results
## 3. Two loops: Outerloop for the subject, and inner loop for the activity
## 4. compute the mean of the standard deviation and means for the activity.
## 5. Add the mean to the o/p data frame meanDF
##
summarize <- function(df) {
  if (gdebug) {
    message("in function summarize()")
    cat("\ndf",dim(df))
  }
  
  ## sort the data frame by subject and activity
  df <- df[order(df[,68],df[,67]),] 
  
  
  ##addedCols <- c("Subject","Activity")
  ##temp <- setdiff(names(df),addedCols)
  
  ##df1 <- df[addedCols,temp]
    
  cat("\nSorting by Subject and Activity....complete")
  
  meanDF <- data.frame()
  meanDF <- rbind(names(df))

  for (subI in 1:30) { ## first loop for the subject
    message("Subj loop",subI)
    subjDF <- split(df,df[,68])[[subI]]
    for (actI in 1:6) { ## second loop to loop through the 
      message("act loop",actI)
      activityDF <- split(subjDF,subjDF[,67])[[actI]]
      meanDF <- rbind(meanDF,as.numeric(colMeans(activityDF)))
      ##cat(dim(meanDF))
    }
  }
  
  cat("\nMean Calculation....complete")
  

  if(gdebug) { ## display the debug content
  }
  meanDF
}

##
## this is more of a utility function that calls createTidyData and then summarize. Trying to mimic java static main
##
main <- function(debug=F,path="",nsubj=as.vector(1:30)) {
  gdebug <<- debug
  subj <<- nsubj
  tidyDF <- createTidyData(path)
  reportDF <- summarize(tidyDF)
  tidyDF
}