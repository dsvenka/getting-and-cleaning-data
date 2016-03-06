##### Getting and Cleaning Data Course Project #####


# 0. Basics ---------------------------------------------------------------

libraries <- c("data.table")

# 1. Merging the training and the test sets to create one data set --------

#!!!!!! IMPORTNAT ASSUMPTION !!!!!!
# The data is extracted in folder "UCI HAR Dataset" inside the R project directory

## saving file names and direcotries where the files are stored
path <- paste(getwd(),"/UCI HAR Dataset",sep="")
dirs <- list.files(path = path,pattern = ".txt",all.files = T, recursive = T,full.names = T)

dirs.test <- grep(pattern = "_test.txt",x = dirs,value = T)
dirs.train <- grep(pattern = "_train.txt",x = dirs,value = T)


## Loop to load and merge all train and test datasets 
for (i in dirs.test)
{
  ### read files
  temp.dir.train <- gsub(pattern = "test",replacement = "train",x = i)
  temp.test <- read.table(file = i,header = F)
  temp.train <- read.table(file = temp.dir.train,header = F)
  
  ### rbind files
  temp.all <- rbind(temp.test,temp.train)
  
  ### assign name
  temp.name <- gsub(pattern = paste(path,"/test/",sep=""),replacement = "",x = i)
  temp.name <- gsub(pattern = "Inertial Signals/",replacement = "",x = temp.name)
  temp.name <- gsub(pattern = "_test.txt",replacement = "",x = temp.name)
  assign(x = temp.name, value = temp.all)
  rm(temp.all,temp.dir.train,temp.test,temp.train)
}

## labeling variables 

  ### loading variable names
  var_names <- read.table(file = "features.txt",header = F) 
  var_names <- var_names[,2]
  
  ### adding variable names
  names(X) <- var_names
  
  
# 2. Extracting  only the measurements on the mean and SD --------

  ## saving labels of variables with mean and SD
  mean_vars <- grep(pattern = "mean()",x = var_names,value = T)
  std_vars <- grep(pattern = "std()",x = var_names,value = T)
  
  ## extracting only mean and SD measurments
  good_vars <- c(mean_vars,std_vars)
  X <- data.table(X)
  extract_mean_SD <- X[,.SD,.SDcols = good_vars]

# 3. Using descriptive activity names to name the activities --------

  ## loading descriptive activity names
  activity_labels <- read.table(file = "activity_labels.txt",header = F) 
  names(activity_labels) <- c("ACTIVITY_LABEL","ACTIVITY_DESCRIPTION")
  
  ## adding labels to the dataset
  names(y) <- "ACTIVITY_LABEL"
  extract_labels <- cbind(y,extract_mean_SD)
  
  ## adding descriptive labels to the dataset
  extract_desc_labels <- merge(activity_labels,extract_labels, by="ACTIVITY_LABEL",all.y=T)

# 4. Appropriately labeling the data set with descriptive variable names --------

  ## creating labels names with 'human readable' names = required to have  the tidy dataset 
  
  original_labels <- c( "tBodyAcc",
                        "tGravityAcc",
                        "tBodyAccJerk",
                        "tBodyGyro",
                        "tBodyGyroJerk",
                        "tBodyAccMag",
                        "tGravityAccMag",
                        "tBodyAccJerkMag",
                        "tBodyGyroMag",
                        "tBodyGyroJerkMag",
                        "fBodyAcc",
                        "fBodyAccJerk",
                        "fBodyGyro",
                        "fBodyAccMag",
                        "fBodyAccJerkMag",
                        "fBodyGyroMag",
                        "fBodyGyroJerkMag")
  
  human_labels <- c( "Time-Signal-Body-accelerometer",
                     "Time-Signal-Gravity-accelerometer",
                     "Time-Signal-Body-accelerometer-Jerk-signal",
                     "Time-Signal-Body-gyroscope",
                     "Time-Signal-Body-gyroscope-Jerk-signal",
                     "Time-Signal-Body-accelerometer-Magnitude",
                     "Time-Signal-Gravity-accelerometer-Magnitude",
                     "Time-Signal-Body-accelerometer-Jerk-signal-Magnitude",
                     "Time-Signal-Body-gyroscope-Magnitude",
                     "Time-Signal-Body-gyroscope-Jerk-signal-Magnitude",
                     "Frequency-signal-Body-accelerometer",
                     "Frequency-signal-Body-accelerometer-Jerk-signal",
                     "Frequency-signal-Body-gyroscope",
                     "Frequency-signal-Body-accelerometer-Magnitude",
                     "Frequency-signal-Body-accelerometer-Jerk-signal-Magnitude",
                     "Frequency-signal-Body-gyroscope-Magnitude",
                     "Frequency-signal-Body-gyroscope-Jerk-signal-Magnitude")
  
  good_labels <- data.frame("ORIGINAL_LABELS" = original_labels,"HUMAN_LABELS" = human_labels)
      
      ##### sorting from longest to shortes label
      good_labels$length <- nchar(as.character(good_labels$ORIGINAL_LABELS))
      good_labels <- good_labels[order(good_labels$length,decreasing = T),] 
      
      ## replacing label names with human readable labels
      col_names <- colnames(extract_desc_labels)
      
      #### fixing typos
      col_names <- gsub(pattern = "BodyBody", replacement = "Body", x = col_names)
      
      #### loop to change labels
      
      for (i in 1:nrow(good_labels))
      {
        temp <- good_labels[i,]
        col_names <- gsub(pattern = temp$ORIGINAL_LABELS, replacement = temp$HUMAN_LABELS, x = col_names)
      }
    
      #### changing std to standard deviation
      col_names <- gsub(pattern = "std()", replacement = "Standard-deviation", x = col_names)
 
  ## renaming columns
  names(extract_desc_labels) <- col_names
      
  
# 5. From the data set in step 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject --------

  ## Aggregating data by activity
  extract_aggregate <- aggregate(data = extract_desc_labels, . ~ ACTIVITY_DESCRIPTION, FUN=mean)
  
  ## removing unnecessary label
  extract_aggregate$ACTIVITY_LABEL <- NULL
  
  ## saving results
  write.table(extract_aggregate,file = "mean_sd_by_activity.txt",row.names = F)

