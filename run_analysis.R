# Getting and Cleaning Data 

read_in_data <- function() {
  
  filename <- "getdata-projectfiles-UCI HAR Dataset.zip"
  if(!file.exists(filename)) stop("Unable to open data file / make sure the session working directory include the file")
  
  print("Recorded time stamp in variable time_stamp")
  time_stamp <<- Sys.time()
  
  print("Reading X_test.txt")
  test_data <- read.table(unz(filename, "UCI HAR Dataset/test/X_test.txt"))
  print("Reading y_test.txt")
  test_labels <- read.table(unz(filename, "UCI HAR Dataset/test/y_test.txt"))
  
  print("Reading X_train.txt")
  train_data <- read.table(unz(filename, "UCI HAR Dataset/train/X_train.txt"))
  print("Reading y_train.txt")
  train_labels <- read.table(unz(filename, "UCI HAR Dataset/train/y_train.txt"))
  
  features <- read.table(unz(filename, "UCI HAR Dataset/features.txt"))
  activity_labels <- read.table(unz(filename, "UCI HAR Dataset/activity_labels.txt"))
  
  print("Combining data")
  test <- cbind(test_labels, test_data)
  train <- cbind(train_labels, train_data)
  
  dataset <- rbind(test, train)
  
  #Label columns
  colnames(dataset)[1] <- "Activity"
  colnames(dataset)[2:ncol(dataset)] <- as.vector(features[["V2"]])
  
  #Get colnames to keep 
  colnames <- colnames(dataset)
  colnames <- colnames[grepl("Activity", colnames) | grepl("mean\\(\\)", colnames) | grepl("std\\(\\)", colnames)]
  
  #select data so it contains only mean and std columns
  dataset <- dataset[,colnames]
  
  #Label activities correctly
  dataset <- merge(activity_labels, dataset,  by.x="V1", by.y="Activity")
  dataset <- dataset[,2:ncol(dataset)]
  colnames(dataset)[1] <- "Activity"
  
  #Generate means 
  split_data <- split(data_set, data_set$Activity)
  averages_lines <- lapply(split_data, function(x) apply(x[,2:ncol(x)], 2, mean))
  averages <- do.call(rbind.data.frame, averages_lines)
  avgcolnames <- lapply(colnames[2:length(colnames)], function(x) paste("avg_", x, sep=""))
  colnames(averages) <- avgcolnames
  
  data_set <<- dataset
  averages_set <<- averages
  
  print("Data read in")
  
  data_set
  
}
