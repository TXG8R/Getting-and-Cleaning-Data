run_analysis <- function(){

### Working Folder paths for "Getting and Cleaning Data" ###
#pathWork <- "D:/Dropbox/Coursera/Johns Hopkins - Data Science Certificate Program/3 - Getting and Cleaning Data/Course Project/UCI HAR Dataset/"
#setwd(pathWork)

pathHome <- "D:/Dropbox/Coursera/Johns Hopkins - Data Science Certificate Program/3 - Getting and Cleaning Data/Course Project/UCI HAR Dataset"
setwd(pathHome)

#pathLaptop <- ""
#setwd(pathLaptop)

# Load the train data set #
data.xTrain <- read.table("./train/X_train.txt", header = FALSE)  # "X" in filename is UPPERCASE
data.yTrain <- read.table("./train/y_train.txt", header = FALSE)  # "y" in filename is lowercase
data.subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
data.features <- read.table("./features.txt", header = FALSE)
data.activityType <- read.table("./activity_labels.txt", header = FALSE)

# Set column names for all loaded train data sets
colnames(data.features) <- c("FeatureID", "FeatureName")
colnames(data.xTrain) <- data.features[,2]
colnames(data.yTrain) <- "ActivityID"
colnames(data.subjectTrain) <- "SubjectID"
colnames(data.activityType) <- c("ActivityID", "ActivityType")

# Load the test data set #
data.xTest <- read.table("./test/X_test.txt", header = FALSE)  # "X" in filename is UPPERCASE
data.yTest <- read.table("./test/y_test.txt", header = FALSE)  # "y" in filename is lowercase
data.subjectTest <- read.table("./test/subject_test.txt", header = FALSE)

# Assign column names to the test data set
colnames(data.xTest) <- data.features[,2]  # Assign the column names of the xTest data to the column name values found in the features.txt table/file
colnames(data.yTest) <- "ActivityID"  # Assign the Activity ID column name to the yTest data column
colnames(data.subjectTest) <- "SubjectID"   # Assign the column names of the subjectTest data to the column name values found in the subject_train.txt table/file

# Combine the y, subject, and x data to each other as sets of adjacent columns
data.combined.Train <- cbind(data.yTrain, data.subjectTrain, data.xTrain)
data.combined.Test <- cbind(data.yTest, data.subjectTest, data.xTest)

# Combine the Train row data and Test row data (same number of columns each) into one single data set
data.combined.all <- rbind(data.combined.Train, data.combined.Test)

# Assign a vector of values for ALL of the column names of the newly fully-combined data set
columnNames.combined <- colnames(data.combined.all)

# Generate a vector of those columns that have ActivityID, SubjectID, mean or std in their name
validColumnNames <- (grepl("ActivityID",columnNames.combined) | 
                     grepl("SubjectID",columnNames.combined) | 
                     grepl("-mean",columnNames.combined) & !grepl("-meanFreq",columnNames.combined) | 
                     grepl("-std",columnNames.combined))

# Using the validColumnNames vector, generate the data set that contains only the columns of interest
data.results <- data.combined.all[validColumnNames]

# Join the data.results and data.activityType by Activity ID so that meaningful Activity names will be available in the final result set, thus make the data more readable to end users
data.results <- merge(data.results, data.activityType, by = "ActivityID", all.x = TRUE)

# Store vector of column names for final result data set
columnNames.results <- colnames(data.results)

# Clean up column names to create more readable column headers
for (i in 1:length(columnNames.results))
    {
        columnNames.results[i] <- gsub("^f", "frequency", columnNames.results[i])
        columnNames.results[i] <- gsub("^t", "time", columnNames.results[i])
        columnNames.results[i] <- gsub("Acc", "Accelerometer", columnNames.results[i])
        columnNames.results[i] <- gsub("Gyro", "Gyroscope", columnNames.results[i])
        columnNames.results[i] <- gsub("Mag", "Magnitude", columnNames.results[i])
        columnNames.results[i] <- gsub("BodyBody", "Body", columnNames.results[i])
    }

# Apply more readable column names to the data.results data set
colnames(data.results) <- columnNames.results


###### CREATE TIDY DATA SET #######

tidyData.initial <- data.results[, colnames(data.results) != "ActivityType"]

tidyData.final <- aggregate(tidyData.initial[, colnames(tidyData.initial) != c("ActivityID", "SubjectID")], by = list(ActivityID=tidyData.initial$ActivityID, SubjectID=tidyData.initial$SubjectID), mean)

tidyData.final <- merge(tidyData.final, data.activityType, by = "ActivityID", all.x = TRUE)

write.csv(tidyData.final, file = "tidydata.csv")
}
