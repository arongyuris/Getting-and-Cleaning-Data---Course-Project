# Load plyr, dplyr and stringr

library(plyr)
library(dplyr)
library(stringr)

# Download and Unzip files from source

CourseProjectURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("./UCI HAR Dataset.zip")) {
        download.file(CourseProjectURL, destfile = "./UCI HAR Dataset.zip", method = "curl")
        unzip("./UCI HAR Dataset.zip")
}

# Load Training and Test data into R

training_activity <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
colnames(training_activity) <- "activity"
training_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
colnames(training_subject) <- "subject"
training_features <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)

test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
colnames(test_activity) <- "activity"
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
colnames(test_subject) <- "subject"
test_features <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Merge Training and Test data by category 

activity_merge <- rbind(training_activity, test_activity)
subject_merge <- rbind(training_subject, test_subject)
features_merge <- rbind(training_features, test_features)

# Apply names to columns

test_features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
test_features_names <- make.names(as.character(test_features[, 2]), unique = TRUE)
names(features_merge) <- test_features_names

# Merge labeled data into one table

Galaxy_Data <- cbind(subject_merge, activity_merge, features_merge)

# Select columns with mean and STD

Galaxy_Data_mean_STD <- select(Galaxy_Data, subject, activity, contains(".mean."), contains(".std."))

# Convert activity labels code to activity label

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
Galaxy_Data_mean_STD[, 2] <- as.factor(Galaxy_Data_mean_STD[, 2])
levels(Galaxy_Data_mean_STD$activity) <- activity_labels[, 2]

# Rename truncated column names to human readable format

Galaxy_Data_mean_STD_names <- colnames(Galaxy_Data_mean_STD)
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, 'Acc', 'Acceleration')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, 'Gyro', 'Velocity')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, 'Mag', 'Magnitude')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, '^f', 'frequency')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, '^t', 'time')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, '\\-mean\\(\\)', 'Mean')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, '\\-std\\(\\)', 'Std')
Galaxy_Data_mean_STD_names <- str_replace_all(Galaxy_Data_mean_STD_names, '\\-', '')
colnames(Galaxy_Data_mean_STD) <- Galaxy_Data_mean_STD_names

## Create second data frame with the average of each variable for each activity and each subject

tidy_data <- ddply(Galaxy_Data_mean_STD, .(subject, activity), function(x) colMeans(x[,3:68]))

## Write second data frame to .txt output file

write.table(tidy_data, file = 'tidy_data.txt', row.names = FALSE)