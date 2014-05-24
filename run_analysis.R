##################
## Instructions ##
##################

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#Here are the data for the project: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#You should create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Apropriately labels the data set with descriptive activity names. 
#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

## 0. Data Preparation
## Load packages that we would use

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# change the working directory, where to load the data

path <- setwd("~/Coursera/Data Science Specialization/3. Getting and Cleaning Data/Peer Assessments/Getting-and-Cleaning-Data-Project")
path

## Download the data

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f), method = "curl")

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

## Read the files

dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

fileToDataTable <- function (f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))


## 1. Merges the training and the test sets to create one data set

dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

# Merge columns.

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

# Set key.

setkey(dt, subject, activityNum)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Tips found in the forum: check int the 'features.txt' file for the names of the variables in `dt` which are mean and standard deviation.

dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# The following will subset only measurements for the mean and standard deviation.

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# Convert the column numbers to a vector of variable names matching columns in `dt`.

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

# Subset these variables using variable names.

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]


#3. Uses descriptive activity names to name the activities in the data set

# Tips: the file 'activity_labels.txt'can be used to add descriptive names to the activities.

dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#4. Apropriately labels the data set with descriptive activity names. 

# The following will merge the activity names
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

# Add 'activityName as a key.

setkey(dt, subject, activityNum, activityName)

# Melt the data table to reshape it from a short and wide format to a tall and narrow format.

dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# Merge activity name.

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# Create a new variable, `activity` that is equivalent to `activityName` as a factor class.
# Create a new variable, `feature` that is equivalent to `featureName` as a factor class.

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# Seperate features from `featureName` using the helper function `grepthis`.

grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
head(dtTidy)
write.table(dtTidy, "tidy.txt", quote = FALSE, row.names = FALSE)
