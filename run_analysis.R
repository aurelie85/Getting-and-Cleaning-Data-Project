## Read in the test data.
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Read the training data in the same way

trainSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")


## Chek the features of the data
features <- read.table("./UCI HAR Dataset/features.txt")
#features

## Convert the features colums to character.
featureVector <- as.character(features[,2])

## Use the function "grep" to find the mean and the standard deviation
meanCols <- grep("mean", featureVector)
sdCols <- grep("std", featureVector)

## Combine both into inly one.
allCols <- sort(c(meanCols,sdCols))

## Create the test subset
subsetTestSet <- testSet[,allCols]

## Add the activity label and name columns

subsetTestWithLabels <- cbind(testLabels,subsetTestSet)
subsetTestWithNames <- cbind(subjectTest,subsetTestWithLabels)

## Create the training subset.
subsetTrainSet <- trainSet[,allCols]

## Add the activity label and name columns.
subsetTrainWithLabels <- cbind(trainLabels, subsetTrainSet)
subsetTrainWithNames <- cbind(subjectTrain, subsetTrainWithLabels)

## Combine the two data frames into one.
combinedDataset <- rbind(subsetTestWithNames, subsetTrainWithNames)

## Add column names.
colnames(combinedDataset)[1] <- "subject"
colnames(combinedDataset)[2] <- "activity"
colnames(combinedDataset)[3:(ncol(combinedDataset))] <- featureVector[allCols]

## Summarize the data by name and activity caluculating the mean of the columns.
data <- aggregate(combinedDataset, by=list(combinedDataset$subject, 
                                                  combinedDataset$activity), FUN=mean, na.rm=TRUE)
## Remove the first two columns, which are redundant to the third and fourth.
data <- data[, 3:(ncol(data))]

## Convert the activity data into human-readable factors.
data[,2] <- factor(data[,2])
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
levels(data[,2]) <- activities[,2]

## Create the output file to upload.
write.table(data, "./tidy.txt")