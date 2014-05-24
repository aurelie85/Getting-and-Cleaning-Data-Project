
## Instructions

* The R script called run_analysis.R does the following. 
* 1. Merges the training and the test sets to create one data set.
* 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
* 3. Uses descriptive activity names to name the activities in the data set
* 4. Apropriately labels the data set with descriptive activity names. 
* 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Below, you will find the explication for the steps.
  
## 0. Data Preparation
* Load packages that we would use
* change the working directory, where to load the data
* Download the data
* Read the files

## 1. Merges the training and the test sets to create one data set

* Use the "rbind" function to merge the data
* Merge then columns.
* Create key on a data table

## 2. Extracts only the measurements on the mean and standard deviation for each measurement 

* read the 'features.txt' file for the names of the variables which are mean and standard deviation
* subset only measurements for the mean and standard deviation.
* Convert the column numbers to a vector of variable names matching columns in the created file.
* Subset these variables using variable names.

## 3. Uses descriptive activity names to name the activities in the data set
  
* Read the file 'activity_labels.txt' to add the descriptive names to the activities.

## 4. Apropriately labels the data set with descriptive activity names. 

* merge the activity names
* Add 'activityName' as the key.
* Convert object into a molten data frame
* Merge activity name.

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
  
* generate the tidy data set by using the 'write.table' function
