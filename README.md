# whats in run_analysis.R by Jaymin Barot

## The README that explains the analysis files is clear and understandable.
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement.
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive activity names.
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Dependencies

* reshape2 and data.table. run_analysis.R will download both package & load them both automatically. 

## Steps
* Download the data source and put into a folder on your local drive. You'll have a UCI HAR Dataset folder.
* Put run_analysis.R in the parent folder of UCI HAR Dataset, then set it as your working directory using setwd() function in RStudio.
* Run source("run_analysis.R"), then it will generate a new file tiny_data.txt in your working directory.