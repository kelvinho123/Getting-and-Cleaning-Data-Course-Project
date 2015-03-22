# File name: run_analysis.R
# Data Source used: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This R script does the following:
#
#	1.	Merges the training and the test sets to create one data set.
#	2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
#	3.	Uses descriptive activity names to name the activities in the data set
#	4.	Appropriately labels the data set with descriptive variable names. 
#	5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# ------------------------------------------------------------------	
# Task 1 : Merges the training and the test sets to create one data set.
#-------------------------------------------------------------------
temp1 <- read.table("train/X_train.txt")	#read input from txt file
temp2 <- read.table("test/X_test.txt")		#read input from txt file
X <- rbind(temp1, temp2)			#combine rows of X train and test data

temp1 <- read.table("train/subject_train.txt")	#read input from txt file
temp2 <- read.table("test/subject_test.txt")	#read input from txt file
S <- rbind(temp1, temp2)			#combine rows of Subject train and test data

temp1 <- read.table("train/y_train.txt")	#read input from txt file
temp2 <- read.table("test/y_test.txt")		#read input from txt file
Y <- rbind(temp1, temp2)			#combine rows of Y train and test data

#-------------------------------------------------------------------------------------------------
# Task 2 : Extracts only the measurements on the mean and standard deviation for each measurement.
#--------------------------------------------------------------------------------------------------
features <- read.table("features.txt")
Selected_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, Selected_features]
names(X) <- features[Selected_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))	#replace () with blanks
names(X) <- tolower(names(X))			#convert to lower case

#--------------------------------------------------------------------------------
# Task 3 : Uses descriptive activity names to name the activities in the data set.
#---------------------------------------------------------------------------------
activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

#-----------------------------------------------------------------------------
# Task 4 : Appropriately labels the data set with descriptive activity names.
#-----------------------------------------------------------------------------
names(S) <- "subject"
merged_cleaned_data <- cbind(S, Y, X)				#combine columns of S, Y, X data
write.table(merged_cleaned_data, "merged_clean_data.txt")	#output to txt file

#--------------------------------------------------------------------------------------------------------------------
# Task 5 : Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
#------------------------------------------------------------------------------------------------------------------------

uniqueSubjects = unique(S)[,1]				#select distinct values from S data set
numSubjects = length(unique(S)[,1])			#count nos of distinct values from S data set
numActivities = length(activities[,1])			#count nos of activities values from activities data set
numCols = dim(merged_cleaned_data)[2]		
result = merged_cleaned_data[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- merged_cleaned_data[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages_nownames_FALSE.txt",row.names=FALSE)

