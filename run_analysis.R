library(plyr)

# Loading test and training data sets into R

testSet<-read.table("test/X_test.txt")
trainSet<-read.table("train/X_train.txt")

# Loading features file to set up readable data headers
# Previously to be used they will be modified to remove parenthesis and commas

features<-read.table("features.txt")
features$V2<-sub("\\()", "", features$V2) # remove parenthesis
features$V2<-sub("\\(", "_", features$V2) # change left parenth. by underscore
features$V2<-sub("\\)", "_", features$V2) # change right parenth. by underscore
features$V2<-sub(",", "_", features$V2) # change comma by underscore

features$V2<-sub("\\)", "_", features$V2) # typo in headers, this has to be done twice

# Apply readable headers to the datasets, the second column of the features dataframe
# contains the readable column values for the data

names(testSet)<-features$V2
names(trainSet)<-features$V2

# Now we load the activity labels and the subject__IDs involved in each row

testLabels<-read.table("test/y_test.txt")
trainLabels<-read.table("train/y_train.txt")
testSubjects<-read.table("test/subject_test.txt")
trainSubjects<-read.table("train/subject_train.txt")

# We build complete data frames for training and test

testFull<-cbind(testSubjects, testLabels, testSet)
trainFull<-cbind(trainSubjects, trainLabels, trainSet)

# And put them together in one data frame as requested

fullData<-rbind(testFull, trainFull)

# Clear up memory to facilitate new operations
rm(testLabels, trainLabels, testSubjects, trainSubjects, testFull, trainFull, testSet, trainSet)

# Label 2 first columns with their correct and readable names
names(fullData)[2]<-paste("Activity")
names(fullData)[1]<-paste("Subject")

# Load activity Labels in order to display clear activity literals in data, change values

fullData$Activity[fullData$Activity == 1]<-paste("Walking")
fullData$Activity[fullData$Activity == 2]<-paste("Walking_Upstairs")
fullData$Activity[fullData$Activity == 3]<-paste("Walking_Downstairs")
fullData$Activity[fullData$Activity == 4]<-paste("Sitting")
fullData$Activity[fullData$Activity == 5]<-paste("Standing")
fullData$Activity[fullData$Activity == 6]<-paste("Laying")

# Transform to factor variables to be able to extract information from the dataset (summary)
fullData$Activity<-as.factor(fullData$Activity)
fullData$Subject<-as.factor(fullData$Subject)

# create a dataset with mean and std deviation values as requested in assignment
select1<-fullData[, grep("mean", names(fullData))]
select2<-fullData[, grep("std", names(fullData))]
# Bind mean and std values
selectData<-cbind(select1, select2)
# Add subject and Activity columns to the resulting dataset
selectData<-cbind(fullData$Activity, selectData)
selectData<-cbind(fullData$Subject, selectData)
names(selectData)[1]<-paste("Subject")
names(selectData)[2]<-paste("Activity")
# Sort by Subject/Activity
selectData<-arrange(selectData, Subject, Activity)

# Create tidy set from selectData
tidySet<-melt(selectData, id=c("Subject", "Activity"), measure.vars=3:81)
names(tidySet)[3]<-paste("Measurement")
# Group the tidy DataSet by Subject and Activity to summarize
tidySet<-group_by(tidySet, Subject, Activity, Measurement)

tidyResultSet<-summarize(tidySet, average_value = mean(value))
# Write the results file to "tidyresultset.txt" file
write.table(tidyResultSet, file="tidyresultset.txt", row.names = FALSE, col.names = FALSE)
rm(features, select1, select2)


