
# 1. Merge the training and the test sets to create one data set

# set working directory
setwd("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset")

features= read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/features.txt",header=FALSE); #imports features.txt
activityLabels = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/activity_labels.txt",header=FALSE); #imports activity_labels.txt
subjectTrain = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/train/subject_train.txt",header=FALSE); #imports subject_train.txt
xTrain = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/train/x_train.txt",header=FALSE); #imports x_train.txt
yTrain = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/train/y_train.txt",header=FALSE); #imports y_train.txt

# column names ^^
colnames(activityLabels)  = c('activityId','activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2]; 
colnames(yTrain) = "activityId";

#Training data merge
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/test/subject_test.txt",header=FALSE); #imports subject_test.txt
xTest = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/test/x_test.txt",header=FALSE); #imports x_test.txt
yTest = read.table("/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/data/UCI HAR Dataset/test/y_test.txt",header=FALSE); #imports y_test.txt

# column names ^^
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# test Data merge
testData = cbind(yTest,subjectTest,xTest);

# merge traingData and testData 
allData = rbind(trainingData,testData);
colNames  = colnames(allData); 

# 2. Extracts only the measurements on the mean and standard deviation for each measurement

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
allData = allData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# merget activityType data with allData
allData = merge(allData,activityType,by='activityId',all.x=TRUE);
# update column names after merge
colNames  = colnames(allData); 

# 4. Appropriately labels the data set with descriptive variable names

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","Stan.Deviat",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(allData) = colNames;

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

# Create a new table, tidyDataSummary without the activityType column
tidyDataSummary = allData[,names(allData) != 'activityType'];

# Summarize tidyDataSummary table to include only the mean of each variable for each activity and each subject
tidyData = aggregate(tidyDataSummary[,names(tidyDataSummary) != c('activityId','subjectId')],by=list(activityId=tidyDataSummary$activityId,subjectId = tidyDataSummary$subjectId),mean);

# Merging the tidyData with activityType
tidyData=  merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, "/Users/daryamoldavskaya/Documents/DataScience/Getting and Cleaning Data/Project/tidyData.txt",row.names=TRUE,sep='\t');
