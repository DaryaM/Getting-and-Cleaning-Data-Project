### Description
A full description of the data is available here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

### Data Source
The data for the project can be obtained here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

### Attrivute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### run_analysis.R 
### 1. Merges the training and the test sets to create one data sets
Merges the following files
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

### 2. Extract only the measurements on the mean and standard deviation for each measurement. 
Creates a vector that contains true values for the ID, mean and stdev columns and false values for the others. Subsets data 

### 3. Use descriptive activity names to name the activities in the data set
Merges data subset with the activityType table

### 4. Appropriately label the data set with descriptive activity names.
Cleans up data lables using the gsub function

### 5. Create a second, independent tidy data set with the average of each variable for each activity and each subjec
Produces a data set with the average of each veriable for each activity and subject



