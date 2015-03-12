##########################################################################################################

## Coursera Getting and Cleaning Data Course Project

#You should create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################




## load all data
xtrain <- read.table("train\\X_train.txt")
ytrain <- read.table("train\\y_train.txt")
subjecttrain <- read.table("train\\subject_train.txt")

xtest <- read.table("test\\X_test.txt")
ytest <- read.table("test\\y_test.txt")
subjecttest <- read.table("test\\subject_test.txt")

features     = read.table('features.txt',header=FALSE)

activityLables = read.table('activity_labels.txt',header=FALSE);


# step 1 Merges the training and the test sets to create one data set.
###############################################################################
## merge  data
colnames(subjecttrain)  = "subjectId";
colnames(ytrain)        = "activityId";
#'features.txt': List of all features.
colnames(xtrain)        = features[,2]; 
# bind train data
traindata = cbind(ytrain,subjecttrain,xtrain);

colnames(subjecttest) = "subjectId";
colnames(xtest)       = features[,2]; 
colnames(ytest)       = "activityId";
testdata = cbind(ytest,subjecttest,xtest);

alldataWithAllVars = rbind(traindata,testdata);

# 
# 2. Extract only the measurements on the mean and standard deviation for each measurement
###############################################################################

# get only columns with mean() or std() in their names
mean_and_std_Cols <- grepl("(mean|std|activity|subject)", colnames(alldataWithAllVars))
# View(mean_and_std_Cols)
alldata <- alldataWithAllVars[,mean_and_std_Cols]


#3. Uses descriptive activity names to name the activities in the data set
###############################################################################
colnames(activityLables)  = c('activityId','activityLabel')
#add  activity label
alldata = merge(alldata,activityLables,by='activityId',all.x=TRUE);

#4. Appropriately labels the data set with descriptive variable names. 
###############################################################################

# Remove parentheses
names(alldata) <- gsub('\\(|\\)',"",names(alldata))    

#create descriptive names
names(alldata) <- gsub('Acc',"Acceleration",names(alldata))
names(alldata) <- gsub('GyroJerk',"AngularAcceleration",names(alldata))
names(alldata) <- gsub('Gyro',"AngularSpeed",names(alldata))
names(alldata) <- gsub('Mag',"Magnitude",names(alldata))
names(alldata) <- gsub('^t',"TimeDomain.",names(alldata))
names(alldata) <- gsub('^f',"FrequencyDomain.",names(alldata))
names(alldata) <- gsub('\\.mean',".Mean",names(alldata))
names(alldata) <- gsub('\\.std',".StandardDeviation",names(alldata))
names(alldata) <- gsub('Freq\\.',"Frequency.",names(alldata))
names(alldata) <- gsub('Freq$',"Frequency",names(alldata))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################

# filter  activityLabel column
alldata_NoActivityType  = alldata[,names(alldata) != 'activityLabel'];

# Summarizing the table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(alldata_NoActivityType[,!names(alldata_NoActivityType) %in% c('activityId','subjectId')],by=list(activityId=alldata_NoActivityType$activityId,subjectId = alldata_NoActivityType$subjectId),mean);

# Merge data
tidyData    = merge(tidyData,activityLables,by='activityId',all.x=TRUE);

# write data to file
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');