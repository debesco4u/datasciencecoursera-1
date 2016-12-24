# Code

# Loading required packages (install it if necessary)
library(plyr)
library(dplyr)

#####READING AND EXPLORING FILES
# you can use the folder "data_to_use_for_script" provided in this repo as working directory to source all files needed.
# otherwise download and unzip the data from the provided URL and make sure you place all the necessary files in the same working directory.

#Features
features<- read.table("features.txt") # to be used for both X_train & X_test data
dim(features) # is 561 as the X_train number of variables

#Activity labels
activity_labels<- read.table("activity_labels.txt")
head(activity_labels) # is a reference data frame with numbers corresp.activities

#Train data
X_train<- read.table("X_train.txt")
subject_train<- read.table("subject_train.txt")
y_train<- read.table("y_train.txt")

train<- cbind(subject_train, y_train, X_train)

#Test data
X_test<- read.table("X_test.txt")
subject_test<- read.table("subject_test.txt")
y_test<- read.table("y_test.txt")

test<- cbind(subject_test, y_test, X_test)

#####STEP 1: MERGE TRAIN AND TEST DATASETS VERTICALLY.

mergedDF<-rbind(train,test)

# Change all variables names: 
varNames<-c("subject","activity_num",as.character(features$V2))
names(mergedDF)<-varNames

# Add activity column:
table(mergedDF$activity_num) #checking activity occurrencies

lookup<- activity_labels
names(lookup)<- c("activity_num","activity_type")
labeledDF<- join(mergedDF,lookup,by='activity_num')

# checking that labels have been aded correctly
table(labeledDF$activity_type)
head(labeledDF[labeledDF$activity_type=="WALKING",1:3])

#####STEP 2: EXCTRACT ONLY MEASUREMENTS ON THE MEAN AND STD DEVIATION.

# Can't perform the extraction because of duplicated column names: first of all will have to fix duplicated columns
sum(duplicated(features$V2)) # there are 84 columns that have duplicated names
features$V2[duplicated(features$V2)] # luckily, none of those is a measurement we need to build the final data frame
# Remove the duplicated columns:
to.remove<-as.character(features$V2[duplicated(features$V2)])
dropDF<-labeledDF
dropDF<-dropDF[, !(colnames(dropDF) %in% to.remove)]

#Now subset with only measurements on the mean and standard deviation:
subDF<-select(dropDF,contains("subject"), contains("activity_type"), contains("mean"), contains("std"))
dim(subDF) # final dataset has 88 columns

#####STEP 3: USE DESCRIPTIVE ACTIVITY NAMES for THE ACTIVITIES IN THE DATA SET.
#already done in step 1 

#####STEP 4: LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES 
#already done in step 1

#####STEP 5: CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVG. OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT.

df2<- aggregate(subDF, by=list(subDF$subject, subDF$activity_type),  FUN=mean)
dim(df2) # final dataset has 180 rows which makes sense. But 2 columns have been added.

#Fix columns created with the aggregate function:
df2$subject<-NULL
df2$activity_type<-NULL
colnames(df2)[1:2]<-c("subject","activity_type")
str(df2) #is a data frame, subject is integer, activity_type is factor and the rest of columns are all numeric class.

#Write final dataset in a .txt file:
write.table(df2, "tidy_dataset_with_avg.txt", row.name=FALSE)
