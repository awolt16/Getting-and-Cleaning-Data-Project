download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ","Dataset.zip", mode="wb")
unzip("Dataset.zip")

subject_train <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/train/subject_train.txt", quote="\"")
View(subject_train)
X_train <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/train/X_train.txt", quote="\"")
View(X_train)
y_train <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/train/y_train.txt", quote="\"")
View(y_train)

subject_test <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/test/subject_test.txt", quote="\"")
View(subject_test)
X_test <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/test/X_test.txt", quote="\"")
View(X_test)
y_test <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/test/y_test.txt", quote="\"")
View(y_test)

activity_labels <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/activity_labels.txt", quote="\"")
View(activity_labels)

trainData<-cbind(subject_train,X_train,y_train)
testData<-cbind(subject_test,X_test,y_test)
DataSet1<-rbind(trainData,testData)

features <- read.table("~/Math 377/Math 378 R/UCI HAR Dataset/features.txt", quote="\"")
View(features)
features2<-cbind("Subject",t(features),"Activity")
colnames(DataSet1)<-features2[2,]

meanCol<-grep("mean()",colnames(DataSet1))
stdCol<-grep("std()",colnames(DataSet1))

DataSet2<-cbind(DataSet1[,1],DataSet1[,563],DataSet1[,meanCol],DataSet1[,stdCol])

colnames(DataSet2)[1]<-"Subject"
colnames(DataSet2)[2]<-"Activity"

meanFreq<-grep("meanFreq()",colnames(DataSet2))
DataSet2<-DataSet2[,-meanFreq]

TidyData<-c()

subjects=c(1:30)

for (i in 1:30){
  for (j in 1:6){
    temp<-subset(DataSet2,Subject==subjects[i] & Activity==activity_labels[j,1])
    temp2<-apply(temp[,3:68],2,mean)
    temp3<-cbind(subjects[i],activity_labels[j,2],t(temp2))
    TidyData<-rbind(TidyData,temp3)
  }
}

for (i in 1:180){
  if (TidyData[i,2]==1){
    TidyData[i,2]<-"WALKING"}
  if (TidyData[i,2]==2){
    TidyData[i,2]<-"WALKING_UPSTAIRS"}
  if (TidyData[i,2]==3){
    TidyData[i,2]<-"WALKING_DOWNSTAIRS"}
  if (TidyData[i,2]==4){
    TidyData[i,2]<-"SITTING"}
  if (TidyData[i,2]==5){
    TidyData[i,2]<-"STANDING"}
  if (TidyData[i,2]==6){
    TidyData[i,2]<-"LAYING"}
}

names(TidyData)<-gsub("^t","Time",names(TidyData))
names(TidyData)<-gsub("Acc","Accelerometer",names(TidyData))
names(TidyData)<-gsub("^f","Frequency",names(TidyData))
names(TidyData)<-gsub("Gyro","Gyroscope",names(TidyData))
names(TidyData)<-gsub("Mag","Magnitude",names(TidyData))
names(TidyData)<-gsub("BodyBody","Body",names(TidyData))

write.table(TidyData,file="tidydata.txt",row.name=FALSE)
