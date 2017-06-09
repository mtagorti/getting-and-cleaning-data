##Part1: First step collecting the primary data after doing the experiments
directory1<-"C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals"#setting directory
directory2<-"C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals"
files_list1<-list.files(directory1, full.names =TRUE)#listing files
data1<-list()#putting all files into a list
for (i in 1:length(files_list1)) {
        data1[[i+2]] <- tbl_df(read.table(files_list1[i]))#loops through the files, rbinding them together
}
data1[[2]]<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"))#adding the subject column
data1[[1]]<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"))#adding the activity column
dataframe_test<-cbind.data.frame(data1)# collecting all the datas
#In the following we will reproduce the same steps
files_list2 <- list.files(directory2, full.names =TRUE)   #creates a list of files
data2<-list()
for (i in 1:length(files_list2)) {
        data2[[i+2]] <- tbl_df(read.table(files_list2[i]))#loops through the files, rbinding them together
}

data2[[2]]<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"))
data2[[1]]<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"))
dataframe_train<-cbind.data.frame(data2)
dataframe_final<-rbind.data.frame(dataframe_train,dataframe_test)# collecting both train and test datas
names(dataframe_final)<-c("subject","activity",paste(replicate(128,"Body_acc_X"),1:128, sep=""),
                          paste(replicate(128,"Body_acc_Y"),1:128, sep=""),paste(replicate(128,"Body_acc_Z"),1:128, sep=""),
                          paste(replicate(128,"Body_gyro_X"),1:128, sep=""),paste(replicate(128,"Body_gyro_Y"),1:128, sep=""),
                          paste(replicate(128,"Body_gyro_Z"),1:128, sep=""),paste(replicate(128,"Total_acc_X"),1:128, sep=""),
                          paste(replicate(128,"Total_acc_Y"),1:128, sep=""),paste(replicate(128,"Total_acc_Z"),1:128, sep=""))#naming the columns
dataframe_final<-tbl_df(dataframe_final)
activity_label<-read.table("activity_labels.txt")
dataframe_final<-mutate(dataframe_final, activity=activity_label[activity,2])#putting labels to the columns
##Part2: Collecting the features from the train and test sets
X_test<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt"))
X_test_subject<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"))
X_test_activity<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"))
X_test_final<-cbind.data.frame(X_test_subject,X_test_activity,X_test)
X_train<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",header=FALSE))
X_train_subject<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",header=FALSE))
X_train_activity<-tbl_df(read.table("C:/Users/HP/downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",header=FALSE))
X_train_final<-cbind.data.frame(X_train_subject,X_train_activity,X_train)
X_subject_activity<-rbind.data.frame(X_train_final,X_test_final)
##Part3: naming the dataframe and extracting the pertinent features
features<-read.table("features.txt",sep=" ",header=FALSE)#reading the features labels
features<-paste(features[,2], features[,1], sep="")
names(X_subject_activity)<-c("subject","activity",as.character(unique(features)))# attributing columnnames to the dataframe
X_subject_activity<-tbl_df(X_subject_activity)
X_subject_activity<-select(X_subject_activity,subject,activity,grep("mean", names(X_subject_activity)),grep("Mean",names(X_subject_activity)),grep("std", names(X_subject_activity)))#looking for the key terms to extract the data
##Part4: Attributing labels to the activity
activity_label<-read.table("activity_labels.txt")
X_subject_activity<-mutate(X_subject_activity, activity=activity_label[activity,2])
##Part5: Getting a tidy data
Gr_by<-group_by(X_subject_activity,subject,activity)#grouping the data by subject and activity
List<-list()
for (i in 3:88){
name<-names(Gr_by)[[i]]#name is the true name of the dataframe column     
names(Gr_by)[[i]]<-"h"  # h is a temporary variable that we use only in the #function summarize    
List[[i-2]]<-as.data.frame(summarize(Gr_by, mean(h)))
names(Gr_by)[[i]]<-name#we reattribute to the dataframe its true name
names(List[[i-2]])<-c("subject","activity",names(Gr_by[,i]))}
tidy_data<-join_all(List)
write.table(tidy_data,"tidy_table.txt", row.name=FALSE,sep=" ")

