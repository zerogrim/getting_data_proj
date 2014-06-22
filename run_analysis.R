
printf <- function(...)print(sprintf(...))

trow = 100

# read the training set
x_training <- read.table(   file = "train/X_train.txt", 
                            sep ="", 
                            header = FALSE,
                            nrow = trow)

y_training <- read.table(   file = "train/y_train.txt", 
                            sep = "", 
                            header = FALSE,
                            nrow = trow)

printf("    read x_training[%d,%d] y_training[%d,%d]", 
            nrow(x_training),
            ncol(x_training),
            nrow(y_training),
            ncol(y_training))

# read the testing set
x_testing <- read.table(    file = "test/X_test.txt", 
                            sep ="", 
                            header = FALSE,
                            nrow = trow )

y_testing <- read.table(    file = "test/y_test.txt", 
                            sep = "", 
                            header = FALSE,
                            nrow = trow)

printf("    read x_testing[%d,%d] y_testing[%d,%d]", 
            nrow(x_testing),
            ncol(x_testing),
            nrow(y_testing),
            ncol(y_testing))

# read the subject data sets
subj_test = read.table(     file="test/subject_test.txt",
                            sep = "",
                            header = FALSE,
                            nrow = trow)

subj_train = read.table(    file="train/subject_train.txt",
                            sep = "",
                            header = FALSE,
                            nrow = trow)

printf("    read subjects train [%d, %d] and test [%d,%d]",
            nrow(subj_train),
            ncol(subj_train),
            nrow(subj_test),
            ncol(subj_test))

# apply y labels to test and train sets
training <- cbind(x_training, y_training, subj_train[,1])
testing <- cbind(x_testing,y_testing,  subj_train[,1])


#training$subj_id <- subj_train[,1]
#testing$subj_id <- subj_test[,1]

printf("    combined training x and y [%d,%d]", 
            nrow(training),
            ncol(training)
            )
printf("    combined testing x and y [%d,%d]", 
            nrow(testing),
            ncol(testing)
            )


#read variable names
labels = read.table(file = 'features.txt', sep="", header = FALSE)
printf("    read labels [%d,%d]",
            nrow(labels),
            ncol(labels)
            )

#apply labels
#colnames(training) = c(as.character(labels[,2]),"activity_id", "subj_id")
#colnames(testing) = c(as.character(labels[,2]),"activity_id", "subj_id")
colnames(training) = c(as.character(labels[,2]),"activity_id", "subj_id")
colnames(testing) = c(as.character(labels[,2]),"activity_id", "subj_id")
printf("    applied label names")


# rbind the two sets
full_set <- rbind(testing, training)
printf("    combined training and testing [%d,%d]",
            nrow(full_set),
            ncol(full_set)
            )




# build list of columns that contain mean or std
mean_cols = grepl("mean", names(full_set))
std_cols = grepl("std", names(full_set))

#Extracts only the measurements on the mean and standard deviation 
#for each measurement and subject and activity data
trimmed_set  = cbind(   full_set[,mean_cols], 
                        full_set[,std_cols]
                    )

trimmed_set$activity_id = factor(full_set[,"activity_id"])
trimmed_set$subj_id = factor(full_set[,"subj_id"])

printf("    trimmed set to mean and std only[%d,%d]",
            nrow(trimmed_set),
            ncol(trimmed_set))

#Uses descriptive activity names to name the activities in the data set
#make new variable activity_name from activity_id
activity_name = list()
id_list = trimmed_set$activity_id
for( i in 1:length(id_list) ){
    id = id_list[i]
    if(id == 1) activity_name[i] = "WALKING"
    if(id == 2) activity_name[i] = "WALKING_UPSTAIRS"
    if(id == 3) activity_name[i] = "WALKING_DOWNSTAIRS"
    if(id == 4) activity_name[i] = "SITTING"
    if(id == 5) activity_name[i] = "STANDING"
    if(id == 6) activity_name[i] = "LAYING"
    #print(activity_name[i])
}

printf("    generated activity names len: %d",
            length(activity_name)
            )


# apply the new variable to trimmed set
#Appropriately labels the data set with descriptive variable names.
trimmed_set$activity_nm = factor(as.character(activity_name) )


ad_idx = which(colnames(trimmed_set) == "activity_id")
s_idx = which(colnames(trimmed_set) == "subj_id")
as_idx = which(colnames(trimmed_set) == "activity_nm")

#Creates a second, independent tidy data set 
tidy_set = trimmed_set[1,-ad_idx]
#tidy_set = tidy_set[-1,-ad_idx]


t_s_idx = which(colnames(tidy_set) == "subj_id")
t_as_idx = which(colnames(tidy_set) == "activity_nm")
newcolnames = paste(    "avg",
                        colnames(tidy_set[,c(-t_s_idx,-t_as_idx)]),
                        sep="_")

newcolnames = c(newcolnames,"subj_id","activity_nm")                  

printf("     created tidy frame [%d %d]",nrow(tidy_set), ncol(tidy_set))
#with the average of each variable for each activity and each subject. 
#
for(subj in levels(trimmed_set$subj_id)){
    for(act in levels(trimmed_set$activity_nm)){
    
        
        newtab = trimmed_set[(  trimmed_set$subj_id == subj &
                                trimmed_set$activity_nm == act),
                                c(-ad_idx, -as_idx, -s_idx)]

        printf("        subj:%s   act: %s [%d,%d]",  subj, 
                                                as.character(act),
                                                nrow(newtab),
                                                ncol(newtab))
        newrow = c()
        #loop through the columns and create rows
        for(col in 1:ncol(newtab)){
            m = mean(newtab[,col])
            newrow = c(newrow,m)
        }

        newrow = c(newrow, subj, act)
        printf("        created new row[%d]",length(newrow))
        tidy_set = rbind(tidy_set,newrow)
    }
}

#rename columns
colnames(tidy_set) = newcolnames

write.table(tidy_set,"tidy.csv",sep=",",col.names=TRUE)
