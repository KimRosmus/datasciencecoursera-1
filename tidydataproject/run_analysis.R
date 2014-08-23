run_analysis <- function() {
    ##1 reading the training data 
    subject_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt")
    X_train <- read.table("UCI_HAR_Dataset/train/X_train.txt")
    y_train <- read.table("UCI_HAR_Dataset/train/y_train.txt")
    
    ##2 read the features data
    features <- read.table("UCI_HAR_Dataset/features.txt")
    
    ##3 Extracts the measurements on the mean and standard deviation for each measurement.
    meanIndices <- grep("mean",features$V2)
    stdIndices <- grep("std()",features$V2)
    f_indices <- c(meanIndices,stdIndices)
    
    ##4 get the refined training data which has mean and std deviation measurements.
    xtrain_final <- X_train[,f_indices]    
    yX_train <- cbind(y_train, xtrain_final)
    syx_train <- cbind(subject_train, yX_train)
    
    ##5 reading the test data
    subject_test <- read.table("UCI_HAR_Dataset/test/subject_test.txt")
    X_test <- read.table("UCI_HAR_Dataset/test/X_test.txt")
    y_test <- read.table("UCI_HAR_Dataset/test/y_test.txt")
    
    ##6 get the refined test data which has mean and std deviation measurements.
    xtest_final <- X_test[,f_indices]        
    yX_test <- cbind(y_test, xtest_final)
    syx_test <- cbind(subject_test, yX_test)
    
    ##7 create a final raw data set combining the data from 4th and 6th step ie refined training data and test data.
    ## here we are done with the requirement 1 & 2 of the assignment.
    fdata = rbind(syx_train, syx_test)
    
    ##8 label the first and second column appropriately ie "subject" and "activity"
    names(fdata)[1] <- "subject"
    names(fdata)[2] <- "activity"
    
    ##9 here we create the result dataframe which will eventually contain the final tidy data set - step 5 of assignment.
    result <- data.frame()
    
    ##10 here we are calculating the average of each variable for each activity and each subject. 
    ## when the loop finishes we will have the final "tidy data set" - requirement 5th of assignment with labels missing.
    for (i in 1:30) {
        for (j in 1:6) {
            ss <- fdata[fdata$subject == i & fdata$activity == j,]
            ss <- aggregate(.~activity,FUN=mean,data=ss[,1:81], na.rm=T)
            result <- rbind(result,ss)
        }
    }
    ##11 because of step 10 above "subject" & "activity" columns are interchanged - fixing it.
    vect <- c(names(result))
    v <- vect[1]
    vect[1] <- vect[2]
    vect[2] <- v
    result <- result[, vect]
    
    ##12 reading the activity labels since we need to label the activities in the final tidy set appropriately.
    al <- read.table("UCI_HAR_Dataset/activity_labels.txt")
    nr <- nrow(al)
    
    ##13 Using descriptive activity names to name the activities in the data set - result
    for (k in 1:nr) {
        result$activity[result$activity == al$V1[k]] <- as.character(al$V2[k])
    }
    
    ##14 labeling the columns of data set with descriptive variable names.
    for (n in 3:81) {
        ind <- as.numeric(substring(names(result)[n],2))
        colnames(result)[n] <- as.character(features$V2[ind])
    }
    
    ##15 returning the required "tidy data set" as mentioned in the step 5 of the problem statement of assignment.
    return (result)
}