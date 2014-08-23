Instructions to get the Tidy Data Set as outlined in the course project of "Getting and Cleaning Data"
=========

- Place the Run_Analysis.R script in the same working directory as the samsung data - ie UCI_HAR_Dataset

- Step 4th and 6th of the Run_Analysis.R script achieves the step 1 & 2 of the requirement ie merging the training and test sets to create one data set with only measurements on the mean and standard deviation for each measurement.

-  Step 13th and 14th of the Run_Analysis.R script labels the activities and column names of the resulting tidy data set. This completes the requirements 1 to 4 of the assignment.

-  At step 15th of the Run_Analysis.R script we are returning the final "tidy data set" with the average of each variable for each activity and each subject.  

- In the final "tidy data set" the total columns is 81 and total rows are 180 (30x6). First column is the "subject" column (ranges from 1 to 30), second column is "activity" column with ranges from 1 to 6 (for label description please refer to activity_labels.txt) and rest of 79 columns describe the average of the mean and standard deviation measurements with appropriate labeling.
