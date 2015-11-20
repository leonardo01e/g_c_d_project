# This script does the following:
# 1 - Merges the training and the test sets to create one data set.
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 - Uses descriptive activity names to name the activities in the data set
# 4 - Appropriately labels the data set with descriptive variable names. 
# 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
# Most of the learnig used to solve this project was found in this article:
# https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment/

# Reading and downloading Data from the internet

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./data.zip")

# Extracting files and folders of zip file downloaded to the WD.

unzip("data.zip")

# Reading all important .txt files of extracted file "data.zip"
# Remember: Set the files folder as "working directory" before read

features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")
subject_test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")

# initializing some R libraries used in this script

library(dplyr)
library(tidyr)
library(gdata)

# 1 - Merging training and test sets to create one data set for each.

# Training set
y_train <- rename(y_train, activity = V1)
subject_train <- rename(subject_train, id = V1)
tmp <- bind_cols(y_train, x_train)
training <- bind_cols(subject_train, tmp)

# Test set
y_test <- rename(y_test, activity = V1)
subject_test <- rename(subject_test, id = V1)
tmp2 <- bind_cols(y_test, x_test)
test <- bind_cols(subject_test, tmp2)

# Merging the data sets training and test in the total data set

total <- bind_rows(training, test)

# Data merged.

# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

# Puting the labels in the variables of "total" dataser using "features" dataset
# Indentifying the indexes of duplicated (if exist) labels in "features" 

# Vector dup receiving duplicated labels. If duplicated, then number 1 is assigned, else 0.
dup <- as.numeric(duplicated(features$V2))

ind <-vector() #initializing vector ind
j <- 0
for (i in 1:561){ #number of labels in the features$V2
  if (dup[i]==1){ # if equal 1, means that there is a duplicated label
     j <- j+1
     ind[j] <- i # vector that contains the indexes of duplicated labels in features.
  }
}

# There is some duplicated labels
# Removing duplicated labels from features using unique function
features_nodup <- unique(features$V2)

# using the indexes of duplicated features of ind vector to exclude them from dataframe total

total_nodup <- total[,-c(319:346,398:425, 477:504)]
# total_nodup: total with no duplicated label

# Applying the correct label to the variables in total_nodup dataset 
for (i in 3:479){ # start in 3 because the first two factor are named already
  names(total_nodup)[i] <- as.character(features_nodup[i-2,1])
}

# Checking new names variables

names(total_nodup)

# Selecting only variables related to the "mean" and "std"

tmp_mean <- select(total_nodup, c(id, activity, contains("mean", ignore.case = TRUE)))
tmp_std <- select(total_nodup, c(id, activity, contains("std", ignore.case = TRUE)))

# Bind "mean" and "std" variables. The "id" and "activity" variables of tmp_std 
# dataset was excluded (duplicated)
# The total_final dataset was created.

total_final <- bind_cols(tmp_mean, tmp_std[,-c(1,2)])

# 3 - Uses descriptive activity names to name the activities in the data set

# Used recode function of library(car) to recode the numbers of
# activity using the names in activity_label dataset

library(car)
total_final$activity <- recode(total_final$activity, "1 = 'WALKING'; 2 = 'WALKING_UPSTAIRS'; 3 = 'WALKING_DOWNSTAIRS'; 4 = 'SITTING'; 5 = 'STANDING'; 6 = 'LAYING'")

# 4 - Appropriately labels the data set with descriptive variable names.

# storing the labels of total_final dataset in a new df actual_names

actual_names <- as.data.frame(names(total_final))

# export actual_names to .csv file. (reason: This will help to create a new file with descriptive variable names)

write.csv(actual_names, "actual_names.csv")

# Manualy edit new two files in libreoffice calc, then save in .csv files

# first, the codification table (table to relate the old labels with the new ones)
# read this file and store in cod_table dataset

cod_table <- read.csv(codification_of_labels_table.csv, header = TRUE)

# second, separate only the columns with the new labels
# read this file and store in new_names dataset

new_names <- read.csv("new_names.csv", header = TRUE)

# rename the total_final dataset using new_names

for (i in 1:88){ # 88 is the total of variables of total_final dataset
  names(total_final)[i] <- as.character(new_names[i,1])
}

# determining the factors in the total_final dataset

total_final$id_subject <- as.factor(total_final$id_subject)
total_final$type_of_activity <- as.factor(total_final$type_of_activity)

# Finally the tidy dataset is ready.

view(total_final)

# 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Using library "plyr" to help in this part

library(plyr)

# The tidy dataset with the average of each variable for each activity and each subject.

HAR_Smartphones_tidy <- ddply(total_final, .(id_subject, type_of_activity), function(x) colMeans(x[, - (1:2)]))

# writing the tidy dataset to a .txt file "HAR_Smartphones_tidy.txt"

write.table(HAR_Smartphones_tidy, file = "HAR_Smartphones_tidy.txt", row.name = FALSE)

# end of script
