## Getting/Cleaning Data Project
library(tidyverse)
library(data.table)

## Load in Uncleaned data

training_dataset <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/train/X_train.txt")
testing_dataset <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/test/X_test.txt")

## Load in feature names
feature_names <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/features.txt")
rm(feature_names); gc()
## Set feature names 
names(training_dataset) <- feature_names$V2
names(testing_dataset) <- feature_names$V2


## Merge Datasets 

merged_data <- rbind(training_dataset,testing_dataset)
rm(training_dataset)
rm(testing_dataset); gc()

## Extract only the mean and standard deviation of each measurement
b = grep("std()|mean()",names(merged_data))

extracted_data <- merged_data[,b]
rm(merged_data); rm(b); gc()

## Load lables 
training_labels <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/train/y_train.txt")
testing_labels <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/test/y_test.txt")

## Merge lables
full_lables <- rbind(training_labels,testing_labels)

## Rename lables with descriptive names
full_lables <- sapply(full_lables, function(x) case_when(
                      x == 1 ~ 'Walking',
                      x == 2 ~ "Walking Upstairs",
                      x == 3 ~ "Walking Downstairs",
                      x == 4 ~ "Sitting",
                      x == 5 ~  "Standing",
                      x == 6 ~ "Laying"))


## Attach lables to dataset 
extracted_data_with_labels <- cbind(full_lables,extracted_data)
colnames(extracted_data_with_labels)[1] <- c('Activity_Label')
rm(testing_labels); rm(training_labels); rm(full_lables);gc()

## Load subjects 
training_subjects <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/train/subject_train.txt")
testing_subjects <- read.table("C:/Users/HP/Documents/UCI HAR Dataset/test/subject_test.txt")

## Combine subjects 
full_subjects <- rbind(training_subjects,testing_subjects)
rm(training_subjects);rm(testing_subjects);gc()

## Attach Subject to dataset
ending_tidy_set <- cbind(full_subjects,extracted_data_with_labels)
colnames(ending_tidy_set)[1] <- 'Subject_Number'
rm(full_subjects);rm(extracted_data);rm(extracted_data_with_labels); gc()

## Create Summary dataset
final_summary <- ending_tidy_set %>% group_by(Subject_Number,Activity_Label) %>%
    summarise_all(mean)

## Rename Final Columns 
final_names <- sapply(names(ending_tidy_set), function(x) (
  paste('Average',x,sep = " ")))
names(final_summary) <- final_names
rm(final_names); gc()