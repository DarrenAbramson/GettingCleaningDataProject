library(plyr)
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

features <- read.table("./UCI HAR Dataset/features.txt")


subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")

## dim confirms all of these have 2947 rows.
## Therefore, these can be column-binded.
dim(subjectTest)
dim(yTest)
dim(Xtest)

## The order of cbinding will result in subject ID, followed by
## activity type and then data.
mergedTestData <- cbind(subjectTest, yTest, Xtest)

subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")

## dim confirms all of these have 7352 rows.
## Therefore, these can be column-binded.
dim(subjectTrain)
dim(yTrain)
dim(Xtrain)


## The order of cbinding is as above.
mergedTrainData <- cbind(subjectTrain, yTrain, Xtrain)

## Now we can merge all of the data vertically using rbind.
mergedData <- rbind(mergedTestData, mergedTrainData)

## We can confirm the correct dimensions of the merged data.
dim(mergedData)

## Now we select out the columns we want.

## First, we make a logical vector that corresponds to those rows in the second
## column of the features table which contains a "mean()" or "std()". 
## By using the escape characters to match left and right parentheses, we guarantee
## that these are features for which mean and std functions were *calculated*.
featuresLogicalVector <- with(features, grepl("mean\\(\\)", features$V2) | grepl("std\\(\\)", features$V2))

## We need to pad this vector with two "TRUE"s so as to keep the ID and activity.
paddingColumns <- c(TRUE, TRUE)
columnsToKeep <- c(paddingColumns, featuresLogicalVector)

## Now we can subset out the columns we want.
## We keep all rows, and only columns with mean and standard deviation data.
averagesAndStds <- mergedData[,columnsToKeep]

## Next, the second column which denotes activities is substituted according to the activityLabels table.
## This is done using the "match" operator.
m <- match(averagesAndStds$V1.1, activityLabels$V1)
averagesAndStds$V1.1 <- activityLabels[m,2]

## Now we give the data set's variables meaningful names, corresponding to the 
## features in the original data set.
columnNames <- c("ID", "Activity", as.character(features$V2[featuresLogicalVector]))
colnames(averagesAndStds) <- columnNames

## Finally, we produce a tidy data set with 180 rows and 68 columns.
## The columns include Activity, ID, and 66 features, each with the mean
## for that Activity, ID and feature. This corresponds to a tidy format
## as described in the assigned paper.

tidyData <- aggregate(.~Activity + ID, data=averagesAndStds, FUN=mean)
dim(tidyData)

write.table(tidyData, "tidyData.txt")






