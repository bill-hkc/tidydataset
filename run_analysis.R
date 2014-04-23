library(plyr)

# set working dir to the dataset directory
# setwd("E:\\UCI HAR Dataset")

# max number of rows to read
# adjust this number to a smaller number for debug
# or to a large number for import whole dataset
nrows = 10000

# read the list of feature names
features <- read.table("features.txt")

# select feature names with std() or mean()
# note meanFreq() is not selected
column.select <- grepl("(mean|std)\\(\\)",(features$V2))

# unused columns are set to NULL, to skip reading them
colClasses <- rep("NULL",length(column.select))
colClasses[column.select] <- "numeric"

# extract the names of selected features, and remove punct characters around
names.select <- features$V2[column.select]
names.select <- sub("[[:punct:]]+mean[[:punct:]]+", "Mean", names.select)
names.select <- sub("[[:punct:]]+std[[:punct:]]+", "Std", names.select)

# fix the "BodyBody" in feature names
names.select <- sub("BodyBody", "Body", names.select)

# read data files in a dir, combine them to a data frame, 
# and only return selected columns
read.dir.data <- function(name) {
  y <- read.table(paste0(name,"/y_",name,".txt"), nrows=nrows)
  x <- read.table(paste0(name,"/X_",name,".txt"), nrows=nrows, colClasses=colClasses)
  subject <- read.table(paste0(name,"/subject_",name,".txt"), nrows=nrows)

  df <- data.frame(y, subject, x)
  names(df) <- c("activity","subject",names.select)
  df
}

# read the data from two directories, "train", and "test"
# the output is a list of data.frames
df.list <- lapply(c("train","test"),read.dir.data)
# combine the list of data.frames into single data.frame
df <- do.call(rbind,df.list)

# read the name of activies
activity_labels <- read.table("activity_labels.txt")
# and map the activity column from integers to meaningful names
df$activity <- factor(df$activity, levels=activity_labels$V1, labels=activity_labels$V2)


#split the data frame by (subject,activitiy) combinatins, 
# then calculate means of other variables to form the new tidy data set
df.tidy <- ddply(df, .(activity, subject), function(x){colMeans(x[c(-1,-2)])})

# save tidy dataset to file
write.table(df.tidy,file="tidy_dataset.txt",row.names=F, quote=F)




