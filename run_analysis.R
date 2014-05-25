# 2014-05-25, abungsy
----------------------------------------------------------------------------------------------------------------------------------
# DOWNLOAD AND UNZIP DATA 
----------------------------------------------------------------------------------------------------------------------------------  
#Create a temp. file name
temp <- tempfile()
#Use download.file() to fetch the file into the temp. file
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
#Use unzip() to extract the target file from temp. file, overwriting the existing, zipped file. Place the extracted data in "extracted" folder
unzip(temp, exdir = "extracted", overwrite = TRUE)

----------------------------------------------------------------------------------------------------------------------------------
# LOAD AND MERGE DATA 
---------------------------------------------------------------------------------------------------------------------------------- 
current_directory <- getwd()  #register current working directory

# LOAD FEATURE NAMES (test_x and train_x)
features <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/features.txt",sep="/"))

# LOAD TEST DATA
test_x <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/test/X_test.txt",sep="/"))
names(test_x) <- features$V2  #apply column names to test_x
test_y <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/test/y_test.txt",sep="/"))
test_subject <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/test/subject_test.txt",sep="/"))
# Combine columns of test_x and test_y
test_x$labels <- test_y$V1
# Combine columns of test_x and subject data
test_x$subject <- test_subject$V1

# LOAD TRAIN DATA
train_x <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/train/X_train.txt",sep="/"))
names(train_x) <- features$V2  #apply column names to train_x
train_y <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/train/y_train.txt",sep="/"))
train_subject <- read.table(paste(current_directory,"extracted/UCI HAR Dataset/train/subject_train.txt",sep="/"))
# Combine columns of test_x and test_y
train_x$labels <- train_y$V1
# Combine columns of test_x and subject
train_x$subject <- train_subject$V1 

# MERGE TEST AND TRAIN DATA 
install.packages("plyr");
library(plyr);
to_join = list(train_x,test_x);
merged_data = join_all(to_join);

----------------------------------------------------------------------------------------------------------------------------------
# EXTRACT ONLY THE MEASUREMENTS ON THE MEAN AND STDEV FOR EACH MEASUREMENT
---------------------------------------------------------------------------------------------------------------------------------- 
myfields<- grep("mean|std|labels|subject",names(merged_data),value = TRUE)
new_data <- merged_data[,myfields]
----------------------------------------------------------------------------------------------------------------------------------
# USE DESCRITIVE NAMES FOR ACTIVITIES
---------------------------------------------------------------------------------------------------------------------------------- 
# Create a function called desc_labels that gives descriptive names to activity label's
desc_labels <- function(x)
  {
    if(x==1){
      y <- "WALKING"
        } else if(x==2){
          y <- "WALKING_UPSTAIRS"
              } else if(x==3){
                  y <- "WALKING_DOWNSTAIRS"
                    } else if(x==4){
                      y <- "SITTING" 
                        } else if(x==5){
                            y <- "STANDING"
                              } else if(x==6){
                                y <- "LAYING"   
                                  }
  y }


new_data$labels <- sapply(new_data$labels,desc_labels) # apply the function to each row
#View(new_data)
----------------------------------------------------------------------------------------------------------------------------------
# CLEAN UP COLUMN NAMES
---------------------------------------------------------------------------------------------------------------------------------- 
tolower(names(new_data)) # chance all names to lowercase
names(new_data) <- sapply(names(new_data),gsub,pattern= "-",replacement="_") # replace "-" with underscore
names(new_data) <- sapply(names(new_data),gsub,pattern= "()",replacement="") # replace "-" with nothing
---------------------------------------------------------------------------------------------------------------------------------- 
# SUMMARY
summary(new_data)
---------------------------------------------------------------------------------------------------------------------------------- 
# OUTPUT
tidy_data <- new_data
write.table(tidy_data,"tidy_data.txt",sep="/t")

  

