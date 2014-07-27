#1# Merges the training and the test sets to create one data set.
# First, unzip the data file
data_file = "getdata_projectfiles_UCI HAR Dataset.zip"
unzip(data_file)

# Load train & test sets
X_train_file = "UCI HAR Dataset/train/X_train.txt"
Y_train_file = "UCI HAR Dataset/train/y_train.txt"
X_test_file = "UCI HAR Dataset/test/X_test.txt"
Y_test_file = "UCI HAR Dataset/test/y_test.txt"
X_train = read.table(X_train_file)
X_test = read.table(X_test_file)
Y_train = read.table(Y_train_file)
Y_test = read.table(Y_test_file)

# Merge data
X = rbind(X_train, X_test)
Y = rbind(Y_train, Y_test)

#2# Extracts only the measurements on the mean and standard deviation
#     for each measurement. 
#4# Appropriately labels the data set with descriptive variable names. 
#  Get the list of features names
X_names_file = "UCI HAR Dataset/features.txt"
X_names = read.table(X_names_file, stringsAsFactors=F)

# Get the features that are either mean or standard deviation
u = grepl("mean\\(|std", X_names[, 2])

# Filter features
X = X[, u]

# Label X features
names(X) = X_names[u, 2]

#3# Uses descriptive activity names to name the activities in the data set
# Get activity names & merge
activity_names_file = "UCI HAR Dataset/activity_labels.txt"
activity_names = read.table(activity_names_file, stringsAsFactors = F)
Y = merge(Y, activity_names)

# Restrict Y to activity name and name Y column
Y = Y[, 2]
names(Y) = "activity"

#5# Creates a second, independent tidy data set with the average
#     of each variable for each activity and each subject. 
# Get subjects
subject_train_file = "UCI HAR Dataset/train/subject_train.txt"
subject_test_file = "UCI HAR Dataset/test/subject_test.txt"
subject_train = read.table(subject_train_file)
subject_test = read.table(subject_test_file)
subject = rbind(subject_train, subject_test)
names(subject) = "subject"

# Average features based on (activity, subject)
flat_average = function (x) {
   m = tapply(x, cbind(Y, subject), mean)
   v = as.numeric(m)
   V = cbind(activity=dimnames(m)[1]$Y, subject=rep(1:30, each=6), value=v)
}
# Z is a list: 1 item is for 1 feature
Z = lapply(X, flat_average)



ZZ = Z[[1]]
str(ZZ)





