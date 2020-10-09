setwd("C:/Users/USUARIO/Desktop/HelloYu/Work3/UCI HAR Dataset") ## Se configura el escritorio de trabajo donde están los datos
library(plyr) ## Cargamos el paquete de plyr que permite usar funciones como: arrange, count, mutate, summarize,
library(data.table) ## Cargamos la librería de data.table para manipular nuestros datos

## Leemos los datos correspondientes a train
subTrain = read.table('./train/subject_train.txt',header=FALSE) ## Leemos la base de datos de subject_train
xT = read.table('./train/x_train.txt',header=FALSE) ## Leemos los datos de X_train
yT = read.table('./train/y_train.txt',header=FALSE) ## Leemos los datos de y_train

##Leemos los datos correspondientes a test

subTest = read.table('./test/subject_test.txt',header=FALSE) ## Leemos los datos de Subject_test
xTe = read.table('./test/x_test.txt',header=FALSE) ## Leemos los datos de x_test
yTe = read.table('./test/y_test.txt',header=FALSE) ## Leemos los datos de y_test

## 1. Merges the training and the test sets to create one data set.

xDS <- rbind(xT, xTe) ## x dataset
yDS <- rbind(yT, yTe) ## y dataset
subDataSet <- rbind(subTrain, subTest) # subject dataset

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
xDS_mean_std <- xDS[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]
names(xDS_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(xDS_mean_std)
dim(xDS_mean_std)

## 3. Uses descriptive activity names to name the activities in the data set

yDS[, 1] <- read.table("activity_labels.txt")[yDS[, 1], 2]
names(yDS) <- "Activity"
View(yDS)

## 4. Appropriately labels the data set with descriptive variable names.

names(subDataSet) <- "Subject"
summary(subDataSet)

OneDataSet <- cbind(xDS_mean_std, yDS, subDataSet)

# Defining descriptive names for all variables.
names(OneDataSet) <- make.names(names(OneDataSet))
names(OneDataSet) <- gsub('Acc',"Acceleration",names(OneDataSet))
names(OneDataSet) <- gsub('GyroJerk',"AngularAcceleration",names(OneDataSet))
names(OneDataSet) <- gsub('Gyro',"AngularSpeed",names(OneDataSet))
names(OneDataSet) <- gsub('Mag',"Magnitude",names(OneDataSet))
names(OneDataSet) <- gsub('^t',"TimeDomain.",names(OneDataSet))
names(OneDataSet) <- gsub('^f',"FrequencyDomain.",names(OneDataSet))
names(OneDataSet) <- gsub('\\.mean',".Mean",names(OneDataSet))
names(OneDataSet) <- gsub('\\.std',".StandardDeviation",names(OneDataSet))
names(OneDataSet) <- gsub('Freq\\.',"Frequency.",names(OneDataSet))
names(OneDataSet) <- gsub('Freq$',"Frequency",names(OneDataSet))

View(OneDataSet)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

names(OneDataSet)

D2<-aggregate(. ~Subject + Activity, OneDataSet, mean)
D2<-D2[order(D2$Subject,D2$Activity),]
write.table(D2, file = "tidydata.txt",row.name=FALSE)
