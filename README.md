# Getting and Cleaning Data Course Project

Explaination run_analysis.R


## 0. Preparation
* Download data set and extract them into project folder.
* set working directory: project folder
* install package: dplyr, tidyr
```R
install.packages("dplyr")
install.packages("tidyr")
```
```R
source('run_analysis.R')
```

## 1. import libraries: dplyr and tidyr

```R
library(dplyr)
library(tidyr)
```

## 2. load activity labels. 

Make a list to mapping activty number to name
```R
df_alabel <- read.table("activity_labels.txt", head=FALSE)
m_label = list()
for(i in 1:nrow(df_alabel)) {m_label[[df_alabel[,1][i]]] = df_alabel[,2][i]}
df_feature <- read.table("features.txt")
```

## 3. Declare the function to load X and y and then concatenate them into one data frame
load_dataset <- function(X_file, y_file) {
  df_X <- read.table(X_file, head=FALSE)
  df_y <- read.table(y_file, head=FALSE)
  df_X <- mutate(df_X, activity=df_y[, 1])
  df_X
}

## 4. load train set and test set then merge them
```R
df_train <- load_dataset("train/X_train.txt", "train/y_train.txt")
df_test <- load_dataset("test/X_test.txt", "test/y_test.txt")
df_all <- rbind(df_train, df_test)
```

## 5. Mapping activity numbers to names and assign column names

```R
df_all <- mutate(df_all, activity=unlist(m_label[activity]))
rm(df_train)
rm(df_test)
```

 Assign column names
```R
names(df_all)[1:561] <- df_feature[, 2]
featureName <- df_feature[, 2]
```

Select only the columns contain "mean()" or "std()" and rename them to transform the dataset to tidy form easily

* 'fBodyBody' is misspelled, it should be replaced 'fBody'
* Mag is same level postfix with -X, -Y, -Z. So replace them -Mag like -X, -Y, -Z

```R
target_feature <- featureName[grep("mean\\(\\)|std\\(\\)", featureName)]
df_all <- select(df_all, all_of(target_feature), activity)
target_feature <- sub("fBodyBody", "fBody", target_feature)
target_feature <- sub("Mag-mean\\(\\)", "-mean\\(\\)-Mag", target_feature)
target_feature <- sub("Mag-std\\(\\)", "-std\\(\\)-Mag", target_feature)
names(df_all)[1:length(target_feature)] <- target_feature
```

## 6. Make tidy frame averaging values

* gather subject-type-axis and separate them, to reform the feature names
* t stands for time, f stands for frequency. cut them from subject
* pasting prefix, axis and type and assign it to feature_name
* averaging the value grouping by activity, subject and feature_name

```R
df_tidy <- gather(df_all, "subject-type-axis", value, -activity) %>% 
  separate("subject-type-axis", c("subject", "type", "axis")) %>%
  mutate(prefix=substr(subject, 1, 1), subject=substr(subject, 2, length(subject))) %>%
  mutate(feature_name=paste(prefix, axis, type, sep="-")) %>% 
  select(-c(type, axis, prefix)) %>%
  group_by(activity, subject, feature_name) %>% summarise(avg=mean(value))
```

* Timble to data frame and spread feature-name
```R
df_tidy <- as.data.frame(df_tidy) %>% spread(feature_name, avg)
```
