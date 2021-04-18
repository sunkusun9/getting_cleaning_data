# import dplyr and tidyr
library(dplyr)
library(tidyr)

# load activity labels. make a list to mapping activty number to name
df_alabel <- read.table("activity_labels.txt", head=FALSE)
m_label = list()
for(i in 1:nrow(df_alabel)) {m_label[[df_alabel[,1][i]]] = df_alabel[,2][i]}
df_feature <- read.table("features.txt")

# load X and y and then concatenate them into one data frame
load_dataset <- function(X_file, y_file) {
  df_X <- read.table(X_file, head=FALSE)
  df_y <- read.table(y_file, head=FALSE)
  df_X <- mutate(df_X, activity=df_y[, 1])
  df_X
}

# load train set
df_train <- load_dataset("train/X_train.txt", "train/y_train.txt")
# load test set
df_test <- load_dataset("test/X_test.txt", "test/y_test.txt")

# Merge train and test set into a data frame
df_all <- rbind(df_train, df_test)

# Mapping activity numbers to names
df_all <- mutate(df_all, activity=unlist(m_label[activity]))
rm(df_train)
rm(df_test)

# Assign column names
names(df_all)[1:561] <- df_feature[, 2]
featureName <- df_feature[, 2]

# Select only the columns contain "mean()" or "std()"
target_feature <- featureName[grep("mean\\(\\)|std\\(\\)", featureName)]
df_all <- select(df_all, all_of(target_feature), activity)
target_feature <- sub("fBodyBody", "fBody", target_feature)
target_feature <- sub("Mag-mean\\(\\)", "-mean\\(\\)-Mag", target_feature)
target_feature <- sub("Mag-std\\(\\)", "-std\\(\\)-Mag", target_feature)
names(df_all)[1:length(target_feature)] <- target_feature

# Make tidy frame averaging values
df_tidy <- gather(df_all, "subject-type-axis", value, -activity) %>% 
  separate("subject-type-axis", c("subject", "type", "axis")) %>%
  mutate(prefix=substr(subject, 1, 1), subject=substr(subject, 2, length(subject))) %>%
  mutate(feature_name=paste(prefix, axis, type, sep="-")) %>% 
  select(-c(type, axis, prefix)) %>%
  group_by(activity, subject, feature_name) %>% summarise(avg=mean(value))

# Timble to data frame and spread feature_name
df_tidy <- as.data.frame(df_tidy) %>% spread(feature_name, avg)
