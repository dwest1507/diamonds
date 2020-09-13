# The purpose of this script is to create the data set from the 
# ggplot2 package. A random sample of 80% of the data will create
# the training data. The remainder will be the test data.

# Saving the diamonds data as an object: data
data <- ggplot2::diamonds

# Reordering color so it's from worst to best. Just like all the other ordered features.
data <- data %>% 
  mutate(
    color = ordered(color,
                    levels = c(
                      "J",
                      "I",
                      "H",
                      "G",
                      "F",
                      "E",
                      "D"
                    )))

# Setting seed for reproducibility
set.seed(123)
# Creating a random sample index
train_index <- sample(1:nrow(data), round(nrow(data)*0.8, digits = 0))

# Subsetting the data based on the random index
train <- data[train_index,]
test <- data[-train_index,]

# Removing the index object
rm(train_index)