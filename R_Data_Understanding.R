# Load  dataset with  separator ";", a header and empty cells recognized as NAs
data <- read.csv("marketing.csv", sep = ";", header = TRUE, na.strings = "")

# Check the data set------------------------------------------------------------
head(data) # Displays the first lines of the data set
str(data) # Outputs a summary of the data structure
nrow(data) # cross checking number or rows

# Determine the number of missing values for each variable----------------------
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values # Show the number of missing values for each variable


# Relationship between previous = 0 & poutcome = nonexistent--------------------
# Number of previous = 0
count_previous_0 <- sum(data$previous == 0) 
count_previous_0

# Number of poutcome ="nonexistent"
count_poutcome_nonexistent <- sum(data$poutcome == "nonexistent") 
count_poutcome_nonexistent

# Number of previous = 0 & poutcome = "nonexistent" at the same time
count <- sum(data$poutcome[data$previous == 0] == "nonexistent") 
count

# check if relationship is true (all numbers are equal)
realtionship <- (count_previous_0 == count_poutcome_nonexistent) && +
  (count_previous_0 == count) && (count_poutcome_nonexistent == count)
realtionship


# statistical analysis of the data----------------------------------------------
# statistical Summary of numeric variables
summary_numeric <- summary(data[, sapply(data, is.numeric)]) 
summary_numeric

# standard deviation of of numeric variables
sd_numeric <- sapply(data[, sapply(data, is.numeric)], sd)
sd_numeric

# Frequency of the individual values for discrete numeric variables (integers)
frequency_int_data <- lapply(data[, sapply(data, is.integer)], table)
frequency_int_data

# Unique values for discrete numeric variables (integers)
unique_int_values <- sapply(data[, sapply(data, is.integer)], unique)
unique_int_values

# Frequency of individual categories for non-numeric variables including NA
frequency_data <- lapply(data[, sapply(data, is.character)], function(x) table(x, useNA = "always"))
frequency_data

# Number of unique values for non-numeric variables
unique_data <- sapply(data[, sapply(data, is.character)], function(x) length(unique(x)))
unique_data




