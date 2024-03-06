# Load  dataset with  separator ";", a header and empty cells recognized as NAs
data <- read.csv("marketing.csv", sep = ";", header = TRUE, na.strings = "")

# Check the data set
head(data) # Displays the first lines of the data set
str(data) # Outputs a summary of the data structure

# Determine the number of missing values for each variable
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values # Show the number of missing values for each variable


# Relationship between previous = 0 & poutcome = nonexistent
count_previous_0 <- sum(data$previous == 0) # Number previous = 0
count_previous_0

count_poutcome_nonexistent <- sum(data$poutcome == "nonexistent") # Number poutcome ="nonexistent"
count_poutcome_nonexistent

count <- sum(data$poutcome[data$previous == 0] == "nonexistent") # Number previous = 0 & poutcome = "nonexistent" at the same time
count

realtionship_previous_poutcome <- (count_previous_0 == count_poutcome_nonexistent) && +
  (count_previous_0 == count) && (count_poutcome_nonexistent == count)
realtionship_previous_poutcome


# statistical Summary of the data
summary(data)
