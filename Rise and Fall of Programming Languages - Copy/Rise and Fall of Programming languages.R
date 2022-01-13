# Load libraries
# .... YOUR CODE FOR TASK 1 ....
# .... YOUR CODE FOR TASK 1 ....
library(readr)
library(dplyr)

# Load dataset
by_tag_year <- read_csv("C:/Users/KIIT/Desktop/Data Analytics/Rise and Fall of Programming Languages - Copy/Rise and Fall of Programming Languages - Copy/datasets/by_tag_year.csv")

# Inspect the dataset
# .... YOUR CODE FOR TASK 1 ....
print(by_tag_year)

# Add fraction column
by_tag_year_fraction <-  by_tag_year %>%
  mutate(fraction = number / year_total)

# Print the new table
# .... YOUR CODE FOR TASK 2 ....
print(by_tag_year_fraction)


# Load ggplot2
# .... YOUR CODE FOR TASK 4 ....
library(ggplot2)

# Create a line plot of fraction over time
# .... YOUR CODE FOR TASK 4 ....


# Find total number of questions for each tag
sorted_tags <- by_tag_year %>%
  group_by(tag) %>%
  summarize(tag_total = sum(number)) %>%
  arrange(desc(tag_total))
# .... YOUR CODE FOR TASK 6 ....

# Print the new table
# .... YOUR CODE FOR TASK 6
print(sorted_tags)

# Get the six largest tags
highest_tags <- head(sorted_tags$tag)

# Filter for the six largest tags
by_tag_subset <- by_tag_year_fraction %>%
  filter(tag %in% highest_tags)

# Plot tags over time on a line plot using color to represent tag
# .... YOUR CODE FOR TASK 7 ....
ggplot(by_tag_subset, aes(x = year,y = fraction,color = tag)) +geom_line()

# Get tags of interest
my_tags <- c("android", "ios", "windows-phone")

# Filter for those tags
by_tag_subset <- by_tag_year_fraction %>%
  filter(tag %in% my_tags)

# Plot tags over time on a line plot using color to represent tag
# .... YOUR CODE FOR TASK 8 ....
ggplot(by_tag_subset, aes(x = year,y = fraction,color = tag)) +geom_line()

# Filter for R tags
r_over_time <- by_tag_year_fraction %>%
  filter(tag == "r")

# Print the new table
# .... YOUR CODE FOR TASK 3 ....
print(r_over_time)
ggplot(r_over_time) +
  geom_line(aes(x = year, y = fraction))

# A vector of selected tags
selected_tags <- c("r", "dplyr", "ggplot2")

# Filter for those tags
selected_tags_over_time <- by_tag_year_fraction %>%
  filter(tag %in% selected_tags)

# Plot tags over time on a line plot using color to represent tag
# .... YOUR CODE FOR TASK 5 ....


ggplot(selected_tags_over_time, aes(x = year,y = fraction,color = tag)) + geom_line()



