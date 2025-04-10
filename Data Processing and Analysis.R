# install and load package haven to read data in.dta format
install.packages("haven")
library(haven)
data_diary <- read_dta("enut2021_diario.dta") # reading diary data
# chnage column names to enhance readability
colnames(data_diary)[which(names(data_diary) == "actividad_1")] <- "activity_1"
colnames(data_diary)[which(names(data_diary) == "actividad_2")] <- "activity_2"
colnames(data_diary)[which(names(data_diary) == "actividad_3")] <- "activity_3"
install.packages("dplyr")
library(dplyr)
#insert slots to divide 24 hr time of an individual (id) into 144 slots of 10 mins each
data_diary <- data_diary %>%
  group_by(id) %>%
  mutate(Slot = rep(1:144, length.out = n()))


#Data Cleaning
# Replace 999 with NA in activity columns (activity_1, activity_2, activity_3)
data_diary$activity_1[data_diary$activity_1 == 999] <- NA
data_diary$activity_2[data_diary$activity_2 == 999] <- NA
data_diary$activity_3[data_diary$activity_3 == 999] <- NA
#check for missing values in activity_1
sum(is.na(data_diary$activity_1)) # got 1399
#check slot assignment to ensure there are 144 slots for each individual (id)
data_diary %>% 
  filter(id == 138453) # Checking for any random individual ID
# to check 14350 individual with unique ids and 144 slots each to make 2066400 obs total
unique_ids <- length(unique(data_diary$id)) # Check the number of unique individuals (IDs)
# Check the number of slots for each individual
slots_per_individual <- data_diary %>%
  group_by(id) %>%
  summarise(slots = n())
valid_individuals <- sum(slots_per_individual$slots == 144) # Check if every individual has 144 slots
total_observations <- nrow(data_diary) # Total number of observations
# Print the results
cat("Unique IDs:", unique_ids, "\n")
cat("Individuals with 144 slots:", valid_individuals, "\n")
cat("Total observations:", total_observations, "\n")
# Check the ID of the 144th and 145th rows
id_144 <- data_diary$id[144]
id_145 <- data_diary$id[145]
# Print the results
cat("ID of row 144:", id_144, "\n")
cat("ID of row 145:", id_145, "\n")


# sorting data
# Count how many unique IDs are in each set of 144 slots
unique_ids_per_individual <- data_diary %>%
  group_by(id) %>%
  summarise(slots_count = n())
# Check if every individual has exactly 144 slots
invalid_individuals <- unique_ids_per_individual %>%
  filter(slots_count != 144)
invalid_individuals # Show the invalid IDs
# Sort data by id and slot to ensure correct order
sorted_data <- data_diary %>%
  arrange(id, Slot)
# Check the ids of the 144th and 145th rows in the sorted data
id_144_sorted <- sorted_data$id[144]
id_145_sorted <- sorted_data$id[145]
# Print the results
cat("ID of row 144 (sorted):", id_144_sorted, "\n")
cat("ID of row 145 (sorted):", id_145_sorted, "\n")
# Rename column 'Slot' to 'slot'
data_diary <- data_diary %>%
  rename(slot = Slot)
colnames(data_diary) # Check the column names to confirm
data_diary$id <- as.character(data_diary$id) # Convert 'id' to character
# Sort data by 'id' and 'slot'
sorted_data <- data_diary %>%
  arrange(id, slot)
# Check the IDs of row 144 and row 145 in the sorted data
id_144_sorted <- sorted_data$id[144]
id_145_sorted <- sorted_data$id[145]
# Print the results
cat("ID of row 144 (sorted):", id_144_sorted, "\n")
cat("ID of row 145 (sorted):", id_145_sorted, "\n")
#performing random checks to ensure data consistency
# Check id and slot values for rows 287, 288, and 289
rows_to_check <- sorted_data[c(287, 288, 289), c("id", "slot")]
print(rows_to_check) # Print the results


# checking and removing missing data
# Rename your sorted data to sorted_diary
sorted_diary <- sorted_data
# Check for missing values in all three activity columns (including dots and 999)
missing_activity_check_all <- sorted_diary %>%
  filter(
    (activity_1 == 999 | is.na(activity_1) | activity_1 == "") & 
      (activity_2 == 999 | is.na(activity_2) | activity_2 == "") & 
      (activity_3 == 999 | is.na(activity_3) | activity_3 == "")
  )

# View the result
missing_activity_check_all # i got 396 rows
# Find the unique IDs that have missing activity data in all 3 activity columns
missing_activity_ids <- missing_activity_check_all %>%
  distinct(id)
# View the IDs
missing_activity_ids # got 18 ids
# removing those 18 individulas
# Remove all 144 rows for the 18 individuals with missing activities in all 3 columns
sorted_diary_cleaned <- sorted_diary %>%
  filter(!id %in% missing_activity_ids$id)
# Check the cleaned dataset
head(sorted_diary_cleaned)
# Check the number of rows in the cleaned data
nrow(sorted_diary_cleaned) #perfectly matches (2066400-(18*144))


# reshaping data to create diary form

library(dplyr)
# Drop columns "column1", "column2", "column3"
sorted_diary_cleaned <- sorted_diary_cleaned %>%
  select(-actividad_hora, -actividad_minuto, -n_fila)

install.packages("tidyverse")
library(tidyverse)

# Convert the data to long format (one row per activity per slot)
long_data <- sorted_diary_cleaned %>%
  pivot_longer(cols = starts_with("activity"), 
               names_to = "activity_slot", 
               values_to = "activity_code") %>%
  filter(!is.na(activity_code))  # Remove rows where activity_code is NA (no activity)

# Create a new column to count the number of activities performed in each slot
long_data <- long_data %>%
  group_by(id, slot) %>%
  mutate(activity_count = n()) %>%  # Count the number of activities for each slot, to account for simultaneous
  ungroup() %>%                     # activities and divide 10 mins into into 3.33, 5 or 10 mins slot
  mutate(time_spent = case_when(    # depending on number of cativities performed in a slot
    activity_count == 1 ~ 10,  # 1 activity: 10 minutes
    activity_count == 2 ~ 5,   # 2 activities: 5 minutes each
    activity_count == 3 ~ 3.33 # 3 activities: 3.33 minutes each
  ))

#To categorise different activities under broad headings
activity_data <- read.csv("activity_data.csv") # Load the lookup table (activity_data.csv)
# Ensure activity_code in both dataframes are of the same type (character)
activity_data$activity_code <- as.character(activity_data$activity_code)
long_data$activity_code <- as.character(long_data$activity_code)
# Add categories from the lookup table to the long data
long_data <- long_data %>%
  left_join(activity_data, by = "activity_code")
print(long_data[200000:200050, ], n = 50)

write_dta(sorted_diary_cleaned, "sorted_diary_cleaned.dta") #save sorted, cleaned data
write_dta(long_data, "long_data.dta") #save long_data

# We first group by 'id' (individual) and then sum the total time spent to
#check if each individual's time equals 24*60 mins
total_time_per_individual <- long_data %>%
  group_by(id) %>%
  summarise(total_time = sum(time_spent, na.rm = TRUE))
# View summary statistics for total time spent (should be 1440 for each individual)
summary_statistics <- total_time_per_individual %>%
  summarise(
    min_time = min(total_time),
    max_time = max(total_time),
    mean_time = mean(total_time),
    median_time = median(total_time),
    sd_time = sd(total_time)
  )
print(summary_statistics) # View summary statistics
# Filter rows where activity_count is 3
long_data_3_activities <- long_data %>% 
  filter(activity_count == 3)
# View the first few rows to check
head(long_data_3_activities)

#Ensuring data is well-structured
# List all unique categories
unique_categories <- long_data %>%
  distinct(category) %>%
  pull(category)
print(unique_categories)
# Remove extra whitespace from category names
long_data <- long_data %>%
  mutate(category = str_trim(category))
# Now list unique categories again
unique_categories <- long_data %>%
  distinct(category) %>%
  pull(category)
print(unique_categories)
# Overwrite the existing file
write_dta(long_data, "long_data.dta")
# Checking the no. of individuals in data; should be 14332 (original 14350-18 removed=14332); Matches 14332
# Count unique IDs
num_unique_ids <- long_data %>%
  summarise(n_individuals = n_distinct(id))
print(num_unique_ids)


# Calculate avg time spent by grouping categories
# Calculate total time per category and average time per individual
avg_time_per_category <- long_data %>%
  group_by(category) %>%
  summarise(total_time = sum(time_spent, na.rm = TRUE)) %>%
  mutate(avg_time = total_time / 14332)
avg_time_per_category # View the results
# Create a new data frame with 'category' and 'avg_time'
avg_time_data <- tibble(
  category = c("childcare", "domestic work", "education", "leisure", "paid work", "unpaid work"),
  avg_time = c(43.9, 171, 33.9, 982.9, 187, 21.7)  # Replace these values with your calculated values
)
# Save it as a CSV file
write.csv(avg_time_data, "avg_time_data.csv", row.names = FALSE)


# Remove all objects except long_data
rm(list = setdiff(ls(), "long_data"))
# Perform garbage collection
gc()


#loading base data to add demographic info like age, gender to diary data

# Load the haven package
library(haven)
# Load the .dta file, selecting only the required columns
base_data <- read_dta("enut2021_base.dta", col_select = c("id", "sexo_sel", "edad_sel", "bhch07_sel"))
# View the first few rows of the dataset to confirm the selected columns
head(base_data)
# Load the dplyr package for renaming
library(dplyr)
# Rename columns for clarity
base_data <- base_data %>%
  rename(
    sex = sexo_sel,
    age = edad_sel,
    marital_status = bhch07_sel
  )
# View the updated dataset
head(base_data)


#data cleaning of base_data
summary(base_data)
# Check if 99 is present in 'age', 'sex', or 'marital_status'
summary(base_data)
#replace 99 with NA (null) for clarity
base_data$age[base_data$age == 99] <- NA
base_data$sex[base_data$sex == 99] <- NA
base_data$marital_status[base_data$marital_status == 99] <- NA
# After replacement, check if there are any NAs in the columns
any(is.na(base_data$age))
any(is.na(base_data$sex))
any(is.na(base_data$marital_status)) # marital status has null values(40 null values)
summary(base_data) #40 NAs in marital status


#merging

# Check data type of id in both datasets
str(long_data$id)  # Check the data type of 'id' in long_data
str(base_data$id)  # Check the data type of 'id' in base_data
# Convert id to character  (for consistency)
base_data$id <- as.character(base_data$id)
# Now merge the datasets based on 'id'
merged_data <- left_join(long_data, base_data, by = "id")
# View the merged data to ensure the information from base_data is added to long_data
head(merged_data)
write_dta(merged_data, "merged_data.dta")


#analysis by gender, age, marital status

library(dplyr) # Load necessary libraries

# Compute the average time spent per category by sex
avg_time_by_sex <- merged_data %>%
  group_by(sex, category) %>%               # Group by sex (gender) and category
  summarise(total_time = sum(time_spent, na.rm = TRUE),  # Total time spent for each group
            avg_time = total_time / n_distinct(id)) %>%  # Divide total time by number of unique ids
  ungroup()                                   # Remove grouping for further operations
print(avg_time_by_sex)

# Save the data to a CSV file
write.csv(avg_time_by_sex, "avg_time_by_sex.csv", row.names = FALSE)


# by marital status

# Exclude NA values for marital status
merged_data_no_na <- merged_data %>%
  filter(!is.na(marital_status))

# Compute total time and average time by marital status (excluding NAs)
avg_time_by_marital_status <- merged_data_no_na %>%
  group_by(marital_status, category) %>%
  summarise(
    total_time = sum(time_spent, na.rm = TRUE),
    avg_time = total_time / n_distinct(id)
  ) %>%
  ungroup()
print(avg_time_by_marital_status)

# save the result to CSV
write.csv(avg_time_by_marital_status, "avg_time_by_marital_status.csv", row.names = FALSE)

# by age group

# Create age groups based on the new ranges in merged_data
merged_data <- merged_data %>%
  mutate(age_group = case_when(
    age >= 14 & age <= 24 ~ "14-24: Young Adults",
    age >= 25 & age <= 40 ~ "25-40: Early Adults",
    age >= 41 & age <= 65 ~ "41-65: Middle-aged Adults",
    age >= 65 ~ "65+: Seniors",
    TRUE ~ "NA"
  ))
# Check the distribution of age groups
table(merged_data$age_group)
# Calculate total time and average time by age group and category
avg_time_by_age <- merged_data %>%
  filter(!is.na(age_group)) %>%  # Exclude any rows with NA age_group
  group_by(age_group, category) %>%
  summarise(total_time = sum(time_spent, na.rm = TRUE),
            n_individuals = n_distinct(id),  # Count unique individuals
            avg_time = total_time / n_individuals) %>%
  ungroup()
print(avg_time_by_age)
# Drop the n_individuals column from avg_time_by_age
avg_time_by_age <- avg_time_by_age %>%
  select(-n_individuals)
head(avg_time_by_age) # view updated dataset
# Save avg_time_by_age as a CSV file
write.csv(avg_time_by_age, "avg_time_by_age.csv", row.names = FALSE)


# visualization

# Load necessary library
library(ggplot2)
library(readr)
# Read the CSV file
avg_time_data <- read.csv("avg_time_data.csv")
# View the first few rows to check if the data is loaded correctly
head(avg_time_data)

#create bar graph
ggplot(avg_time_data, aes(x = category, y = avg_time)) +
  +     geom_bar(stat = "identity", fill = "skyblue", color = "blue") +  # Shades of blue
  +     geom_text(aes(label = round(avg_time, 1)), vjust = -0.5, color = "black") +  # Add labels on top of bars
  +     labs(title = "Average Time Per Day in Minutes by Category", 
             +          x = "Category", 
             +          y = "Average Time Per Day (Minutes)") +  # Axis labels
  +     theme_minimal() +
  +     theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# plot by sex
avg_time_by_sex <- read.csv("avg_time_by_sex.csv")
library(ggplot2)
ggplot(avg_time_by_sex, aes(x = category, y = avg_time_hrs, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(avg_time_hrs, 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#1f77b4", "#aec7e8")) +  # Shades of blue
  labs(title = "Average Time per Day in Hours by Category and Sex",
       x = "Category",
       y = "Average Time (Hours)",
       fill = "Sex") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot by marital status
avg_time_by_marital_status <- read.csv("avg_time_by_marital_status.csv")
head(avg_time_by_marital_status)
#facet grid
ggplot(avg_time_by_marital_status, aes(x = category, y = avg_time_hrs, fill = marital_status)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(avg_time_hrs, 2)), 
            vjust = -0.3, size = 3) +
  facet_wrap(~ marital_status) +  # Create separate plots by marital status
  labs(title = "Average Time per Day (in Hours) by Marital Status and Category",
       x = "Category", 
       y = "Average Time (hours)") +
  scale_fill_brewer(palette = "Set2") +  # Softer color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# plot by age grp
avg_time_by_age <- read.csv("avg_time_by_age.csv")
ggplot(avg_time_by_age, aes(x = category, y = avg_time_hrs, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar chart with categories side by side
  geom_text(aes(label = round(avg_time_hrs, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) +  # Adding bar height labels
  labs(
    title = "Avg Time in Hrs per Day by Category and Age Group",
    x = "Category",
    y = "Average Time (hours)"
  ) +
  theme_minimal() +  #  makes the plot cleaner
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed































