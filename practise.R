library(sleep_health_and_lifestyle_dataset)
View(Sleep_health_and_lifestyle_dataset)
head(Sleep_health_and_lifestyle_dataset)
str(Sleep_health_and_lifestyle_dataset)
summary(Sleep_health_and_lifestyle_dataset)
colSums(is.na(Sleep_health_and_lifestyle_dataset))
anyDuplicated(Sleep_health_and_lifestyle_dataset)
df=unique(Sleep_health_and_lifestyle_dataset)


library(dplyr)
df <- df %>%
  mutate(BMI.Category = ifelse(BMI.Category == "Normal Weight", "Normal", BMI.Category))
# Replace Column1, Column2 with the names of the columns to drop
df <- df %>% select(-c(Person.ID))

library(tidyr)

df <- df %>%
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/")

write.csv(df, file = "df.csv", row.names = FALSE)


levels(df$Occupation)

levels(df$Gender)
levels(df$BMI.Category)
levels(df$Sleep.Disorder)

df$Occupation_numeric <- as.numeric(df$Occupation)
df$Gender_Male<- as.numeric(df$Gender)
df$Gender_Female<- as.numeric(df$Gender)


library(dplyr)
df <- df %>%
  mutate(BMI.Category = ifelse(BMI.Category == "Normal Weight", "Normal", BMI.Category))
# Replace Column1, Column2 with the names of the columns to drop
df <- df %>% select(-c(Person.ID))

library(tidyr)

df <- df %>%
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/")

write.csv(df, file = "df.csv", row.names = FALSE)
write.csv(df, file = "C:/Users/User/Documents/sleep/data/df.csv", row.names = FALSE)
file.exists("df.csv")


levels(df$Occupation)

levels(df$Gender)
levels(df$BMI.Category)
levels(df$Sleep.Disorder)

df$Occupation_numeric <- as.numeric(df$Occupation)
df$Gender_Male<- as.numeric(df$Gender)
df$Gender_Female<- as.numeric(df$Gender)


anyDuplicated(df)
# Identify duplicates
duplicates <- duplicated(df)

# Display duplicate rows
df[duplicates, ]


# Logical vector of duplicates
duplicates <- duplicated(df)

# View duplicate rows
df[duplicates, ]

library(dplyr)

df <- df %>% select(-c(Occupation_Numerical))

library(dplyr)


# Combine "Salesperson" and "Sales Representative" into "Sales"


# Replace "Salesperson" and "Sales Representative" with "Sales"
df$Occupation <- ifelse(df$Occupation %in% c("Salesperson", "Sales Representative"), 
                        "Sales Representative", 
                        df$Occupation)

df$Occupation <- ifelse(df$Occupation %in% c("Software Engineer", "Engineer"), 
                        "Engineer", 
                        df$Occupation)



# Define a custom mapping
occupation_map <- c("Engineer" = 1, "Teacher" = 2, "Doctor" = 3, "Sales Representative" = 4, "Scientist" = 5, "Accountant" = 6, "Nurse" = 7, "Lawyer" = 8 )

# Apply the mapping to create a numeric column
df$Occupation_Numeric <- as.numeric(occupation_map[df$Occupation])

Sleep.Disorder_map <- c("Sleep Apnea" = 1, "Insomnia" = 2, "None" = 3)
df$Sleep.Disorder_Numeric <- as.numeric(Sleep.Disorder_map[df$Sleep.Disorder])

BMI.Category_map <- c("Normal"= 1, "Overweight" = 2, "Obese" = 3)
df$BMI.Category_Numeric <- as.numeric(BMI.Category_map[df$BMI.Category])

# Remove rows where Occupation is "Manager"
df <- df[df$Occupation != "Manager", ]

anyDuplicated(df)
View(anyDuplicated(df))

# Count duplicates
num_duplicates <- sum(duplicated(df))
print(num_duplicates)

# Check if duplicates remain
any_duplicates <- anyDuplicated(df)  # 0 means no duplicates
print(any_duplicates)

# Remove duplicates
df <- df[!duplicated(df), ]
print(any_duplicates)

library(dplyr)

# Add a new column based on blood pressure thresholds
df$BloodPressure.Category <- ifelse(
  df$Systolic >= 140 | df$Diastolic >= 90, "Hypertensive",
  ifelse(df$Systolic< 120 & df$Diastolic < 80, "Normal", "Pre-Hypertensive")
)
getwd




# Count occurrences for BMI_Category
bmi_counts <- as.data.frame(table(df$BMI.Category))

# Count occurrences for Occupation
occupation_counts <- as.data.frame(table(df$Occupation))



summary(df)
library(ggplot2)

library(ggplot2)

library(ggplot2)
ggplot(df, aes(x = Gender, fill = Quality.of.Sleep)) +
  geom_bar(position = "dodge") +
  labs(title = "Quality of sleep by Gender",
       x = "Gender",
       y = "sleep quality",
       fill = "Sleep Disorder")


# Save the specific plot
ggsave("my_plot.png", plot = my_plot, width = 8, height = 6)



library(ggplot2)
ggplot(df, aes(x = Sleep.Duration, y = Quality.of.Sleep)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(
    title = "Trend Between Duration and Quality of Sleep",
    x = "Duration of Sleep (hours)",
    y = "Quality of Sleep (numerical)"
  ) +
  theme_minimal()

# Assign the plot to an object
my_plotsleep <- ggplot(df, aes(x = Duration_of_Sleep, y = Quality_of_Sleep)) +
  geom_point() +
  labs(title = "Sleep Duration vs. Quality")

# Save the specific plot
ggsave("my_plotsleep.png", plot = my_plot, width = 8, height = 6)

ggsave("C:/Users/User/Documents/sleep/data/my_plotsleep.png", width = 8, height = 6)


file.exists("my_plotsleep.png")

#importing plot

install.packages("magick")
library(magick)

# Import the image
my_plotsleep <- image_read("C:/Users/User/Documents/sleep/data/my_plotsleep.png")

# Display the image
print(my_plot)


# Install necessary packages
install.packages("png")   # For PNG files

# Load the packages
library(png)


# Read and display a PNG image
img <- readPNG("C:/Users/User/Documents/sleep/data/my_plotsleep.png")

plot(1:2, type = "n", ann = FALSE)
rasterImage(img, 1, 1, 2, 2)

library(ggplot2)


df<-data.frame(Physical.Activity.Level, Quality.of.Sleep)

ggplot(df, aes(x = Physical.Activity.Level, y = Quality.of.Sleep)) +
  geom_point(color = "orange", size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Regression line with confidence interval
  labs(
    title = "Relationship Between Physical Activity and Sleep Quality",
    x = "Physical Activity (minutes)",
    y = "Sleep Quality"
  ) +
  theme_minimal()

model <- lm(Sleep_Quality ~ Physical_Activity, data = data)
summary(model)
ggsave("physical_activity_vs_sleep_quality.png", width = 8, height = 6)


df$Age <- cut(df$Age, breaks = c(0, 20, 30, 40, 50, 60, Inf),
              labels = c( "0","20", "30", "40", "50","60"))

ggplot(df, aes(x = Age, y = Sleep.Duration)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  # Box plot
  geom_jitter(width = 0.2, color = "red", alpha = 0.5) +  # Add jitter for individual points
  labs(
    title = "Relationship Between Age and Sleep Duration",
    x = "Age",
    y = "Sleep Duration (hours)"
  ) +
  theme_minimal()

ggsave("age_vs_sleep_duration_boxplot.png", width = 8, height = 6)

library(ggplot2)



bmi_counts <- as.data.frame(table(df$BMI.Category))
colnames(bmi_counts) <- c("BMI", "Frequency")

Occupation_counts <- as.data.frame(table(df$Occupation_Numeric))
colnames(Occupation_counts) <- c("Occupation", "Frequency")



library(ggplot2)

# Example dataset
work <- data.frame(
  Category = c("Engineer", "Teacher", "Doctor", "Sales Representative", "Scientist", "Accountant", "Nurse", "Lawyer"),
  Value = c(25, 15, 24, 10, 2, 11, 29, 15)
)

# Correct mapping
ggplot(work, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(title = "Distribution of Occupation")
theme_minimal()





library(dplyr)


Stress.Level_counts <- as.data.frame(table(df$Stress.Level))

# Create a density plot
ggplot(df, aes(x = Stress.Level)) +
  geom_density(color = "blue", fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Stress Level Distribution (Continuous)",
    x = "Stress.Level",
    y = "Density"
  ) +
  theme_minimal()

ggsave("stress_level_distribution_lineplot.png", width = 8, height = 6)






colnames(bmi_counts) <- c("BMI", "Frequency")



# Count occurrences of each BMI category
bmi_counts <- table(df$BMI.Category)


# Convert to a data frame
bmi_summary <- as.data.frame(bmi_counts)


ggplot(bmi_summary, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "BMI Category Distribution",
    x = NULL, y = NULL,
    fill = "bmi_counts"
  ) +
  theme_void()



# Count occurrences of each BMI category
bp_counts <- table(df$BloodPressure.Category)

# Convert to a data frame
bp_summary <- as.data.frame(bp_counts)

ggplot(bp_summary, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Blood Pressure Distribution",
    x = NULL, y = NULL,
    fill = "Blood Pressure"
  ) +
  theme_void()

library(ggplot2)

ggplot(df, aes(x = Daily.Steps, y = Heart.Rate)) +
  geom_point(color = "green", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Daily Steps and Heart Rate",
       x = "Daily Steps",
       y = "Heart Rate (bpm)") +
  theme_minimal()


# Load ggplot2
library(ggplot2)

# Create a stacked histogram
ggplot(df, aes(x = Age, fill = Occupation)) +
  geom_histogram(binwidth = 2, position = "stack", color = "black") +
  labs(
    title = "Distribution of Stress Levels Across Age Groups and Occupations",
    x = "Age",
    y = "Count",
    fill = "Occupation"
  ) +
  theme_minimal()


# Load dplyr
library(dplyr)

# Create a new column for stress level categories
df <- df %>%
  mutate(Stress.Level_Category = case_when(
    Stress.Level >= 1 & Stress.Level <= 3 ~ "Low",      # Group 1-3 as Low
    Stress.Level >= 4 & Stress.Level <= 6 ~ "Moderate", # Group 4-6 as Moderate
    Stress.Level >= 7 & Stress.Level <= 10 ~ "High"     # Group 7-10 as High
  ))

# View the updated dataset
head(data)



