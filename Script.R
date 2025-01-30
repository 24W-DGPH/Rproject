#installing packages
install.packages("pacman")
install.packages("renv")
renv::init()
renv::restore() 
renv::snapshot()

#load necessary libriaries
library(pacman)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)


#overview of dataset
View(Sleep_health_and_lifestyle_dataset)
head(Sleep_health_and_lifestyle_dataset)
str(Sleep_health_and_lifestyle_dataset)
summary(Sleep_health_and_lifestyle_dataset)

#clean dataset

colSums(is.na(Sleep_health_and_lifestyle_dataset))
anyDuplicated(Sleep_health_and_lifestyle_dataset)
df=unique(Sleep_health_and_lifestyle_dataset)

df <- df %>%
  mutate(BMI.Category = ifelse(BMI.Category == "Normal Weight", "Normal", BMI.Category))

# Remove column 
df <- df %>% select(-c(Person.ID))

#create a new columm for systolic and diastolic
df <- df %>%
  separate(Blood.Pressure, into = c("Systolic", "Diastolic"), sep = "/")


# Combine "Salesperson" and "Sales Representative" into "Sales"

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

BMI.Category_map <- c("Normal"= 1, "Overweight" = 2, "Obese" = 3)
df$BMI.Category_Numeric <- as.numeric(BMI.Category_map[df$BMI.Category])

# Add a new column based on blood pressure thresholds
df$BloodPressure.Category <- ifelse(
  df$Systolic >= 140 | df$Diastolic >= 90, "Hypertensive",
  ifelse(df$Systolic< 120 & df$Diastolic < 80, "Normal", "Pre-Hypertensive")
)

#create a new column for stress level categories
df <- df %>%
  mutate(Stress.Level_Category = case_when(
    Stress.Level >= 1 & Stress.Level <= 3 ~ "Low",      # Group 1-3 as Low
    Stress.Level >= 4 & Stress.Level <= 6 ~ "Moderate", # Group 4-6 as Moderate
    Stress.Level >= 7 & Stress.Level <= 10 ~ "High"     # Group 7-10 as High
  ))


# Convert Gender column to numeric
df$Gender_Numeric <- ifelse(df$Gender == "Male", 1, 
                              ifelse(df$Gender == "Female", 2, NA))


# Remove rows where Occupation is "Manager"
df <- df[df$Occupation != "Manager", ]


# Identify and correcting duplicates
anyDuplicated(df)
View(anyDuplicated(df))

# Count duplicates
num_duplicates <- sum(duplicated(df))
print(num_duplicates)

# Remove duplicates
df <- df[!duplicated(df), ]


# Count occurrences for BMI_Category
bmi_counts <- as.data.frame(table(df$BMI.Category))
colnames(bmi_counts) <- c("BMI", "Frequency")
bmi_summary <- as.data.frame(bmi_counts)

#count occurence for Age
Age_counts <- as.data.frame(table(df$Age))

# Count occurrences for Occupation
occupation_counts <- as.data.frame(table(df$Occupation))
colnames(Occupation_counts) <- c("Occupation", "Frequency")

#count occurrences for stress level
Stress.Level_category_counts <- as.data.frame(table(df$Stress.Level_Category))
colnames(Stress.Level_category_counts) <- c("Category", "Freqency")

# Count occurrences of each quality of sleep category
Quality.of.sleep_count <- as.data.frame(table(df$Quality.of.Sleep))
colnames(Quality.of.sleep_count) <- c("Quality", "Frequency")
Stress.Level_counts <- as.data.frame(table(df$Stress.Level))

# Count occurrences of each Blood pressure category

# Count occurrences of each BMI category
bp_counts <- table(df$BloodPressure.Category)
bp_summary <- as.data.frame(bp_counts)

#summarise the cleaned data "df"
summary(df)

#visualization
ggplot(df, aes(x = Gender, fill = Quality.of.Sleep)) +
  geom_bar(position = "dodge") +
  labs(title = "Quality of sleep by Gender",
       x = "Gender",
       y = "sleep quality",
       fill = "Sleep Disorder")


# Save the specific plot
ggsave("my_plot.png", plot = my_plot, width = 8, height = 6)


#trends between duration and quality of sleep
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


#Relationship between physical activity and sleep quality
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

summary(model)
ggsave("physical_activity_vs_sleep_quality.png", width = 8, height = 6)


#Relationship between age and sleep duration
df$Age <- cut(df$Age, breaks = c(0,20,30,40,50,60),
              labels = c( "20","30","40","50","60"))

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



#Distribution of occupation
work <- data.frame(
  Category = c("Engineer", "Teacher", "Doctor", "Sales Representative", "Scientist", "Accountant", "Nurse", "Lawyer"),
  Value = c(25, 15, 24, 10, 2, 11, 29, 15)
)

# Correct mapping
ggplot(work, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(title = "Distribution of Occupation")
theme_minimal()



# Create a density plot for stress level distribution
ggplot(df, aes(x = Stress.Level)) +
  geom_density(color = "blue", fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Stress Level Distribution (Continuous)",
    x = "Stress.Level",
    y = "Density"
  ) +
  theme_minimal()

ggsave("stress_level_distribution_lineplot.png", width = 8, height = 6)


#BMI Category Disrtibution
ggplot(bmi_summary, aes(x = "", y = Frequency, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "BMI Category Distribution",
    x = NULL, y = NULL,
    fill = "bmi_summary"
  ) +
  theme_void()


#Blood pressure distribution
ggplot(bp_summary, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Blood Pressure Distribution",
    x = NULL, y = NULL,
    fill = "Blood Pressure"
  ) +
  theme_void()

#Relationship between daily step and heartrate
ggplot(df, aes(x = Daily.Steps, y = Heart.Rate)) +
  geom_point(color = "green", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship Between Daily Steps and Heart Rate",
       x = "Daily Steps",
       y = "Heart Rate (bpm)") +
  theme_minimal()


# Distribution of stress across age groups and occupation
ggplot(df, aes(x = Age, fill = Occupation)) +
  geom_histogram(binwidth = 2, position = "stack", color = "black") +
  labs(
    title = "Distribution of Stress Levels Across Age Groups and Occupations",
    x = "Age",
    y = "Count",
    fill = "Occupation"
  ) +
  theme_minimal()

library(readr)
df<-read.csv("df.csv")
View(df)


write.csv(df_cleaned, "cleaned_sleep_data.csv", row.names = FALSE)
write.csv(df, "C:/Users/user/Documents/sleep/Rproject/cleaned_df.csv", row.names = FALSE)


#shiny app
# Convert necessary columns
df$Occupation <- as.factor(df$Occupation)
df$Age <- as.numeric(df$Age)  # Ensure Age is numeric

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Quality of Sleep by Age Group and Occupation"),
  
  # Sidebar layout with slider input for age bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of Age Groups (Bins):",
                  min = 2,  # Minimum number of bins
                  max = 10, # Maximum number of bins
                  value = 5) # Default number of bins
    ),
    
    # Main panel to show the plot
    mainPanel(
      plotOutput("sleepPlot") # Output the plot
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$sleepPlot <- renderPlot({
    
    # Create dynamic age bins
    bin_breaks <- seq(min(df$Age, na.rm = TRUE), 
                      max(df$Age, na.rm = TRUE), 
                      length.out = input$bins + 1)
    
    df$Age_Group <- cut(df$Age, breaks = bin_breaks, 
                        include.lowest = TRUE, 
                        labels = paste0("Group ", 1:input$bins))
    
    # Create the bar plot
    ggplot(df, aes(x = Age_Group, fill = Occupation)) +
      geom_bar() +
      labs(title = "Quality of Sleep Across Age Groups and Occupations",
           x = "Age Group",
           y = "Count",
           fill = "Occupation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


