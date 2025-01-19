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
