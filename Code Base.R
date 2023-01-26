## Library and packages to load
install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)


## Read and add the data sets
dir <- "/cloud/project/Data"

file_names <- list.files(dir)

for (file in file_names) {
  data <- read.csv(file.path(dir, file))
  assign(file, data)
}
rm(data)

## Create a list with all data sets
all_files <- list(dailyActivity_merged.csv,
                 dailyCalories_merged.csv,
                 dailyIntensities_merged.csv,
                 dailySteps_merged.csv,
                 heartrate_seconds_merged.csv,
                 hourlyCalories_merged.csv,
                 hourlyIntensities_merged.csv,
                 hourlySteps_merged.csv,
                 minuteCaloriesNarrow_merged.csv,
                 minuteCaloriesWide_merged.csv,
                 minuteIntensitiesNarrow_merged.csv,
                 minuteIntensitiesWide_merged.csv,
                 minuteMETsNarrow_merged.csv,
                 minuteSleep_merged.csv,
                 minuteStepsNarrow_merged.csv,
                 minuteStepsWide_merged.csv,
                 sleepDay_merged.csv,
                 weightLogInfo_merged.csv)

## To reduce Ram usage in running codes I divided in 3 list of data sets
lst1 <- list(dailyActivity_merged.csv,
            dailyCalories_merged.csv,
            dailyIntensities_merged.csv,
            dailySteps_merged.csv,
            heartrate_seconds_merged.csv,
            hourlyCalories_merged.csv)

lst2 <- list(hourlyIntensities_merged.csv,
            hourlySteps_merged.csv,
            minuteCaloriesNarrow_merged.csv,
            minuteCaloriesWide_merged.csv,
            minuteIntensitiesNarrow_merged.csv,
            minuteIntensitiesWide_merged.csv)

lst3 <- list(minuteMETsNarrow_merged.csv,
            minuteSleep_merged.csv,
            minuteStepsNarrow_merged.csv,
            minuteStepsWide_merged.csv,
            sleepDay_merged.csv,
            weightLogInfo_merged.csv)

## Count number of duplicates Rows
lapply(lst1, function(x) nrow(x[duplicated(x), ]))
lapply(lst2, function(x) nrow(x[duplicated(x), ]))
lapply(lst3, function(x) nrow(x[duplicated(x), ]))

nrow(sleepDay_merged.csv[duplicated(sleepDay_merged.csv),])
## Removing the duplicates Rows
lst1 <- Map(function(x) x %>% distinct(.keep_all = TRUE), lst1)
lst2 <- Map(function(x) x %>% distinct(.keep_all=TRUE), lst2)
lst3 <- Map(function(x) x %>% distinct(.keep_all=TRUE), lst3)
allfiles <- Map(function(x) x %>% distinct(.keep_all=TRUE), allfiles)

combined_hourlyData <-  combined_hourlyData %>% distinct(.keep_all=TRUE)

## Move out the data sets in the list to Environment
original_names_lst1 <- c("dailyActivity_merged.csv",
                        "dailyCalories_merged.csv",
                        "dailyIntensities_merged.csv",
                        "dailySteps_merged.csv",
                        "heartrate_seconds_merged.csv",
                        "hourlyCalories_merged.csv")

original_names_lst2 <- c("hourlyIntensities_merged.csv",
                        "hourlySteps_merged.csv",
                        "minuteCaloriesNarrow_merged.csv",
                        "minuteCaloriesWide_merged.csv",
                        "minuteIntensitiesNarrow_merged.csv",
                        "minuteIntensitiesWide_merged.csv")

original_names_lst3 <- c("minuteMETsNarrow_merged.csv",
                        "minuteSleep_merged.csv",
                        "minuteStepsNarrow_merged.csv",
                        "minuteStepsWide_merged.csv",
                        "sleepDay_merged.csv",
                        "weightLogInfo_merged.csv")

for (i in seq_along(lst1)) {
  element <- lst1[[i]]
  assign(original_names_lst1[i], element)
}
for (i in seq_along(lst2)) {
  element <- lst2[[i]]
  assign(original_names_lst2[i], element)
}
for (i in seq_along(lst3)) {
  element <- lst3[[i]]
  assign(original_names_lst3[i], element)
}
rm(element)

## Ensures that there's only characters, numbers, and underscores in the names
lapply(lst1, function(x) head(clean_names(x)))

## Create a Data set wide for sleepday
sleepDay_Wide_merged.csv  <- sleepDay_merged.csv %>% pivot_wider(names_from = )


## Taking a look to the data set to confirm that is okay
lapply(lst1, function(x) head(x))
lapply(lst2, function(x) head(x))
lapply(lst3, function(x) head(x))

## Indentify their colums names
lapply(lst1, function(x) colnames(x))
lapply(lst2, function(x) colnames(x))
lapply(lst3, function(x) colnames(x))

## Show how many uniques ID in the data sets
lapply(all_files, function(x) n_distinct(x$Id))

## Analysis in daily_intensities data set
# *Having the information you may create daily task as option, and creating a UI design offering the information may help retain more customors and their interest in the app
dailyIntensities_merged.csv %>% select(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, SedentaryActiveDistance, LightActiveDistance, ModeratelyActiveDistance) %>%  
  summary()
ggplot(data=dailyIntensities_merged.csv) +
  geom_point(mapping=aes(x0LightlyActiveMinutes, y=LightActiveDistance, color=LightActiveDistance )) + 
  geom_jitter()

ggplot(data=dailyIntensities_merged.csv) +
  geom_point(mapping=aes(x=FairlyActiveMinutes, y=ModeratelyActiveDistance, color=ModeratelyActiveDistance ))

## Analysis in dailyActivity_merged.csv data set
### observed cluster between 1500 to 2500 (using x as Calories)
dailyActivity_merged.csv <- dailyActivity_merged.csv %>% rename_at("ActivityDate", ~"ActivityDay")

combDaily <- list(dailyActivity_merged.csv,
                  dailySteps_merged.csv)

combined_dailyData <- combDaily %>%  reduce(full_join, by = c("Id","ActivityDay"))
rm(combDaily)
combined_dailyData <- combined_dailyData %>% distinct(.keep_all=TRUE)


combined_dailyData %>% select(TotalSteps, TotalDistance, Calories ) %>%  summary()

### use this grapich with 
ggplot(data <- combined_dailyData) +
  geom_point( mapping = aes(x=Calories, y=LightActiveDistance))

ggplot(data <- combined_dailyData) +
  geom_point( mapping = aes(x=Calories, y=ModeratelyActiveDistance))

ggplot(data <- combined_dailyData) +
  geom_jitter( mapping = aes(x=Calories, y=VeryActiveDistance)) + 
  geom_jitter(mapping = aes ( x = Calories, y = ModeratelyActiveDistance, color = "purple", alpha = ModeratelyActiveDistance ))
ggplot(data <- combined_dailyData) +
  geom_line( mapping = aes(x=StepTotal, y=Calories))

ggplot(data <- combined_dailyData) +
  geom_line( mapping = aes(x=LightlyActiveMinutes, y=Calories)) +
  geom_jitter(mapping = aes(x =LightActiveDistance , y =Calories ))

ggplot(data <- combined_dailyData) +
  geom_line( mapping = aes(x=TotalDistance, y=VeryActiveDistance))

##before plots

combined_dailyData %>% select(VeryActiveDistance, LightActiveDistance, TotalDistance ) %>%  summary()

ggplot(data <- combined_dailyData) +
  geom_jitter(mapping = aes(x =TotalDistance , y =VeryActiveDistance, color = VeryActiveDistance))
ggplot(data <- combined_dailyData) +
  geom_jitter(mapping = aes(x =TotalDistance , y =LightActiveDistance, color = LightActiveDistance ))


ggplot(data <- combined_dailyData) + 
  geom_jitter(mapping = aes(x =TotalDistance , y =VeryActiveDistance, color = "VeryActiveDistance" )) +
  geom_jitter(mapping = aes(x =TotalDistance , y =LightActiveDistance, color = "LightActiveDistance" ))

ggplot(data <- combined_dailyData) + 
  geom_jitter(mapping = aes(x =Calories , y =VeryActiveMinutes, color = "red" )) +
  geom_jitter(mapping = aes(x =Calories , y =LightlyActiveMinutes, color = "yellow" ))



ggplot(data <- combined_dailyData) +
  geom_line( mapping = aes(x=SedentaryMinutes, y=Calories))

ggplot(data <- combined_dailyData) +
  geom_line( mapping = aes(x=FairlyActiveMinutes, y=Calories))

ggplot(data <- combined_dailyData) +
  geom_line( mapping = aes(x=VeryActiveMinutes, y=VeryActiveDistance))



ggplot(data <- dailyActivity_merged.csv) +
  geom_point( mapping=aes(x = Calories, y=TotalDistance))
### this function to create goals and challanges with their callories related to distance
dailyActivity_merged.csv %>% summarize(mean(Calories), mean(TotalDistance), mean(TotalSteps))

### sendentary time x calories
ggplot(data <- dailyActivity_merged.csv) +
  geom_point(mapping=aes (x=SedentaryMinutes, y=Calories, color=SedentaryMinutes ))

## hear data set
ggplot(data = heartrate_seconds_merged.csv) + 
  geom_point(mapping = aes(x = Time, y = Value ))

## hourly data set
combHourly <- list(hourlyCalories_merged.csv, hourlyIntensities_merged.csv, hourlySteps_merged.csv)
combined_hourlyData <- combHourly %>%  reduce(full_join, by = c("Id","ActivityHour"))
rm(combHourly)
combined_hourlyData <- combined_hourlyData %>% distinct(.keep_all=TRUE)



ggplot(data = combined_hourlyData)+
  geom_line(mapping = aes(y = StepTotal , x = Calories)) +
  geom_line(mapping = aes(y = TotalIntensity , x = Calories, color = "purple"))
  
ggplot(data = hourlyCalories_merged.csv) +
  geom_point(mapping = aes(x = Calories, y = ActivityHour ))


