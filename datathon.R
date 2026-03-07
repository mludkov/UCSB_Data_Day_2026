## Loading R Packages
library(tidyverse)

## Loading Data
df <- cidf <- read_csv("data_for_csuci_datathon_2014_2016.csv") # Loads the Data Set into the objects df and cidf

## Cleaning Data ----

#### Renaming Categories

df <- df |> mutate( # Will change the variables in the df data set
    Vaccinations = case_when(is.na(Vaccinations) ~ "NV", .default = Vaccinations), # Reclassifies Missing Data in Vaccination as "N" per code book.
    ReproductiveStatus = case_when(is.na(ReproductiveStatus) ~ "UN", .default = ReproductiveStatus), # Reclassifies Missing Data in Vaccination as "N" per code book.
    BodyCondition = case_when( # Begins to modify BodyCondition
        as.character(BodyCondition) == "1" ~ "emaciated", # Changes 1 to emaciated
        as.character(BodyCondition) == "2" ~ "thin", # Changes 2 to thin
        as.character(BodyCondition) == "3" ~ "healthy wild", # Changes 3 to healthy wild 
        as.character(BodyCondition) == "4" ~ "extra fat reserves", # Changes 4 to Extra Fat Reserve
        as.character(BodyCondition) == "5" ~ "extreme fat reserves" # Changes 5 to Extreme Fat Reserves
        )
    )

### Viewing Missing Data

summary(df)

### Removing Missing Data
df <- drop_na(df, AgeClass, Weight, BodyCondition) # Removes the missing values (NA) using the variables AgeClass, Weight, and BodyCondition


## After cleaning the data, this should result in 1,967 recorded foxes!
nrow(df)

## Fox Weight Analysis ----

### Descriptive Statistics

mean_weight<-mean(df$Weight)
sd_weight<-sd(df$Weight)
mean_weight
sd_weight #printing the values
count_df <- table(df$Sex) # Using the 'Sex' variable from the `df` data set, we count the frequencies of each category with the table function and storing it in rs_df.
count_df # Printing out the contents of "rs_df"
prop.table(count_df) # Computing the Proportions of "rs_df"


### Data Visualization
ggplot(df) + # Setting up the data to create a plot.
  geom_bar(aes(Sex)) # Creating a bar chart based on the variable "ReproductiveStatus"

### Weight vs Sex

#### Data Visualization

ggplot(df, aes(x = Sex, y = Weight)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Weight by Sex",
    x = "Sex",
    y = "Weight"
  )

#### Numerical Statistics

weight_summary <- df %>%
  group_by(Sex) %>%
  summarise(
    mean_weight = mean(Weight),
    sd_weight = sd(Weight),
    .groups = "drop"
  )

weight_summary

#### Weight vs Sex over time

weight_summary_yearly <- df %>%
  group_by(Sex, SamplingYear) %>%
  summarise(
    mean_weight = mean(Weight, na.rm = TRUE),
    sd_weight = sd(Weight, na.rm = TRUE),
    .groups = "drop"
  ) #group the data by Sex and Year and calculate mean and standard deviation of weight

weight_summary_yearly

ggplot(weight_summary_yearly, aes(x = SamplingYear, y = mean_weight)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_weight - sd_weight,
                    ymax = mean_weight + sd_weight), width = 0.2) +
  facet_wrap(~ Sex) +
  labs(
    title = "Average Weight by Year and Sex",
    y = "Mean Weight",
    x = "Year"
  )


### Challenges


#### Weight vs Sex over time by Island
#Santa Rosa Fox population 
SR_df<-filter(df,Island=="SRI")
#San Miguel Fox population
SM_df<-filter(df,Island=="SMI")


#### Vaccinations vs Weight

# Analyse the interaction between `Weight` and `Vaccinations`
### Body Condition vs Weight

# Analyse the interaction between `Weight` and `BodyCondition`


## Fox Reproductive Status Analysis {#fox-rs} ----


### Descriptive Statistics

rs_df <- table(df$ReproductiveStatus) # Using the 'ReproductiveStatus' variable from the `df` data set, we count the frequencies of each category with the table function and storing it in rs_df.
rs_df # Printing out the contents of "rs_df"
prop.table(rs_df) # Computing the Proportions of "rs_df"

### Data Visualization

ggplot(df) + # Setting up the data to create a plot.
  geom_bar(aes(ReproductiveStatus)) # Creating a bar chart based on the variable "ReproductiveStatus"

### Sex and Reproductive Status

#### Numerical Statistics

xy_df <- table(df$ReproductiveStatus, df$Sex)
# Use the variables "ReproductiveStatus" and "Sex" from the "df" data set
# Use the table function to compute the crosstabs
# Store the results in the xy_df object

xy_df # Print results out
prop.table(xy_df)

#### Data Visualization

ggplot(df) +
  geom_bar(aes(Sex, fill = ReproductiveStatus))

### Challenges

#### Age Class
# Analyse the interaction between `ReproductiveStatus` and `AgeClass`
#### Vaccinations
# Analyse the interaction between `ReproductiveStatus` and `Vaccinations`
### Capture Type
# Analyse the interaction between `ReproductiveStatus` and `Capture Type`
