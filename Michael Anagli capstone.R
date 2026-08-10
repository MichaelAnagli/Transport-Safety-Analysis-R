# Calling libraries 
library(tidyverse)
library('psych')
# Taking a look at the data I am working with
rating_safety
glimpse(rating_safety)

# Checking class and typeof my datasets
class(rating_safety)
typeof(rating_safety$escooter_frequency)

# Viewing first 6 rows and last 6 rows of the datasets
head(rating_safety)
tail(rating_safety)

# Changing the name of the datasets
transport_safety<-rating_safety
view(transport_safety)

# Checking the dimensions of my data
dim(transport_safety)

#Checking variable names
names(transport_safety)

# Checking data structure
str(transport_safety)

# Summary of the datasets
summary(transport_safety)

# Handling missing values
colSums(is.na(transport_safety))
sum(is.na(transport_safety))

# Checks for duplicates
sum(duplicated(transport_safety))

# Renaming some of my columns
library(dplyr)

transport_safety <- transport_safety %>%
  rename(
    participant_id = pid,
    transport_mode = tmode,
    perceived_safety = psafe,
    scenario_type = type,
    pavement = pav,
    obstacles = obst,
    crossing_type = cross,
    vehicle_traffic = veh,
    bicycle_traffic = bike,
    pedestrian_traffic = ped,
    scenario_type1 = type1,
    scenario_type2 = type2,
    scenario_type3 = type3,
    scenario_type4 = type4,
    crossing_option1 = cross1,
    crossing_option2 = cross2,
    owns_car = car_own,
    owns_motorcycle = moto_own,
    owns_bicycle = cycle_own,
    owns_escooter = escoot_own,
    public_transport_frequency = PT_frequency,
    metro_use_frequency = metro_frequency,
    young_adult = young
  )
names(transport_safety)

# Converting the variables into factors
transport_safety$transport_mode<-factor(
  transport_safety$transport_mode,
  levels = c('car', 'ebike', 'escoot', 'walk'),
  labels = c('Car', 'E-Bike', 'E-Scooter', 'Walking'))
  
view(transport_safety)

class(transport_safety$transport_mode)

transport_safety$gender<- factor(
  transport_safety$gender,
  levels = c(0,1),
  labels = c('Male', 'Female')
)
class(transport_safety$gender)

transport_safety$scenario_type <- factor(
  transport_safety$scenario_type,
  levels = c(1,2,3,4),
  labels = c(
    "Urban Road (<1.5m Sidewalk)",
    "Urban Road (>=1.5m Sidewalk)",
    "Urban Road with Cycle Lane",
    "Shared Space (30 km/h)"
  )
)
class(transport_safety$scenario_type)

transport_safety$pavement <- factor(
  transport_safety$pavement,
  levels = c(0,1),
  labels = c("Bad","Good")
)
class(transport_safety$pavement)

transport_safety$obstacles <- factor(
  transport_safety$obstacles,
  levels = c(0,1),
  labels = c("No Obstacles","Many Obstacles")
)
class(transport_safety$obstacles)

transport_safety$crossing_type <- factor(
  transport_safety$crossing_type,
  levels = c(0,1,2),
  labels = c(
    "No Zebra Crossing",
    "Zebra Crossing",
    "Signalized Zebra Crossing"
  )
)
class(transport_safety$crossing_type)

transport_safety$age <- factor(
  transport_safety$age,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "<18",
    "18-30",
    "31-40",
    "41-50",
    "51-65",
    "65+"
  )
)
class(transport_safety$age)


transport_safety$education <- factor(
  transport_safety$education,
  levels = c(0,1,2,3,4),
  labels = c(
    "No Education",
    "Primary",
    "Secondary",
    "Higher",
    "Master"
  )
)
class(transport_safety$education)

transport_safety$employment <- factor(
  transport_safety$employment,
  levels = c(0,1,2,3),
  labels = c(
    "Inactive",
    "Student",
    "Unemployed",
    "Active"
  )
)
class(transport_safety$employment)

transport_safety$income <- factor(
  transport_safety$income,
  levels = c(0,1,2,3,4),
  labels = c(
    "No Income",
    "< €750",
    "€750-1500",
    "€1500-2500",
    ">= €2500"
  )
)
class(transport_safety$income)

transport_safety$owns_car <- factor(
  transport_safety$owns_car,
  levels = c(0,1),
  labels = c("No","Yes")
)
class(transport_safety$owns_car)


transport_safety$owns_motorcycle <- factor(
  transport_safety$owns_motorcycle,
  levels = c(0,1),
  labels = c("No","Yes")
)
class(transport_safety$owns_motorcycle)


transport_safety$owns_bicycle <- factor(
  transport_safety$owns_bicycle,
  levels = c(0,1),
  labels = c("No","Yes")
)
class(transport_safety$owns_bicycle)


transport_safety$owns_escooter <- factor(
  transport_safety$owns_escooter,
  levels = c(0,1),
  labels = c("No","Yes")
)
class(transport_safety$owns_escooter)


transport_safety$young_adult <- factor(
  transport_safety$young_adult,
  levels = c(0,1),
  labels = c("No","Yes")
)
class(transport_safety$young_adult)

# Using the same labels for all 4 frequencies because they share the same coding
frequency_levels <- c(0,1,2,3,4)

frequency_labels <- c(
  "Almost Never",
  "A Few Times a Year",
  "A Few Times a Month",
  "A Few Times a Week",
  "Daily"
)

transport_safety$bike_frequency <- factor(
  transport_safety$bike_frequency,
  levels = frequency_levels,
  labels = frequency_labels
)

transport_safety$escooter_frequency <- factor(
  transport_safety$escooter_frequency,
  levels = frequency_levels,
  labels = frequency_labels
)

transport_safety$public_transport_frequency <- factor(
  transport_safety$public_transport_frequency,
  levels = frequency_levels,
  labels = frequency_labels
)

transport_safety$metro_use_frequency <- factor(
  transport_safety$metro_use_frequency,
  levels = frequency_levels,
  labels = frequency_labels
)

view(transport_safety)

# Checking the cleaned data
dim(transport_safety)
sum(duplicated(transport_safety))

colSums(is.na(transport_safety))
sum(is.na(transport_safety))
summary(transport_safety)



# Exporting the cleaned file
write.csv(
  transport_safety,
  "cleaned_transport_safety_data.csv",
  row.names = FALSE
)

# Summarizing the numerical variables
library(psych)
describe(
  transport_safety %>%
    select(
      perceived_safety,
      vehicle_traffic,
      bicycle_traffic,
      pedestrian_traffic
    )
)

# Checking for distribution across categories
table(transport_safety$transport_mode)
prop.table(table(transport_safety$transport_mode))*100

table(transport_safety$gender)
prop.table(table(transport_safety$gender))*100

table(transport_safety$age)
prop.table(table(transport_safety$age))*100

table(transport_safety$education)

table(transport_safety$employment)

# Reordering some variables in the datasets
transport_safety %>% 
  relocate(scenario_type,gender,age,education, employment,income, young_adult,.before = transport_mode) %>% 
  view()


# Data Exploration Annalysis (EDA) and Visualization
library(ggplot2)
library(plotly)
packageVersion("SmartEDA")
packageVersion("ggplot2")
library(SmartEDA)

# Histograms
# perceived safety frequency Distribution Histogram
perceived_safety_frequency<-ggplot(transport_safety,
       aes(x = perceived_safety))+
  geom_histogram(fill = 'steelblue', binwidth = 0.5, color = 'white')+
  labs(
    title = "Distribution of Perceived Safety Ratings",
    x = "Perceived Safety",
    y = "Frequency"
  )+
  theme_minimal()
ggplotly(perceived_safety_frequency)

# Vehicle Traffic Distribution Histogram
vehicle_trafficDistribution<-ggplot(transport_safety,
       aes(x = vehicle_traffic)) +
  geom_histogram(fill = '#32a83a' ,binwidth = 20) +
  labs(
    title = "Vehicle Traffic Distribution",
    x = "Vehicle Traffic",
    y = "Frequency"
  ) +
  theme_minimal()
ggplotly(vehicle_trafficDistribution)

# Bar Charts
# Transport Mode Bar Chart
transportMode<-ggplot(transport_safety,
       aes(x = transport_mode)) +
  geom_bar(fill = 'steelblue') +
  labs(
    title = "Transport Modes",
    subtitle = 'Car, E-Bike, E-Scooter, Walking',
    x = "Transport Mode",
    y = "Mode Count"
  ) +
  theme_minimal()
ggplotly(transportMode)

# Gender Bar Chart
gender_count<-ggplot(transport_safety,
       aes(x = gender)) +
  geom_bar(fill = '#3238a8') +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Gender Count"
  )+
  theme_minimal()
ggplotly(gender_count)

# Bivariate  Analysis
# Perceived Safety by Transport Mode
peceivedSafety_byTmode<-ggplot(transport_safety,
       aes(
         x = transport_mode,
         y = perceived_safety,
         fill = transport_mode
       )) +
  geom_boxplot() +
  labs(
    title = "Perceived Safety by Transport Mode",
    x = "Transport Mode",
    y = "Perceived Safety"
  ) +
  theme(panel.grid = element_blank())
ggplotly(peceivedSafety_byTmode)

# Perceived Safety by Gender
gender_safety<-ggplot(transport_safety,
       aes(
         x = gender,
         y = perceived_safety,
         fill = gender
       )) +
  geom_boxplot() +
  labs(
    title = "Perceived Safety by Gender",
    x = "Gender",
    y = "Perceived Safety"
  ) +
  theme_minimal()
ggplotly(gender_safety)

# Scenario type by perceived safety
scenario_safety<-ggplot(transport_safety,
       aes(
         x = scenario_type,
         y = perceived_safety,
         fill = scenario_type
       ))+
  geom_boxplot()+
  labs(
    title = 'Perceived Safety by Scenario Type',
    x = 'Scenario Type',
    y = 'Perceived Safety'
  )+
  theme(panel.grid = element_blank())
ggplotly(scenario_safety)

# Checking Mean perceived safety
#1. By Transport Mode
transport_safety %>%
  group_by(transport_mode) %>%
  summarise(
    Mean = mean(perceived_safety),
    Median = median(perceived_safety),
    SD = sd(perceived_safety),
    Count = n()
  )
#2. By Gender
transport_safety %>%
  group_by(gender) %>%
  summarise(
    mean = mean(perceived_safety),
    Median = median(perceived_safety),
    SD = sd(perceived_safety),
    Count = n()
  )

# Cross Tabulations
# Transport mode by Gender
table(
  transport_safety$gender,
  transport_safety$transport_mode
)

# Summarizing the statistics by scenario
transport_safety %>%
  group_by(scenario_type) %>%
  summarise(
    Mean = mean(perceived_safety),
    Median = median(perceived_safety),
    SD = sd(perceived_safety),
    Count = n()
  )

# Correlation matrix
numeric_data <- transport_safety %>%
  select(
    perceived_safety,
    vehicle_traffic,
    bicycle_traffic,
    pedestrian_traffic
  )
cor(numeric_data)

library(corrplot)

corrplot(
  cor(numeric_data),
  method = "color",
  type = "upper",
  tl.cex = 0.1,
  addCoef.col = "black"
)

# Correlation Heatmap
install.packages('ggcorrplot')
library(ggcorrplot)

corr <- cor(numeric_data)

ggcorrplot(
  corr,
  lab = TRUE
)

# Percentage tables for; gender, transport mode, education, age and employment
# 1. Gender
round(
  prop.table(table(transport_safety$gender))*100,
  4
)

# 2. Transport mode
round(
  prop.table(table(transport_safety$transport_mode))*100,
  4
)

# 3. Education 
round(
  prop.table(table(transport_safety$education))*100,
  4
)

# 4. Age
round(
  prop.table(table(transport_safety$age))*100,
  4
)

# 5. Employment
round(
  prop.table(table(transport_safety$employment))*100,
  4
)

# Average Perceived Safety Between Male and Female
by(
  transport_safety$perceived_safety,
  transport_safety$gender,
  shapiro.test
)


ggplot(transport_safety,
       aes(sample = perceived_safety)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~gender)

# Checking Equal Variance
install.packages('car')
library(car)
leveneTest(
  perceived_safety ~ gender,
  data = transport_safety
)

t.test(
  perceived_safety ~ gender,
  data = transport_safety,
  var.equal = TRUE
)

install.packages('rstatix')
library(rstatix)

transport_safety %>%
  group_by(transport_mode) %>%
  shapiro_test(perceived_safety)

safety_count<-ggplot(transport_safety,
       aes(perceived_safety)) +
  geom_histogram(binwidth = 0.5,
                 fill = "steelblue",
                 color = "white") +
  facet_wrap(~transport_mode) +
  theme_minimal()
ggplotly(safety_count)

# Variance

leveneTest(
  perceived_safety ~ transport_mode,
  data = transport_safety
)

eta_squared(anova_model)
TukeyHSD(anova_model)

oneway.test(
  perceived_safety ~ transport_mode,
  data = transport_safety,
  var.equal = FALSE
)

games_howell_test(
  transport_safety,
  perceived_safety ~ transport_mode
)

# Checking missing values 
library(DataExplorer)
DataExplorer::plot_missing(transport_safety)

# Checking effect size
install.packages('effectsize')
library(effectsize)

cohens_d(
  perceived_safety ~ gender,
  data = transport_safety,
  pooled_sd = TRUE
)

# Multiple Linear Regression 
model <- lm(
  perceived_safety ~
    transport_mode +
    scenario_type +
    vehicle_traffic +
    bicycle_traffic +
    pedestrian_traffic +
    pavement +
    obstacles +
    gender +
    age,
  data = transport_safety
)

summary(model)



vif(model)























































































