# install library
# install.packages("ggplot2")
# install.packages("dplyr")

# load library
library(ggplot2)
library(dplyr)

# read the data
data <- read.csv("PopulationDataset/population_state.csv")

summary(data)
date <- unique(data$date)
print(date)
ethnic <- unique(data$ethnicity)
print(ethnic)
age_group <- unique(data$age)
print(age_group)

# Question 1: Show population in Penang from 2020 to 2023 based on the ethnicity, gender, and age group
new_data <- data[data$date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01") &
                   data$state == "Pulau Pinang" & data$sex %in% c('male', 'female') & 
                   data$ethnicity %in% c('chinese', 'indian', 'bumi_malay') & 
                   data$age %in% c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),  ]

summary(new_data)

# Create a stacked bar chart
new_data$age <- factor(new_data$age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

# Create a grouped bar chart 
p <- ggplot(new_data, aes(x = age, y = population, fill = interaction(ethnicity, sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population Distribution in Pulau Pinang from 2020-2023",
       x = "Age Group",
       y = "Population",
       fill = "Ethnicity") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white"))
ggsave("1.png", p)

# Question 2: Show the top 10 state with the highest numbers of female in the age range of 15 to 29.
new_data_Q2 <- data[data$sex %in% c('female') & 
                   data$ethnicity %in% c('chinese', 'indian', 'bumi_malay') & 
                   data$age %in% c("15-19", "20-24", "25-29"),  ]

total_female_by_state <- aggregate(population ~ state, data = new_data_Q2, sum)

# Sort the total_female_by_state data
total_female_by_state <- total_female_by_state %>% arrange(desc(population))
# Top 10 population of female state 
top_10_states <- total_female_by_state[1:10, ]

# Create data visual
p <- ggplot(top_10_states, aes(x = reorder(state, -population), y = population)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 States with Highest Female Population (Age 15-29)",
       x = "State",
       y = "Total Female Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.background = element_rect(fill = "white"))
ggsave("2.png", p)  

# Question 3: Show the states which have higher female than male with age 80+ from 2020 to 2023
new_data_Q3 <- data[data$date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01") &
                   data$sex %in% c('male', 'female') & 
                   data$ethnicity %in% c('chinese', 'indian', 'bumi_malay') & 
                   data$age %in% c("80-84", "85+"),  ]

# Combine age 60-84 and 85+
new_data_Q3 <- new_data_Q3 %>%mutate(grouped_age = ifelse(age %in% c("80-84", "85+"), "80+", age))

# Summarize data with the age > 80 and combine it
summarized_data <- new_data_Q3 %>%
  group_by(date, state, sex, grouped_age) %>%
  summarise(total_population = sum(population))

# Create data visual
p <- ggplot(summarized_data, aes(x = state, y = total_population, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rows = vars(date), scales = "free_y") +
  labs(title = "Population Distribution by State and Sex (Age > 80)",
       x = "State",
       y = "Total Population",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.background = element_rect(fill = "white"))
ggsave("3.png", p)

# Find the state which female higher than male
female_higher_than_male_state <- summarized_data %>%
  group_by(date, state) %>%
  summarise(female_population = sum(total_population[sex == "female"]),
            male_population = sum(total_population[sex == "male"])) %>%
  filter(female_population > male_population)

# Print all the state which female higher than male
print(female_higher_than_male_state)

# Question 4:  Show the number of people in Perak in the age group of 0-4 by year. Is there any increase in the number of people in this age group from 2020 to 2023 in Perak? Explain your answer with appropriate visuals.
# Filter the data
new_data_Q4 <- data[data$date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01") &
                   data$state == "Perak" & 
                   data$age == "0-4",  ]

# Group the data by year and calculate the total population
population_by_year <- aggregate(population ~ date, data = new_data_Q4, sum)

# Create a bar chart
p <- ggplot(population_by_year, aes(x = date, y = population)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population in Perak (Age 0-4) from 2020-2023",
       x = "Year",
       y = "Total Population") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))
ggsave("4.png", p)  

# Question 5: For the years 2020, 2021, 2022, and 2023, create a visual for each year that show the composition of the following: Bumi Malay; Bumi Other, Chinese, Indian, Other Citizen, Other Non-citizen. 
# Filter the data
new_data_Q5 <- data[data$date %in% c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01") &
                   data$ethnicity %in% c('bumi_malay', 'bumi_other', 'chinese', 'indian', 'other_citizen', 'other_non_citizen'),  ]

# Group the data by year and ethnicity, and calculate the total population
population_by_year_ethnicity <- aggregate(population ~ date + ethnicity, data = new_data_Q5, sum)

# Create a pie chart for each year and save as PNG files
for (year in unique(population_by_year_ethnicity$date)) {
  year_data <- population_by_year_ethnicity[population_by_year_ethnicity$date == year, ]
  
  # Create a new graphics device
  png(filename = paste0("5_population_composition_", year, ".png"))
  
  # Create a pie chart
  pie(year_data$population, labels = year_data$ethnicity, main = paste("Population Composition in", year))
  
  # Close the graphics device
  dev.off()
}

#Question 6:Imagine that you are an industrial player in the manufacturing industry. Based on the given population dataset, create an analysis to show that the analysis can help in supporting a business decision. 
# Filter the data
new_data_Q6 <- data[data$age %in% c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64") &
                   data$date == "2023-01-01",  ]

# Group the data by state and calculate the total population
population_by_state <- aggregate(population ~ state, data = new_data_Q6, sum)

# Create a bar chart
p <- ggplot(population_by_state, aes(x = reorder(state, -population), y = population)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population Distribution by State (Age 20-60) in 2023",
       x = "State",
       y = "Total Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.background = element_rect(fill = "white"))
ggsave("6.png", p)
