# Install and load the required packages
library(tidyverse)
library(lubridate)
library(ggplot2)

# Read the dataset
covid_data <- read.csv("COVID-19Dataset/covid_cases_age.csv")

# Convert date format
covid_data$date <- as.Date(covid_data$date)

# Extract year and month information
covid_data$year <- lubridate::year(covid_data$date)
covid_data$month <- lubridate::month(covid_data$date)

# Question 7: Show the monthly COVID-19 cases in Penang from 2020 to 2023
penang_data <- covid_data %>% 
    dplyr::filter(state == "Pulau Pinang" & year %in% c(2020, 2021, 2022, 2023))

# Check if there are valid values for faceting
if (nrow(penang_data) > 0) {
    # Display using appropriate visualization tool (e.g., ggplot2)
    p <- ggplot(penang_data, aes(x = month, y = cases_0_4)) +
        geom_bar(stat = "sum", position = "dodge", fill = "steelblue") +
        facet_wrap(~year, scales = "free_y") +
        labs(title = "Monthly COVID-19 Cases in Penang (2020-2023)", x = "Month", y = "Number of Cases (0-4 age group)")
    ggsave("7.png", p)
} else {
    print("No data available for faceting.")
}

# Question 8: Show the monthly trend of COVID-19 cases in Malaysia by age group
malaysia_data <- covid_data %>% dplyr::filter(state == "Malaysia")

# Display using appropriate visualization tool (e.g., ggplot2)
p <- ggplot(malaysia_data, aes(x = month, y = cases_0_4, fill = "0-4")) +
    geom_bar(stat = "sum", position = "dodge") +
    labs(title = "Monthly Trend of COVID-19 Cases in Malaysia (0-4 age group)", x = "Month", y = "Number of Cases (0-4 age group)")
ggsave("8.png", p)

# Question 9: Identify the top 3 states with the highest COVID-19 cases in 2021
top_states_2021 <- covid_data %>% dplyr::filter(year == 2021) %>%
    group_by(state) %>%
    summarize(total_cases = sum(cases_0_4)) %>%
    top_n(3, total_cases)

# Display using appropriate visualization tool (e.g., ggplot2)
p <- ggplot(top_states_2021, aes(x = state, y = total_cases, fill = state)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 3 States with Highest COVID-19 Cases in 2021", x = "State", y = "Number of Cases (0-4 age group)")
ggsave("9.png", p)

# Question 10: Which five states show the lowest number of COVID-19 cases in 2020, 2021, 2022, and 2023 respectively. Explain your answer with appropriate visuals.
# Calculate the total number of cases for each state and year
state_year_cases <- covid_data %>%
    group_by(state, year) %>%
    summarise(total_cases = sum(cases_0_4, cases_5_11, cases_12_17, cases_18_29, cases_30_39, cases_40_49, cases_50_59, cases_60_69, cases_70_79, cases_80))

# Sort the data for each year and select the five states with the lowest cases
lowest_cases <- state_year_cases %>%
    group_by(year) %>%
    arrange(total_cases) %>%
    slice_head(n = 5)

# Display the results
print(lowest_cases)

# Create a bar plot
p <- ggplot(lowest_cases, aes(x = reorder(state, total_cases), y = total_cases, fill = state)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = total_cases), vjust = -0.3) +
    facet_wrap(~year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "State", y = "Number of Cases", title = "States with the Lowest Number of COVID-19 Cases by Year")

# Save the image with a width of 10
ggsave("10.png", p, width = 10)

# Question 11: Based on the number of COVID-19 cases in Malaysia, create an analysis to visualize the relationship between child+adolescent and adult from 2020 to 2023. 

# Calculate the number of cases for child+adolescent and adult
covid_data$cases_child_adolescent <- covid_data$cases_child + covid_data$cases_adolescent
covid_data$cases_adult <- covid_data$cases_18_29 + covid_data$cases_30_39 + covid_data$cases_40_49 + covid_data$cases_50_59 + covid_data$cases_60_69 + covid_data$cases_70_79 + covid_data$cases_80

# Filter the data for Malaysia and limit it to the years 2020 to 2023
malaysia_data <- covid_data %>%
    filter(state == "Malaysia" & year %in% c(2020, 2021, 2022, 2023))

# Create a scatter plot and add a linear regression line
p <- ggplot(malaysia_data, aes(x = cases_child_adolescent, y = cases_adult)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(x = "Number of Cases (Child + Adolescent)", y = "Number of Cases (Adult)", title = "Relationship between Child+Adolescent and Adult COVID-19 Cases in Malaysia (2020-2023)")
ggsave("11.png", p, width = 10)

# Perform linear regression analysis
model <- lm(cases_adult ~ cases_child_adolescent, data = malaysia_data)
summary(model)

#Question 12: Imagine that you are an industrial player in the healthcare industry. Based on the given COVID-19 dataset, create an analysis to show that the analysis can help in supporting a business decision.
# Calculate the total number of cases for each age group
age_cases <- covid_data %>%
    summarise(
        cases_0_4 = sum(cases_0_4),
        cases_5_11 = sum(cases_5_11),
        cases_12_17 = sum(cases_12_17),
        cases_18_29 = sum(cases_18_29),
        cases_30_39 = sum(cases_30_39),
        cases_40_49 = sum(cases_40_49),
        cases_50_59 = sum(cases_50_59),
        cases_60_69 = sum(cases_60_69),
        cases_70_79 = sum(cases_70_79),
        cases_80 = sum(cases_80)
    ) %>%
    gather(age_group, cases)

# Create a bar plot
p <- ggplot(age_cases, aes(x = reorder(age_group, -cases), y = cases, fill = age_group)) +
    geom_bar(stat = "identity") +
    labs(x = "Age Group", y = "Number of Cases", title = "COVID-19 Cases by Age Group")
ggsave("12.png", p, width = 10)