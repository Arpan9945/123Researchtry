library(readxl)
library(dplyr)
library(tidyr)

#Importing the necessary dataset
fertility <- read_excel("data/fertility.xls")
laborforce <- read_excel("data/laborforce.xls")

#creating transpose so that we have country-wise, and, year-wise data
fertility$`Indicator Name` <- NULL
fertility$`Indicator Code` <- NULL

fertility_new <- fertility %>%
  pivot_longer(cols = !c(`Country Name`, `Country Code`), names_to = 'year', values_to = 'Fertility_rate')

#same thing for female labor force participation
laborforce$`Indicator Name` <- NULL
laborforce$`Indicator Code` <- NULL

laborforce_new <- laborforce %>%
  pivot_longer(cols = !c(`Country Name`, `Country Code`), names_to = 'year', values_to = 'LFP')

#merging the two datasets
merged_data <- inner_join(fertility_new, laborforce_new, by = c ("Country Code", "year"))

#rename the column-name
colnames(merged_data)[colnames(merged_data) == "Country Name.x"] <- "Country Name"

merged_data <- merged_data %>%
  select(-"Country Name.y")

#Let's keep only the countries that are necessary
merged_data <- merged_data %>%
  filter(`Country Code` %in% c("PAK", "NPL", "BGD", "KOR", "SYR", "JOR", "LSO", "LKA", 
                                "SDN", "THA", "FJI", "MYS", "DOM", "KEN", "MEX", "IDN", 
                                "PER", "GUY", "TTO", "COL", "PRY", "CRI", "PAN", "PHL", 
                                "HTI", "VEN", "JAM"))

#Now, I think the data before 1980s are not relevant for my study, I'll remove them
#First I need to convert the year into integer

merged_data$year <- as.integer(merged_data$year)

merged_data <- merged_data %>%
  filter(year >= 1990)

ggplot(data = merged_data, aes(x = year, y = LFP, colour = `Country Name`))+
  geom_line(size = 1)+
  theme_minimal()

#The graph is not informative enough, so categorise it based on son-preference group
merged_data <- merged_data %>%
  mutate(category = case_when(
    `Country Code` %in% c("PAK", "NPL", "BGD", "KOR", "SYR", "JOR")~ 1,
    `Country Code` %in% c("LSO", "LKA", "SDN", "THA", "FJI", "MYS", "DOM", "MEX")~ 2,
    `Country Code` %in% c("KEN", "IDN","PER", "GUY", "TTO", "COL", "PRY", "CRI", "PAN", "PHL", "HTI") ~ 3,
    `Country Code` %in% c("VEN", "JAM") ~ 4,
    TRUE ~ 5
  ))

#Now I will see what facet grid plot looks like

ggplot(merged_data, aes(x = year, y = `Fertility_rate`, colour = `Country Name`)) +
  geom_line()+
  geom_point()+
  facet_wrap(~category, scales = "free_y")
#Not very informative, now I will create new data frame for each categories

strong_son <- merged_data %>%
  filter(category == 1)

#first let me create the data into long format

strong_son_long <- strong_son %>%
  gather("variables", "values", Fertility_rate, LFP)

ggplot(strong_son, aes(x = year, y = `Fertility_rate`, colour = `Country Name`)) +
  geom_line()+
  geom_point()+
  facet_wrap(~`Country Name`, scales = "free_y")


ggplot(strong_son_long, aes(x = year, y = values, colour = `Country Name`, group = variables)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ `Country Name`, scales = "free_y")

  scale_colour_manual(values = c("blue", "gre
                                 en"), labels = c("Fertility Rate", "Labor Force Participation")) +  # Custom colors
  labs(title = "Fertility Rate and Labor Force Participation Over Time", 
       x = "Year", 
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "top")


  



