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
