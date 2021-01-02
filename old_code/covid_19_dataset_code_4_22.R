# COVID-19 Dataset Challenge 
# Maria Ren

# Import libraries
library(dplyr)
library(data.table)

#Automatic Update Links
covid_19_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-21-2020.csv"

# Process a couple of data documents that are not in the 
# same format as others

# Obesity Data
obesity <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/other_statistics/FAOSTAT_percent_obesity.csv")
obesity <- obesity %>% select(c("Area", "Value"))
colnames(obesity) <- c("Country", "Obesity")

# Undernourished Data
undernourished <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/other_statistics/FAOSTAT_percentage_under_nourished.csv")
undernourished <- undernourished %>% select(c("Area", "Value"))
colnames(undernourished) <-  c("Country", "Undernourished")

# Helpful function for replacing blanks with NA
replace_na <- function(x) gsub("^$|^ $", NA, x)

# Helpful function to update naming
update_name <- function(data, old, new) {
  levels(data$Country)[levels(data$Country)== old] <- new
}

# Helpful function to process the csv files 
process_csv <- function(file) {
  csv <- read.csv(file)
  value_colname <- as.character(csv$Item[1])
  colnames(csv)[which(names(csv) == "Value")] <-  value_colname
  colnames(csv)[which(names(csv) == "Area")] <- "Country"
  select_columns <- csv %>% select("Country", value_colname) 
  levels(select_columns$Country)[levels(select_columns$Country)== "Bolivia (Plurinational State of)"] <- "Bolivia"
  levels(select_columns$Country)[levels(select_columns$Country)== "China, Taiwan Province of"] <- "Taiwan*"
  levels(select_columns$Country)[levels(select_columns$Country)== "Viet Nam"] <- "Vietnam"
  levels(select_columns$Country)[levels(select_columns$Country)== "Côte d'Ivoire"] <- "Cote d'Ivoire"
  # levels(select_columns$Country)[levels(select_columns$Country)== "China, Hong Kong SAR"] <- "China"
  # levels(select_columns$Country)[levels(select_columns$Country)== "China, Macao SAR"] <- "China"
  # levels(select_columns$Country)[levels(select_columns$Country)== "China, mainland"] <- "China"
  # levels(select_columns$Country)[levels(select_columns$Country)== "United States of America"] <- "US"
  # levels(select_columns$Country)[levels(select_columns$Country)== "Côte d'Ivoire"] <- "Cote d'Ivoire"
  levels(select_columns$Country)[levels(select_columns$Country)== "Democratic People's Republic of Korea"] <- "Korea, North"
  levels(select_columns$Country)[levels(select_columns$Country)== "Republic of Korea"] <- "Korea, South"
  # levels(select_columns$Country)[levels(select_columns$Country)== "Iran (Islamic Republic of)"] <- "Iran"
  # levels(select_columns$Country)[levels(select_columns$Country)== "Lao People's Democratic Republic"] <- "Laos"
  # levels(select_columns$Country)[levels(select_columns$Country)== "Republic of Moldova"] <- "Moldova"
  # levels(select_columns$Country)[levels(select_columns$Country)== "Russian Federation"] <- "Russia"
  # levels(select_columns$Country)[levels(select_columns$Country)== "United Republic of Tanzania"] <- "Tanzania"
  # levels(select_columns$Country)[levels(select_columns$Country)== "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
  china_rows <- which(select_columns$Country %like% "China,")
  china <- select_columns[china_rows,]
  select_columns[which(select_columns$Country == "China"),][2] <- sum(china[,2])
  select_columns <- select_columns[-china_rows,]
  return(select_columns)
}

process_extra_csv <- function(csv) {
  levels(csv$Country)[levels(csv$Country)== "Bolivia (Plurinational State of)"] <- "Bolivia"
  levels(select_columns$Country)[levels(select_columns$Country)== "Viet Nam"] <- "Vietnam"
  levels(csv$Country)[levels(csv$Country)== "China, Taiwan Province of"] <- "Taiwan"
  levels(csv$Country)[levels(csv$Country)== "Democratic People's Republic of Korea"] <- "Korea, North"
  levels(csv$Country)[levels(csv$Country)== "Republic of Korea"] <- "Korea, South"
  china <- csv[csv$Country %like% "China,",]
  csv[which(csv$Country == "China"),][2] <- sum(china[,2])
  csv <- csv[-c(32:34),]
  return(csv)
}


# Helpful function to convert all data to percentages
percentage <- function(data) {
  data[,2:24] <- sweep(data[,2:24], MARGIN=1, FUN="/",STATS=rowSums(data[,2:24]))
  data[,2:24] <- sweep(data[,2:24], MARGIN=1, FUN="*",STATS=100)
  data[,2:24] <- round(data[,2:24], digits = 4)
  return(data)
}

# Vegetal Product for Food Supply (kg)
food_kg_vegetal <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_vegetal_prod_unprocessed.csv")
food_kg_vegetal <- food_kg_vegetal %>% select("Area", "Value") 
colnames(food_kg_vegetal)[which(names(food_kg_vegetal) == "Value")] <- "Vegetal Products" 
colnames(food_kg_vegetal)[which(names(food_kg_vegetal) == "Area")] <- "Country"
food_kg_vegetal <- dplyr::group_by(food_kg_vegetal, Country) %>% dplyr::summarise_all(sum)
write.csv(food_kg_vegetal, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_vegetal_prod.csv", row.names = FALSE)
food_kg_vegetal <- process_extra_csv(food_kg_vegetal)

# Animal Product for Food Supply (kg)
food_kg_animal <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_animal_unprocessed.csv")
food_kg_animal <- food_kg_animal %>% select("Area", "Value")
colnames(food_kg_animal)[which(names(food_kg_animal) == "Value")] <- "Animal Products" 
colnames(food_kg_animal)[which(names(food_kg_animal) == "Area")] <- "Country"
food_kg_animal <- dplyr::group_by(food_kg_animal, Country) %>% dplyr::summarise_all(sum)
write.csv(food_kg_animal, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_animal.csv", row.names = FALSE)
food_kg_animal <- process_extra_csv(food_kg_animal)


# Food Supply Quantity (kg) dataset

setwd("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Food_Supply_Quantity_Kg")
fileNames1 <- Sys.glob("*.csv")


food_supply_kg_data <- process_csv(fileNames1[1])
fileNames1 <- tail(fileNames1, -1)
for (file in fileNames1) {
    new_data <- process_csv(file)
    food_supply_kg_data <- full_join(food_supply_kg_data, new_data)
}

food_supply_kg_data <- full_join(food_supply_kg_data, food_kg_vegetal)
food_supply_kg_data <- full_join(food_supply_kg_data, food_kg_animal)
reorder_subset <- food_supply_kg_data[,c(2:24)]
reorder_subset <- reorder_subset[,order(colnames(reorder_subset))]
food_supply_kg_data <- cbind(food_supply_kg_data$Country, reorder_subset)
colnames(food_supply_kg_data)[1] <- "Country"

# Treating NA values as 0 - this will make it easier for percentage
# calculations at the end.
food_supply_kg_data[is.na(food_supply_kg_data)] <- 0

# Add obesity and undernourished data
food_supply_kg_data <- merge(food_supply_kg_data, obesity, by = "Country", all.x = TRUE)
food_supply_kg_data <- merge(food_supply_kg_data, undernourished, by = "Country", all.x = TRUE)
food_supply_kg_data$Undernourished <- replace_na(food_supply_kg_data$Undernourished)

# Format into percentages (keep all units the same across)
food_supply_kg_data <- percentage(food_supply_kg_data)

# Write the output file
write.csv(food_supply_kg_data, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Cleaned_Datasets/Food_Supply_Quantity_kg_Data.csv", row.names = FALSE)



# Food Supply (kcal) Dataset

setwd("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Food_Supply_Kcal")
fileNames2 <- Sys.glob("*.csv")

food_supply_kcal_data <- process_csv(fileNames2[1])
fileNames2 <- tail(fileNames2, -1)
for (file in fileNames2) {
  new_data <- process_csv(file)
  food_supply_kcal_data <- full_join(food_supply_kcal_data, new_data)
}

# Treating NA values as 0 - this will make it easier for percentage
# calculations at the end.
food_supply_kcal_data[is.na(food_supply_kcal_data)] <- 0

# Add obesity and undernourished data
food_supply_kcal_data <- merge(food_supply_kcal_data, obesity, by = "Country", all.x = TRUE)
food_supply_kcal_data <- merge(food_supply_kcal_data, undernourished, by = "Country", all.x = TRUE)
food_supply_kcal_data$Undernourished <- replace_na(food_supply_kcal_data$Undernourished)

# Format into percentages (keep all units the same across)
food_supply_kcal_data <- percentage(food_supply_kcal_data)

# Write the output file.
write.csv(food_supply_kcal_data, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Cleaned_Datasets/Food_Supply_kcal_Data.csv", row.names = FALSE)



# Protein Supply Quantity Dataset

setwd("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Protein_Supply")
fileNames3 <- Sys.glob("*.csv")

protein_data <- process_csv(fileNames3[1])
fileNames3 <- tail(fileNames3, -1)
for (file in fileNames3) {
  new_data <- process_csv(file)
  protein_data <- full_join(protein_data, new_data)
}


# Treating NA values as 0 - this will make it easier for percentage
# calculations at the end.
protein_data[is.na(protein_data)] <- 0

# Add obesity and undernourished data
protein_data <- merge(protein_data, obesity, by = "Country", all.x = TRUE)
protein_data <- merge(protein_data, undernourished, by = "Country", all.x = TRUE)
protein_data$Undernourished <- replace_na(protein_data$Undernourished)

# Format into percentages (keep all units the same across)
protein_data <- percentage(protein_data)

# Write the output file.
write.csv(protein_data, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Cleaned_Datasets/Protein_Supply_Quantity_Data.csv", row.names = FALSE)



# Fat Supply Quantity Dataset

setwd("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Fat_Supply")
fileNames4 <- Sys.glob("*.csv")

fat_data <- process_csv(fileNames4[1])
fileNames4 <- tail(fileNames4, -1)
for (file in fileNames4) {
  new_data <- process_csv(file)
  fat_data <- full_join(fat_data, new_data)
}

# Treating NA values as 0 - this will make it easier for percentage
# calculations at the end.
fat_data[is.na(fat_data)] <- 0

# Add obesity and undernourished data
fat_data <- merge(fat_data, obesity, by = "Country", all.x = TRUE)
fat_data <- merge(fat_data, undernourished, by = "Country", all.x = TRUE)
fat_data$Undernourished <- replace_na(fat_data$Undernourished)

# Format into percentages (keep all units the same across)
fat_data <- percentage(fat_data)

# Write the output file.
write.csv(fat_data, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Cleaned_Datasets/Fat_Supply_Quantity_Data.csv", row.names = FALSE)


# Data Descriptions File

description <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/FAOSTAT_data_descriptions.csv")
description <- description %>% select("Item.Group", "Item")
colnames(description) <- c("Categories", "Items")
combine <- function(x) paste(unique(x), collapse="; ")
description_cleaned <- aggregate(description, by = list(description$Categories), FUN = combine )
description_cleaned <- description_cleaned %>% select(c("Categories", "Items"))
description_cleaned <- description_cleaned[!(description_cleaned$Categories == "Grand Total"),]

# Write the output file.
write.csv(description_cleaned, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Cleaned_Datasets/Data_Descriptions.csv", row.names = FALSE)


# Grab COVID 19 death count data from JHU CSSE


covid_data <- fread(covid_19_link)
covid_data <- covid_data %>% select(c("Country_Region", "Confirmed", "Deaths", "Recovered", "Active"))
covid_data$Country_Region[which(covid_data$Country_Region == "Congo (Brazzaville)")] <-  "Congo"
covid_data$Country_Region[which(covid_data$Country_Region == "Congo (Kinshasa)")] <-  "Congo"
covid_data <- dplyr::group_by(covid_data, Country_Region) %>% dplyr::summarise_all(sum)
colnames(covid_data)[1] <- "Country"

# Helpful function to update naming
update_name <- function(old, new) {
  # levels(covid_data$Country)[levels(covid_data$Country)== old] <- new
  gsub(old, new, covid_data$Country)
}

# covid_data$Country <- update_name("Bolivia", "Bolivia (Plurinational State of)")
# covid_data$Country <- update_name("Taiwan", "China, Taiwan Province of")
# covid_data$Country <- update_name("Vietnam", "Viet Nam") 
# update_name("China", "China, Hong Kong SAR"
# update_name"China, Macao SAR"] <- "China"
# update_name"China, mainland"] <- "China"
covid_data$Country <- update_name("US", "United States of America")
# covid_data$Country <- update_name("Korea, North", "Democratic People's Republic of Korea")
covid_data$Country <- update_name("Iran", "Iran (Islamic Republic of)")
covid_data$Country <- update_name("Laos", "Lao People's Democratic Republic") 
covid_data$Country <- update_name("Moldova", "Republic of Moldova")
covid_data$Country <- update_name("Russia", "Russian Federation") 
covid_data$Country <- update_name("Tanzania", "United Republic of Tanzania")
covid_data$Country <- update_name("Venezuela", "Venezuela (Bolivarian Republic of)")

# Compare country names for better merging
a <- fat_data$Country
b <- covid_data$Country
anotb <- a[!a%in%b]
bnota <- b[!b%in%a]

trial <- merge(fat_data, covid_data, by = "Country", all.x = TRUE)

population_link <- "https://datacenter.prb.org/download/international/indicator/population/csv"
population_data <- fread(population_link)
population_data <- population_data %>% select("Name",  "Data")
colnames(population_data) <- c("Country", "Population")

# Helpful function to update naming
update_country_name <- function(old, new) {
  # levels(covid_data$Country)[levels(covid_data$Country)== old] <- new
  gsub(old, new, population_data$Country)
}
population_data$Country <- update_country_name("United States", "United States of America")
# covid_data$Country <- update_name("Korea, North", "Democratic People's Republic of Korea")
population_data$Country <- update_country_name("Iran", "Iran (Islamic Republic of)")
population_data$Country <- update_country_name("Laos", "Lao People's Democratic Republic") 
population_data$Country <- update_country_name("Moldova", "Republic of Moldova")
population_data$Country <- update_country_name("Russia", "Russian Federation") 
population_data$Country <- update_country_name("Tanzania", "United Republic of Tanzania")
population_data$Country <- update_country_name("Venezuela", "Venezuela (Bolivarian Republic of)")
population_data$Country <- update_country_name("Taiwan", "Taiwan*")
population_data$Country <- update_country_name("Bosnia-Herzegovina", "Bosnia and Herzegovina")
population_data$Country <- update_country_name("Cape Verde", "Cabo Verde")
population_data$Country <- update_country_name("eSwatini", "Eswatini")
population_data$Country <- update_country_name("St. Kitts-Nevis", "Saint Kitts and Nevis")
population_data$Country <- update_country_name("St. Vincent and the Grenadines", "Saint Vincent and the Grenadines")

trial2 <- merge(trial, population_data, by = "Country", all.x = TRUE)
# trial2$Country[which(is.na(trial2$Population))]
trial2[,31] <- trial2[,31]*10^6
trial2[,27:30] <- trial2[,27:30]/trial2[,31]*100
library(tibble)
unit <- rep("%", nrow(trial2))
trial2 <- cbind(unit, trial2)
trial2 <- trial2 %>% select(-unit, everything())
names(trial2)[32] <- "Unit (all except Population)"
