# COVID-19 Dataset Challenge 
# Maria Ren

# Import libraries
library(dplyr)

# Process a couple of data documents that are not in the 
# same format as others

# Vegetal Product for Food Supply (kg)
food_kg_vegetal <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_vegetal_prod_unprocessed.csv")
food_kg_vegetal <- food_kg_vegetal %>% select("Area", "Value") 
colnames(food_kg_vegetal)[which(names(food_kg_vegetal) == "Value")] <- "Vegetal Products" 
colnames(food_kg_vegetal)[which(names(food_kg_vegetal) == "Area")] <- "Country"
food_kg_vegetal <- dplyr::group_by(food_kg_vegetal, Country) %>% dplyr::summarise_all(sum)
write.csv(food_kg_vegetal, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_vegetal_prod.csv", row.names = FALSE)

# Animal Product for Food Supply (kg)
food_kg_animal <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_animal_unprocessed.csv")
food_kg_animal <- food_kg_animal %>% select("Area", "Value")
colnames(food_kg_animal)[which(names(food_kg_animal) == "Value")] <- "Animal Products" 
colnames(food_kg_animal)[which(names(food_kg_animal) == "Area")] <- "Country"
food_kg_animal <- dplyr::group_by(food_kg_animal, Country) %>% dplyr::summarise_all(sum)
write.csv(food_kg_animal, "/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/Unprocessed_Data/FAOSTAT_food_kg_animal.csv", row.names = FALSE)

# Obesity Data
obesity <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/obesity_undernourished/FAOSTAT_percent_obesity.csv")
obesity <- obesity %>% select(c("Area", "Value"))
colnames(obesity) <- c("Country", "Obesity")

# Undernourished Data
undernourished <- read.csv("/Users/mariaren/Desktop/COVID_Dataset_Challenge/Data/obesity_undernourished/FAOSTAT_percentage_under_nourished.csv")
undernourished <- undernourished %>% select(c("Area", "Value"))
colnames(undernourished) <-  c("Country", "Undernourished")


# Helpful function for replacing blanks with NA
replace_na <- function(x) gsub("^$|^ $", NA, x)

# Helpful function to convert all data to percentages
percentage <- function(data) {
  data[,2:24] <- sweep(data[,2:24], MARGIN=1, FUN="/",STATS=rowSums(data[,2:24]))
  data[,2:24] <- sweep(data[,2:24], MARGIN=1, FUN="*",STATS=100)
  data[,2:24] <- round(data[,2:24], digits = 4)
  return(data)
}

# Helpful function to process the csv files 
process_csv <- function(file) {
  csv <- read.csv(file)
  value_colname <- as.character(csv$Item[1])
  colnames(csv)[which(names(csv) == "Value")] <-  value_colname
  colnames(csv)[which(names(csv) == "Area")] <- "Country"
  select_columns <- csv %>% select("Country", value_colname) 
  return(select_columns)
}


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

# Create a "Total" column for the sum across each row
food_supply_kg_data <- food_supply_kg_data %>% mutate(Total = rowSums(.[2:24]))

# Add obesity and undernourished data
food_supply_kg_data <- merge(food_supply_kg_data, obesity, by = "Country", all.x = TRUE)
food_supply_kg_data <- merge(food_supply_kg_data, undernourished, by = "Country", all.x = TRUE)
food_supply_kg_data$Undernourished <- replace_na(food_supply_kg_data$Undernourished)

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

# Create a "Total" column for the sum across each row
food_supply_kcal_data <- food_supply_kcal_data %>% mutate(Total = rowSums(.[2:24]))

# Add obesity and undernourished data
food_supply_kcal_data <- merge(food_supply_kcal_data, obesity, by = "Country", all.x = TRUE)
food_supply_kcal_data <- merge(food_supply_kcal_data, undernourished, by = "Country", all.x = TRUE)
food_supply_kcal_data$Undernourished <- replace_na(food_supply_kcal_data$Undernourished)

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

# Create a "Total" column for the sum across each row
protein_data <- protein_data %>% mutate(Total = rowSums(.[2:24]))

# Add obesity and undernourished data
protein_data <- merge(protein_data, obesity, by = "Country", all.x = TRUE)
protein_data <- merge(protein_data, undernourished, by = "Country", all.x = TRUE)
protein_data$Undernourished <- replace_na(protein_data$Undernourished)

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

# Create a "Total" column for the sum across each row
fat_data <- fat_data %>% mutate(Total = rowSums(.[2:24]))

# Add obesity and undernourished data
fat_data <- merge(fat_data, obesity, by = "Country", all.x = TRUE)
fat_data <- merge(fat_data, undernourished, by = "Country", all.x = TRUE)
fat_data$Undernourished <- replace_na(fat_data$Undernourished)

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





# Trial
fat_data2 <- fat_data
fat_data2[,2:24] <- sweep(fat_data2[,2:24], MARGIN=1, FUN="/",STATS=rowSums(fat_data2[,2:24]))
fat_data2[,2:24] <- sweep(fat_data2[,2:24], MARGIN=1, FUN="*",STATS=100)
fat_data2[,2:24] <- round(fat_data2[,2:24], digits = 4)
rowSums(fat_data2[,2:24])
fat_data2 <- percent(fat_data2[1, 2:24])
fat_data2


