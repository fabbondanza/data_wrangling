if (!require('tidyverse')) install.packages('tydiverse'); library(tidyverse)
if (!require('data.table')) install.packages('data.table'); library(data.table)

#TODO
# Get dictionary and 
# Replace empty string to NA
# BE needs to export data with following format phenotypeName-dataset-instance-array.
# Otherwise we lose where the pheontypes came from

path <- "~/Documents/Lifebit/GEL data/LabKey/"
file <- "cb_phenotype_data.csv"
DD <- "cb_phenotype_metadata.csv"
out_name <- 'out.csv'

setwd(path)

# Functions ####
get_columns_to_pivot <- function(mydata){

  # Get columns to pitvot ie. array >1. Exclude participant id
  
  working_data <- as.data.frame(colnames(mydata))
  colnames(working_data) <- "V1"
  working_data <- working_data %>%
    filter(V1 != "i") %>%
    separate(data = ., col = V1, sep = '[.]' , into = c("phenotype", "array")) %>%
    group_by(phenotype) %>%
    summarise(max_array = n()) %>%
    filter(max_array > 1) %>%
    select(phenotype)
  
  return(working_data$phenotype)
}

# Main script ####

data <- fread(file)

phenotypes_with_arrays <- get_columns_to_pivot(data)

for (i in phenotypes_with_arrays) {
  message(paste0('Pheontypes with arrays: ', gsub('-0','',i) )) 
}

# Make subset of data with no arrays
data_no_arrays <- data %>%
  select(c(i, !starts_with(phenotypes_with_arrays))) %>% # Get data
  rename(participant_id = i) # Update participant id name
colnames(data_no_arrays) <- gsub('-0.0','', colnames(data_no_arrays)) # Update names


# Pivot phenotypes with arrays
data_to_pivot <- data %>%
  select(c(i, starts_with(phenotypes_with_arrays))) %>%
  purrr::discard(., ~all(is.na(.))) %>% # Remove empty columns, just a sanity check
  rename(participant_id = i) %>%
  mutate_all(as.character) %>% # To avoid data incompabilities
  pivot_longer(-participant_id) %>% # Pivto data
  separate(name, into = c('temp','array'),sep = "\\.") %>% # Create a list of arrays to then spread the data
  pivot_wider(names_from = temp, values_from = value) %>%
  select(-array) %>%
  unnest(everything()) # Data is returned as a dataframe of list
colnames(data_to_pivot) <- gsub('-0','', colnames(data_to_pivot)) # Update names

# Merge the data
data.table::fwrite(left_join(data_no_arrays, data_to_pivot, by = 'participant_id'), out_name, sep=',')
message('Script finish')