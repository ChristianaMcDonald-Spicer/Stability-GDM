#####Plot a histogram of AUC values of maxent models from SDM stability loop

#read in species list
data_file           <- read.csv('C:/SDM_Stability_models/Data/KW_data_rounded_to_0.041_for_maxent.csv')
list_species        <- unique(data_file$species)
list_species
list_species_subset <- list_species[287:335]
list_species_subset


#file path
model_folder        <- "F:/SDM Stability models/"

#create df
df                  <- data.frame(Species=character(),
                        AUC_value=numeric(), 
                        stringsAsFactors=FALSE) 

#loop that:
for (species in list_species) {
  #open folder
  maxent_folder     <-  paste(model_folder, "maxent.output_", species, '/', sep="")
  
  #open csv file
  file_name         <- paste(maxent_folder, "maxentResults.csv", sep="")
  
  #check if the file exists before continuing
  if (file.exists(file_name))
    maxent_results    <- read.csv(file_name)
  
    #read AUC value
    auc_value         <- maxent_results$Training.AUC[1]
  
    #create row in df and paste data in
    df[nrow(df)+1, ]  <- c(species, auc_value)

}
  
#View the dataframe
head(df)
View(df)

df_numeric <- transform(df, AUC_value = as.numeric(AUC_value))

#plot histogram of auc values
hist(df_numeric$AUC_value)
  
