# Data Analysis on FIFA Game 
install.packages("ggthemes", repos = "https://cran.rstudio.com") #installing package ggthemes
###Loading all packages
library(data.table) # Imprting data
library(tidyverse) # Data manupulation
library(tibble) # used to create tibbles
library(tidyr) # used to tidy up data
library(dplyr) # used for data manipulation
library(DT) # used for datatable function for displaying dataset
library("ggthemes") #for using themes for plots

fifa19 <- as_tibble(fread("C:/Users/ajayb/OneDrive/Desktop/data.csv"))
######Data Cleaning  process
#removing million character 'M'from player's value
fifa19$Value <- gsub(".*¬", "", fifa19$Value)
fifa19$Value <- gsub("M$", "", fifa19$Value) 
#removing thousand character 'K' from wage
fifa19$Wage <- gsub(".*¬", "", fifa19$Wage) 
fifa19$Wage <- gsub("K$", "", fifa19$Wage) 
#Top 11 players
fifa19 %>% 
  arrange(-Overall) %>% 
  top_n(11,wt = Overall) %>% 
  select( Name, Age,Overall,Club,`Value in Million Euros`,`PreferedPosition1`) %>% datatable(options = list(scrollX = TRUE, pageLength = 11))
#Converting into a dataframe
fifa19 <- as.data.frame(fifa19)
for (i in 14:47) # Converting columns with player attributes to numeric
{
  fifa19[,i] <- sub("\\+.*", "", fifa19[,i])
  fifa19[,i] <- sub("\\-.*", "", fifa19[,i])
  
}
fifa19 <- as_tibble(fifa19)   #Converting back to tibble
colnames(fifa19)[11] <- "Value in Million Euros"
colnames(fifa19)[12] <- "Wage in '000 Euros"
for (i in 11:47) # Converting columns with player attributes to numeric
{
  fifa19[,i] <- as.numeric(unlist(fifa19[,i] ))
}
names(fifa19)[64] <- "PreferedPosition" 
fifa19_v1 <- separate(fifa19, PreferedPosition, c("PreferedPosition1", "PreferedPosition2", "PreferedPosition"), sep = " ") # Splitting a player's prefered positions

colSums(is.na(fifa19_v1)) #To identify variables containing NULL values

fifa19_v1[is.na(fifa19_v1)] <- 0 #Replace all NA values with 0

colSums(is.na(fifa19_v1)) #Check if all NA values are converted to 0

(is.null(fifa19_v1)) #Check for NULL values

length(unique(fifa19_v1$ID)) #Calculating number of duplicates duplicates

fifa19_v2 <- fifa19_v1[!duplicated(fifa19_v1$ID),] # Removing duplicates
fifa19_final <- fifa19_v2
# ANalysis
# Top 11 players with their high wages   
fifa19_final %>% 
  arrange(-Wage) %>% 
  top_n(11,wt = Wage) %>% 
  select( Name, Age,Wage,Club,`Value in Million Euros`,`PreferedPosition1`) %>% datatable(options = list(scrollX = TRUE, pageLength = 11))

# top 10 players with high wage compared with potential
fifa19_final %>% group_by(PreferedPosition1) %>%
  arrange(-Wage) %>% 
  top_n(1,wt = Potential) %>% 
  select( `PreferedPosition1`,Potential,Name, Overall,Club,Nationality,Wage) %>% 
  datatable(options = list(scrollX = TRUE, pageLength = 10))

# The graph shows the how potantial is based on their wages.
ggplot(fifa19_final,aes(Wage, Potential)) +
  geom_point( size = 2, alpha = .9) + geom_jitter() + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))

# Varations with age
ggplot(fifa19_final) +
  geom_tile(aes(Wage, Potential, fill = Age)) + 
  scale_fill_distiller(palette = "Spectral") + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))
