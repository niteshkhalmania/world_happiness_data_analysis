library(dbplyr)
#data manuplation for the project

#creating/calling database

db <- read.csv("world-happiness-data.csv")

View(db)
#Lets consider data does not have any error

db <- db[,-5]

#Lets Create a new column continent for Classification
db$Continent<- NA

#Australia
db$Continent[which(db$Region %in% c("Australia and New Zealand"))] <- "Australia"
#North America
db$Continent[which(db$Region %in% c("North America"))] <- "North America"
#Europe
db$Continent[which(db$Region %in% c("Western Europe", "Central and Eastern Europe"))] <- "Europe"
#Africa
db$Continent[which(db$Region %in% c("Sub-Saharan Africa","Middle East and Northern Africa"))] <- "Africa"
#Asia
db$Continent[which(db$Region %in% c("Eastern Asia","Southern Asia", "Southeastern Asia"))] <- "Asia"
db$Continent[which(db$Region %in% c("Latin America and Caribbean"))] <- "South America"

# Calculate the average of numerical columns grouped by Continent
hp <- aggregate(. ~ Continent, data = db[, c("Happiness.Score", "Health..Life.Expectancy.", "Freedom", "Trust..Government.Corruption.", "Generosity", "Dystopia.Residual", "Continent")], mean)

# View the aggregated data
View(hp)


 