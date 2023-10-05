#Data Visvalization for Happiness Report
library(ggplot2)
library(corrgram)
library(corrplot)

#Lets start with the basics
summary(db)

#Lets graph the mean data of all continents
ggplot(hp, aes(x=Continent, y=Happiness.Score, fill = Continent)) + geom_bar(stat="Identity") + ggtitle("Happiness Score of Each Continent") + ylab("Happiness Score") + xlab("Continent")

#Lets find out the coorelation in variables
col <- sapply(db, is.numeric)
cor.data <- cor(db[, col])
corrplot(cor.data, method ="square", type="upper")
corrplot(cor.data, method ="number", type="upper")

#Lets create boxplots region wise
box <- ggplot(db, aes(x=Region, y=Happiness.Score, color = Region))
box + geom_boxplot() + geom_jitter(aes(color=Country), size=1.0) + ggtitle("Happiness score for Regions and Countries") + coord_flip() + theme(legend.position = "none")

# box Plot for continents
ggplot(db, aes(x=Continent, y=Happiness.Score, color = Continent)) + geom_boxplot() + ggtitle("Happiness Score for Continents")

#Regression for all continents

ggplot(db, aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
  geom_point(aes(color = Continent), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = Continent), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ Continent) + theme_bw()
  ggtitle("Scatter Plot for Life Expectancy")

#Economy

ggplot(db, aes(x = Economy..GDP.per.Capita. , y = Happiness.Score)) +
  geom_point(aes(color = Continent), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = Continent), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ Continent) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

#Freedom
ggplot(db, aes(x = Freedom , y = Happiness.Score)) +
  geom_point(aes(color = Continent), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = Continent), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ Continent) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

#for family
ggplot(db, aes(x = Economy..GDP.per.Capita. , y = Happiness.Score)) +
  geom_point(aes(color = Continent), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = Continent), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ Continent) + theme_bw() +
  ggtitle("Scatter Plot for family")

#for trust in Government
ggplot(db, aes(x = Trust..Government.Corruption. , y = Happiness.Score)) +
  geom_point(aes(color = Continent), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = Continent), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ Continent) + theme_bw() +
  ggtitle("Scatter Plot for Trust in Government")

#plots were for the whole continent
#plot for the most unhappiest place, noticed in box plots
box <- ggplot(db, aes(x=Region, y=Happiness.Score, color = Region))
box + geom_boxplot() + geom_jitter(aes(color=Country), size=1.0) + ggtitle("Happiness score for Regions and Countries") + coord_flip() + theme(legend.position = "none")

#sub saharan Africa is the most unhappiest region
#classify all of the data based on happiest neutral and less happy region

db$happinessmeter <- NA
db$happinessmeter[which(db$Region %in% c("Australia and New Zealand", "Western Europe", "North America"))] <- "Happiest"
db$happinessmeter[which(db$Region %in% c("Sub-Saharan Africa"))] <- "Least Happiest"
db$happinessmeter[which(db$Region %in% c("Southern Asia", "Southeastern Asia", "Middle East and Northern Africa", "Latin America and Caribbean", "Eastern Asia", "Central and Eastern Europe"))] <- "Neutral" 

# plot regression for all three region

ggplot(db, aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
  geom_point(aes(color = happinessmeter), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = happinessmeter), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ happinessmeter) + theme_bw()
ggtitle("Scatter Plot for Life Expectancy")

#plot for econonmy
ggplot(db, aes(x = Economy..GDP.per.Capita. , y = Happiness.Score)) +
  geom_point(aes(color = happinessmeter), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = happinessmeter), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ happinessmeter) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

#for family
ggplot(db, aes(x = Family , y = Happiness.Score)) +
  geom_point(aes(color = happinessmeter), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = happinessmeter), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ happinessmeter) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

#for freedom
ggplot(db, aes(x = Freedom , y = Happiness.Score)) +
  geom_point(aes(color = happinessmeter), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = happinessmeter), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ happinessmeter) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

#for Generosity
ggplot(db, aes(x = Generosity , y = Happiness.Score)) +
  geom_point(aes(color = happinessmeter), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = happinessmeter), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ happinessmeter) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

#for Dystopia.Residual
ggplot(db, aes(x = Dystopia.Residual , y = Happiness.Score)) +
  geom_point(aes(color = happinessmeter), size = 3, alpha = 0.8) +
  geom_smooth(aes( color = happinessmeter), method = "lm", fullrange = TRUE) +  
  facet_wrap(~ happinessmeter) + theme_bw() +
  ggtitle("Scatter Plot for Life Expectancy")

# plot the GDP and Health Expectancy on world map

#we will install rworldmap package

library(rworldmap)
d <- data.frame(country = db$Country, value = db$Economy..GDP.per.Capita.)
n <- joinCountryData2Map(d, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData(n, nameColumnToPlot = "value", mapTitle = "World Map for GDP 2015", colourPalette = "terrain")

#Map for Health Expectanancy
d <- data.frame(country = db$Country, value = db$Health..Life.Expectancy.)
n <- joinCountryData2Map(d, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData(n, nameColumnToPlot = "value", mapTitle = "World Map for Health Expectancy", colourPalette = "terrain")



