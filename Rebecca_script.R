# Lab 3A
# Rebecca Sebastian  
# Starting with Data

# EvoGenomics-course exercise
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")
surveys <- read.csv("data/portal_data_joined.csv")
surveys
head (surveys)
library(tidyverse)
surveys <- read.csv("data/portal_data_joined.csv")
surveys
head (surveys)
view (surveys)
str (surveys)

##Size
dim (surveys)
nrow (surveys)
ncol (surveys)

##Content
head (surveys)
tail (surveys)

##Names
names (surveys)
rownames (surveys)

##summary
str(surveys)
summary (surveys)

##Challenge Answer
## class: data.frame
## number of rows: 34786, number of columns: 13
## number of species: 48

surveys[1, 1]
surveys [1, 6]
surveys [, 1]
surveys [1]
surveys [1:3, 7]
surveys["species_id"]
surveys[, "species_id"]

##Challenge Answer
##1. 
surveys_200 <- surveys[200, ]
##2.
surveys [34786,]
tail (surveys)
surveys[nrow(surveys),]
surveys_last <- surveys[nrow(surveys),]
surveys_last
##3.
middle <-  nrow(surveys) / 2
middle

surveys_middle <- surveys[nrow(middle),]

##4
surveys[-(7:nrow(surveys)),]
  
## Factors Challenge
sex <- surveys[,7]
levels(sex)[1:3] <- c("undetermined", "female","male")

sex <- factor(sex, levels = c("female","male", "undetermined"))
plot(sex)

## challenge data frame
animal_data <- data.frame(
  animal = c("dog", "cat", "sea cucumber","sea urchin"),
  feel = c("furry", "squishy", "spiny","spiky"),
  weight = c(45, 8, 1.1, 0.8))

country_climate <- data.frame(
  country = c("Canada", "Panama", "South Africa", "Australia"),
  climate = c("cold", "hot", "temperate", "hot/temperate"),
  temperature = c(10, 30, 18, "15"),
  northern_hemisphere = c(TRUE, TRUE, FALSE, "FALSE"),
  has_kangaroo = c(FALSE, FALSE, FALSE, 1))
  
class(country_climate$country)
class(country_climate$climate)
class(country_climate$temperature)
class(country_climate$northern_hemisphere)
class(country_climate$has_kangaroo)

## No,because countries could be a string, temperature should ideally
## be a numeric value and "has_kangaroo" could be logical

country_climate <- data.frame(
  country = c("Canada", "Panama", "South Africa", "Australia"),
  climate = c("cold", "hot", "temperate", "hot/temperate"),
  temperature = c(10, 30, 18, "15"),
  northern_hemisphere = c(TRUE, TRUE, FALSE, "FALSE"),
  has_kangaroo = c(FALSE, FALSE, FALSE, 1), stringsAsFactors = FALSE)

class(country_climate$country)
class(country_climate$climate)
class(country_climate$temperature)
class(country_climate$northern_hemisphere)
class(country_climate$has_kangaroo)

# all factors became charactor!!

country_climate <- data.frame(
  country = c("Canada", "Panama", "South Africa", "Australia"),
  climate = c("cold", "hot", "temperate", "hot/temperate"),
  temperature = c(10, 30, 18, 15),
  northern_hemisphere = c(TRUE, TRUE, FALSE, FALSE),
  has_kangaroo = c(FALSE, FALSE, FALSE, TRUE))

class(country_climate$country)
class(country_climate$climate)
class(country_climate$temperature)
class(country_climate$northern_hemisphere)
class(country_climate$has_kangaroo)