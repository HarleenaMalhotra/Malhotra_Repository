
title: "Malhotra_week_08"
author: "Harleena Malhotra"
date: "11/25/2016"


## Function that inputs 1 column from the WHO data frame and outputs a 3 column data frame
fix_who_column <- function(z){
  library(tidyverse)
  library(stringr)
  library(dplyr)


  z<-str_replace_all(z," ", "")
  
  ## Function to create lower bound column
  get_lower_bound <- function(x){
   x <- str_extract(x, "\\[[0-9]+\\-") %>% str_replace_all("\\[", "") %>% str_replace_all("-", "")
    x
  }

  ## Function to create upper bound column
  get_upper_bound <- function(x){
    x <- str_extract(x, "-[0-9]+\\]") %>% str_replace_all("-", "") %>% str_replace_all("\\]", "")
    x
  }

  ## Function to tidy maternal mortality rate column
  get_mr <- function(x){
    x <- str_extract(x, "[0-9]+\\[") %>% str_replace_all("\\[", "")
    x
  }

  mortality_rate <- 0
  lower_bound <- 0
  upper_bound <- 0
  clean_mr_df <- data.frame(mortality_rate, lower_bound, upper_bound)
  
  z <- data.frame(z)
  
  i<-1
  while(i<=nrow(z)){
    mortality_rate <- as.integer(get_mr(z[i,]))
    lower_bound <- as.integer(get_lower_bound(z[i,]))
    upper_bound <- as.integer(get_upper_bound(z[i,]))
    tempdf <- data.frame(mortality_rate, lower_bound, upper_bound)
    clean_mr_df <- rbind(clean_mr_df, tempdf)
    i <- i+1
  }
  clean_mr_df <- clean_mr_df[-1,]  
  return(clean_mr_df)
}



##Step 1:
##I imported the csv file on maternal mortality and observed the data. 

MMData <- read_csv("data/xmart.csv")
head(MMData)
glimpse(MMData)
summary(MMData)

Func_Return <- fix_who_column(MMData$MortalityRatio)




##Step 2:
I tidied the maternal mortality data by renaming the columns, removing any extra spaces/characters, and creating upper bound and lower bound columns. 

```{r, warning=FALSE}
## Rename columns
colnames(MMData) <- c("Country", "Year", "MortalityRatio", "BirthsAttended")

## Getting rid of spaces in between numbers (where they should have commas)
MMData$MortalityRatio <- str_replace_all(MMData$MortalityRatio, " ", "")

## Creating lower bound and upper bound columns
MMData$lower_bound <- ""
MMData$upper_bound <- ""

i<-1
while(i<=nrow(MMData)){
  
  MMData[i,]$lower_bound <- str_extract(MMData[i,][, "MortalityRatio"], "\\[[0-9]+\\-") %>% str_replace_all("\\[", "") %>% 
    str_replace_all("-", "")
  
  MMData[i,]$upper_bound <- str_extract(MMData[i,][, "MortalityRatio"], "-[0-9]+\\]") %>% str_replace_all("\\]", "") %>% 
    str_replace_all("-", "")
  
  MMData[i,]$MortalityRatio <- substr(MMData[i,]$MortalityRatio, 1, str_locate(MMData[i,]$MortalityRatio, "\\[") -1)
  
  ## Tidying year column
  if(grepl("-", MMData[i,]$Year)) {
    MMData <- rbind(MMData, c(MMData[i, ]$Country, substr(MMData[i,]$Year, 1,4), MMData[i, ]$MortalityRatio, MMData[i,]$BirthsAttended, MMData[i,]$upper_bound, MMData[i,]$lower_bound))
    MMData[i,]$Year <- substr(MMData[i,]$Year, 6,9)
  }
  
  i<-i+1
}

## Changing some of the columns to type integer or numeric. 
MMData$upper_bound <- as.integer(MMData$upper_bound)
MMData$lower_bound <- as.integer(MMData$lower_bound)
MMData$MortalityRatio <- as.integer(MMData$MortalityRatio)
MMData$Year <- as.integer(MMData$Year)
MMData$BirthsAttended <- as.numeric(MMData$BirthsAttended)
```

##Step 3:
I imported the csv file on gross national income and observed the data.

```{r, warning=FALSE}
## Import income data
IncomeData <- read_csv("data/IncomeData.csv", skip = 1)
head(IncomeData)
glimpse(IncomeData)
summary(IncomeData)
```

##Step 4:
I tidied the data so that there was only one entry per country per year. 

```{r, warning=FALSE}
## Wide to long
LongIncomeData <- gather(IncomeData, "Year", "Gross national income per capita", 2:25)
LongIncomeData$`Gross national income per capita` <- as.integer(str_replace_all(LongIncomeData$`Gross national income per capita`, " ", ""))
LongIncomeData$Year <- as.integer(LongIncomeData$Year)

```

##Step 5:
I merged the two files by country and year, retaining only rows for which I had per capita income and either maternal mortality or percentage attended births.

```{r, warning=FALSE}
## Removing rows from the income data frame that have NA values for the gross income per capita column
NewIncomeDat <- na.omit(LongIncomeData)

## Merging the two files by country and year
FinalDF <- inner_join(MMData, NewIncomeDat, by = c("Country" = "Country", "Year" = "Year"))
```

##Step 6:
I made two graphs, one relating income to maternal mortality and the other relating income to percentage attended births.

```{r, warning=FALSE}
## Changing column name for convenience 
colnames(FinalDF)[7] <- "GrossIncome"

## Graph 1:
ggplot() + geom_point(data = FinalDF, mapping = aes(GrossIncome, MortalityRatio)) + geom_smooth(data = FinalDF, mapping = aes(GrossIncome, MortalityRatio), se = FALSE) + ggtitle("Income vs Materal Mortality") + xlab("Income") + ylab("Maternal Mortality")
## According to this graph, the blue line shows a clear connection between income and mortality rate. The lower the income, the higher the mortality rate. As the income increases, the mortality rate greatly decreases. Therefore, income and maternal mortality are inversely proportional. 

##Graph 2:
ggplot() + geom_point(data = FinalDF, mapping = aes( BirthsAttended, GrossIncome)) + geom_smooth(data = FinalDF, mapping = aes( BirthsAttended, GrossIncome), se = FALSE) + ggtitle("Income vs Percentage Attended Births") + xlab("Income") + ylab("Percentage Births Attended")
## According to this graph, the percentage births attended increases as the income increases. Therefore, income and percentage births attended are directly proportional.                                                                                               
```

##Step 7:
I saved a csv file of the FinalDF created in Step 5.

```{r, warning=FALSE}
## Creating csv file
write_csv(FinalDF, "/Users/Harleena/Documents/MSDS/FALL 2016/Data Wrangling and Husbandry/FinalDF.csv", na = "NA", append = FALSE)
```

##Part 8:
Using the FinalDF created in Step 5, I made a world map of maternal mortality, where the most recent year was used for each country. 

```{r, warning=FALSE}
## Getting the most recent year for each country from FinalDF
FinalDFSort <- FinalDF %>%
  arrange(Country, desc(Year))
library(plyr)
FinalDF2 <- ddply(.data = FinalDFSort, .var = c("Country"), .fun = function(x) x[1,])
library(dplyr)

## Renaming the columns before creating the map 
colnames(FinalDF2)[1] <- "region"
colnames(FinalDF2)[3] <- "value"

## Changing the country names to lower case for convenience
FinalDF2$region <- tolower(FinalDF2$region)  

## Renaming countries so they match the names in the choropleth country list
i <- 1
while(i <= nrow(FinalDF2))
{
  if(FinalDF2[i,]$region == "bahamas") FinalDF2[i,]$region <- "the bahamas"
  if(FinalDF2[i,]$region == "bolivia (plurinational state of)") FinalDF2[i,]$region <-  "bolivia"
  if(FinalDF2[i,]$region == "brunei darussalam") FinalDF2[i,]$region <-  "brunei"
  if(FinalDF2[i,]$region == "congo") FinalDF2[i,]$region <-  "republic of congo"
  if(FinalDF2[i,]$region == "cote d'ivoire") FinalDF2[i,]$region <- "ivory coast"
  if(FinalDF2[i,]$region == "guinea-bissau") FinalDF2[i,]$region <- "guinea bissau"
  if(FinalDF2[i,]$region == "iran (islamic republic of)") FinalDF2[i,]$region <- "iran"
  if(FinalDF2[i,]$region == "lao people's democratic republic") FinalDF2[i,]$region <- "laos"
  if(FinalDF2[i,]$region == "republic of korea") FinalDF2[i,]$region <- "south korea"
  if(FinalDF2[i,]$region == "republic of moldova") FinalDF2[i,]$region <- "moldova"
  if(FinalDF2[i,]$region == "russian federation") FinalDF2[i,]$region <- "russia"
  if(FinalDF2[i,]$region == "serbia") FinalDF2[i,]$region <- "republic of serbia"
  if(FinalDF2[i,]$region == "the former yugoslav republic of macedonia") FinalDF2[i,]$region <- "macedonia"
  if(FinalDF2[i,]$region == "timor-leste") FinalDF2[i,]$region <- "east timor"
  if(FinalDF2[i,]$region == "united kingdom of great britain and northern ireland") FinalDF2[i,]$region <- "united kingdom"
  if(FinalDF2[i,]$region == "venezuela (bolivarian republic of)") FinalDF2[i,]$region <- "venezuela"
  if(FinalDF2[i,]$region == "viet nam") FinalDF2[i,]$region <- "vietnam"
  i <- i+1
}

## Countries that are in FinalDF2, but are missing from the choroplethr country list:
##bahrain
##barbados
##cabo verde
##comoros
##grenada
##kiribati
##maldives
##malta
##marshall islands
##mauritius
##micronesia (federated states of)
##saint lucia
##saint vincent and the grenadines
##samoa
##seychelles
##singapore
##tonga
##tuvalu

## Creating the maternal mortality map
mmMap <- country_choropleth(FinalDF2, title = "World Map of Maternal Mortality", num_colors = 1)
mmMap



