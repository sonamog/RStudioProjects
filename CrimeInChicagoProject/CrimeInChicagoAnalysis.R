MyData <- read.csv("Chicago_Crimes_2012_to_2017.csv",  header=TRUE, sep=",")

apply(MyData,2,function(x) sum(is.na(x)))#Check for Missing

str(MyData)

# Checking for missing data within the dataset 
summary(MyData)
apply(MyData,2,function(x) sum(is.na(x)))

# Calculate percent missing  
percentmissing = function(x){sum(is.na(x))/length(x)*100}

# percent missing by rows (1 for rows)
missingData = apply(MyData,1,percentmissing) 
table(missingData)

# 5 percent, subsetting out good rows
replace_Row = subset(MyData, missingData <= 5)
missingData2 = apply(replace_Row,1,percentmissing)
table(missingData2)

# excluding all categorical columns 
replace_col  <- replace_Row[,-c(3:11, 16, 20, 23,25)]  
dont_col  <- replace_Row[,c(3:11, 16, 20, 23,25)]

# mice package to fix missing data
library(mice)
temp_no_miss = mice(replace_col, m=1, method = 'cart', seed=500)

no_missing = complete(temp_no_miss,1)

# Join the data parts together
MyDataNoMissing <- cbind(dont_col,no_missing)

# Check for missing data within new dataset.   
summary(MyDataNoMissing)
apply(MyDataNoMissing,2,function(x) sum(is.na(x)))


library(readr)

library(dplyr)


# Add count and hour column      
library(lubridate)
MyData$Date <- mdy_hms(MyData$Date) 
MyData$count <- 1

MyData$Hour <- substring(MyData$Date, 12,13)

# Pie chart to show the result of crimes reported. 
# True means an Arrest was made, False means either no suspect/arrest yet or not a substantial case.

library(dplyr)
Piedata <-select(MyDataNoMissing, Arrest)
Piedata <- Piedata %>% group_by(Arrest) %>% summarise(count_of_arrest_or_not = length(Arrest))

Piedata <- Piedata %>% 
  mutate(prop = count_of_arrest_or_not / sum(Piedata$count_of_arrest_or_not) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

Piedata 

library(ggplot2)
# bar
PieChart = ggplot(Piedata, aes(x="", 
                               y=count_of_arrest_or_not, 
                               fill=Arrest)) + geom_bar(stat="identity", width=1)

PieChart = PieChart + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.4))

PieChart = PieChart + scale_fill_manual(values=c("lightgreen", "khaki")) 

# add title and label
PieChart = PieChart + labs(x = NULL, y = NULL, fill = "Crime Result", title = "Crimes Report - Conclusion summary")

PieChart  + theme_void() 


# Provide number of crimes in the last five years

Stat_Crime <- MyDataNoMissing %>% group_by(Primary.Type, Year) %>% summarise(Number_Reported = length(Primary.Type))
arrange(Stat_Crime, Year)

# Which crime is the most committed and for what year
Stat_CrimeForMost <- MyDataNoMissing %>% group_by(Primary.Type) %>% summarise(Number_Reported = length(Primary.Type))

# Theft is the most reported crime in Chicago,  this is regardless of whether the crime led to an arrest or not. 
head(arrange(Stat_CrimeForMost, -Number_Reported)) 

# ...And with an arrest made, the most commited crime is Narcotics 
Stat_CrimeForMostArrest <- subset(MyDataNoMissing, Arrest=='True') %>% group_by(Primary.Type) %>% summarise(Number_Reported = length(Primary.Type))
arrange(Stat_CrimeForMostArrest, -Number_Reported) 
head(arrange(Stat_CrimeForMostArrest, -Number_Reported))







