# r starting...
MyData <- read.csv("Chicago_Crimes_2012_to_2017.csv",  header=TRUE, sep=",")

# Checking for missing data within the dataset 
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
replace_col  <- replace_Row[,-c(3:11, 16, 20, 23)]  
dont_col <- replace_Row[ ,c(3:11, 16, 20, 23)]

# mice package to fix missing data
library(mice)
temp_no_miss = mice(replace_col, m=1, method = 'cart', seed=500)

no_missing = complete(temp_no_miss,1)

# Join the data parts together
MyDataNoMissing <- cbind(dont_col,no_missing)

# Check for missing data within new dataset.   
apply(MyDataNoMissing,2,function(x) sum(is.na(x)))

library(readr)
library(dplyr)

# Add count and hour column      
library(lubridate)
MyDataNoMissing$Date <- mdy_hms(MyDataNoMissing$Date) 
MyDataNoMissing$count <- 1

MyDataNoMissing$Hour <- substring(MyDataNoMissing$Date, 12,13)

# Provide number of crimes in the last five years
Stat_Crime <- MyDataNoMissing %>% group_by(Primary.Type, Year) %>% summarise(Number_Reported = length(Primary.Type)) 
head(arrange(Stat_Crime, -Number_Reported),n=10) 


# Which crime is the most committed and for what year
Stat_CrimeForMost <- MyDataNoMissing %>% group_by(Primary.Type) %>% summarise(Number_Reported = length(Primary.Type))

# Theft is the most reported crime in Chicago,  this is regardless of whether the crime led to an arrest or not. 
head(arrange(Stat_CrimeForMost, -Number_Reported)) 

# ...And with an arrest made, the most commited crime is Narcotics 
Stat_CrimeForMostArrest <- subset(MyDataNoMissing, Arrest=='True') %>% group_by(Primary.Type) %>% summarise(Number_Reported = length(Primary.Type))
arrange(Stat_CrimeForMostArrest, -Number_Reported) 
head(arrange(Stat_CrimeForMostArrest, -Number_Reported))

# Pie chart to show the result of crimes reported. 
# True = an Arrest was made, False = either no suspect/arrest yet or not a substantial case.

library(dplyr)
Piedata <-select(MyDataNoMissing, Arrest)
Piedata <- Piedata %>% group_by(Arrest) %>% summarise(count_of_arrest_or_not = length(Arrest))

Piedata <- Piedata %>% mutate(prop = count_of_arrest_or_not / sum(Piedata$count_of_arrest_or_not) *100) %>% mutate(ypos = cumsum(prop)- 0.5*prop )

library(ggplot2)
# Pie Chart
PieChart <- ggplot(Piedata, aes(x=1, y=count_of_arrest_or_not, fill=Arrest)) + geom_bar(stat="identity", width=1, color="black")
PieChart <- PieChart + coord_polar(theta="y", start=0) + geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.4))
PieChart <- PieChart + scale_fill_manual(values=c("lightgreen", "khaki")) 
# add title and label
PieChart <- PieChart + labs(x = NULL, y = NULL, fill = "Crime Result", title = "Crimes Report - Conclusion summary")
PieChart  + theme_void() 





