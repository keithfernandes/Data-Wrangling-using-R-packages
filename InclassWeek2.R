#1 ?airquality

#2The data were obtained from the New York State Department of Conservation (ozone data) and the National Weather Service (meteorological data).

#3The data were obtained from the New York State Department of Conservation (ozone data) and the National Weather Service (meteorological data).

#4Daily readings of the following air quality values for May 1, 1973 (a Tuesday) to September 30, 1973.

#6
dim(airquality)

#7 yes
str(airquality)

#8
head(airquality,10)
tail(airquality,10)

#9
str(airquality)

#10
is.na(airquality)
sum(is.na(airquality))
colSums(is.na(airquality))

#11
summary(airquality$Temp)
sd(airquality$Temp)
quantile(airquality$Temp, .85) 

#12 yes
plot(airquality$Temp,airquality$Ozone)

#13 somehwhat
hist(airquality$Tem,25)


#14
boxplot(airquality$Ozone ~ airquality$Month)


#IMPORT
# 1

library(data.table)

flights <- fread("flights.csv")

head(flights)
dim(flights)

#3
View(flights)
#4
str(flights)
#5
str(flights)
#6,7
sum(is.na(flights))
colSums(is.na(flights))

flights_clean <- na.omit(flights)

#8

table(flights_clean$carrier)



#9 Yes
summary(flights_clean$dep_delay)
hist(flights_clean$dep_delay)
boxplot(flights_clean$dep_delay,horizontal = TRUE)

#10 

boxplot(flights_clean$dep_delay ~ flights_clean$carrier)
#11 

barplot(table(flights_clean$carrier))


#12

library(readxl)
excel_sheets("aircraft.xlsx")
aircraft <- read_excel("aircraft.xlsx", sheet = "Trainers",skip = 3)

#3 NUMERIC UNDERSTANDING

summary(aircraft)

#4 
table(aircraft$MD)

aircraft$FY <- as.character(aircraft$FY)
is.character(aircraft$FY)

#5

table(aircraft$FY)


#6
quantile(aircraft$FH, .90) 

#7 


range(aircraft$Cost)

#8

hist(aircraft$FH)

#9 T-6, T-38, T -1, AT-38

boxplot(aircraft$FH ~ aircraft$MD)

#10

barplot(table(aircraft$FY))


#Webscrape

#1

library(readr)
url_delim <- "http://academic.udayton.edu/kissock/http/Weather/gsod95-current/OHCINCIN.txt"

weather <- read_table(url_delim,col_names = c("Month","Day","Year","Temp"))

head(weather)


mean(weather$Temp)

range(weather$Temp)

weather_cleaning <- weather
weather_cleaning[weather_cleaning$Temp == -99,]$Temp <- NA

colSums(is.na(weather_cleaning))

#5

boxplot(weather_cleaning$Temp ~ weather_cleaning$Month)









