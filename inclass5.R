library(ggplot2)

ggplot(data = mpg, aes(x= cty))+
  geom_histogram()

ggplot(data = mpg, aes(x= manufacturer))+
  geom_bar()

ggplot(data = mpg, aes(x= cty,y=displ))+
  geom_point(color = "blue", size = 2)


### error
ggplot(data = mpg, aes(x= displ,y=cyl,color = displ,size = displ,shape = displ))+
  geom_point()

ggplot(data = mpg, aes(x= displ,y=cyl),shape= 4,fill = "White",stroke = 5 )+
  geom_point()


ggplot(data = mpg, aes(x= displ,y=cyl ,color = cyl < 5))+
  geom_point()


#####

ggplot(data = mpg, aes(x=displ,y=cty))+
  geom_point()+
  facet_wrap(year~.)


ggplot(data = mpg, aes(x=displ,y=cty))+
  geom_point()+
  facet_grid(year~cyl)

ggplot(data = mpg, aes(x=displ,y=cty))+
  geom_point()+
  facet_grid(~cyl+year)


ggplot(data = mpg, aes(x=displ,y=cty))+
  geom_point()+
  facet_grid(cyl~year,scales = "free", space = "free")


#overplotting

ggplot(mpg,aes(x=class,y=hwy))+
  geom_boxplot()


ggplot(mpg,aes(x=class,y=hwy))+
  geom_jitter(width = 0.2,alpha = 0.5)


ggplot(mpg,aes(x=displ,y=cty))+
  geom_smooth()+
  geom_rug()

ggplot(mpg,aes(x=class,fill = factor(cyl)))+
geom_bar(position = "dodge")

ggplot(mpg,aes(x=class,fill = factor(cyl)))+
  geom_bar(position = "fill")

ggplot(mpg,aes(x=class,fill = factor(cyl)))+
  geom_bar(position = "stack")

ggplot(mpg,aes(class,fill = factor(cyl)))+
  geom_bar(position = "fill")+
   scale_y_continuous(name = "Percent",labels= scales::percent)+
    coord_flip()



install.packages("ggplot2movies")
library(ggplot2movies)
library(tidyverse)
library(dplyr)

ggplot(movies,aes(x=length))+
  geom_bar()+
  coord_cartesian(xlim = c(0,500))


ggplot(movies,aes(x=length))+
  geom_histogram(binwidth = 5)+
  coord_cartesian(xlim = c(0,240))

movies %>%
  filter(length >= quantile(length,0.95)) %>%
  tally()


##

movies %>%
  filter(length >= quantile(length,0.20)) %>%
  tally()
 
  mean(movies$length >= quantile(movies$length,0.20), na.rm= TRUE)
  

movies %>%
  mutate(movies,category = ifelse(length >= 125,"Long",
                                  ifelse(length <= 40, "Short","Regular"
                                  )) )

movies  <- movies %>%
  mutate(category = case_when(length <= 40 ~ "Short",
                                     length <= 125 ~ "Regular",
                                     length > 125 ~ "Long",
                                     TRUE ~ NA_character_
                                      ))


  ##
mean(movies$category == "Regular", na.rm= TRUE)


 movies %>%
   group_by(category)%>%

   summarize(avg = mean(rating))
 

###########################33
 
 
 
 #1 Packages required
 
 library(readr) # to import the csv file
 library(dplyr) # to use functions like glimpse
 library(lubridate) #for date formatting
 library(DT)# to use the datatable function
 
 #2 Data Preparation
 
 
 #Loading the data
 crime_data_initial <- 
   read_csv("city_of_cincinnati_police_data_initiative_crime_incidents.csv",
            col_names = TRUE)
 View(crime_data_initial)
 
 dim(crime_data_initial)
 colnames(crime_data_initial)
 
 # Check the structure the dplyr way
 glimpse(crime_data_initial)
 summary(crime_data_initial)
 
 
 #3 Creating tidy data
 
 #3 Creating tidy data
 
 #Replace all na with 0 for total victims and suspects
 #as we do not want to end up omiting
 # criminal records in which no suspects were found or no victims identified
 crime_data_initial$TOTALNUMBERVICTIMS[is.na(crime_data_initial$TOTALNUMBERVICTIMS)] <- 0
 crime_data_initial$TOTALSUSPECTS[is.na(crime_data_initial$TOTALSUSPECTS)] <- 0
 crime_data_initial$LATITUDE_X[is.na(crime_data_initial$LATITUDE_X)] <- 0
 crime_data_initial$LONGITUDE_X[is.na(crime_data_initial$LONGITUDE_X)] <- 0
 
 
 #Now replace all NA in Theft code with NOT APPLICABLE as we do not want to end up omiting
 # records which are not thefts when we use na.omit
 crime_data_initial$THEFT_CODE[is.na(crime_data_initial$THEFT_CODE)] <- "NOT APPLICABLE"
 
 #Similarly floor,side and opening too as they are not applicable
 # for all types of crimes
 crime_data_initial$FLOOR[is.na(crime_data_initial$FLOOR)] <- "NOT APPLICABLE"
 crime_data_initial$SIDE[is.na(crime_data_initial$SIDE)] <- "NOT APPLICABLE"
 crime_data_initial$OPENING[is.na(crime_data_initial$OPENING)] <- "NOT APPLICABLE"
 
 #delete all na records, all records with no lat long will be deleted
 crime_data_no_na <- na.omit(crime_data_initial)
 
 #drop 1st column INSTANCEID as it is only a row identifier
 crime_data_no_na <- crime_data_no_na[,-1]
 
 #convert data types found to be incorrect as per glimpse and summary output
 crime_data_no_na$INCIDENT_NO  <- as.numeric(crime_data_no_na$INCIDENT_NO) 
 
 #delete any records having conversion error resulting in NA coercion
 # this is because there were 326 alphanumeruc incident numbers
 crime_data_no_na <- na.omit(crime_data_no_na)
 
 #format all date and time columns
 crime_data_no_na$DATE_REPORTED <- mdy_hms(crime_data_no_na$DATE_REPORTED)
 crime_data_no_na$DATE_FROM <- mdy_hms(crime_data_no_na$DATE_FROM)
 crime_data_no_na$DATE_TO <- mdy_hms(crime_data_no_na$DATE_TO)
 crime_data_no_na$DATE_OF_CLEARANCE <-
   mdy_hms(crime_data_no_na$DATE_OF_CLEARANCE)
 
 
 #Rename columns which are not named correctly.
 #Rename CLSD to Clearances
 names(crime_data_no_na)[5]<-"Clearances"
 
 #Rename VICTIM_AGE to VICTIM_AGE_RANGE
 colnames(crime_data_no_na)[colnames(crime_data_no_na)=="VICTIM_AGE"] <-
   "VICTIM_AGE_RANGE"
 
 #Rename SUSPECT_AGE TO SUSPECT_AGE_RANGE
 colnames(crime_data_no_na)[colnames(crime_data_no_na)=="SUSPECT_AGE"] <- "SUSPECT_AGE_RANGE"
 
 #Separate dates to date and time
 
 crime_data_mutated <-
   mutate(crime_data_no_na,
          REPORTED_DATE  = format(crime_data_no_na$DATE_REPORTED, "%m/%d/%Y") ,
          REPORTED_TIME  = format(crime_data_no_na$DATE_REPORTED, "%H:%M:%S"),
          FROM_DATE = format(crime_data_no_na$DATE_FROM, "%m/%d/%Y") ,
          FROM_TIME = format(crime_data_no_na$DATE_FROM, "%H:%M:%S"),
          TO_DATE = format(crime_data_no_na$DATE_TO, "%m/%d/%Y") ,
          TO_TIME = format(crime_data_no_na$DATE_TO, "%H:%M:%S"),
          CLEARANCE_DATE  = format(crime_data_no_na$DATE_OF_CLEARANCE , "%m/%d/%Y") ,
          CLEARANCE_TIME  = format(crime_data_no_na$DATE_OF_CLEARANCE , "%H:%M:%S"),
          
          
   )
 
 #Drop unrequired date columns which we just mutated. Also drop HOUR_FROM
 # and HOUR_TO as this info is already present in FROM_TIME and TO_TIME
 crime_data_mutated <-
   subset(crime_data_mutated, select=-c(DATE_REPORTED,DATE_FROM,DATE_TO,
                                        DATE_OF_CLEARANCE,HOUR_FROM,HOUR_TO
   ))
 
 
 #Verify if any NA values still present
 sum(is.na(crime_data_mutated))
 
 


