library(nycflights13)
library(dplyr)

flights
browseVignettes()

flights%>%
  filter(arr_delay >= 120)

flights%>%
  filter(dest %in% c("IAH","HOU"))


flights%>%
  filter(arr_delay >= 120 & dep_delay <= 0)


flights%>%
arrange(desc(dep_delay))

flights%>%
  arrange(dep_time)

flights%>%
  arrange(desc(distance))

flights%>%
  arrange(distance)


flights%>%
select(dep_delay,dep_delay)

flights%>%
  select(dep_delay,dep_delay)
var1 <- c("MONTH","month","day","dep_delay","arr_delay")

flights%>%
  select(one_of(var1))

flights%>%
  select(contains("TIME"))



#mutate

flights_sml <- select(flights,year:day,ends_with("delay"),
                      distance,air_time)

flights_sml <- mutate(flights_sml,gain = arr_delay - dep_delay,
       speed = distance / air_time *60
       
)

flights_sml <- mutate(flights_sml,gain = arr_delay - dep_delay,
      hours = air_time / 60,
       gain_per_hour = gain/hours
       
       )

flights_sml <- mutate(flights_sml,distance_km = distance * 1.6093)

flights_sml <- mutate(flights_sml,time_per_km = air_time/distance_km)
                      

summarise(flights,dep_delay_mean = mean(dep_delay, na.rm =  TRUE))


flights%>%
group_by(carrier) %>%
summarise(largest_dep_delay_mean =  mean(dep_delay, na.rm =  TRUE)) %>%
  arrange(desc(largest_dep_delay_mean))

flights%>%
  group_by(carrier) %>%
  summarise(diff =  max(dep_delay,na.rm =  TRUE) - min(dep_delay,na.rm =  TRUE)) %>%
  arrange(desc(diff))

flights%>%
  group_by(month) %>%
  summarise(diff = sd(arr_delay,na.rm =  TRUE)) %>%
  arrange(desc(diff))


#pipe operator
flights%>%
  filter(!is.na(tailnum))%>%
  group_by(tailnum)%>%
  summarise(avg_delay = mean(dep_delay,na.rm = T), count_ob = n())%>%
  arrange(desc(avg_delay))
  
#pipe operator
flights%>%
    group_by(month)%>%
  summarise(grt_arr_delay = max(arr_delay,na.rm = T))%>%
  arrange(desc(grt_arr_delay))

flights%>%
  group_by(month)%>%
  summarise(grt_arr_delay = sum(arr_delay,na.rm = T))%>%
  mutate(ran)
  arrange(desc(grt_arr_delay))
  
  
#joins
    
  flights%>%
    left_join(airlines,by="carrier")%>%
    filter(name == "Virgin America")%>%
    group_by(time_hour)%>%
    summarise(mean_dep_delay = mean(dep_delay,na.rm = T))%>%
    arrange(desc(mean_dep_delay))%>%
    top_n(10)

    

  flights%>%
    left_join(airports,by= c("origin" = "faa"))%>%
    left_join(airports,by= c("dest" = "faa"))%>%
    select(origin,origin_lat = lat.x,origin_lon = lon.x,
           dest,dest_lat = lat.y,des_lon = lon.y
           )
  
  #
  flights%>%
    semi_join(planes,by = "tailnum")%>%
    tally()
  
  #
  flights%>%
    anti_join(planes,by = "tailnum")%>%
    tally()  
  
  airports%>%
    anti_join(flights, by = c("faa" = "dest"))%>%
                distinct()%>%
                tally()
  
  
  



#challenge
  programmatics <- 
    read_csv("ws-programmatics.csv",
             col_names = TRUE)

  categorization <- 
    read_csv("ws-categorization.csv",
             col_names = TRUE)



  programmatics%>%
   left_join(categorization, by = c("Base","MD"))%>%
   filter(Base == "Minot AFB")%>%
    filter(System == "Aircraft" | System == "Missile systems")%>% 
    group_by(System)%>% 
    summarise(mn = mean(Total_O.S,na.rm = T),ms = mean(End_Strength,na.rm = T))







