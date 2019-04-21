library(tidyverse)

file_1 <- "Month-"
file_2 <-".csv"
montharray <- 1:13

for(month in montharray)
{

if(month %in% 1:9)
{
  file3 <- paste0("data/",file_1,0,month,file_2)
  if(file.exists(file3))
  {
    df <- read.csv(file3)
    assign(paste0("df.month.",month),df)
    rm(df)
  }
  
}else if(month %in% 10:12){
  
  file3 <- paste0("data/",file_1,month,file_2)
  if(file.exists(file3))
  {
    df <- read.csv(file3)
    assign(paste0("df.month.",month),df)
    rm(df)
  }
  
}else{
  
  print("invalid month")
}
  
  
}

##
iris %>%
map_chr(class)

iris %>%
  map(mean)

iris %>%
  map(mean)  %>%
  map_lgl(~. > 5)
 
iris %>%
  map(mean)   > 5


##
library(nycflights13)
colSums(is.na(flights))


map(flights, function(x) mean(is.na(x)))


flights %>%
  map_df(~sum(is.na(.))) 


diamonds %>%
  split(.$cut) %>%
  map(~(lm(price ~ carat, data = .))) %>%
  map(~summary)

ratio <- function(x,y)
  
{
  
  return(x/y)
  
}


rescale <- function(x,y,na.rm = TRUE)
{
  if(is.numeric(x))
  {
    if(length(y)==1)
    {
   if(isTRUE(na.rm)) x <- na.omit(x)
   round((x - min(x,na.rm = TRUE))/
   ( max(x,na.rm = TRUE) -min(x,na.rm = TRUE)),y)
  
    }   
  }
  
}


mtcars %>% 
map(~rescale(.,2))









