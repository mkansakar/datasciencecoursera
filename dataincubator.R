library("stringr")
library('dplyr')
library(lubridate)
df <- read.csv("c:\\temp\\boston_311_calls.csv", header=T)

#Q 1.1
total1 <- nrow(df)


#Q 1.2
traffic1 <- nrow(df[str_detect(df$case_title, fixed('traffic', ignore_case=TRUE)), ])
fraction1 <- round(traffic1/total1, digits = 3)


# 1.3
n_years <- n_distinct(df$year1)
df$police_district1 <- gsub("-",'',df$police_district)
df$year1 <- year(df$open_dt)
df %>%
  group_by(police_district1) %>%
  summarize(AVE_points = round(n()/n_distinct(year1))) %>%
  arrange(AVE_points)


# 1.4
df_zip_pop <- read.csv("c:\\temp\\boston_population_by_zip.csv", colClasses  = c(Zip_Code = "character"))
df$Zip_Code <- str_extract(df$location, "\\d{5}")

df_calls_by_zip <- df %>% 
  filter(!is.na(Zip_Code)) %>%
  group_by(Zip_Code) %>%
  summarise(total_calls=n()) %>% 
  as.data.frame()

df_call_by_zip_by_pop <- merge(df_calls_by_zip,df_zip_pop,by="Zip_Code", all.x = FALSE)

df_grt_100 <- df_call_by_zip_by_pop[df_call_by_zip_by_pop['total_calls'] > 100,] 
Person_corr<- cor.test(df_grt_100$total_calls,df_grt_100$Population,method = 'pearson')


# 1.5
df_call_by_year <- df %>% 
  group_by(year(df$open_dt))%>%
  summarise(total_call2=n())%>% as.data.frame()

plot(df_call_by_year)
abline(lm(df_call_by_year$total_call2 ~ df_call_by_year$`year(df$open_dt)`))
lm(df_call_by_year$total_call2 ~ df_call_by_year$`year(df$open_dt)`)


#Q 1.6
calls <- read.csv("c:\\temp\\boston_311_calls.csv", header=T, stringsAsFactors = FALSE)[,c('case_enquiry_id','open_dt','latitude', 'longitude','type','location')]
garbage <- read.csv("c:\\temp\\boston_02128_garbage_schedule.csv", colClasses  = c(zip_code = "character"))[ ,c('latitude', 'longitude','trashday')]
garbage <- distinct(garbage)
calls$Zip_Code <- str_extract(calls$location, "\\d{5}")
calls <- filter(calls, type == "Requests for Street Cleaning" & Zip_Code == "02128")
calls$open_dt <- as.Date(calls$open_dt)

euclidean1 <- function(x, y) sqrt(sum((x - y)^2))
calls$trashday <- ""
for (i in 1:nrow(calls)) {
  min_dist1 <- 1000
  k <- i + 1
  for (j in 1:nrow(garbage)) {
    dist <- euclidean1(c(calls$longitude[i], calls$latitude[i]), c(garbage$latitude[j],garbage$longitude[j]))
    if(dist < min_dist1){
      tday <- ""
      min_dist1 <- dist
      tday = garbage[j,3]
    }
  }
  
  calls[i,8] <- tday
}

days <- c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday")
date <- as.Date(calls$open_dt)   
weekday <- weekdays(date)
days_difference <- ifelse(weekday == "Friday", 0, 6 - match("Friday", days) + match(weekday, days))
mean(days_difference)

