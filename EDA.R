library(arrow)
library(dplyr)
library(purrr)
library(tidyverse)
library(lubridate)

df <- read_parquet("yellow_tripdata_2025-02.parquet", header=TRUE)
names(df)
dim(df)
summary(df)

# Create tip_pct to better evaluate tipping as a percent of fare
df <- df %>% mutate(tip_pct = tip_amount / fare_amount * 100) %>%
  filter(payment_type %in% c(0,1), #only flex fare and credit cards
         fare_amount > 0,   #non-zero fares only
         trip_distance > 0, #non-zero distance only
         RatecodeID != 99,  #null/unknown values discarded
         tip_pct <= 100)    #limit to reasonable tip % only

# Join taxi pickup/drop off location IDs to taxi zone lookup information
locations <- read.csv("taxi_zone_lookup.csv", header = TRUE)

df <- df %>%
  left_join(locations, by = c("PULocationID" = "LocationID")) %>%
  left_join(locations, by = c("DOLocationID" = "LocationID"), 
            suffix = c("_pickup", "_dropoff"))

# verify join worked as intended
print(head(df), width=Inf)
dim(df)
summary(df)

# draw 1% sample from each RatecodeID; RatecodeID==1 dominates dataset
sample_prop <- df %>% group_by(RatecodeID) %>%
  sample_frac(0.01, replace=FALSE) %>%
  ungroup()

# create time features to explore patterns
df1 <- sample_prop %>% mutate(
  hour = lubridate::hour(tpep_pickup_datetime),
  weekday = lubridate::wday(tpep_pickup_datetime, label = TRUE))

# plot distribution of tip_pct 
df1 %>% ggplot(aes(tip_pct)) + geom_histogram(bins=50) + 
  facet_wrap(~weekday)

df1 %>% ggplot(aes(hour,fill = tip_pct>25)) + geom_bar(position="fill")

df1 %>% group_by(PULocationID, Zone_pickup, Borough_pickup) %>%
  summarize(mean_tip = mean(tip_pct)) %>%
  arrange(desc(mean_tip))

#Rate code(1-standard,2-JKF,3-Newark,4-Nassau/Westchester,5-Negotiated,6-Group ride)
df1 %>% group_by(RatecodeID) %>% 
  summarize(mean_tip = mean(tip_pct)) %>%
  ggplot(aes(RatecodeID,mean_tip)) + geom_bar(stat = "identity")

# Scatterplots
df1 %>% ggplot(aes(trip_distance, tip_pct)) +
  geom_point()

df1 %>% ggplot(aes(trip_distance, tip_amount)) +
  geom_point()

df1 %>% ggplot(aes(Borough_pickup, tip_amount)) +
  geom_point(aes(col=Borough_dropoff),position="jitter")

# Correlations
df1 %>%
  select(tip_pct, tip_amount, RatecodeID, trip_distance, fare_amount, hour) %>%
  cor(use = "complete.obs")

