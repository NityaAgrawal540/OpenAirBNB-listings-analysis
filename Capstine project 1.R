
## Checking the data types of each table ####################################################
summary.default(capstone1_dim_calendar_canada)
summary.default(capstone1_dim_calendar_sub)
summary.default(capstone1_dim_reviews_sub)
summary.default(capstone1_fact_listings_20240108)
summary.default(capstone1_fact_listings_20240109)
summary.default(capstone1_fact_listings_20240110)

## Converting the datatypes of date to as.date format #######################################

capstone1_dim_reviews_sub$date_review <- as.Date(capstone1_dim_reviews_sub$date_review)
capstone1_dim_calendar_sub$date_cal_price <- as.Date(capstone1_dim_calendar_sub$date_cal_price)
capstone1_fact_listings_20240108$last_scraped <- as.Date(capstone1_fact_listings_20240108$last_scraped)
capstone1_fact_listings_20240108$host_since <- as.Date(capstone1_fact_listings_20240108$host_since)
capstone1_fact_listings_20240109$last_scraped <- as.Date(capstone1_fact_listings_20240109$last_scraped)
capstone1_fact_listings_20240109$host_since <- as.Date(capstone1_fact_listings_20240109$host_since)
capstone1_fact_listings_20240110$last_scraped <- as.Date(capstone1_fact_listings_20240110$last_scraped)
capstone1_fact_listings_20240110$host_since <- as.Date(capstone1_fact_listings_20240110$host_since)



## Converting the datatypes of integer as.integer format ######################################

capstone1_fact_listings_20240108$beds <- as.integer(capstone1_fact_listings_20240108$beds)
capstone1_fact_listings_20240108$bathrooms <- as.integer(capstone1_fact_listings_20240108$bathrooms)
capstone1_fact_listings_20240108$accommodates <- as.integer(capstone1_fact_listings_20240108$accommodates)
capstone1_fact_listings_20240108$bedrooms <- as.integer(capstone1_fact_listings_20240108$bedrooms)




capstone1_fact_listings_20240109$beds <- as.integer(capstone1_fact_listings_20240109$beds)
capstone1_fact_listings_20240109$bedrooms <- as.integer(capstone1_fact_listings_20240109$bedrooms)
capstone1_fact_listings_20240109$accommodates <- as.integer(capstone1_fact_listings_20240109$accommodates)
capstone1_fact_listings_20240109$bathrooms <- as.integer(capstone1_fact_listings_20240109$bathrooms)




capstone1_fact_listings_20240110$beds <- as.integer(capstone1_fact_listings_20240110$beds)
capstone1_fact_listings_20240110$bedrooms <- as.integer(capstone1_fact_listings_20240110$bedrooms)
capstone1_fact_listings_20240110$accommodates <- as.integer(capstone1_fact_listings_20240110$accommodates)
capstone1_fact_listings_20240110$bathrooms <- as.integer(capstone1_fact_listings_20240110$bathrooms)




### Renaming the columns##############################################
# Another way to rename columns
colnames(capstone1_fact_listings_20240108)[colnames(capstone1_fact_listings_20240108) == "id"] <- "listing_id"
colnames(capstone1_fact_listings_20240108)[colnames(capstone1_fact_listings_20240108) == "host_since"] <- "date_host_since"
colnames(capstone1_fact_listings_20240108)[colnames(capstone1_fact_listings_20240108) == "bathrooms_text"] <- "bathroom_type"
colnames(capstone1_fact_listings_20240108)[colnames(capstone1_fact_listings_20240108) == "minimum_nights"] <- "min_nights"
colnames(capstone1_fact_listings_20240108)[colnames(capstone1_fact_listings_20240108) == "maximum_nights"] <- "max_nights"
colnames(capstone1_fact_listings_20240108)[colnames(capstone1_fact_listings_20240108) == "neighbourhood_cleansed"] <- "neighbourhood"


### check if there are any duplicate values ############################

duplicates <- duplicated(capstone1_dim_reviews_sub)
capstone1_dim_reviews_sub[duplicates, ] 
reviws_unique <- capstone1_dim_reviews_sub[!duplicated(capstone1_dim_reviews_sub), ]


duplicates <- duplicated(capstone1_dim_calendar_sub)
capstone1_dim_calendar_sub[duplicates, ] 
calender_unique <- capstone1_dim_calendar_sub[!duplicated(capstone1_dim_calendar_sub), ]


duplicates <- duplicated(capstone1_fact_listings_20240108)
capstone1_fact_listings_20240108[duplicates, ] 
fact_listiings_unique <- capstone1_fact_listings_20240108[!duplicated(capstone1_fact_listings_20240108), ]


### Check any missing data 
sum(is.na(capstone1_fact_listings_20240108))

### check number of listings where price > = 100 ##########

num_listings_over_100 <- sum(!is.na(capstone1_fact_listings_20240108$price) & capstone1_fact_listings_20240108$price >= 100)

# Print the result
print(num_listings_over_100)

### check number of listings where accommodates equal 2  ########

sum(capstone1_fact_listings_20240108$accommodates == 2)

## subset for both criteria 
df_fact_listing_sub <- subset(capstone1_fact_listings_20240108, (capstone1_fact_listings_20240108$price >= 100 & 
                                                                   capstone1_fact_listings_20240108 == 2))


## subset for accommodates == 2

df_fact_listing_sub_2 <- subset(capstone1_fact_listings_20240108, (capstone1_fact_listings_20240108$accommodates == 2))

## replace the null values with zero ####################

df_fact_listing_sub_2$price <- replace(df_fact_listing_sub_2$price,
                                       is.na(df_fact_listing_sub_2$price),0)

######################################################################################################

### left join #####################
library(dplyr)
df_joined <- capstone1_fact_listings_20240108 %>%
  left_join(capstone1_dim_reviews_sub, by = c('listing_id' = 'listing_id')) %>%
  left_join(capstone1_dim_calendar_sub, by = c('listing_id' = 'listing_id'))


rm(capstone1_fact_listings_20240108, capstone1_dim_reviews_sub, capstone1_dim_calendar_sub)

df_joined <- df_joined[c(2,5:8,10,1,3,11:18,40:39,19:22,31,37:38)]

names(df_joined)


### sort the data #####################################

df_joined <- df_joined[order(df_joined$host_id, df_joined$listing_id , df_joined$date_review),]

### subset data for sampling: specific customer
df_joined_sub <- subset(df_joined, df_joined$host_id %in%
                          c('17903871', '528228958', '33692669'))

### latest review per listing id ##################### subset-- using max 

library(sqldf)

df_subset_max_review <- sqldf('SELECT A1.* FROM df_joined_sub A1,
                              (SELECT listing_id AS LIST_ID, MAX(date_review) AS MAX_DT_RVU
                              FROM df_joined_sub GROUP BY listing_id)
                              WHERE A1.listing_id = LIST_ID
                              AND A1.date_review = MAX_DT_RVU')





### if price is NA then Y otherwise N 

df_subset_max_review$data_quality_ind <- ifelse(is.na(df_subset_max_review$price),"Y","N")





##TIME INTELLIGENCE ##################################
library(lubridate)
install.packages("dplyr")
library(dplyr)



df_joined_time <- subset(df_joined, select = c(1,2,4,5,7,3,24,9:11,16,25))

## calculate a set of time feature from "date_host_since" #################

df_joined_time$date_host_since_yr <- year(df_joined_time$date_host_since)
df_joined_time$date_host_since_mnth <- month(df_joined_time$date_host_since)
df_joined_time$date_host_since_week <- week(df_joined_time$date_host_since)
df_joined_time$date_host_since_day <- day(df_joined_time$date_host_since)
df_joined_time$date_host_since_wkday <- weekdays(df_joined_time$date_host_since)



### sequence for all listings per host###########################

df_joined_time <- df_joined_time %>%
  arrange(host_id,date_host_since)%>%
  group_by(host_id) %>%
  mutate(list_seq = as.numeric(factor(listing_id)))


df_joined_time$prev_date_review <- lag(df_joined_time$date_review)
df_joined_time$prev_host_id <- lag(df_joined_time$host_id)

df_joined_time$prev_date_view <- ifelse(df_joined_time$prev_host_id != df_joined_time$host_id,
                                        NA, df_joined_time$prev_date_review)








#### DATA VISULAIZATION ##########################
library(ggplot2)


# stacked chart by property type and room type

ggplot(capstone1_fact_listings_20240108, aes(x = property_type, fill=room_type)) +
  geom_bar() +
  scale_y_continuous(labels = function(x) format(x,scientific = FALSE))


#  Pie chart #####################################

df_demo_pie <- capstone1_fact_listings_20240108 %>%
  group_by(room_type) %>%
  summarize(NUM_HOST = n_distinct(host_id)) %>%
  mutate(PCT_HOSTS = round(NUM_HOST/sum(NUM_HOST) * 100,2))

# label position
df_demo_pie <- df_demo_pie %>%
  arrange(desc(room_type)) %>%
  mutate(LAB_POS = cumsum(PCT_HOSTS)- 0.5*PCT_HOSTS)

# plot 
ggplot(df_demo_pie, aes(x = "", y = PCT_HOSTS, fill = room_type)) +
  geom_bar(width = 1, stat = "identity", color = "white")+
  coord_polar("y", start =0)+
  geom_text(aes(y = LAB_POS, label = PCT_HOSTS), color = "white", size = 4)+
              theme_void()

## univariate numerical charts
# Prepare data
df_demo_ts <- capstone1_fact_listings_20240108 %>%
  mutate(year_mth = as.Date(paste0(format(host_since, '%Y-%m'), "-01"))) %>%
  group_by(year_mth) %>%
  summarize(
    TOTAL_EST_REV = sum(price),
    NUM_HOSTS = n_distinct(host_id)) %>%
  mutate(EST_REV_PER_HOST = TOTAL_EST_REV / NUM_HOSTS)

# Plot a histogram: total revenue
ggplot(df_demo_ts, aes(x = TOTAL_EST_REV)) +
  geom_histogram(bins = 30) +
  ggtitle('Histogram of Number of Hosts')

# Plot a line chart: total revenue by year-month since host
ggplot(df_demo_ts, aes(x = year_mth, y = TOTAL_EST_REV)) +
  geom_line() +
  labs(x = "Yr-Mth Since Host", y = "$ Total Revenue", title = "$ Total Revenue by Year Month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## box and whiskers plot
ggplot(capstone1_fact_listings_20240108, aes(x =room_type, y= number_of_reviews)) +
  geom_boxplot()


# Prepare data
df_demo_sct <- capstone1_fact_listings_20240108 %>%
  group_by(host_id, room_type) %>%
  summarize(
    NUM_REVIEWS = sum(number_of_reviews),
    TOTAL_EST_REV = sum(price)
  )

# Shows reviews vs. estimated total revenue by "room type"
ggplot(df_demo_sct, aes(x = NUM_REVIEWS, y = TOTAL_EST_REV)) +
  geom_point(aes(color = factor(room_type))) +
  scale_y_continuous(labels = scales::comma)


##########################################################################################################################################
##########################################################################################################################################