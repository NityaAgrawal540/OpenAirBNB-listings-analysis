# OpenAirBNB-listings-analysis

Project Overview

This project analyzes Airbnb listings data to gain insights into host behavior, pricing trends, and customer reviews. The analysis involves data cleaning, transformation, and visualization using R.

Dataset

The project uses multiple datasets containing information about Airbnb listings, hosts, reviews, and pricing details.

Key Tables:

capstone1_fact_listings_20240108, capstone1_fact_listings_20240109, capstone1_fact_listings_20240110: Contain Airbnb listing details.

capstone1_dim_calendar_sub: Contains pricing information over time.

capstone1_dim_reviews_sub: Contains customer reviews.

Key Features & Steps

1. Data Cleaning & Transformation

Converted date fields to Date format.

Converted numeric fields (e.g., beds, bathrooms, accommodates) to integer format.

Renamed columns for better readability.

Removed duplicate records and handled missing values.

2. Data Aggregation & Joins

Merged listings data with reviews and calendar data using left_join.

Created time-based features like year, month, week, and day from host registration date.

Generated sequence numbers for listings per host.

3. Exploratory Data Analysis (EDA)

Univariate Analysis: Histograms, boxplots, and frequency distributions.

Bivariate Analysis: Scatter plots and correlation between numerical features.

Time Series Analysis: Trends in revenue and host activity over time.

Categorical Analysis: Bar charts and pie charts for property type and room type distribution.

4. Data Visualization

Stacked Bar Chart: Number of listings by property type and room type.

Pie Chart: Distribution of room types across hosts.

Histogram: Distribution of total revenue.

Line Chart: Revenue trends over time.

Boxplot: Distribution of reviews by room type.

Scatter Plot: Relationship between the number of reviews and total estimated revenue.

Technologies Used

R (dplyr, ggplot2, lubridate, sqldf)

RStudio

GitHub (for version control)
