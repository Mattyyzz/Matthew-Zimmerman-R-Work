# Matthew-Zimmerman-R-Work

Partner Assignment 1 Miller Zimmerman

2025-02-17
Part a:
library(readr)
library(dplyr)
##
## Attaching package: ’dplyr’
## The following objects are masked from ’package:stats’:
##
## filter, lag
## The following objects are masked from ’package:base’:
##
## intersect, setdiff, setequal, union
weather <- read_csv("ATLweather.csv")
## Rows: 52513 Columns: 4
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (1): Timestamp
## dbl (3): Temperature, Humidity, Precipitation
##
## i Use ‘spec()‘ to retrieve the full column specification for this data.
## i Specify the column types or set ‘show_col_types = FALSE‘ to quiet this message.
electricity <- read_csv("electricityprices.csv")
## Rows: 52581 Columns: 2
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (1): Timestamp
## dbl (1): cost
##
## i Use ‘spec()‘ to retrieve the full column specification for this data.
## i Specify the column types or set ‘show_col_types = FALSE‘ to quiet this message.
1
merged_data <- inner_join(weather, electricity, by = "Timestamp")
## Warning in inner_join(weather, electricity, by = "Timestamp"): Detected an unexpected many-to-many re## i Row 43085 of ‘x‘ matches multiple rows in ‘y‘.
## i Row 7441 of ‘y‘ matches multiple rows in ‘x‘.
## i If a many-to-many relationship is expected, set ‘relationship =
## "many-to-many"‘ to silence this warning.
head(merged_data)
## # A tibble: 6 x 5
## Timestamp Temperature Humidity Precipitation cost
## <chr> <dbl> <dbl> <dbl> <dbl>
## 1 1/1/2010 1:00 44 100 0 5.54
## 2 1/1/2010 2:00 44 96 0 6.50
## 3 1/1/2010 3:00 44 93 0 6.54
## 4 1/1/2010 4:00 44 89 0 5.65
## 5 1/1/2010 5:00 43 89 0 6.56
## 6 1/1/2010 6:00 42 83 0 6.65
Part b:
merged_data$cost <- as.numeric(gsub(",", "", merged_data$cost))
cor_temp_price <- cor.test(merged_data$Temperature, merged_data$cost)
cor_humidity_price <- cor.test(merged_data$Humidity, merged_data$cost)
cor_temp_price
##
## Pearson’s product-moment correlation
##
## data: merged_data$Temperature and merged_data$cost
## t = 42.434, df = 52506, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## 0.1738064 0.1903459
## sample estimates:
## cor
## 0.182089
cor_humidity_price
##
## Pearson’s product-moment correlation
##
## data: merged_data$Humidity and merged_data$cost
## t = -41.011, df = 52414, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
2
## 95 percent confidence interval:
## -0.1846102 -0.1680208
## sample estimates:
## cor
## -0.176328
The temperature has a slightly positive correlation due to the estimate being ~0.18 and is statistically
significant since it’s P-value is 0. Humidity has a slightly negative correlation with its estimate being ~ -0.17
and also has a P-value of 0. This means as temperature increases the price increases as well, and when the
humidity increases the price tends to decrease.
Part c:
library(lubridate)
##
## Attaching package: ’lubridate’
## The following objects are masked from ’package:base’:
##
## date, intersect, setdiff, union
merged_data$Timestamp <- mdy_hm(merged_data$Timestamp)
merged_data$Month <- month(merged_data$Timestamp)
summer_data <- filter(merged_data, Month %in% c(7, 8, 9))
fall_data <- filter(merged_data, Month %in% c(10, 11, 12))
head(summer_data)
## # A tibble: 6 x 6
## Timestamp Temperature Humidity Precipitation cost Month
## <dttm> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 2010-07-01 00:00:00 74 90 0 6.12 7
## 2 2010-07-01 01:00:00 73 93 0 5.77 7
## 3 2010-07-01 02:00:00 73 93 0 6.14 7
## 4 2010-07-01 03:00:00 73 90 0 5.90 7
## 5 2010-07-01 04:00:00 73 93 0 5.88 7
## 6 2010-07-01 05:00:00 73 90 0 5.90 7
head(fall_data)
## # A tibble: 6 x 6
## Timestamp Temperature Humidity Precipitation cost Month
## <dttm> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 2010-10-01 00:00:00 62 67 0 5.83 10
## 2 2010-10-01 01:00:00 61 70 0 5.75 10
## 3 2010-10-01 02:00:00 60 70 0 5.62 10
## 4 2010-10-01 03:00:00 60 70 0 5.60 10
## 5 2010-10-01 04:00:00 60 70 0 5.56 10
## 6 2010-10-01 05:00:00 58 75 0 5.59 10
3
Part d:
cor_temp_price_summer <- cor.test(summer_data$Temperature, summer_data$cost)
cor_humidity_price_summer <- cor.test(summer_data$Humidity, summer_data$cost)
cor_temp_price_fall <- cor.test(fall_data$Temperature, fall_data$cost)
cor_humidity_price_fall <- cor.test(fall_data$Humidity, fall_data$cost)
cor_temp_price_summer
##
## Pearson’s product-moment correlation
##
## data: summer_data$Temperature and summer_data$cost
## t = 66.584, df = 12493, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## 0.4987230 0.5246084
## sample estimates:
## cor
## 0.5117819
cor_humidity_price_summer
##
## Pearson’s product-moment correlation
##
## data: summer_data$Humidity and summer_data$cost
## t = -44.26, df = 12470, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## -0.3835359 -0.3531993
## sample estimates:
## cor
## -0.3684657
cor_temp_price_fall
##
## Pearson’s product-moment correlation
##
## data: fall_data$Temperature and fall_data$cost
## t = -10.429, df = 13213, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## -0.10724508 -0.07342363
## sample estimates:
## cor
## -0.09036041
4
cor_humidity_price_fall
##
## Pearson’s product-moment correlation
##
## data: fall_data$Humidity and fall_data$cost
## t = -26.979, df = 13171, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## -0.2449609 -0.2125949
## sample estimates:
## cor
## -0.2288412
Part E & F
When regarding temperature, Summer has a positive correlation and Fall has a weak negative correlation.
This means that during Summer temperature has an effect on cost, while temperature doesn’t really matter
during the fall. In Summer the cooling demand increases while Fall there isn’t as much of a cooling demand
which could explain this. When regarding humidity, Summer has a negative correlation while Fall also has
a negative correlation meaning prices go down with an increased humidity index. We think this may be
because higher humidity may mean rainy days so the demand for cooling goes down when It’s rainy or
cloudy.
Solar Radiation Section
Part 1:
solar_data <- read_csv("SolarRadiationAthens.csv")
## Rows: 262770 Columns: 2
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl (1): SolarWatt
## dttm (1): TimeStamp
##
## i Use ‘spec()‘ to retrieve the full column specification for this data.
## i Specify the column types or set ‘show_col_types = FALSE‘ to quiet this message.
average_solar <- mean(solar_data$SolarWatt, na.rm = TRUE)
min_solar <- min(solar_data$SolarWatt, na.rm = TRUE)
max_solar <- max(solar_data$SolarWatt, na.rm = TRUE)
cat("Average Solar Radiation:", average_solar, "W/m2\n")
## Average Solar Radiation: 193.2395 W/m2
5
cat("Minimum Solar Radiation:", min_solar, "W/m2\n")
## Minimum Solar Radiation: 0 W/m2
cat("Maximum Solar Radiation:", max_solar, "W/m2\n")
## Maximum Solar Radiation: 1457 W/m2
Bonus Question:
solar_data$TimeStamp <- ymd_hms(solar_data$TimeStamp)
## Warning: 360 failed to parse.
solar_data$Month <- month(solar_data$TimeStamp)
area <- 25
efficiency <- 0.2
solar_data$ElectricityGen <- (solar_data$SolarWatt * area * efficiency) / 1000
july_data <- filter(solar_data, Month == 7)
feb_data <- filter(solar_data, Month == 2)
july_daily_avg <- july_data %>%
group_by(date(TimeStamp)) %>%
summarize(Daily_kWh = sum(ElectricityGen, na.rm = TRUE)) %>%
summarise(Average_kWh = mean(Daily_kWh))
feb_daily_avg <- feb_data %>%
group_by(date(TimeStamp)) %>%
summarize(Daily_kWh = sum(ElectricityGen, na.rm = TRUE)) %>%
summarise(Average_kWh = mean(Daily_kWh))
cat("Average Daily Electricity Generation in July (kWh):", july_daily_avg$Average_kWh, "\n")
## Average Daily Electricity Generation in July (kWh): 924.3742
cat("Average Daily Electricity Generation in February (kWh):", feb_daily_avg$Average_kWh, "\n")
## Average Daily Electricity Generation in February (kWh): 542.1898
The average daily electricity generation in July is 924.37 kWh while February is 542.19 kWh. July has longer
daylight hours than February and usually there is more cloud cover in February.
6
