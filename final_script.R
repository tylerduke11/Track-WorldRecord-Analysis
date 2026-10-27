# DATA PREP

# Load packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)
library(fpp3)
library(gt)


# Load raw dataset exported from Python
pg_raw <- read_csv("~/Downloads/NCSU/Blog Post/records_by_progression.csv")


# DATA CLEANING

# Make new columns needed for data visualizations
pg_clean <- pg_raw %>%
  mutate(
    date = as.Date(date),
    dob = as.Date(dob),
    gender = str_to_lower(gender),
    age = floor(time_length(lubridate::interval(dob, date), "years")),
    year = year(date),
    month = month(date),
    decade = floor(year / 10) * 10,
    event_type = case_when(
      unit == 'points' ~ 'Multi',
      unit == 'meters' ~ 'Field',
      grepl('Short Track', discipline) ~ 'Indoor Track',
      grepl('Race Walk|Road|Marathon|Hour', discipline) ~ 'Road',
      TRUE ~ 'Track'
    )
  ) %>%
  filter(age_category == 'SENIOR') %>%
  arrange(event_id, date, record_number) %>%
  group_by(event_id) %>%
  mutate(pct_improvement = case_when(
    measure_type == "time" ~ ((lag(mark) - mark) / lag(mark)) * 100,
    TRUE ~ ((mark - lag(mark)) / lag(mark)) * 100
  )) %>%
  ungroup() %>%
  select(
    event_id, event_type, record_number, discipline, gender, age_category, # category divisions
    mark, performance_raw, measure_type, unit, pct_improvement, # record info
    athlete, country, dob, age, # athlete info
    date, month, year, decade # time divisions
  ) 



pg.ts <- pg_clean %>% 
  group_by(year) %>% 
  summarize(records_broken = n(), .groups = 'drop') %>% 
  as_tsibble(index = year) %>% 
  fill_gaps(records_broken = 0) 


# Exploring the idea that there might be a seasonal trend every 4 years in olympic years

summer_olympic_years <- c(
  1900, 1904, 1908, 1912, # cancelled 1916 for WW1
  1920, 1924, 1928, 1932, 1936, # cancelled 1940 and 1944 for WW2
  1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984,
  1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2021, 2024, # 2020 postponed to 2021 for COVID
  2028, 2032, 2036 # future years
)

autoplot(pg.ts, records_broken, color = 'steelblue') +
  # add a vertical line at each olympic year
  geom_vline(xintercept = summer_olympic_years[1:(length(summer_olympic_years) - 3)], linetype = "dashed", alpha = 0.2) +
  scale_x_continuous(breaks = seq(1900, 2030, by = 10)) +
  labs(
    title = "Track and Field World Records Broken by Year",
    subtitle = "Grey lines indicating Summer Olympic years",
    x = "Year",
    y = "Number of Records",
    caption = "Summer Olympics were cancelled in 1916 (WW1), 1940 & 1944 (WW2), and 2020 was moved to 2021 (COVID-19)"
  ) +
  guides(colour = guide_legend(title = "Record Age Category")) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0))




# Analyze one year as a quarter to see if there is a seasonal cycle
pg.ts_olympic <- pg.ts %>%
  mutate(
    original_year = year,
    olympic_cycle = floor((year - 1900) / 4),
    cycle_position = (year - 1900) %% 4 + 1,
    synthetic_year = 1900 + olympic_cycle,
    quarter = yearquarter(paste0(synthetic_year, " Q", cycle_position))
  ) %>%
  select(original_year, quarter, records_broken) %>%
  as_tsibble(index = quarter)

# STL Decomposition
stl_dcmp <- pg.ts_olympic %>% model(stl = STL(records_broken))

# Components Plot
components(stl_dcmp) %>% autoplot() + theme_bw()

stl_features <- pg.ts_olympic %>% features(records_broken, feat_stl)
stl_features






# Fit models on the quarterly Olympic data
fit <- pg.ts_olympic %>% model(arima = ARIMA(records_broken))
fit
# best model found: ARIMA(0,1,1)(1,0,0)[4]

# Forecast 10 cycles ahead (through 2036 olympics)
fc <- fit %>% forecast(h = 10)


# Transform quarterly index back to original calendar years
fc_calendar <- fc %>%
  as_tibble() %>%
  mutate(
    synthetic_year = year(quarter),
    cycle_position = quarter(quarter),
    original_year = 1900 + (synthetic_year - 1900) * 4 + (cycle_position - 1),
    point_forecast = .mean
  ) %>%
  select(.model, original_year, point_forecast, records_broken)
fc_calendar %>% filter(original_year %in% summer_olympic_years)

hist_calendar <- pg.ts_olympic %>% as_tibble()



## MAIN FIGURE ##

# Plot historical and forecast lines
ggplot() +
  # Olympic lines
  geom_vline(xintercept = summer_olympic_years, linetype = "dashed", alpha = 0.2) +
  
  # Historical line
  geom_line(
    data = hist_calendar, aes(x = original_year, y = records_broken, color = "Historical"),
    linewidth = 0.6
  ) +
  
  # Forecast line
  geom_line(
    data = fc_calendar, aes(x = original_year, y = point_forecast, color = "Forecast"),
    linewidth = 0.6
  ) +
  
  # Set line colors and legend labels
  scale_color_manual(
    name = "Data",
    values = c("Historical" = "steelblue", "Forecast" = "firebrick2")
  ) +
  
  scale_x_continuous(breaks = seq(1900, 2040, by = 10)) +
  labs(
    title = "Track and Field World Records Broken by Year - Forecast",
    subtitle = "Grey lines indicating Summer Olympic years",
    x = "Year",
    y = "Number of Records",
    caption = "Summer Olympics were cancelled in 1916 (WW1), 1940 & 1944 (WW2), and 2020 was moved to 2021 (COVID-19)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )


# Prediction intervals table
fc %>%
  hilo(level = c(80, 90, 95)) %>%
  unpack_hilo(c("80%", "90%", "95%")) %>%
  as_tibble() %>%
  mutate(
    original_year = 1900 + (year(quarter) - 1900) * 4 + (quarter(quarter) - 1),
    # CI80 = paste0('[', round(`80%_lower`, 2), ', ', round(`80%_upper`, 2), ']'),
    CI90 = paste0('[', round(`90%_lower`, 2), ', ', round(`90%_upper`, 2), ']'),
    # CI95 = paste0('[', round(`95%_lower`, 2), ', ', round(`95%_upper`, 2), ']'),
    MCI = paste(round(.mean, 2), CI90)
  ) %>%
  select(Year = original_year, MCI) 




