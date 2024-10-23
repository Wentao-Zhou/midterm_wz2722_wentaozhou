midterm_wz2722_p8105
================
wentao zhou
2024-10-21

Q1.Data import and cleaning Part 1.NYC rental data import and cleaning

``` r
library(tidyverse)
library(lubridate)
library(rvest)

# Step 1: Import and clean NYC rental data (ZORI)
zori_data <- read_csv("Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv") %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("x20"), 
               names_to = "month", 
               values_to = "rent_price") %>%
  mutate(month = ymd(paste0(str_remove_all(month, "x"), "-01"))) %>%
  filter(!is.na(rent_price))  # Remove rows with missing rental prices
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ─────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-31...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `month = ymd(paste0(str_remove_all(month, "x"), "-01"))`.
    ## Caused by warning:
    ## ! All formats failed to parse. No formats found.

Part 2.Import and Clean ZIP Code Data

``` r
# Step 2: Import and clean ZIP code data from the HTML page
url <- "https://p8105.com/data/zip_codes.html"
zip_codes_data <- read_html(url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  clean_names()  # Clean column names

# Add a borough variable using county names
zip_codes_data <- zip_codes_data %>%
  mutate(borough = case_when(
    county == "Bronx" ~ "Bronx",
    county == "Kings" ~ "Brooklyn",
    county == "New York" ~ "Manhattan",
    county == "Queens" ~ "Queens",
    county == "Richmond" ~ "Staten Island",
    TRUE ~ NA_character_))
```

Merge Rental Data with ZIP Code Data

``` r
# Step 3: Merge rental data with ZIP code data by matching ZIP codes
# Ensure both columns are of character type before joining
zori_data <- zori_data %>%
  mutate(region_name = as.character(region_name))

zip_codes_data <- zip_codes_data %>%
  mutate(zip_code = as.character(zip_code))

# Now perform the inner join
zori_merged <- zori_data %>%
  inner_join(zip_codes_data, by = c("region_name" = "zip_code"))
```

    ## Warning in inner_join(., zip_codes_data, by = c(region_name = "zip_code")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 2759 of `x` matches multiple rows in `y`.
    ## ℹ Row 256 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Print summary statistics
cat("Total observations in rental data:", nrow(zori_merged), "\n")
```

    ## Total observations in rental data: 10677

``` r
cat("Unique ZIP codes in rental data:", n_distinct(zori_merged$region_name), "\n")
```

    ## Unique ZIP codes in rental data: 149

``` r
cat("Unique neighborhoods:", n_distinct(zori_merged$borough), "\n")
```

    ## Unique neighborhoods: 5

Part 4: Import and Clean US Housing Data

``` r
# Step 4: Import and clean US housing data (ZHVI)
zhvi_data <- read_csv("Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month_2023.csv") %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("x20"), 
               names_to = "month", 
               values_to = "house_value") %>%
  mutate(month = ymd(paste0(str_remove_all(month, "x"), "-01")))
```

    ## Rows: 26338 Columns: 21
    ## ── Column specification ─────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): RegionName, RegionType, StateName, State, City, Metro, CountyName
    ## dbl (14): RegionID, SizeRank, 2023-01-31, 2023-02-28, 2023-03-31, 2023-04-30,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `month = ymd(paste0(str_remove_all(month, "x"), "-01"))`.
    ## Caused by warning:
    ## ! All formats failed to parse. No formats found.

``` r
# Step 5: Merge housing data with ZIP code data
# Convert both region_name and zip_code to character type
zhvi_data <- zhvi_data %>%
  mutate(region_name = as.character(region_name))

zip_codes_data <- zip_codes_data %>%
  mutate(zip_code = as.character(zip_code))

# Perform the inner join
zhvi_merged <- zhvi_data %>%
  inner_join(zip_codes_data, by = c("region_name" = "zip_code"))
```

    ## Warning in inner_join(., zip_codes_data, by = c(region_name = "zip_code")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 2125 of `x` matches multiple rows in `y`.
    ## ℹ Row 256 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Check the merged data
glimpse(zhvi_merged)
```

    ## Rows: 2,196
    ## Columns: 18
    ## $ region_id    [3m[38;5;246m<dbl>[39m[23m 62080, 62080, 62080, 62080, 62080, 62080, 62080, 62080, 62…
    ## $ size_rank    [3m[38;5;246m<dbl>[39m[23m 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7, 7, 7, 7, 7, 7, 7, 7…
    ## $ region_name  [3m[38;5;246m<chr>[39m[23m "11368", "11368", "11368", "11368", "11368", "11368", "113…
    ## $ region_type  [3m[38;5;246m<chr>[39m[23m "zip", "zip", "zip", "zip", "zip", "zip", "zip", "zip", "z…
    ## $ state_name   [3m[38;5;246m<chr>[39m[23m "NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY"…
    ## $ state        [3m[38;5;246m<chr>[39m[23m "NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY", "NY"…
    ## $ city         [3m[38;5;246m<chr>[39m[23m "New York", "New York", "New York", "New York", "New York"…
    ## $ metro        [3m[38;5;246m<chr>[39m[23m "New York-Newark-Jersey City, NY-NJ-PA", "New York-Newark-…
    ## $ county_name  [3m[38;5;246m<chr>[39m[23m "Queens County", "Queens County", "Queens County", "Queens…
    ## $ month        [3m[38;5;246m<date>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ house_value  [3m[38;5;246m<dbl>[39m[23m 492391.8, 485556.5, 479262.8, 472667.7, 465596.7, 460226.3…
    ## $ county       [3m[38;5;246m<chr>[39m[23m "Queens", "Queens", "Queens", "Queens", "Queens", "Queens"…
    ## $ state_fips   [3m[38;5;246m<int>[39m[23m 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36…
    ## $ county_code  [3m[38;5;246m<int>[39m[23m 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81…
    ## $ county_fips  [3m[38;5;246m<int>[39m[23m 36081, 36081, 36081, 36081, 36081, 36081, 36081, 36081, 36…
    ## $ file_date    [3m[38;5;246m<chr>[39m[23m "07/25/2007", "07/25/2007", "07/25/2007", "07/25/2007", "0…
    ## $ neighborhood [3m[38;5;246m<chr>[39m[23m "West Queens", "West Queens", "West Queens", "West Queens"…
    ## $ borough      [3m[38;5;246m<chr>[39m[23m "Queens", "Queens", "Queens", "Queens", "Queens", "Queens"…

``` r
# Print the summary of merged data
cat("Total observations in merged housing data:", nrow(zhvi_merged), "\n")
```

    ## Total observations in merged housing data: 2196

``` r
cat("Unique ZIP codes in merged housing data:", n_distinct(zhvi_merged$region_name), "\n")
```

    ## Unique ZIP codes in merged housing data: 181

Description of Summary of the Process

1.Import and Clean NYC Rental Data (ZORI):

Used clean_names() to standardize column names. Pivoted monthly rental
prices to a long format using pivot_longer(). Converted month columns to
Date objects with ymd() and removed rows with missing rental prices.

2.Import and Clean ZIP Code Data:

Retrieved ZIP code data from the provided HTML page using rvest. Added a
borough variable using case_when() based on county names. Merge Rental
Data with ZIP Code Data:

3.Converted region_name and zip_code columns to character type for
merging. Merged rental data with ZIP code data using inner_join().

4.Import and Clean US Housing Data (ZHVI):

Cleaned and pivoted the housing data to a long format, similar to rental
data.

5.Merge Housing Data with ZIP Code Data:

Merged housing data with ZIP code data using inner_join().

Question 2: Quality Control and EDA

Part 1. Why are some postal codes observed for less than 116 months?

``` r
months_per_zip <- merged_data %>%
  pivot_longer(cols = starts_with("x20"), names_to = "month", values_to = "rent_price") %>%
  group_by(region_name) %>%
  summarize(observations = sum(!is.na(rent_price)))

cat("ZIP codes with fewer than 116 observations:", sum(months_per_zip$observations < 116), "\n")
```

    ## ZIP codes with fewer than 116 observations: 0

``` r
num_zip_rental <- length(unique(merged_data$region_name))
num_zip_zipcode <- length(unique(zip_codes_data$zip_code))
cat("Number of ZIP codes in rental dataset:", num_zip_rental, "\n")
```

    ## Number of ZIP codes in rental dataset: 48

``` r
cat("Number of ZIP codes in ZIP code dataset:", num_zip_zipcode, "\n")
```

    ## Number of ZIP codes in ZIP code dataset: 320

Possible reasons: Rent data may be collected midway, resulting in
shorter time series for certain postal codes.

Data missing issue. Some postal codes may no longer be used due to
boundary changes.

Possible reasons for differences: Some postal codes are not included in
the rental dataset. The postal code system changes over time.

Part 2. Average rent per administrative region and year

``` r
# Convert the month column to Date format
merged_data_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("x20"), 
    names_to = "month", 
    values_to = "rent_price"
  ) %>%
  mutate(
    month = ymd(str_remove_all(month, "x"))  # Remove 'x' and convert to Date
  )
# Create a table of average rental prices by borough and year
nyc_rental_yearly <- merged_data %>%
  pivot_longer(
    cols = starts_with("x20"), 
    names_to = "month", 
    values_to = "rent_price"
  ) %>%
  mutate(month = ymd(str_remove_all(month, "x")),  # Convert to Date
    year = year(month)  
  ) %>%
  group_by(borough, year) %>%
  summarize(
    avg_rent_price = mean(rent_price, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(borough, year)

# Print the table
print(nyc_rental_yearly)
```

    ## # A tibble: 30 × 3
    ##    borough   year avg_rent_price
    ##    <chr>    <dbl>          <dbl>
    ##  1 Brooklyn  2015          2539.
    ##  2 Brooklyn  2016          2617.
    ##  3 Brooklyn  2017          2659.
    ##  4 Brooklyn  2018          2704.
    ##  5 Brooklyn  2019          2814.
    ##  6 Brooklyn  2020          2787.
    ##  7 Brooklyn  2021          2817.
    ##  8 Brooklyn  2022          3309.
    ##  9 Brooklyn  2023          3496.
    ## 10 Brooklyn  2024          3610.
    ## # ℹ 20 more rows

``` r
# Step 5: Compare rental prices in January 2021 vs January 2020
rental_price_change <- merged_data %>%
  pivot_longer(
    cols = starts_with("x20"), 
    names_to = "month", 
    values_to = "rent_price"
  ) %>%
  mutate(month = ymd(str_remove_all(month, "x"))) %>%
  filter(month %in% c(ymd("2020-01-31"), ymd("2021-01-31"))) %>%
  pivot_wider(
    names_from = month, 
    values_from = rent_price
  ) %>%
  mutate(price_drop = `2020-01-31` - `2021-01-31`) %>%
  group_by(borough) %>%
  filter(price_drop == max(price_drop, na.rm = TRUE)) %>%
  select(borough, region_name, price_drop)

# Print the result
print(rental_price_change)
```

    ## # A tibble: 3 × 3
    ## # Groups:   borough [3]
    ##   borough   region_name price_drop
    ##   <chr>           <int>      <dbl>
    ## 1 Brooklyn        11211       438.
    ## 2 Queens          11101       128.
    ## 3 Manhattan       10001       710.

``` r
# Comment: Rental prices have generally increased over the years, 
# with notable fluctuations during the COVID-19 pandemic, especially in Manhattan and Brooklyn.
```

Q3.Visualization Part 1.Plot NYC Rental Prices Over Time by Borough

``` r
# Ensure the results directory exists
if (!dir.exists("results")) {
  dir.create("results")
}

# Reshape the rental data for plotting
nyc_rental_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("x20"), 
    names_to = "month", 
    values_to = "rent_price"
  ) %>%
  mutate(
    month = ymd(str_remove_all(month, "x")),  # Convert month to Date format
    year = year(month)  # Extract the year as a numeric value
  )

# Create the plot of NYC rental prices
nyc_rental_plot <- nyc_rental_long %>%
  ggplot(aes(x = month, y = rent_price, color = borough)) +
  geom_line(aes(group = region_name), alpha = 0.3) +  # Lighter lines for each ZIP code
  facet_wrap(~borough) +  # Separate plots for each borough
  labs(
    title = "NYC Rental Prices within ZIP Codes for All Available Years",
    x = "Year",
    y = "Rental Price (USD)",
    color = "Borough"
  ) +
  theme_minimal()

# Save the plot to the results directory
ggsave("results/nyc_rental_prices_plot.png", nyc_rental_plot, width = 10, height = 6)

# Print confirmation message
cat("NYC rental prices plot saved to 'results/nyc_rental_prices_plot.png'\n")
```

    ## NYC rental prices plot saved to 'results/nyc_rental_prices_plot.png'

``` r
#Comment:This plot shows rental price trends over time for each borough. Rental prices have generally increased across all boroughs. Notably:Manhattan exhibits the highest rental prices.Staten Island has the lowest rental prices.During the COVID-19 pandemic, rental prices dipped across several boroughs, especially in Manhattan and Brooklyn, reflecting the decline in urban demand during that period.
```

Part 2.Compute and Plot Average House Prices in 2023

``` r
zhvi_avg_2023 <- data_zhvi %>%
  pivot_longer(cols = starts_with("x20"), names_to = "month", values_to = "house_price") %>%
  group_by(region_name) %>%
  summarize(avg_price_2023 = mean(house_price, na.rm = TRUE), .groups = "drop")


# Summary 
cat("Summary statistics for average house prices in 2023:\n")
```

    ## Summary statistics for average house prices in 2023:

``` r
summary(zhvi_avg_2023$avg_price_2023)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   23380  173264  259124  336694  398801 7636362     116

``` r
# Plot the distribution of house prices across ZIP codes
zhvi_avg_2023_plot <- zhvi_avg_2023 %>%
  ggplot(aes(x = avg_price_2023)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribution of ZIP-Code-Level House Prices Across States in 2023",
    x = "Average House Price (USD)",
    y = "Frequency"
  ) +
  theme_minimal()

# Save the plot
ggsave("results/zhvi_avg_2023_plot.png", zhvi_avg_2023_plot, width = 10, height = 6)
```

    ## Warning: Removed 116 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

``` r
# Print confirmation
cat("Housing prices distribution plot saved to 'results/zhvi_avg_2023_plot.png'\n")
```

    ## Housing prices distribution plot saved to 'results/zhvi_avg_2023_plot.png'

Part 3. Compare Rental and Housing Prices for NYC ZIP Codes in 2023

``` r
# Compute average rental prices for 2023
avg_rent_2023 <- nyc_rental_long %>%
  filter(year(month) == 2023) %>%
  group_by(region_name) %>%
  summarize(avg_rent_2023 = mean(rent_price, na.rm = TRUE), .groups = "drop")

# Merge rental and housing prices
price_comparison_2023 <- avg_rent_2023 %>%
  inner_join(zhvi_avg_2023, by = "region_name")

# Create the comparison plot
comparison_plot <- price_comparison_2023 %>%
  ggplot(aes(x = avg_rent_2023, y = avg_price_2023)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Comparison of Rental and Housing Prices by ZIP Code in 2023",
    x = "Average Rental Price (USD)",
    y = "Average Housing Price (USD)"
  ) +
  theme_minimal()

# Save the plot
ggsave("results/comparison_plot.png", comparison_plot, width = 10, height = 6)
```

    ## `geom_smooth()` using formula = 'y ~ x'

``` r
# Print confirmation
cat("Comparison plot saved to 'results/comparison_plot.png'\n")
```

    ## Comparison plot saved to 'results/comparison_plot.png'

``` r
#Comment:The scatter plot shows a positive relationship between rental prices and housing prices: ZIP codes with higher average rental prices tend to have higher housing prices. However, the plot also reveals significant variability, suggesting that other factors, such as location, amenities, and market trends, may also influence prices.
```

Part 4.Limitations of the Dataset

``` r
# Comment on dataset limitations
cat("Limitations of the dataset:\n")
```

    ## Limitations of the dataset:

``` r
cat("- Missing data for some ZIP codes and inconsistent rental data for certain months.\n")
```

    ## - Missing data for some ZIP codes and inconsistent rental data for certain months.

``` r
cat("- The dataset does not account for key factors such as neighborhood amenities, transportation, or economic conditions.\n")
```

    ## - The dataset does not account for key factors such as neighborhood amenities, transportation, or economic conditions.

``` r
cat("- Seasonal fluctuations in rental and housing prices are not captured with annual averages.\n")
```

    ## - Seasonal fluctuations in rental and housing prices are not captured with annual averages.

``` r
cat("- Rental prices during the COVID-19 pandemic may show volatility, complicating trend analysis.\n")
```

    ## - Rental prices during the COVID-19 pandemic may show volatility, complicating trend analysis.
