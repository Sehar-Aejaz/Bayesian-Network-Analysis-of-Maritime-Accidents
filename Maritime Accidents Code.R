##############LOADING AND CLEANING THE DATA####################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(tibble)

# Load the CSV file
data <- read_csv("/Users/seharaejaz/Downloads/accident-incident-reporting-data.csv")

# Replace "NULL" strings with actual NA
data[data == "NULL"] <- NA

# Convert columns to appropriate types
data_clean <- data %>%
  mutate(
    `Event Date` = dmy(`Event Date`),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    `Gross Tonnage` = as.numeric(`Gross Tonnage`),
    `Length Overall` = as.numeric(`Length Overall`),
    `Year of Build` = as.integer(`Year of Build`),
    Sector = as.factor(Sector),
    `Vessel Class` = as.factor(`Vessel Class`),
    `Safety System` = as.factor(`Safety System`)
  )

# View summary
summary(data)

# Read scraped weather data
weather <- read_csv("/Users/seharaejaz/scraped_weather.csv")

weather_clean <- weather %>%
  rename(
    Latitude = lat,
    Longitude = lon,
    `Event Date` = date
  ) %>%
  mutate(
    `Event Date` = as.Date(`Event Date`),
    Latitude = round(as.numeric(Latitude), 5),
    Longitude = round(as.numeric(Longitude), 5),
    `Max temp` = as.numeric(gsub("℉", "", `Max temp`)),
    `Min temp` = as.numeric(gsub("℉", "", `Min temp`)),
    `Total Precip` = as.numeric(gsub("in", "", `Total Precip`)),
    `Max daily precip` = as.numeric(gsub("in", "", `Max daily precip`)),
    `Max sustained wind` = as.numeric(gsub("mph", "", `Max sustained wind`))
  )

# Ensure matching columns in data_clean are rounded too
data_clean <- data_clean %>%
  mutate(
    Latitude = round(as.numeric(Latitude), 5),
    Longitude = round(as.numeric(Longitude), 5)
  )

# Merge datasets on Date, Latitude, Longitude
merged_data <- left_join(
  data_clean,
  weather_clean,
  by = c("Event Date", "Latitude", "Longitude")
)

# Drop rows where any key weather value is NA
merged_data <- merged_data %>%
  filter(
    !is.na(`Max temp`) &
      !is.na(`Min temp`) &
      !is.na(`Total Precip`) &
      !is.na(`Max daily precip`) &
      !is.na(`Max sustained wind`) &
      !is.na(`Latitude`) &
      !is.na(`Longitude`) &
      !is.na(`Event Date`)
  )

# Remove redundant columns
merged_data <- merged_data %>%
  select(-`Max daily precip`, -`Rain days`)

summary(merged_data)

################## FILTER EXTREME OUTLIERS######################
# Calculate 99th percentiles
gt_threshold <- quantile(merged_data$`Gross Tonnage`, probs = 0.99, na.rm = TRUE)
lo_threshold <- quantile(merged_data$`Length Overall`, probs = 0.99, na.rm = TRUE)

# Filter using these percentiles
merged_data <- merged_data %>%
  filter(`Gross Tonnage` <= gt_threshold,
         `Length Overall` <= lo_threshold)

####################### CLEAN CATEGORICAL COLUMNS######################
cat_cols <- c("Vessel Class", "Sector", "Safety System")

merged_data[cat_cols] <- lapply(merged_data[cat_cols], function(x) {
  x <- as.character(x)
  x <- trimws(x)  # Remove leading/trailing whitespace
  x <- tools::toTitleCase(tolower(x))  # Standardize case
  return(x)
})

# Check for remaining missing values
sum(is.na(merged_data))

# Drop rows with NA as they can cause error in the bayesian network
merged_data <- merged_data %>%
  drop_na()

############## DISCRETIZE CONTINUOUS VARIABLES#############

# Calculate breaks separately
precip_breaks <- unique(quantile(merged_data$`Total Precip`, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE))

# Ensure there are at least 4 break points (for 3 bins)
if (length(precip_breaks) < 4) {
  # Fallback to evenly spaced bins 
  precip_breaks <- c(min(merged_data$`Total Precip`, na.rm = TRUE),
                     quantile(merged_data$`Total Precip`, probs = c(0.5), na.rm = TRUE),
                     max(merged_data$`Total Precip`, na.rm = TRUE) + 1e-6)
  labels_precip <- c("Low", "High")  # only two bins if three breaks
} else {
  labels_precip <- c("Low", "Moderate", "High")
}



# Discretize continuous variables
merged_data <- merged_data %>%
  mutate(
    GT_bin = cut(`Gross Tonnage`,
                 breaks = quantile(`Gross Tonnage`, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                 labels = c("Small", "Medium", "Large"), include.lowest = TRUE),
    
    LO_bin = cut(`Length Overall`,
                 breaks = quantile(`Length Overall`, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                 labels = c("Short", "Medium", "Long"), include.lowest = TRUE),
    
    MaxTemp_bin = cut(`Max temp`,
                      breaks = quantile(`Max temp`, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                      labels = c("Cool", "Moderate", "Hot"), include.lowest = TRUE),
    
    MinTemp_bin = cut(`Min temp`,
                      breaks = quantile(`Min temp`, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                      labels = c("Cold", "Mild", "Warm"), include.lowest = TRUE),
    
    Precip_bin = cut(`Total Precip`,
                     breaks = precip_breaks,
                     labels = labels_precip, include.lowest = TRUE),
    
    Wind_bin = cut(`Max sustained wind`,
                   breaks = quantile(`Max sustained wind`, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                   labels = c("Calm", "Breezy", "Windy"), include.lowest = TRUE)
  )

############### DISTRIBUTIONS ##############

library(knitr)

# Select numeric variables
numeric_vars <- merged_data %>%
  select(where(is.numeric))

# Compute summary statistics
summary_stats <- numeric_vars %>%
  summarise_all(list(
    count = ~sum(!is.na(.)),
    mean = ~round(mean(., na.rm = TRUE), 2),
    median = ~round(median(., na.rm = TRUE), 2),
    sd = ~round(sd(., na.rm = TRUE), 2),
    min = ~round(min(., na.rm = TRUE), 2),
    max = ~round(max(., na.rm = TRUE), 2)
  ))

# Transpose and print table
print(as.data.frame(t(summary_stats)))

#HISTOGRAMS

# Gross Tonnage
ggplot(merged_data, aes(x = `Gross Tonnage`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Gross Tonnage", x = "Gross Tonnage", y = "Frequency")

# Length Overall
ggplot(merged_data, aes(x = `Length Overall`)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Length Overall", x = "Length Overall (m)", y = "Frequency")

# Max Temp
ggplot(merged_data, aes(x = `Max temp`)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Max Temperature", x = "Max Temp (°C)", y = "Frequency")

# Min Temp
ggplot(merged_data, aes(x = `Min temp`)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Min Temperature", x = "Min Temp (°C)", y = "Frequency")

# Total Precip
ggplot(merged_data, aes(x = `Total Precip`)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Total Precip", x = "Total Precip (inches)", y = "Frequency")

# Total Precip
ggplot(merged_data, aes(x = `Max sustained wind`)) +
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Max sustained wind", x = "Max sustained wind", y = "Frequency")

#BAR PLOTS

# Vessel Class
ggplot(merged_data, aes(x = `Vessel Class`)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Distribution of Vessel Classes", x = "Vessel Class", y = "Count")

#Safety System
ggplot(merged_data, aes(x = `Safety System`)) +
  geom_bar(fill = "lightgreen", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Distribution of Safety Systems", x = "Safety System", y = "Count")

# Sector
ggplot(merged_data, aes(x = `Sector`)) +
  geom_bar(fill = "orchid", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Sector-wise Accident Distribution", x = "Sector", y = "Count")

# Where Happened
ggplot(merged_data, aes(x = `Where Happened`)) +
  geom_bar(fill = "coral", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Location of Accidents", x = "Where Happened", y = "Count")

# What Happened
ggplot(merged_data, aes(x = `What Happened`)) +
  geom_bar(fill = "tomato", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Types of Accidents", x = "What Happened", y = "Count")

#####################BAYESIAN NETWORKS##########################
library(bnlearn)

# Select relevant columns and convert to factors
merged_data <- merged_data %>%
  select(`What Happened`, `Where Happened`, `Sector`, `Number of Injured Persons`, 
         `Vessel Class`, GT_bin, LO_bin, `Safety System`,
         MaxTemp_bin, MinTemp_bin, Precip_bin, Wind_bin)

# First, convert all character columns to factors
merged_data[] <- lapply(merged_data, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else if (is.numeric(x)) {
    # Convert numeric to factors if it has a small number of unique values (e.g., 3 or less)
    if (length(unique(x)) <= 5) {
      as.factor(x)
    } else {
      stop("Numeric column has too many levels: needs proper discretization")
    }
  } else {
    x
  }
})

# Transform to a datframe
merged_data <- as.data.frame(merged_data)

# Bayesian Network Structure Learning

# 1. Tabu Search with BIC Score
bn_tabu <- tabu(merged_data, score = "bic")

# 2. MMHC
bn_mmhc <- mmhc(merged_data)

# 3. Grow-Shrink (GS)
bn_gs <- gs(merged_data)
bn_gs_dag <- cextend(bn_gs)# Convert to fully directed DAG

# 4. Fast-IAMB
bn_fastiamb <- fast.iamb(merged_data)
bn_fastiamb_dag <- cextend(bn_fastiamb)# Convert to fully directed DAG

#Plot the learned networks
par(mfrow = c(2, 2))  

# Plot each of the Bayesian networks
plot(bn_tabu, main = "Tabu Search (BIC)")
plot(bn_mmhc, main = "MMHC")
plot(bn_gs_dag, main = "Grow-Shrink (GS)")
plot(bn_fastiamb_dag, main = "Fast-IAMB")

#Parameter Learning /Fit the models to data
fitted_tabu <- bn.fit(bn_tabu, data = merged_data)
fitted_mmhc <- bn.fit(bn_mmhc, data = merged_data)
fitted_gs <- bn.fit(bn_gs_dag, data = merged_data)
fitted_fastiamb <- bn.fit(bn_fastiamb_dag, data = merged_data)

#Compare the networks using BIC scores
bic_tabu <- score(bn_tabu, merged_data, type = "bic")
bic_mmhc <- score(bn_mmhc, merged_data, type = "bic")
bic_gs <- score(bn_gs_dag, merged_data, type = "bic")
bic_fastiamb <- score(bn_fastiamb_dag, merged_data, type = "bic")

#BIC scores
cat("BIC Scores:\n")
cat("Tabu Search (BIC): ", bic_tabu, "\n")
cat("MMHC BIC: ", bic_mmhc, "\n")
cat("Grow-Shrink (GS) BIC: ", bic_gs, "\n")
cat("Fast-IAMB BIC: ", bic_fastiamb, "\n")

#Compare the learned networks
# Function to compare two networks based on their adjacency matrices
compare_networks <- function(bn1, bn2) {
  sum(amat(bn1) != amat(bn2))
}

cat("Network Comparison (Differences in Edges):\n")
cat("Tabu vs MMHC: ", compare_networks(bn_tabu, bn_mmhc), "\n")
cat("Tabu vs GS: ", compare_networks(bn_tabu, bn_gs_dag), "\n")
cat("Tabu vs Fast-IAMB: ", compare_networks(bn_tabu, bn_fastiamb_dag), "\n")

#####################################################################
