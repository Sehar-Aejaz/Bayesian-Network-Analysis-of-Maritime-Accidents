# Maritime Accident Analysis and Bayesian Network Modeling with Weather Integration

This project analyzes maritime accident and incident reports, integrating external weather data and using Bayesian networks to discover probabilistic dependencies. The analysis includes comprehensive data cleaning, transformation, discretization, and network structure learning using multiple algorithms.

---

## Data Sources

- **accident-incident-reporting-data.csv**: Maritime incident reports including vessel details, dates, accident types, and outcomes.
- **scraped_weather.csv**: Weather data scraped and matched by date and location to enrich the maritime dataset.

---

## Data Preprocessing Workflow

### Cleaning and Transformation
- Replaced `"NULL"` strings with `NA` values.
- Converted date, numeric, and categorical columns to appropriate types.
- Removed extreme outliers based on the 99th percentile of:
  - `Gross Tonnage`
  - `Length Overall`
- Cleaned categorical fields (`Vessel Class`, `Sector`, `Safety System`) by trimming whitespace and standardizing capitalization.

### Weather Data Integration
- Merged maritime and weather datasets by:
  - `Event Date`
  - `Latitude`
  - `Longitude`
- Ensured consistent rounding of coordinates for accurate joins.
- Removed rows with missing weather fields post-merge.

---

## Feature Engineering

### Discretization of Continuous Variables
Binned continuous variables into qualitative categories using quantiles:
- `Gross Tonnage` → `GT_bin` (`Small`, `Medium`, `Large`)
- `Length Overall` → `LO_bin` (`Short`, `Medium`, `Long`)
- `Max temp` and `Min temp` → `Cool`, `Moderate`, `Hot` / `Cold`, `Mild`, `Warm`
- `Total Precip` → `Low`, `Moderate`, `High`
- `Max sustained wind` → `Calm`, `Breezy`, `Windy`

---

## Exploratory Data Analysis

- **Histograms** for all continuous variables (Tonnage, Temperature, Precipitation, etc.)
- **Bar plots** for:
  - Vessel Classes
  - Safety Systems
  - Accident Types
  - Locations
  - Maritime Sectors

---

## Bayesian Network Modeling

Used `bnlearn` to build probabilistic graphical models capturing the relationships between accident characteristics, vessel types, safety systems, and weather.

### Algorithms Applied:
- **Tabu Search (BIC Score)**
- **MMHC (Max-Min Hill Climbing)**
- **Grow-Shrink (GS)**
- **Fast-IAMB**

### Model Evaluation
- All networks were:
  - Fitted using `bn.fit()`
  - Compared using **BIC scores**
  - Visualized as directed acyclic graphs
  - Evaluated based on edge similarity (adjacency matrix comparison)

---

## Technologies Used

- **Language**: R
- **Libraries**:
  - `dplyr`, `tidyr`, `readr`, `lubridate`, `tibble` — Data wrangling
  - `ggplot2` — Visualization
  - `bnlearn` — Bayesian Network structure learning and inference
  - `knitr` — Summary tables

---

## Key Skills Demonstrated

- Real-world data cleaning (with missing and inconsistent entries)
- Spatial and temporal data merging
- Quantile-based discretization
- Advanced probabilistic modeling (Bayesian Networks)
- EDA and result visualization in R

---

## Author

**Sehar Aejaz**  
seharaejaz@gmail.com  

---

## Summary

This project showcases a complete analytical pipeline: from loading and cleaning messy maritime data to enriching it with weather features and discovering hidden relationships via Bayesian networks. The insights derived here can support maritime safety policies, accident prevention strategies, and operational decision-making.

