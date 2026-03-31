# Electric Vehicle Population Dashboard

An interactive R Shiny dashboard analyzing the Electric Vehicle (EV) population in Washington State, built using a dataset of ~200,000 registered vehicles.

## Overview

This dashboard provides a comprehensive exploration of EV adoption trends, manufacturer dominance, vehicle specifications, and geographic distribution across Washington State. It features 10 interactive tabs powered by Plotly visualizations and custom calculated fields.

## Dataset

- **Source:** [Electric Vehicle Population Data – Kaggle](https://www.kaggle.com/datasets/ratikkakkar/electric-vehicle-population-data)
- **Records:** ~200,000 registered EVs in Washington State
- **Key Fields:** VIN, County, City, State, Model Year, Make, Model, Electric Vehicle Type, CAFV Eligibility, Electric Range, Base MSRP, Legislative District, Vehicle Location

## Features

- **10 Interactive Tabs** covering registration trends, manufacturer analysis, geographic distribution, range & pricing insights, and more
- **Plotly Interactivity** — hover tooltips, zoom, pan, and dynamic filtering
- **Calculated Fields** — Price Tier, Range per Dollar, and other derived metrics
- **Responsive Layout** — clean UI with sidebar filters and tabbed navigation

## Tech Stack

- **R** / **R Shiny**
- **Plotly** for interactive charts
- **dplyr**, **tidyr** for data wrangling
- **shinydashboard** for layout

## Getting Started

### Prerequisites

- R (≥ 4.0)
- RStudio (recommended)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/om-sri/EV-Population-Dashboard.git
   cd EV-Population-Dashboard
   ```

2. Install required packages:
   ```r
   install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr", "ggplot2", "DT"))
   ```

3. Run the app:
   ```r
   shiny::runApp()
   ```

