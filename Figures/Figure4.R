library(tidyverse)
library(readxl)
library(RColorBrewer)
library(patchwork)

# === Continent mapping
continent_map <- tibble::tibble(
  Country = c("Chile", "Peru", "China", "Dem. Republic of the Congo", "Australia", "United States", 
    "Mexico", "Russia", "Canada", "Zambia", "Kazakhstan", 
    "Indonesia", "Brazil", "Mongolia", "Argentina", "Ecuador", "Iran", "Poland"),
  Continent = c("South America", "South America", "Asia", "Africa", "Oceania", "North America",
    "North America", "Europe", "North America", "Africa", "Asia",
    "Asia", "South America", "Asia", "South America", "South America", "Asia", "Europe")
)

# === Load data
Base_Julia_High <- read_csv("Results/Ambitious-Baseline-High Capacity-Baseline-Baseline_OtherLow/Base_Julia.csv")

Base_Julia_EDB   <-  read_csv("Results/Ambitious-Baseline-Baseline-Baseline-Baseline/Base_Julia.csv")

Deposit_cap <- read_csv("C:/Users/simon/OneDrive/Desktop/Copper Research/Critical Minerals EV model/Critical-Minerals-EV-main - Updated/Parameters/Data_final.csv") |> select(d = Deposit_Name, Country)
const            <- read_excel("C:/Users/simon/OneDrive/Desktop/Copper Research/Copper2.0/Prod_of_const_country.xlsx")

# === Helper function to process data
get_results_total <- function(data, Deposit_cap, const) {
  data |> 
    left_join(Deposit_cap, join_by(d)) |> 
    filter(t <= 2050) |> 
    group_by(t, Country) |> 
    summarise(Production = sum(tons_extracted1 + tons_extracted2 + tons_extracted3), .groups = "drop") |> 
    left_join(const, join_by(Country, t)) |> 
    mutate(Production = rowSums(across(c(Production.x, Production.y)), na.rm = TRUE))
}

# === Get production totals and top countries
Results_High <- get_results_total(Base_Julia_High, Deposit_cap, const)
Results_EDB   <- get_results_total(Base_Julia_EDB,   Deposit_cap, const)

Top15_High <- Results_High |> group_by(Country) |> summarise(Total = sum(Production), .groups = "drop") |> arrange(desc(Total)) |> slice(1:15) |> pull(Country)
Top15_EDB   <- Results_EDB   |> group_by(Country) |> summarise(Total = sum(Production), .groups = "drop") |> arrange(desc(Total)) |> slice(1:15) |> pull(Country)

All_top_countries <- union(Top15_High, Top15_EDB)

# === Assign color vector
colors_all <- colorRampPalette(brewer.pal(11, "Set1"))(length(All_top_countries))
color_vec <- setNames(colors_all, All_top_countries)
color_vec["Rest of World"] <- "gray0"

# === Function to create plot
make_plot <- function(Base_Julia, Top15, scenario_title) {
  df <- Base_Julia |> 
    left_join(Deposit_cap, join_by(d)) |> 
    filter(t <= 2050) |> 
    group_by(t, Country) |> 
    summarise(Production = sum(tons_extracted1 + tons_extracted2 + tons_extracted3), .groups = "drop") |> 
    left_join(const, join_by(Country, t)) |> 
    mutate(Production = rowSums(across(c(Production.x, Production.y)), na.rm = TRUE)) |> 
    mutate(Country_grouped = if_else(Country %in% Top15, Country, "Rest of World")) |> 
    group_by(t, Country_grouped) |> 
    summarise(Total_Production = sum(Production), .groups = "drop") |> 
    left_join(continent_map, by = c("Country_grouped" = "Country")) |> 
    group_by(Country_grouped) |> 
    mutate(Total_Country_Prod = sum(Total_Production, na.rm = TRUE)) |> 
    ungroup() |> 
    arrange(Continent, desc(Total_Country_Prod)) |> 
    mutate(Country_grouped = factor(Country_grouped, levels = unique(Country_grouped)))
  
  ggplot(df, aes(x = t, y = Total_Production, fill = Country_grouped)) +
    geom_area() +
    labs(title = scenario_title, x = "Year", y = "Production [Mt]", fill = "Country") +
    scale_fill_manual(values = color_vec, drop = FALSE) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_classic(base_size = 5.5)+
    theme( axis.text = element_text(size = 5.5, family = "sans"),
      axis.title = element_text(size = 5.5),
      legend.text = element_text(size = 5.5),
      legend.title = element_text(size = 5.5),
      plot.title = element_text(size = 5.5, hjust = 0.5) ,
      legend.key.size = unit(0.2, "cm"))
}

# === Create both plots with shared color scheme
p_High <- make_plot(Base_Julia_High, Top15_High, "Highest Demand (S5)")
p_EDB   <- make_plot(Base_Julia_EDB,   Top15_EDB,   "Reference (S1)")

# === Final combined plot with ONE shared legend and ordered countries
gg_final <- ( p_EDB | p_High) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

gg_final
