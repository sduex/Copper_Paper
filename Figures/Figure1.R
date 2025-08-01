library(openxlsx)
library(tidyverse)
library(readxl)
Primary_Demand <- read_csv("Data/Demand Model/Primary_Demand.csv")



colnames(Primary_Demand)

peak <- Primary_Demand |>
  group_by(Scenario) |>
  summarise(Peak_Demand = max(Demand, na.rm = TRUE))

Primary_Demand$Scenario <- factor(Primary_Demand$Scenario, levels = c(
  "Ambitious-Baseline-Baseline-Baseline-Baseline",
  "Ambitious-Baseline-Baseline-Baseline-Baseline_OtherLow",
  "Ambitious-Baseline-Baseline-Baseline-Enhanced recycling",
  "Ambitious-Baseline-High Capacity-Baseline-Baseline",
  "Ambitious-Baseline-High Capacity-Baseline-Baseline_OtherLow",
  "Ambitious-Baseline-Low Capacity-Baseline-Baseline",
  "Ambitious-Baseline-Low Capacity-Baseline-Recycling Medium",
  "Aggressive_Recycling"
))

 ggplot(Primary_Demand, aes(Year, Demand/1000, colour = Scenario)) +
  geom_line(size=0.5) +
  scale_color_manual(values = c(
    "Ambitious-Baseline-Baseline-Baseline-Baseline" = "#000000",
    "Ambitious-Baseline-Baseline-Baseline-Baseline_OtherLow" = "#0072B2",
    "Ambitious-Baseline-Baseline-Baseline-Enhanced recycling" = "green",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline_OtherLow" = "#E69F00",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline" = "#D55E00",
    "Ambitious-Baseline-Low Capacity-Baseline-Baseline" = "red",
    "Ambitious-Baseline-Low Capacity-Baseline-Recycling Medium" = "#CC79A7",
    "Aggressive_Recycling"="green4"
  ),
    labels = c(
      "Ambitious-Baseline-Baseline-Baseline-Baseline" = "(1) Reference",
      "Ambitious-Baseline-Baseline-Baseline-Baseline_OtherLow" = "(2) Other Sectors low Recycling",
      "Ambitious-Baseline-Baseline-Baseline-Enhanced recycling" = "(3) LIB Enhanced Recycling",
      "Ambitious-Baseline-High Capacity-Baseline-Baseline_OtherLow" = "(5) High Capacity LIB, other Sectors low Recycling",
      "Ambitious-Baseline-High Capacity-Baseline-Baseline" = "(4) High Capacity LIB",
      "Ambitious-Baseline-Low Capacity-Baseline-Baseline" = "(6) Low Capacity LIB",
      "Ambitious-Baseline-Low Capacity-Baseline-Recycling Medium" = "(7) LIB Recycling Medium, Small Capicity LIB",
      "Aggressive_Recycling" = "(8) Aggressive Recycling"
  
    )) +
  labs(
   ,
    x = "Year",
    y = "Copper Demand [Mt]"
  ) +
  theme_classic()+
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text = element_text(size = 7, family = "sans"),
    axis.title = element_text(size = 7),
    legend.text = element_text(size = 7)     # legend title (if any)
  ) + scale_x_continuous(limits = c(2020, 2050))



 