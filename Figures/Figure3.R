library(tidyverse)
library(readxl)
library(cowplot)

Results_Reference <- read_excel("Results/Results_S1.xlsx")
Results_OtherLow <- read_excel("Results/Results_S2.xlsx")
Results_Enhanced_Recycling <- read_excel("Results/Results_S3.xlsx")
Results_High_Cap <- read_excel("Results/Results_S4.xlsx")
Results_High_Cap_otherLow <- read_excel("Results/Results_S5.xlsx")
Results_Low_Cap <- read_excel("Results/Results_S6.xlsx")
Results_Low_Cap_med_rec  <- read_excel("Results/Results_S7.xlsx")
Results_Aggressive_Recycling  <- read_excel("Results/Results_S8.xlsx")


All_Results <- bind_rows(
  Results_Reference|> mutate(Scenario = "(1) Reference", EDB = "Yes"),
  Results_OtherLow|> mutate(Scenario = "(2) Other Sectors Low Recycling", EDB = "Yes"),
  Results_Enhanced_Recycling |> mutate(Scenario = "(3) LIB Enhanced Recycling", EDB = "Yes"),
  Results_High_Cap |> mutate(Scenario = "(4) High Capacity LIB", EDB = "Yes"),
  Results_Low_Cap|> mutate(Scenario = "(6) Low Capacity LIB", EDB = "Yes"),
  Results_High_Cap_otherLow |> mutate(Scenario = "(5) High Cap LIB, Other Sectors Low Recycling", EDB = "Yes"),
  Results_Low_Cap_med_rec |> mutate(Scenario = "(7) LIB Recycling Medium, Small Capacity LIB", EDB = "Yes"),
  Results_Aggressive_Recycling |> mutate(Scenario ="(8) Aggressive Recycling", EDB="Yes")
)

All_Results <- All_Results |> filter(t<2051)

All_Results$Scenario <- factor(All_Results$Scenario, levels = c(
  "(1) Reference",
  "(2) Other Sectors Low Recycling",
  "(3) LIB Enhanced Recycling",
  "(4) High Capacity LIB",
  "(5) High Cap LIB, Other Sectors Low Recycling",
  "(6) Low Capacity LIB",
  "(7) LIB Recycling Medium, Small Capacity LIB",
  "(8) Aggressive Recycling"
))

# Plot
Ore <- ggplot(All_Results, aes(x = t, y = sum_ore, color = Scenario)) +
  geom_line(size = 0.5) +
  labs(
    x = "Year", y = "Total Ore Processed (Mt)",
    color = "Scenario", linetype = "EDB"
  ) +
  theme_classic()+
  theme(legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.key.size = unit(0.3, "cm"),
    axis.text = element_text(size = 5.5, family = "sans"),
    axis.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5))  +
  scale_color_manual(values = c(
    "(1) Reference" = "#000000",
    "(2) Other Sectors Low Recycling" = "#0072B2",
    "(3) LIB Enhanced Recycling" = "green",
    "(4) High Capacity LIB" = "#D55E00",
    "(5) High Cap LIB, Other Sectors Low Recycling" = "#E69F00",
    "(6) Low Capacity LIB" = "red",
    "(7) LIB Recycling Medium, Small Capacity LIB" = "#CC79A7",
    "(8) Aggressive Recycling"= "green4"
  ))


# Plot
Grade=ggplot(All_Results, aes(x = t, y = Avg_Grade, color = Scenario)) +
  geom_line(size = 0.5) +
  labs(
    x = "Year", y = "Ore Grade (%)",
    color = "Scenario", linetype = "EDB"
  ) +
  theme_classic()+
  theme(legend.position = "none",
    axis.text = element_text(size = 5.5, family = "sans"),
    axis.title = element_text(size = 5.5)
    ) +
  scale_color_manual(values = c(
    "(1) Reference" = "#000000",
    "(2) Other Sectors Low Recycling" = "#0072B2",
    "(3) LIB Enhanced Recycling" = "green",
    "(4) High Capacity LIB" = "#D55E00",
    "(5) High Cap LIB, Other Sectors Low Recycling" = "#E69F00",
    "(6) Low Capacity LIB" = "red",
    "(7) LIB Recycling Medium, Small Capacity LIB" = "#CC79A7",
    "(8) Aggressive Recycling"= "green4"
  ))










library(cowplot)



color_values <- c(
  "(1) Reference" = "#000000",
  "(2) Other Sectors Low Recycling" = "#0072B2",
  "(3) LIB Enhanced Recycling" = "green",
  "(4) High Capacity LIB" = "#D55E00",
  "(5) High Cap LIB, Other Sectors Low Recycling" = "#E69F00",
  "(6) Low Capacity LIB" = "red",
  "(7) LIB Recycling Medium, Small Capacity LIB" = "#CC79A7",
  "(8) Aggressive Recycling" = "green4"
)

All_Results$Scenario <- factor(All_Results$Scenario, levels = names(color_values))

# Plot 1: Ore
Ore <- ggplot(All_Results, aes(x = t, y = sum_ore, color = Scenario)) +
  geom_line(size = 0.5) +
  labs(title = "b. Copper Ore Processed", x = "Year", y = "Total Ore Processed (Mt)", color = "Scenario") +
  scale_color_manual(values = color_values) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 5.5, family = "sans"),
    axis.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5),
    legend.key.size = unit(0.3, "cm"),
    plot.title = element_text(size = 5.5, hjust = 0, face = "bold", family = "sans")
  )

# Plot 2: Grade
Grade <- ggplot(All_Results, aes(x = t, y = Avg_Grade, color = Scenario)) +
  geom_line(size = 0.5) +
  labs(title = "a. Ore Grade Decline", x = "Year", y = "Ore Grade (%)", color = "Scenario") +
  scale_color_manual(values = color_values) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 5.5, family = "sans"),
    axis.title = element_text(size = 5.5),
    legend.text = element_text(size = 5.5),
    legend.key.size = unit(0.3, "cm"),
    plot.title = element_text(size = 5.5, hjust = 0, face = "bold", family = "sans")
  )

Ore_clean <- Ore + theme(legend.position = "none")
Grade_clean <- Grade + theme(legend.position = "none")

# Extract a single shared legend from one plot

legend <- get_legend(Ore + theme(legend.position = "bottom"))

# Combine the plots with patchwork and cowplot
final_plot <- plot_grid(
  Grade_clean, Ore_clean, nrow = 1, align = "hv", rel_widths = c(1, 1), labels = NULL
)

# Add the shared legend below
final_plot <- plot_grid(final_plot, legend, ncol = 1, rel_heights = c(1, 0.1))




final_plot



