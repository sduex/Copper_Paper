library(tidyverse)

library(readxl)

library(stringr)

Demand <- read_excel("Data/Demand Model/Demand by Sector.xlsx")


Demand_long <- Demand |> 
  pivot_longer(cols = -Scenario, names_to = "condition", values_to = "value")



Demand_long <- Demand_long %>%
  mutate(
    Scenario = case_when(
      Scenario == "(2) Other Sectors low Recycling" ~ "(2) Other Sectors\nlow Recycling",
      Scenario == "(7) LIB Recycling Medium, Small Capacity LIB" ~ "(7) LIB Recycling Medium,\nSmall Capacity LIB",
      Scenario == "(5) High Capacity LIB, other Sectors low Recycling" ~ "(5) High Capacity LIB,\nother Sectors low Recycling",
      TRUE ~ Scenario
    ),
    condition = case_when(
      condition == "Other low emissions power generation" ~ "Other low emissions\npower generation",
      TRUE ~ condition
    )
  )
library(forcats)

Demand_long <- Demand_long %>%
  mutate(
    Scenario_num = as.numeric(str_extract(Scenario, "(?<=\\().+?(?=\\))")),
    Scenario = fct_reorder(Scenario, -Scenario_num)  # reverse order: 1 on top
  )


# Plot
ggplot(Demand_long, aes(fill = condition, y = value, x = Scenario)) + 
  geom_bar(position = "stack", stat = "identity", width=0.6) +
  geom_hline(yintercept = 0, color = "black", size = 0.7) +  # horizontal
  coord_flip() +
  theme_classic() +
  labs(
    x = "Scenario", y = "Copper (Mt)", fill = "Sector") +
  theme(axis.text = element_text(size = 5, family = "sans"),
    axis.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.title = element_blank(),
    legend.position = "bottom", 
    legend.key.size = unit(0.4, "cm"),
    legend.justification = "right",
    legend.box.just = "right"
  ) 

 