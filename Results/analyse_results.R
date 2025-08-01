library(tidyverse)


####Load Data
Base_Julia <- read_csv("Results/DemandScenario_EDB_new_discounted_util/Agressive_Recycling_EDB/Base_Julia.csv")

Deposit_cap <- read_csv("Database/Parameters/Database.csv")

###Join Mines in results with countries

Deposit_cap <- Deposit_cap |> select(d= Deposit_Name, Country)
Deposit_cap |> count(d) |> filter(n > 1)

Base_Julia <- Base_Julia |> left_join(Deposit_cap, join_by(d))

##Filter for 2023 -2050

Base_Julia <- Base_Julia |> filter(t<=2050)

##Create Results dataframe
Results <- Base_Julia |> 
  group_by(t) |> 
  summarise(
    total_prod= sum(tons_extracted1)+sum(tons_extracted2)+sum(tons_extracted3),
    total_mines_opened = sum(mine_opened, na.rm = TRUE),
    Cap_expansions = sum(capacity_added > 0, na.rm = TRUE),
    Cap_added =sum(capacity_added),
    Cobalt_production = sum(cobalt1)+sum(cobalt2)+sum(cobalt3),
    sum_ore= sum(total_ore_mined)
  ) 



Results <- Results|> mutate(Avg_Grade=100*total_prod/sum_ore)


##Plot ore grade over time
ggplot(Results, aes(t, Avg_Grade))+
  geom_line()

###Calculate Results by Country
Results_country <- Base_Julia |> 
  group_by(t, Country) |> 
  summarise(
    Production = sum(tons_extracted1 + tons_extracted2 + tons_extracted3, na.rm = TRUE),
    Cobalt_production = sum(cobalt1)+sum(cobalt2)+sum(cobalt3),
    .groups = "drop"
   
  )

####New Mines Open

Results2<- Results |> filter(t>2028)
sum(Results2$total_mines_opened)



###Selected Country level Production over time

Results_Chile <- Results_country |> filter(Country=="Chile")

ggplot(Results_Chile, aes(t, Production))+
  geom_line()

Results_Canada <- Results_country |> filter(Country=="Canada")

ggplot(Results_Canada, aes(t, Production))+
  geom_line()

Results_Congo <- Results_country |> filter(Country=="Dem. Republic of the Congo")

ggplot(Results_Congo, aes(t, Production))+
  geom_line()


Results_USA <- Results_country |> filter(Country=="United States")

ggplot(Results_USA, aes(t, Production))+
  geom_line()


Results_Australia <- Results_country |> filter(Country=="Australia")

ggplot(Results_Australia, aes(t, Production))+
  geom_line()


Results_Peru <- Results_country |> filter(Country=="Peru")

ggplot(Results_Peru, aes(t, Production))+
  geom_line()

Results_Panama <- Results_country |> filter(Country=="Panama")

ggplot(Results_Panama, aes(t, Production))+
  geom_line()

Results_Ecuador <- Results_country |> filter(Country=="Ecuador")

ggplot(Results_Ecuador, aes(t, Production))+
  geom_line()

Results_Brazil <- Results_country |> filter(Country=="Brazil")

ggplot(Results_Brazil, aes(t, Production))+
  geom_line()

Results_Mexico <- Results_country |> filter(Country=="Mexico")

ggplot(Results_Mexico, aes(t, Production))+
  geom_line()

Results_Afghanistan <- Results_country |> filter(Country=="Afghanistan")

ggplot(Results_Afghanistan, aes(t, Production))+
  geom_line()


Results_Argentina <- Results_country |> filter(Country=="Argentina")

ggplot(Results_Argentina, aes(t, Production))+
  geom_line()

Results_Russia <- Results_country |> filter(Country=="Russia")

ggplot(Results_Russia, aes(t, Production))+
  geom_line()


###Plot ore production over time

ggplot(Results, aes(t, sum_ore))+
  geom_line()

#Calculate total added Capacity

sum(Results$Cap_added)

##Calculate cumulative ore production

sum(Results$sum_ore)


## Calculate added Capacity by 2035

Results2 <- Results|>filter(t<2036) |> summarise(Cap=sum(Cap_added))



