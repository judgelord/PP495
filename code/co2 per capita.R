library(tidyverse)

pop <- read_csv("data/population.csv") 

e <- read_csv("data/per-capita-co-emissions.csv") 

d <- full_join(e, pop)|> 
  filter(Year == 2022,
         !str_detect(Entity, "income|World")) 


names(d) <- names(d) |> 
  str_remove("Annual ") |> 
  str_remove(" .*")

d |>
  drop_na(Population, Code) |>
  filter(Population > 100000000) |> 
  mutate(e = `Population`*`CO₂`/1000000000) |> 
  ggplot() + 
  aes(x = Population,
      y = `CO₂`,
      label = Entity) + 
  geom_point(aes(size = `e`),
             alpha = .3, color = "red") + 
  geom_text(aes(size = `CO₂`)) + 
  scale_x_log10(labels = scales::label_number()) + 
  theme_bw() + 
  labs(y = "Per Capita CO2") + 
  theme(legend.position = "none")

