library(tidyverse)

pop <- read_csv("data/population.csv") 

e <- read_csv("data/per-capita-co-emissions.csv") 

d <- full_join(e, pop)|> 
  filter(#Year == 2022,
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
      group = Entity,
      label = ifelse(Year==2022, Entity, NA)) + 
  geom_point(aes(size = `e`, color = `CO₂`, alpha = Year)) + 
  geom_line(aes(color =`CO₂` )) + 
  geom_text(aes(size = `CO₂`), 
            nudge_y = .5,
            check_overlap = T) + 
  scale_x_log10(labels = scales::label_number()) + 
  theme_bw() + 
  labs(y = "Per Capita CO2",
       color = "Per Capita Emissions",
       size = "Per Country Emissions\n(Billions)") + 
  scale_color_viridis_c(option = "A", 
                        direction = -1, begin = .3, end = .8)
  theme(legend.position = "none") 

