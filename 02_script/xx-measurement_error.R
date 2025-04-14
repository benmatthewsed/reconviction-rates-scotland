library(tidyverse)

tibble(
  age = seq(1:4),
  probs = c(0.6, 0.5, 0.4, 0.3),
  
) |>
  mutate(draws = map_dbl(probs, 
                     ~ rbinom(n = 1,
                        size = 1000, 
                        prob = .x)))

         