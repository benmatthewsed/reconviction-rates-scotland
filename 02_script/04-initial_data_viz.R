library(tidyverse)
library(readxl)
library(binom)
library(here)

scot_reconv <- read_xlsx(here("01_data",
                              "reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"),
                         sheet = "AGdata",
                         skip = 4)

# tidying data

scot_reconv <- 
  scot_reconv |> 
  select(-...11:-...14) |> 
  select(-...1) |> 
  rename(la = ...2,
         year = ...3) |> 
  janitor::clean_names() |> 
  filter(la != "LA")

scot_reconv <- 
  scot_reconv |> 
  filter(age != "All",
         gender != "All")



overall_res <- 
  scot_reconv |> 
  group_by(year, age) |> 
  summarise(number_of_offenders = sum(number_of_offenders),
            no_reconvicted = sum(no_reconvicted),
            number_of_reconvictions = sum(number_of_reconvictions),
            .groups = "drop") |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders,
  )

age_res <- 
overall_res |> 
  mutate(conf_low = map2(no_reconvicted,
                          number_of_offenders,
                          ~ binom.wilson(.x, .y)$lower),
         conf_high = map2(no_reconvicted,
                         number_of_offenders,
                         ~ binom.wilson(.x, .y)$upper)
  )

overall_res <- 
overall_res |> 
  group_by(year) |> 
  summarise(number_of_offenders = sum(number_of_offenders),
            no_reconvicted = sum(no_reconvicted),
            number_of_reconvictions = sum(number_of_reconvictions),
            .groups = "drop") |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders,
  ) |> 
  mutate(conf_low = map2(no_reconvicted,
                         number_of_offenders,
                         ~ binom.wilson(.x, .y)$lower[[1]]),
         conf_high = map2(no_reconvicted,
                          number_of_offenders,
                          ~ binom.wilson(.x, .y)$upper[[1]])
  )



overall_res |> 
  unnest(c(conf_low, conf_high)) |> 
  ggplot(aes(x = year, y = as.numeric(prevalence), group = 1)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
              alpha = 0.5)



age_res |> 
  unnest(c(conf_low, conf_high)) |> 
  ggplot(aes(x = year, y = as.numeric(prevalence))) +
  geom_point(aes(fill = age, group = age, colour = age)) +
  geom_line(aes(group = age, colour = age)) +
  geom_ribbon(aes(fill = age, group = age, colour = age, 
                      ymin = conf_low, ymax = conf_high),
              alpha = 0.5) +
  geom_point(data = overall_res) +
  geom_line(data = overall_res,
            aes(group = 1)) +
  geom_ribbon(data = overall_res |> 
                unnest(c(conf_low, conf_high)),
              aes(ymin = conf_low, ymax = conf_high,
                  group = 1),
              alpha = 0.5)
