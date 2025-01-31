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



# but the problem is that the overall rate isn't the average of the rates

age_res |> 
  group_by(year) |> 
  summarise(mean_prevalence = mean(prevalence))

age_res |> 
  group_by(year) |> 
  mutate(age_prop = number_of_offenders / sum(number_of_offenders)) |> 
  summarise(mean_prevalence = mean(prevalence),
            weighted_mean_prevalence = weighted.mean(prevalence, age_prop))


by_age_approx <- 
age_res |> 
  group_by(year) |> 
  mutate(age_prop = number_of_offenders / sum(number_of_offenders)) |> 
  ungroup() |> 
  mutate(age_length = case_when(
    age == "21 to 25" ~ 25 - 21 + 1,
    age == "26 to 30" ~ 30 - 26 + 1,
    age == "31 to 40" ~ 40 - 31 + 1,
    age == "under 21" ~ 21 - 16 + 1,
    age == "over 40" ~ 65 - 41 + 1
  ),
  age_start = case_when(
    age == "21 to 25" ~ 21,
    age == "26 to 30" ~ 26,
    age == "31 to 40" ~ 31,
    age == "under 21" ~ 16,
    age == "over 40" ~ 41
    ),
  age_end = case_when(
    age == "21 to 25" ~ 25,
    age == "26 to 30" ~ 30,
    age == "31 to 40" ~ 40,
    age == "under 21" ~ 20,
    age == "over 40" ~ 65
  ),
  age_seq = map2(age_start, age_end, seq)) |> 
  select(year, age, number_of_offenders, no_reconvicted, age_seq, age_length) |> 
  unnest(age_seq) |> 
  group_by(year, age) |> 
  mutate(mean_number_of_offenders = number_of_offenders / age_length,
         mean_no_reconvicted = no_reconvicted / age_length,
         mean_prevalence = mean_no_reconvicted/mean_number_of_offenders) |> 
  group_by(year) |> 
  mutate(age_prop = number_of_offenders / age_length / sum(number_of_offenders))

by_age_approx |> 
  ungroup() |> View()

by_age_approx |> 
  ungroup() |> 
  ggplot(aes(x = as.numeric(age_seq), y = age_prop)) +
  geom_point() +
  geom_bar(stat = "identity") +
  facet_wrap(~year)


by_age_approx |> 
  ungroup() |> 
  ggplot(aes(x = age_seq, y = fct_rev(year))) +
  ggridges::geom_ridgeline(aes(height = age_prop * 100))
