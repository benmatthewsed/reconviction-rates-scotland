library(tidyverse)
library(readxl)
library(binom)
library(here)
library(patchwork)

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



reconvs <- 
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
reconvs |> 
  mutate(conf_low_95 = map2_dbl(no_reconvicted,
                                number_of_offenders,
                                ~ binom.wilson(.x, .y)$lower[[1]]),
         conf_high_95 = map2_dbl(no_reconvicted,
                                 number_of_offenders,
                                 ~ binom.wilson(.x, .y)$upper[[1]]),
         conf_low_80 = map2_dbl(no_reconvicted,
                                number_of_offenders,
                                ~ binom.wilson(.x, .y, conf.level = 0.8)$lower[[1]]),
         conf_high_80 = map2_dbl(no_reconvicted,
                                 number_of_offenders,
                                 ~ binom.wilson(.x, .y, conf.level = 0.8)$upper[[1]]),
         conf_low_50 = map2_dbl(no_reconvicted,
                                number_of_offenders,
                                ~ binom.wilson(.x, .y, conf.level = 0.5)$lower[[1]]),
         conf_high_50 = map2_dbl(no_reconvicted,
                                 number_of_offenders,
                                 ~ binom.wilson(.x, .y, conf.level = 0.5)$upper[[1]]),
  )


overall_res <- 
reconvs |> 
  group_by(year) |> 
  summarise(number_of_offenders = sum(number_of_offenders),
            no_reconvicted = sum(no_reconvicted),
            number_of_reconvictions = sum(number_of_reconvictions),
            .groups = "drop") |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders,
  ) |> 
  mutate(conf_low_95 = map2_dbl(no_reconvicted,
                         number_of_offenders,
                         ~ binom.wilson(.x, .y)$lower[[1]]),
         conf_high_95 = map2_dbl(no_reconvicted,
                          number_of_offenders,
                          ~ binom.wilson(.x, .y)$upper[[1]]),
         conf_low_80 = map2_dbl(no_reconvicted,
                            number_of_offenders,
                            ~ binom.wilson(.x, .y, conf.level = 0.8)$lower[[1]]),
         conf_high_80 = map2_dbl(no_reconvicted,
                             number_of_offenders,
                             ~ binom.wilson(.x, .y, conf.level = 0.8)$upper[[1]]),
         conf_low_50 = map2_dbl(no_reconvicted,
                            number_of_offenders,
                            ~ binom.wilson(.x, .y, conf.level = 0.5)$lower[[1]]),
         conf_high_50 = map2_dbl(no_reconvicted,
                             number_of_offenders,
                             ~ binom.wilson(.x, .y, conf.level = 0.5)$upper[[1]]),
  )



overall_res |> 
  ggplot(aes(x = year, y = as.numeric(prevalence), group = 1)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = conf_low_95, ymax = conf_high_95),
              alpha = 0.3) +
  geom_ribbon(aes(ymin = conf_low_80, ymax = conf_high_80),
              alpha = 0.3) +
  geom_ribbon(aes(ymin = conf_low_50, ymax = conf_high_50),
              alpha = 0.3) +
  theme_minimal() +
  labs(y = "Reconviction rate",
       x = "Reconviction cohort",
       colour = "Age",
       fill = "Age",
       caption = "Grey line shows overall reconviction rate") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))



age_res |> 
   mutate(age = fct_relevel(age,
                           "under 21",
                           "21 to 25",
                           "26 to 30",
                           "31 to 40",
                           "over 40")) |> 
  ggplot(aes(x = year, y = as.numeric(prevalence))) +
  geom_ribbon(aes(fill = age, group = age, 
                      ymin = conf_low_95, ymax = conf_high_95),
              alpha = 0.3) +
  geom_ribbon(aes(fill = age, group = age, 
                  ymin = conf_low_80, ymax = conf_high_80),
              alpha = 0.3) +
  geom_ribbon(aes(fill = age, group = age, 
                  ymin = conf_low_50, ymax = conf_high_50),
              alpha = 0.3) +
  geom_line(aes(group = age, colour = age)) +
  geom_point(aes(group = age,
                 colour = age)) +
  geom_point(data = overall_res) +
  geom_line(data = overall_res,
            aes(group = 1)) +
  geom_ribbon(data = overall_res,
              aes(ymin = conf_low_95, ymax = conf_high_95,
                  group = 1),
              alpha = 0.3) +
  geom_ribbon(data = overall_res,
              aes(ymin = conf_low_80, ymax = conf_high_80,
                  group = 1),
              alpha = 0.3) +
  geom_ribbon(data = overall_res,
              aes(ymin = conf_low_50, ymax = conf_high_50,
                  group = 1),
              alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(y = "Reconviction rate",
       x = "Reconviction cohort",
       colour = "Age",
       fill = "Age",
       caption = "Grey line shows overall reconviction rate") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))





age_res |> 
  filter(str_detect(year, "2004") |
           str_detect(year, "21")) |> 
  group_by(year) |> 
  mutate(prop = number_of_offenders / sum(number_of_offenders)) |> 
  ungroup() |> 
  select(year, age, prop) |> 
  mutate(age = fct_relevel(age,
                           "under 21",
                           "21 to 25",
                           "26 to 30",
                           "31 to 40",
                           "over 40")) |> 
  arrange(year, age) |> 
  mutate(prop = round(prop, 2)) |> 
  pivot_wider(names_from = year, values_from = prop) |> 
  gt::gt()


age_res |> 
  group_by(year) |> 
  mutate(prop = number_of_offenders / sum(number_of_offenders)) |> 
  ungroup() |> 
  select(year, age, prop) |> 
  mutate(age = fct_relevel(age,
                           "under 21",
                           "21 to 25",
                           "26 to 30",
                           "31 to 40",
                           "over 40")) |> 
  arrange(year, age) |> 
  ggplot(aes(x = year, y = prop, fill = age, colour = age)) +
  geom_bar(stat = "identity") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(y = "Proportion of reconviction cohort",
       x = "Reconviction cohort",
       colour = "Age",
       fill = "Age")
  

# but the problem is that the overall rate isn't the average of the rates
# it's 

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
  arrange(year, age_seq)
  ungroup() |> View()

by_age_approx |> 
  filter(str_detect(year, "2004") |
           str_detect(year, "21")) |> 
  ungroup() |> 
  arrange(year, age_seq) |> 
  ggplot(aes(x = as.numeric(age_seq), y = age_prop)) +
  geom_path() +
  facet_wrap(~year)

# could do a better interpolation here

by_age_approx |> 
  ungroup() |> 
  ggplot(aes(x = age_seq, y = fct_rev(year))) +
  ggridges::geom_ridgeline(aes(height = age_prop * 100))





# illustrative example ----------------------------------------------------


dat <- tribble(
  ~year, ~id, ~group, ~v1, ~v2, ~v3, ~v4, ~v5,
  "1", "1", "1", 1, 1, 1, 0, 0,
  "1", "2","1",  1, 1, 1, 0, 0,
  "1", "3", "1", 1, 1, 1, 0, 0,
  "1", "4", "2", 1, 0,0 , 0 , 0,
  "1", "5", "2", 1, 0, 0, 0, 0,
  "2", "1", "1", 1, 1, 1, 0, 0,
  "2", "2","1",  1, 1, 1, 0, 0,
  "2", "3", "2", 1, 0,0 , 0 , 0,
  "2", "4", "2", 1, 0,0 , 0 , 0,
  "2", "5", "2", 1, 0, 0, 0, 0
)

fig_1 <- 
dat |>
  pivot_longer(cols = v1:v5) |>
  ggplot(aes(x = name, y = fct_rev(id))) +
  geom_point(aes(colour = factor(value)),
             size = 4) +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  scale_colour_manual(values = c("#bdbdbd",
                                 "#636363"))

ggsave(
  here::here("03_figures", "grey_example.svg"),
  fig_1,
  width = 10,
  height = 5
)


fig_2 <- 
dat |>
  pivot_longer(cols = v1:v5) |>
  ggplot(aes(x = name, y = fct_rev(id))) +
  geom_point(aes(colour = interaction(factor(value), group)),
             size = 4) +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  scale_colour_manual(values = c("#9ecae1",
                                 "#3182bd",
                                 "#fee0d2",
                                 "#de2d26"))


ggsave(
  here::here("03_figures", "colour_example.svg"),
  fig_2,
  width = 10,
  height = 5
)

