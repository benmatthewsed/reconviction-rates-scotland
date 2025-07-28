library(DasGuptR)
library(readxl)
library(here)
library(tidyverse)

scot_reconv <- read_xlsx(here("01_data",
                              "reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"),
                         sheet = "AGdata",
                         skip = 4)

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

year_exclude_list <- c(
  "2004-05",
  "2005-06",
  "2006-07"
)

scot_reconv <- 
scot_reconv |> 
  filter(!year %in% year_exclude_list)


overall_res <- 
  scot_reconv |> 
  group_by(year, gender, age) |> 
  summarise(number_of_offenders = sum(number_of_offenders),
            no_reconvicted = sum(no_reconvicted),
            number_of_reconvictions = sum(number_of_reconvictions),
            .groups = "drop") |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders,
  )

# just the first and last year

overall_res |> 
  filter(year == "2007-08" | year == "2020-21") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table()

# and now the category effects

res <- 
  overall_res |> 
  filter(year == "2007-08" | year == "2020-21") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = FALSE)

gender_ce <- 
  res |>
  pivot_wider(names_from = factor, values_from = rate) |>
  group_by(pop, gender) |>
  summarise(
    JBj = sum(gender_struct),
    RTj = sum(prevalence) * (1 / 2),
    CEj = JBj + RTj
  ) |>
  group_by(gender) |>
  summarise(
    comp_ce = diff(JBj),
    rate_ce = diff(RTj),
    tot_ce = diff(CEj)
  )

age_ce <- 
  res |>
  pivot_wider(names_from = factor, values_from = rate) |>
  group_by(pop, age) |>
  summarise(
    JBj = sum(age_struct),
    RTj = sum(prevalence) * (1 / 2),
    CEj = JBj + RTj
  ) |>
  group_by(age) |>
  summarise(
    comp_ce = diff(JBj),
    rate_ce = diff(RTj),
    tot_ce = diff(CEj)
  )


bind_rows(
  gender_ce |> mutate(var = "gender") |> rename(category = gender),
  age_ce |> mutate(var = "age") |> rename(category = age)
) |>
  mutate(
    comp_ce = comp_ce / sum(tot_ce) * 100,
    rate_ce = rate_ce / sum(tot_ce) * 100,
    tot_ce = tot_ce / sum(tot_ce) * 100
  ) |>
  group_by(var) |>
  mutate(across(2:4, ~ round(., 2))) |>
  gt::gt()


# all years

scot_res_sen <- 
dgnpop(overall_res,
          pop="year", 
          factors=c("prevalence"),id_vars=c("gender","age"),
          crossclassified="number_of_offenders")

saveRDS(scot_res_sen,
         here::here("04_results",
                    "scotland_decomposition-2007.rds"))

scot_res_sen |> str()
