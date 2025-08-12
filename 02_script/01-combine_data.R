library(tidyverse)
library(readxl)
library(here)

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




# adding earlier data -----------------------------------------------------

men_1718 <- read_xlsx(here::here("01_data", "reconvictions-2017-18-offender-cohort-main-publication-tables.xlsx"),
                         sheet = "Table 4",
                         skip = 3) |> 
  janitor::clean_names()

tidy_reconv_data <- function(dat, gender_val){

  dat <- 
    dat |> 
  select(age2:reconviction_rate) |> 
  mutate(number_reconvicted = number_of_offenders * (reconviction_rate / 100))

age_list <- 
  dat |> 
  select(age2:reconviction_rate) |> 
  filter(is.na(number_of_offenders)) |> 
  filter(!str_detect(age2, "\\.")) |> 
  pull(age2)

dat |> 
  mutate(year = age2,
         age = if_else(age2 %in% age_list, age2, NA)) |> 
  fill(age, .direction = "down") |> 
  filter(!is.na(number_of_offenders)) |> 
  mutate(gender = gender_val) |> 
  select(year, age, gender, number_of_offenders:number_reconvicted)

}

men_1718 <- tidy_reconv_data(men_1718, "Male")

# repeat the process for women

women_1718 <- read_xlsx(here::here("01_data", "reconvictions-2017-18-offender-cohort-main-publication-tables.xlsx"),
                      sheet = "Table 5",
                      skip = 3) |> 
  janitor::clean_names()

women_1718 <- tidy_reconv_data(women_1718, "Female")

scot_1718 <- bind_rows(men_1718, women_1718)

scot_1718 <- 
scot_1718 |> 
  mutate(prevalence = reconviction_rate / 100) |> 
  select(year:number_of_offenders, prevalence, number_reconvicted)

# combining ---------------------------------------------------------------

overall_res <- 
overall_res |> 
  select(year, age, gender, number_of_offenders, prevalence, number_reconvicted = no_reconvicted)

overall_res <- 
overall_res |> 
  mutate(age = case_when(
    age == "over 40" ~ "Over 40",
    age == "under 21" ~ "Under 21",
    TRUE ~ age
  ))

scot_1718_subset <- 
scot_1718 |> 
  mutate(year_int = as.integer(str_sub(year, 1, 4))) |> 
  filter(year_int < 2004) |> 
  select(-year_int)

combined_dat <- 
bind_rows(
  scot_1718_subset,
  overall_res
)

combined_dat <- 
combined_dat |> 
  mutate(age = fct_relevel(
    age,
    "Under 21", "21 to 25", "26 to 30", "31 to 40", "Over 40" 
  ))

saveRDS(combined_dat,
        here::here("01_data", "combined_reconvictions.rds"))



bind_rows(
scot_1718 |> 
  mutate(source = "17-18"),
overall_res |> 
  mutate(source = "20-21")
) |> 
  pivot_wider(id_cols = c(year, age, gender),
              names_from = "source",
              values_from = "prevalence") |> 
  mutate(diff = `17-18` - `20-21`)


bind_rows(
  scot_1718 |> 
    mutate(source = "17-18"),
  overall_res |> 
    mutate(source = "20-21")
) |> 
  ggplot(aes(x = year, y = number_of_offenders, colour = source)) +
  geom_point() +
  facet_wrap(gender ~ age)
