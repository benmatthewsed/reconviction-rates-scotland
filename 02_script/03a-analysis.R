# remotes::install_github("josiahpjking/DasGuptR@develop", force = TRUE)

library(DasGuptR)
library(readxl)
library(here)
library(tidyverse)
library(binom)

scot_reconv <- readRDS(here::here("01_data", "combined_reconvictions.rds"))


# just the first and last year

scot_reconv |> 
  filter(year == "1997-98" | year == "2020-21") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders",
         agg = TRUE) |> 
  dg_table()

# and now the category effects


# all years

 scot_res <- 
 dgnpop(scot_reconv,
          pop = "year", 
          factors = c("prevalence"),
          id_vars = c("gender","age"),
          crossclassified = "number_of_offenders")

 saveRDS(scot_res,
         here::here("04_results",
                    "scotland_decomposition-1997.rds"))
 
str(scot_res)
 
tmp <-  readRDS(
         here::here("04_results",
                    "scotland_decomposition.rds"))

scot_res_rates <- 
scot_res |> 
  as_tibble() |> 
  mutate(country = "Scotland")
 
 
 all_dat_res <- 
 scot_res_rates |>  
   filter(factor != "crude") |> 
   mutate(factor = case_when(
     factor == "age_struct" ~ "Age structure",
     factor == "gender_struct" ~ "Sex structure",
     factor == "prevalence" ~ "Reconviction prevalence"
   ))
 
 all_dat_res |> 
   ggplot(aes(x=pop, y=rate, colour =factor, group = factor))+
   geom_line(linewidth = 1.5,
             alpha = 0.9) +
   labs(y = "Reconviction rate",
        x = "Reconviction cohort",
        colour = "Factor") +
   scale_colour_manual(values = c(
     "#3d7dca",
     # "black",
     "#ff994c",
     "#006938"
   )) +
   facet_wrap(~ factor) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust=1),
         legend.position = "none") +
   labs(title = "What would the reconviction rate have been if there was only change in...") 
 
 

# looping through each year -----------------------------------------------

 
 
 year_dg <- function(year_1, year_2, dat){
   
   dat |> 
     filter(year == year_1 | year == year_2) |> 
     dgnpop(pop="year", 
            factors=c("prevalence"),id_vars=c("gender","age"),
            crossclassified="number_of_offenders",
            agg = TRUE) |> 
     dg_table() |> 
     rownames_to_column() |> 
     rename(comparison_year = 2)
   
 }

 
 years_df <- 
 scot_reconv |> 
   select(year) |> 
   distinct() |> 
   filter(year != "2020-21")

 years_df <- 
 years_df |> 
   mutate(res = map(year, ~ year_dg(.x, year_2 = "2020-21", dat = scot_reconv)))

 
years_df |> 
  unnest(res) |> 
  filter(rowname != "crude") |> 
  ggplot(aes(x = year, y = decomp, colour = rowname)) +
  geom_point() +
  geom_line(aes(group = rowname)) +
  facet_wrap(~rowname)





# descriptives ------------------------------------------------------------

stir_blue <- c(
  "#122c54", "#2c498a", "#3d7dca", "#77a4da", "#b2cdef"
)

stir_orange <- c(
  "#852903", "#d9541a", "#FF6D00", "#ff994c", "#ffc899"
)

stir_palette <- c(stir_blue, stir_orange)

scot_reconv |> 
  group_by(year) |> 
  mutate(prop = number_of_offenders / sum(number_of_offenders)) |> 
  ungroup() |> 
  select(year, age, prop, gender) |> 
  mutate(age = fct_relevel(age,
                           "Under 21",
                           "21 to 25",
                           "26 to 30",
                           "31 to 40",
                           "Over 40")) |> 
  arrange(year, age) |> 
  ggplot(aes(x = year, y = prop, fill = interaction(age, gender), colour =     interaction(age, gender))) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = stir_palette) +
  scale_fill_manual(values = stir_palette) +
  theme_minimal() +
  labs(y = "Proportion of reconviction cohort",
       x = "Reconviction cohort",
       colour = "Age",
       fill = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))




age_res <- 
  scot_reconv |> 
  mutate(conf_low_95 = map2_dbl(number_reconvicted,
                                number_of_offenders,
                                ~ binom.wilson(.x, .y)$lower[[1]]),
         conf_high_95 = map2_dbl(number_reconvicted,
                                 number_of_offenders,
                                 ~ binom.wilson(.x, .y)$upper[[1]])
  )


age_res |> 
  ggplot(aes(x = year, y = as.numeric(prevalence))) +
  geom_ribbon(aes(fill = age, group = age, 
                  ymin = conf_low_95, ymax = conf_high_95),
              alpha = 0.3) +
  geom_line(aes(group = age, colour = age)) +
  geom_point(aes(group = age,
                 colour = age)) +
  scale_color_manual(values = stir_blue) +
  scale_fill_manual(values = stir_blue) +
  theme_minimal() +
  labs(y = "Reconviction rate",
       x = "Reconviction cohort",
       colour = "Age",
       fill = "Age",
       caption = "Grey line shows overall reconviction rate") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_y_continuous(limits = c(0.1, 0.475)) +
  facet_wrap(~gender)


# from the start to the peak

scot_reconv |> 
  filter(year == "1997-98" | year == "2003-04") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders") |> 
  dg_table() |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  rownames_to_column()


scot_reconv |> 
  filter(year == "2006-07" | year == "2020-21") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders") |> 
  dg_table() |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  rownames_to_column()


# category effects --------------------------------------------------------


res <- 
 scot_reconv |> 
  filter(year == "2004-05" | year == "2020-21") |> 
  mutate(gender_age = paste(gender, age, sep = "-")) |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender_age"),
         crossclassified="number_of_offenders",
         agg = FALSE)

res



res |>
  pivot_wider(names_from = factor, values_from = rate) |>
  group_by(pop, gender_age) |>
  summarise(
    JBj = sum(gender_age_struct),
    RTj = sum(prevalence) * (1 / 1),
    CEj = JBj + RTj
  ) |>
  group_by(gender_age) |>
  summarise(
    comp_ce = diff(JBj),
    rate_ce = diff(RTj),
    tot_ce = diff(CEj)
  ) |> 
  mutate(
    comp_ce = comp_ce / sum(tot_ce) * 100,
    rate_ce = rate_ce / sum(tot_ce) * 100,
    tot_ce = tot_ce / sum(tot_ce) * 100
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
