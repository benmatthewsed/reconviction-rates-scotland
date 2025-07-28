# remotes::install_github("josiahpjking/DasGuptR@develop", force = TRUE)

library(DasGuptR)
library(readxl)
library(here)
library(tidyverse)

data(reconv)
str(reconv)
#> 'data.frame':    130 obs. of  7 variables:
#>  $ year                : int  2004 2004 2004 2004 2004 2004 2004 2004 2004 2004 ...
#>  $ Sex                 : chr  "Female" "Female" "Female" "Female" ...
#>  $ Age                 : chr  "21 to 25" "26 to 30" "31 to 40" "over 40" ...
#>  $ convicted_population: num  49351 49351 49351 49351 49351 ...
#>  $ offenders           : num  1650 1268 2238 1198 1488 ...
#>  $ reconvicted         : num  576 420 558 212 424 ...
#>  $ reconvictions       : num  1145 786 963 361 858 ...

reconv$rate <- reconv$reconvicted/reconv$offenders

dg_rec <- dgnpop(reconv, pop="year", 
                 factors=c("rate"),id_vars=c("Sex","Age"),
                 crossclassified="offenders")
dg_plot(dg_rec$rates)

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

# just the first and last year

overall_res |> 
  filter(year == "2004-05" | year == "2020-21") |> 
  dgnpop(pop="year", 
       factors=c("prevalence"),id_vars=c("gender","age"),
       crossclassified="number_of_offenders",
       agg = TRUE) |> 
  dg_table()

# and now the category effects

res <- 
overall_res |> 
  filter(year == "2004-05" | year == "2020-21") |> 
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

# scot_res <- 
# dgnpop(overall_res,
#          pop="year", 
#          factors=c("prevalence"),id_vars=c("gender","age"),
#          crossclassified="number_of_offenders")

# saveRDS(scot_res,
#         here::here("03_results",
#                    "scotland_decomposition.rds"))

scot_res <- readRDS(here::here("03_results",
                              "scotland_decomposition.rds"))

scot_res_rates <- 
scot_res$rates |> 
  as_tibble() |> 
  mutate(country = "Scotland")


scot_res_rates |>  
  ggplot(aes(x=pop,y=rate,col=factor, group = factor))+
  geom_path()+
  theme_bw()



# now by crime type -------------------------------------------------------

# the data for crime type for scotland only go back to 2012

ew_data <- read_excel("01_data/PRSQ-Overview-data-tool-jan22-mar22.xlsx",
                      sheet = "Data")


# maybe the reason the EW data only go to 2010 is because of changes in
# how reconviction was measured:
# https://assets.publishing.service.gov.uk/media/5a7ef0b7ed915d74e62276f7/consultation-changes-to-reoffending-measure-jan14.pdf

ew_data_agg <- 
ew_data |> 
  filter(category == "Age by Sex") |> 
  group_by(adult_juv, cohort, category, subcat1, subcat2) |> 
  summarise(offenders = sum(offenders, na.rm = TRUE),
            reoffenders = sum(reoffenders, na.rm = TRUE), # check NAs
            reoffences = sum(reoffences, na.rm = TRUE),
            .groups = "drop") |> 
  rename(age = subcat1,
         gender = subcat2,
         number_of_offenders = offenders,
         no_reconvicted = reoffenders,
         number_of_reconvictions = reoffences) |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders
  )

ew_data_agg <- 
ew_data_agg |> 
  filter(adult_juv == "Adult")


ew_data_agg |> 
  filter(cohort == "Apr 2010 to Mar 2011" | cohort == "Apr 2020 to Mar 2021") |> 
  dgnpop(pop="cohort", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders") |> 
  dg_table() |> 
  as_tibble() |> 
  pivot_wider(names_from= pop,
              values_from = n) |> 
  mutate(decomp = diff / diff[factor == "crude"] * 100)


overall_res |> 
  filter(year == "2010-11" | year == "2020-21") |> 
  dgnpop(pop="year", 
         factors=c("prevalence"),id_vars=c("gender","age"),
         crossclassified="number_of_offenders") |> 
  dg_table() |> 
  as_tibble() |> 
  pivot_wider(names_from= pop,
              values_from = n) |> 
  mutate(decomp = diff / diff[factor == "crude"] * 100)

# effect is about twice as big in scotland?

ew_res <- 
dgnpop(ew_data_agg,
       pop="cohort", 
       factors=c("prevalence"),id_vars=c("gender","age"),
       crossclassified="number_of_offenders") 

ew_res$rates |>   
ggplot(aes(x=pop,y=rate,col=factor, group = factor))+
  geom_path()+
  theme_bw()


ew_res_rates <- 
ew_res$rates |> 
  as_tibble() |> 
  mutate(country = "EW")

bind_rows(
  scot_res_rates |>
    mutate(pop = str_sub(as.character(pop), 1, 4)),
  ew_res_rates |>
    mutate(pop = str_sub(as.character(pop), 5, 8))
)
  
  
 scot_res_rates |>  
  ggplot(aes(x=pop,y=rate,col=factor, group = factor))+
  geom_path()+
  theme_bw()

scot_res


# northern ireland --------------------------------------------------------

# https://www.justice-ni.gov.uk/sites/default/files/publications/justice/Adult%20and%20Youth%20Reoffending%20in%20Northern%20Ireland%20%28202122%20Cohort%29.pdf

# The ability to compare and discuss trends in reoffending is important to its usefulness as a performance target within government. However, differences in the
# offending related characteristics of those included in each cohort make comparing reoffending rates problematic, across both time and jurisdictions. In bulletins
# prior to 2017/18, reoffending figures were provided alongside adjusted reoffending rates for adults and the overall cohort, to help provide an estimate of change
# in reoffending. 

# https://www.justice-ni.gov.uk/sites/default/files/publications/doj/16-2015-northern-ireland-reoffending-methodology-methodology-and-glossary-part-2-new-edition-august-2015.pdf
# and https://eprints.lancs.ac.uk/id/eprint/49993/1/francis_predicting_reconviction_rates_in_northern_ireland_7_2005.pdf




# more EW -----------------------------------------------------------------

ew_offence <- 
  ew_data |> 
  filter(category == "Index offence total") |> 
  group_by(adult_juv, cohort, category, subcat1) |> 
  summarise(offenders = sum(offenders, na.rm = TRUE),
            reoffenders = sum(reoffenders, na.rm = TRUE), # check NAs
            reoffences = sum(reoffences, na.rm = TRUE),
            .groups = "drop") |> 
  rename(offence = subcat1,
         number_of_offenders = offenders,
         no_reconvicted = reoffenders,
         number_of_reconvictions = reoffences) |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders
  )


ew_offence |> 
  filter(cohort == "Apr 2010 to Mar 2011" | cohort == "Apr 2020 to Mar 2021", 
         adult_juv == "Adult") |>
  dgnpop(pop="cohort", 
         factors=c("prevalence"),id_vars=c("offence"),
         crossclassified="number_of_offenders") |> 
  dg_table() |> 
  as_tibble() |> 
  pivot_wider(names_from= pop,
              values_from = n) |> 
  mutate(decomp = diff / diff[factor == "crude"] * 100)



# data viz ----------------------------------------------------------------

average_age_df <- 
scot_reconv |> 
  mutate(age_first = case_when(
    age == "under 21" ~ 16,
    age == "21 to 25" ~ 21,
    age == "26 to 30" ~ 26,
    age == "31 to 40" ~ 31,
    age == "over 40" ~ 41,
  ),
  age_last = case_when(
    age == "under 21" ~ 20,
    age == "21 to 25" ~ 25,
    age == "26 to 30" ~ 30,
    age == "31 to 40" ~ 40,
    age == "over 40" ~ 75,
    
  ),
  age_length = age_last - age_first) |> 
  group_by(year, gender, age, age_first, age_last, age_length) |> 
  summarise(number_of_offenders = sum(number_of_offenders)) |> 
  mutate(age_seq = map2(age_first, age_last, ~ seq(from = .x, to = .y))) |> 
  unnest(age_seq) |> 
  ungroup() |> 
  mutate(mean_number_of_offenders = number_of_offenders / age_length)

average_age_df |> 
  ggplot(aes(x = age_seq, y = fct_rev(year), colour = gender, fill = gender, height = mean_number_of_offenders)) +
  geom_density_ridges(stat = "identity",
                      alpha = 0.4) +
  facet_wrap(~ gender)



average_age_df |> 
  mutate(year = as.integer(str_sub(year, 1, 4))) |> 
  ggplot(aes(x = age_seq, y = mean_number_of_offenders, colour = gender, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ gender) +
  labs(title = 'Year: {frame_time}', x = 'Age', y = 'Number of offenders') +
  transition_time(year) +
  enter_appear() +
  exit_disappear()

# maybe do the smooth approximation here
# also... is there a nice table that would work instead? Just the proportions for the first and last year?