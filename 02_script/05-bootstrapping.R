# bootstrapping

library(DasGuptR)

scot_reconv <- read_xlsx(here::here("01_data",
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

# example -----------------------------------------------------------------

eg.wang2000 <- data.frame(
  pop = rep(c("male", "female"), e = 12),
  ethnicity = rep(1:3, e = 4),
  age_group = rep(1:4, 3),
  size = c(
    130, 1305, 1539, 316, 211, 697, 334, 48, 105, 475, 424, 49,
    70, 604, 428, 43, 55, 127, 44, 9, 72, 178, 103, 12
  ),
  rate = c(
    12.31, 34.90, 52.91, 44.44, 16.67, 36.40, 51.20, 41.67, 12.38, 19.20, 21.23, 12.50,
    17.14, 35.55, 48.71, 55.81, 14.55, 39.37, 32.56, 55.56, 22.22, 20.34, 13.59, 8.33
  )
)


getBS <- function() {
  # expand out
  exp <- overall_res |>
    mutate(
      r1 = (prevalence / 100) * number_of_offenders,
      r0 = (1 - (prevalence / 100)) * number_of_offenders
    ) |>
    select(year, gender, age, r0:r1) |>
    pivot_longer(r0:r1, names_to = "r", values_to = "n") |>
    mutate(n = as.integer(n)) |>
    uncount(n)
  
  # sample for each pop
  bs <- lapply(unique(overall_res$year), \(p)
               slice_sample(exp[exp$year == p, ],
                            prop = 1, replace = TRUE
               ) |>
                 group_by(gender, age) |>
                 reframe(
                   pop = p,
                   size = n(),
                   rate = mean(r == "r1") * 100
                 ) |>
                 ungroup())
  
  do.call(rbind, bs)
}

overall_res <- 
overall_res |> 
  select(year, gender, age, number_of_offenders, prevalence)


# this works but it's super slow (4 mins for 200 draws)

bsamps <-
  tibble(
    k = 1:200,
    i = map(1:200, ~ getBS(), .progress = TRUE)
  )

bsamps$i[[1]]


bsamps <- 
bsamps |>
  mutate(
    dgo = map(i, ~ dgnpop(.,
                          pop = "pop", factors = c("rate"),
                          id_vars = c("age", "gender"),
                          crossclassified = "size"
    ), .progress = TRUE)
  )


