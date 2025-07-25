library(tidyverse)
library(readxl)
library(patchwork)
library(scales)

recorded <- read_excel("01_data/recorded-crime-2024-25-bulletin-tables.xlsx", 
                       sheet = "Table_5", skip = 1)

recorded <- janitor::clean_names(recorded)

recorded <- 
  recorded |> 
  mutate(note = if_else(str_detect(year, "note"), 1, 0),
         financial_year = if_else(str_detect(year, "-"), 1, 0),
         year = as.numeric(str_sub(year, 1, 4)),
         year = if_else(financial_year == 1, year + 0.4, year)) |> 
  filter(!is.na(year))


recorded <- 
  recorded |> 
  select(year, y = total_crimes, note) |> 
  mutate(measure = "Police recorded crime (count)") |> 
  select(year, y, measure, note)

# criminal proceedings ----------------------------------------------------



proceedings_2223 <- read_xlsx(here::here("01_data", "criminal-proceedings-scotland-2022-23-main-tables.xlsx"),
                              sheet = "Table_4b",
                              skip = 3)


proceedings_2223 <- janitor::clean_names(proceedings_2223)

proceedings_2223 <- 
  proceedings_2223 |> 
  filter(main_crime_or_offence == "All crimes and offences") |> 
  mutate(across(where(is.double), as.character)) |> 
  select(main_crime_or_offence:x2022_23_note_29) |> 
  pivot_longer(x2013_14:x2022_23_note_29,
               names_to = "year",
               values_to = "n") |> 
  mutate(year = str_sub(year, 2, 8),
         year = str_replace(year, "_", "-"),
         n = as.double(n),
         financial_year = if_else(str_detect(year, "-"), 1, 0),
         year = as.numeric(str_sub(year, 1, 4)),
         year = if_else(financial_year == 1, year + 0.4, year))


proceedings_1314 <- read_xls(here::here("01_data", "criminal_proceedings-1314-tables.xls"),
                             sheet = "Table 4a",
                             skip = 2)

proceedings_1314 <- janitor::clean_names(proceedings_1314)

proceedings_1314 <- 
  proceedings_1314 |> 
  filter(main_crime_or_offence == "All crimes and offences") |> 
  mutate(across(where(is.double), as.character)) |> 
  select(main_crime_or_offence:`x2013_14_1`) |> 
  pivot_longer(x2004_05:`x2013_14_1`,
               names_to = "year",
               values_to = "n") |> 
  mutate(year = str_sub(year, 2, 8),
         year = str_replace(year, "_", "-"),
         n = as.double(n),
         financial_year = if_else(str_detect(year, "-"), 1, 0),
         year = as.numeric(str_sub(year, 1, 4)),
         year = if_else(financial_year == 1, year + 0.4, year))


proceedings <- bind_rows(proceedings_1314, proceedings_2223)

proceedings <- 
  proceedings |> 
  select(year, y = n) |> 
  mutate(measure = "Criminal proceedings, all crimes and offences (count)",
         note = 0)



# scjs --------------------------------------------------------------------

scjs <- read_xlsx(here::here("01_data", "scjs-2023-24-main-report-annex-tables.xlsx"),
                  sheet = "Table A2",
                  skip = 4)

scjs <- janitor::clean_names(scjs)

scjs <- 
  scjs |> 
  filter(type_of_crime == "Violent and property crime") |> 
  mutate(across(where(is.double), as.character)) |> 
  select(type_of_crime:x2023_24) |> 
  pivot_longer(x2008_09:x2023_24,
               names_to = "year",
               values_to = "n") |> 
  mutate(year = str_replace(year, "_", "-"),
         n = as.double(n),
         financial_year = if_else(str_detect(year, "-"), 1, 0),
         year = as.numeric(str_sub(year, 2, 5)),
         year = if_else(financial_year == 1, year + 0.4, year))

scjs <- 
  scjs |> 
  select(year, y = n) |> 
  mutate(measure = "Victimization estimates, violent and property crime (count)",
         note = 0)


# combining ---------------------------------------------------------------

dat <- 
  bind_rows(recorded,
            proceedings,
             scjs)

count_plot <- 
dat |> 
  filter(year >= 2000) |> 
  group_by(measure) |> 
  mutate(std_y = y / max(y)) |> 
  ungroup() |> 
  filter(!str_detect(measure, "Recon")) |> 
  ggplot(aes(x = year, y = y, colour = measure)) +
  geom_line() +
  labs(y = "Count",
       x = "Year",
       colour = "") +
  theme_minimal() +
  ggokabeito::scale_colour_okabe_ito() +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
   theme(legend.position = "bottom",
         legend.justification = "left")

  


std_plot <- 
dat |> 
  filter(year >= 2000) |> 
  group_by(measure) |> 
  mutate(std_y = y / max(y)) |> 
  ungroup() |> 
  filter(!str_detect(measure, "Recon")) |> 
  ggplot(aes(x = year, y = std_y, colour = measure)) +
  geom_line() +
  labs(y = "Standardized count",
       x = "Year",
       colour = "") +
  theme_minimal() +
  ggokabeito::scale_colour_okabe_ito() +
  guides(colour = "none")

combined_plot <- count_plot + std_plot

saveRDS(combined_plot, here::here("03_figures", "combined_plot.rds"))
