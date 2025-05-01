library(tidyverse)
library(readxl)
library(gganimate)


proceedings_1314 <- read_excel(here::here("01_data", "criminal_proceedings-1314-tables.xls"),
                               sheet = "Table 5",
                               range = "A3:K15") |> 
  janitor::clean_names()

proceedings_2324 <- read_excel(here::here("01_data", "criminal_proceedings-2324-tables.xlsx"),
                               sheet = "Table_5c",
                               skip = 3,
                               range = "A4:L31") |> 
  janitor::clean_names()

data_2324 <- 
proceedings_2324 |> 
  filter(type_of_accused_note_14 == "All people",
         age_range != "under 16 [note 17]") |> 
  select(age_range:x2022_23) |> 
  mutate(age = as.numeric(str_extract(age_range, "[0-9]+")))



dat_2324 <- 
data_2324 |> 
  select(-age) |> 
  pivot_longer(cols = -age_range,
               names_to = "year",
               values_to = "rate") |> 
  mutate(year = str_sub(year, 2, 5),
         age_first = str_sub(age_range, 1, 2) |> as.numeric(),
         age_last = str_sub(age_range, 4, 5) |> as.numeric(),
         age_first = if_else(is.na(age_first), 60L, age_first),
         age_last = if_else(is.na(age_last), 70L, age_last), # 70 as last
         age_seq = map2(age_first, age_last, seq),
         age_length = age_last - age_first + 1)  |> 
  unnest(age_seq) |> 
  group_by(year, age_range) |> 
  mutate(mean_rate = mean(rate))
  # set to 70 as oldest



data_1314 <- 
  proceedings_1314[4:12,] |> 
  select(-x2013_14) |> 
  rename(age_range = type_of_accused1)

dat1314 <- 
data_1314 |> 
  mutate(age_numeric = as.numeric(age_range),
         age_first = str_sub(age_range, 1, 2) |> as.numeric(),
         age_last = str_sub(age_range, 4, 5) |> as.numeric(),
         age_first = if_else(is.na(age_first), 40L, age_first),
         age_last = if_else(is.na(age_last), 70L, age_last), # 70 as last
         age_last = if_else(!is.na(age_numeric), age_numeric, age_last),
         age_seq = map2(age_first, age_last, seq),
         age_length = age_last - age_first + 1)  |> 
  unnest(age_seq) |> 
  select(x2004_05:x2012_13, age_seq, age_range, age_length) |> 
  pivot_longer(cols = x2004_05:x2012_13,
               names_to = "year",
               values_to = "rate") |> 
  group_by(year, age_range) |> 
  mutate(mean_rate = mean(rate)) |> 
  mutate(year = str_sub(year, 2, 5))

combined_dat <- 
  bind_rows(
  dat_2324 |> 
    ungroup() |> 
    select(year, mean_rate, age_seq) |> 
    mutate(source = "New age categories"),
  dat1314 |> 
    ungroup() |> 
    select(year, mean_rate, age_seq) |> 
    mutate(source = "Old age categories"))


anim <- 
combined_dat |> 
  mutate(year = as.numeric(year)) |> 
    ggplot(aes(x = age_seq, y = mean_rate)) +
    geom_line(stat = "identity") +
    geom_area(fill = "#006938",
              colour = "#005734",
              alpha = 0.7) +
  labs(x = "Age",
       y = "(Badly) Estimated Conviction Rate",
       colour = "Age categories",
       caption = "Estimated conviction rate per 10,000 people. Source: Scottish Government (2013, 2024)
       Note that the age categories used in the data change in 2013") +
  ggtitle("Year = {current_frame}") +
  transition_manual(frame = year) +
    theme_minimal()
  

acc_anim <- 
  animate(anim, fps = 2, 
        nframes = distinct(combined_dat |> select(year)) |> nrow(),
        height = 900, width = 1600,
        device = "ragg_png",
        end_pause = 2,
        res = 150)  

anim_save(here::here("03_figures", "acc_animation.gif"),
          acc_anim)  
  
  