ew_data <- read_excel("01_data/PRSQ-Overview-data-tool-jan22-mar22.xlsx",
                      sheet = "Data")

ew_data |> 
  filter(category == "Age by Sex") |> 
  group_by(adult_juv, cohort, category, subcat1, subcat2) |> 
  summarise(offenders = sum(offenders, na.rm = TRUE),
            reoffenders = sum(reoffenders, na.rm = TRUE), # check NAs
            reoffences = sum(reoffences, na.rm = TRUE),
            .groups = "drop") |> View()
