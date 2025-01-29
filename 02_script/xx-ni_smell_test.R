library(readODS)

read_ods(here::here("01_data",
                    "Adult and Youth Reoffending in Northern Ireland (202122 Cohort) - Tables.ods"),
         sheet = 5)

# okay looks like it would downloading all the data individually...

# these are all available from https://www.justice-ni.gov.uk/topics/reoffending-statistics
# though