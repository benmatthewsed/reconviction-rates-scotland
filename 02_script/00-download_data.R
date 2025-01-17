xls_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2024/07/reconviction-rates-scotland-2020-21-offender-cohort/documents/reconvictions-2020-21-offender-cohort-additional-datasets/reconvictions-2020-21-offender-cohort-additional-datasets/govscot%3Adocument/reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"

download.file(xls_url,
              destfile = here::here("01_data", "reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"),
              method = "wget")


