xls_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2024/07/reconviction-rates-scotland-2020-21-offender-cohort/documents/reconvictions-2020-21-offender-cohort-additional-datasets/reconvictions-2020-21-offender-cohort-additional-datasets/govscot%3Adocument/reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"

download.file(xls_url,
              destfile = here::here("01_data", "reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"),
              method = "wget")


# criminal proceedings

criminal_proceedings_1314_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2014/12/criminal-proceedings-scotland-2013-14/documents/tables-excel/tables-excel/govscot%3Adocument/00481723.xls"

download.file(criminal_proceedings_1314_url,
              destfile = here::here("01_data", "criminal_proceedings-1314-tables.xls"),
              method = "wget")


criminal_proceedings_2324_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2024/12/criminal-proceedings-scotland-2022-23/documents/criminal-proceedings-scotland-2022-23-main-tables/criminal-proceedings-scotland-2022-23-main-tables/govscot%3Adocument/criminal-proceedings-scotland-2022-23-main-tables.xlsx"

download.file(criminal_proceedings_2324_url,
              destfile = here::here("01_data", "criminal_proceedings-2324-tables.xlsx"),
              method = "wget")
