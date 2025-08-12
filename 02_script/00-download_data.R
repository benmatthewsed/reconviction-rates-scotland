xls_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2024/07/reconviction-rates-scotland-2020-21-offender-cohort/documents/reconvictions-2020-21-offender-cohort-additional-datasets/reconvictions-2020-21-offender-cohort-additional-datasets/govscot%3Adocument/reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"

download.file(xls_url,
              destfile = here::here("01_data", "reconvictions-2020-21-offender-cohort-additional-datasets.xlsx"),
              method = "wget")

# older reconvictions

reconv_1718_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/10/reconviction-rates-scotland-2017-18-offender-cohort/documents/main-publication-tables/main-publication-tables/govscot%3Adocument/main-publication-tables.xlsx"

download.file(reconv_1718_url,
              destfile = here::here("01_data", "reconvictions-2017-18-offender-cohort-main-publication-tables.xlsx"),
              mode = "wb")


# criminal proceedings

criminal_proceedings_1314_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2014/12/criminal-proceedings-scotland-2013-14/documents/tables-excel/tables-excel/govscot%3Adocument/00481723.xls"

download.file(criminal_proceedings_1314_url,
              destfile = here::here("01_data", "criminal_proceedings-1314-tables.xls"),
              method = "wget")


criminal_proceedings_2324_url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2024/12/criminal-proceedings-scotland-2022-23/documents/criminal-proceedings-scotland-2022-23-main-tables/criminal-proceedings-scotland-2022-23-main-tables/govscot%3Adocument/criminal-proceedings-scotland-2022-23-main-tables.xlsx"

download.file(criminal_proceedings_2223_url,
              destfile = here::here("01_data", "criminal_proceedings-2223-tables.xlsx"),
              method = "wget")


# recorded crime

download.file(
  url = "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2025/06/recorded-crime-scotland-2024-25/documents/recorded-crime-in-scotland-2024-25-tables/recorded-crime-in-scotland-2024-25-tables/govscot%3Adocument/Recorded%2BCrime%2B-%2B2024-25%2B-%2BBulletin%2Btables.xlsx",
  destfile = here::here("01_data", "recorded-crime-2024-25-bulletin-tables.xlsx"),
  mode = "wb"
)


# SCJS

download.file(
  url = "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2025/06/scottish-crime-and-justice-survey-2023-24-associated-data-tables/documents/main-report-annex-tables/main-report-annex-tables/govscot%3Adocument/scjs-2023-24-main-report-annex-tables.xlsx",
  destfile = here::here("01_data", "scjs-2023-24-main-report-annex-tables.xlsx"),
  mode = "wb"
)


