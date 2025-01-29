library(DasGuptR)
library(readxl)
library(here)
library(tidyverse)

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



tmp2 <- 
tmp |> 
  select(year, gender, age, la, offenders = number_of_offenders, reconvicted = no_reconvicted) |> 
  group_by(year, la) |> 
  mutate(convicted_population = sum(offenders))

tmp2 <- 
tmp2 |> 
  mutate(
    prevalence = reconvicted/offenders,
    pop_str = offenders/convicted_population
  )

tmp2 <- 
tmp2 |> 
  ungroup()

angus <- 
tmp2 |> 
  filter(la == "Angus")

dgnpop(angus, pop="year",
       factors=c("prevalence"),
       id_vars=c("age","gender"),
       crossclassified="offenders")$rates |>
  select(-std.set) |>
  pivot_wider(names_from=pop,values_from=rate)

dat <- reconv |>
  mutate(
    prev = reconvicted/offenders,
    freq = reconvictions/offenders,
    size = offenders
  )



dgnpop(angus, pop="year",
       factors=c("prevalence"),
       id_vars=c("age","gender"),
       crossclassified="offenders")$rates |>
  ggplot(aes(x=as.numeric(pop),y=rate,col=factor))+
  geom_line()



start <- Sys.time()

tmp2 <- 
tmp2 |> 
  group_by(la) |> 
  nest() |> 
  mutate(dg = map(data, ~ dgnpop(df=.x,
                                  pop="year",
                                        factors=c("prevalence"),
                                        id_vars=c("age","gender"),
                                        crossclassified="offenders")$rates,
                  .progress = TRUE))

end <- Sys.time()

end - start

# will maybe take... 10 minutes?

tmp2 |> 
  select(-data) |> 
  unnest(dg) |> 
  filter(factor == "age_struct") |> 
  ggplot(aes(x=pop,y=rate,col=factor, group = factor))+
  geom_path()+
  facet_wrap(~la) +
  theme_bw()


dgnpop(angus, 
       pop="year",
       factors=c("prevalence"),
       id_vars=c("age","gender"),
       crossclassified="offenders") |> 
  dg_table()


overall_res <- 
tmp |> 
  group_by(year, gender, age) |> 
  summarise(number_of_offenders = sum(number_of_offenders),
            no_reconvicted = sum(no_reconvicted),
            number_of_reconvictions = sum(number_of_reconvictions),
            .groups = "drop") |> 
  mutate(
    prevalence = no_reconvicted/number_of_offenders,
    frequency = number_of_reconvictions/number_of_offenders,
  )

tmp_res <- 
overall_res |> 
  dgnpop(pop="year",
         factors=c("prevalence"),
         id_vars=c("age","gender"),
         crossclassified="number_of_offenders")


tmp_res$rates |> 
 # filter(factor == "age_struct") |> 
  ggplot(aes(x=pop,y=rate,col=factor, group = factor))+
  geom_path()+
  theme_bw()

tmp_res$rates |> 
  dg_table()



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

dg_rec |> 
  dg_table(pop1 = 1,
           pop2 = 3)


overall_res


reconv
overall_res


overall_res |> 
  filter(year == "2004-05" | year == "2020-21") |> 
dgnpop(pop="year", 
       factors=c("prevalence"),id_vars=c("gender","age"),
       crossclassified="number_of_offenders") |> 
  dg_table() |> 
  as_tibble() |> 
  pivot_wider(names_from= pop,
              values_from = n) |> 
  mutate(decomp = diff / diff[factor == "crude"] * 100)

dgo <- dg_rec

# n pops
npops = length(unique(dgo[['pop']]))
# make crude go last
factors = unique(dgo[['factor']])
factor_levels = c(setdiff(factors, "crude"), "crude")
dgo$factor = factor(dgo$factor, levels = factor_levels)

if(is.null(pop1) & is.null(pop2)){
  if(npops>2){
    dgt <- xtabs(rate ~ factor + pop, dgo) |>
      as.data.frame.matrix()
  }else{
    dgt <- xtabs(rate ~ factor + pop, dgo) |>
      addmargins(margin = 2, FUN = diff) |>
      as.data.frame.matrix()
    dgt$decomp <- round(dgt[['diff']]/dgt[row.names(dgt)=="crude",'diff']*100,2)
  }
}else{
  dgt <- droplevels(dgo[dgo[["pop"]] %in% c(pop1, pop2), ]) |>
    xtabs(rate ~ factor + pop, data = _) |>
    addmargins(margin = 2, FUN = diff) |>
    as.data.frame.matrix()
  dgt$decomp <- round(dgt[['diff']]/dgt[row.names(dgt)=="crude",'diff']*100,2)
}
return(dgt)
