

needs(tidyverse, rvest, countrycode)


# Create a new column for country 

pop <- read_csv("data/population-and-demography.csv") |> 
    janitor::clean_names() |> 
    mutate(country = country_name) |> 
    select(country, year, population) |> 
    filter(year > 2017)

pop |> filter(country == "France")

fff <- read_csv("data/fff.csv") |> 
    janitor::clean_names() |> 
    mutate(
        country = str_extract(x0, "(?<=\\=\\=\\= ).*(?= \\=\\=\\=)")
    ) |> 
    filter(!is.na(country)) |> 
    select(country, x1:x13) |> 
    pivot_longer(cols = x1:x13, names_to = "protest_date", values_to = "participants") |> 
    mutate(participants = replace_na(participants, 0), 
           protest_date = case_when(
              protest_date == "x1" ~ "2018-11-30",
              protest_date == "x2" ~ "2019-03-15",
              protest_date == "x3" ~ "2019-09-20",
              protest_date == "x4" ~ "2019-09-27",
              protest_date == "x5" ~ "2019-11-29",
              protest_date == "x6" ~ "2019-12-06",
              protest_date == "x7" ~ "2020-04-24",
              protest_date == "x8" ~ "2020-09-25",
              protest_date == "x9" ~ "2021-09-24",
              protest_date == "x10" ~ "2022-03-25",
              protest_date == "x11" ~ "2022-08-26",
              protest_date == "x12" ~ "2022-09-23",
              protest_date == "x13" ~ "2023-03-03",
           ) |> ymd(), 
           country = str_replace(country, "UK", "United Kingdom"),
           country_code = countrycode(country, "country.name", "iso3c"), 
           year = str_sub(protest_date, 1, 4) |> as.numeric())



fff_pop <- fff |> 
    left_join(pop) |> 
    mutate(protest_share = participants / population * 100) 


# Add protest event numbers

fff_events <- "https://map.fridaysforfuture.org/list-countries" |>
    read_html() |>
    html_element("#tblstats") |>
    html_table() |>
    slice(-c(1:8)) |>
    mutate(country = str_remove(X1, ".+?(?=[A-Z])") |> str_squish()) |>
    janitor::clean_names() |>
    select(-x1) |>
    select(country, everything()) |>
    pivot_longer(cols = x2:x15,
                 names_to = "protest_date",
                 values_to = "events") |>
    mutate(
        events = as.numeric(events),
        events = replace_na(events, 0),
        protest_date = case_when(
            protest_date == "x2" ~ "2018-11-30",
            protest_date == "x3" ~ "2019-03-15",
            protest_date == "x4" ~ "2019-09-20",
            protest_date == "x5" ~ "2019-09-27",
            protest_date == "x6" ~ "2019-11-29",
            protest_date == "x7" ~ "2019-12-06",
            protest_date == "x8" ~ "2020-04-24",
            protest_date == "x9" ~ "2020-09-25",
            protest_date == "x10" ~ "2021-09-24",
            protest_date == "x11" ~ "2022-03-25",
            protest_date == "x12" ~ "2022-08-26",
            protest_date == "x13" ~ "2022-09-23",
            protest_date == "x14" ~ "2023-03-03",
        ) |> ymd()
    ) |>
    drop_na() |>
    mutate(
        country = str_replace(country, "UK", "United Kingdom"),
        country_code = countrycode(country, "country.name", "iso3c"),
        year = str_sub(protest_date, 1, 4) |> as.numeric()
    )

fff_pop <- fff_pop |> 
    left_join(fff_events)

fff_pop |> 
    ggplot(aes(log(events), log(protest_share))) + 
    geom_point() +
    geom_smooth(method = "lm")
    
