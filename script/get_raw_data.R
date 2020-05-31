library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)
library(janitor)

dailynational <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features$attributes %>%
  mutate(Tanggal = as_date(as_datetime(Tanggal/1000, tz = "Asia/Jakarta")),
         Pembaruan_Terakhir = as_datetime(Pembaruan_Terakhir/1000, tz = "Asia/Jakarta")) %>%
  filter(!is.na(Jumlah_Kasus_Kumulatif)) %>% 
  rename(Dates = Tanggal,
         TotalCases = Jumlah_Kasus_Kumulatif,
         Recovered = Jumlah_Pasien_Sembuh,
         Deaths = Jumlah_Pasien_Meninggal,
         Treated = Jumlah_pasien_dalam_perawatan,
         DailyCases = Jumlah_Kasus_Baru_per_Hari,
         DailyRecovered = Jumlah_Kasus_Sembuh_per_Hari,
         DailyDeaths = Jumlah_Kasus_Meninggal_per_Hari,
         DailyTreated = Jumlah_Kasus_Dirawat_per_Hari,
         PctRecovered = Persentase_Pasien_Sembuh,
         PctDeaths = Persentase_Pasien_Meninggal,
         PctTreated = Persentase_Pasien_dalam_Perawatan,
         ExaminedSpecimen = Jumlah_Spesimen_Diperiksa,
         TotalExaminedSpecimen = Jumlah_Kasus_Diperiksa_Spesimen,
         Negative = Jumlah_Negatif,
         DailyNewSpecimen = Kasus_Diperiksa_Spesimen_Baru_Harian
  ) %>% 
  clean_names() %>% 
  select(dates:odp) %>% 
  arrange(dates)

write.csv(dailynational, "data/dailynational.csv", na = "", row.names = FALSE)

world_covid <- read_html("https://www.worldometers.info/coronavirus/") %>%
  html_table() %>%
  .[[1]] %>%
  # filter(`Country,Other` %in% c("Indonesia", "Singapore", "Malaysia", "Thailand", "Brunei", "Vietnam", "Philippines")) %>%
  rename(Country = `Country,Other`) %>%
  # select(Country, TotalCases, TotalDeaths, TotalRecovered) %>% 
  filter(!is.na(`#`)) %>% 
  clean_names() %>% 
  filter(!is.na(continent)) %>% 
  filter(continent != "") %>% 
  filter(!country %in% c("MS Zaandam", "Diamond Princess")) %>% 
  mutate_at(.vars = 3:14, .funs = function(.)as.numeric(gsub("[[:punct:][:alpha:]]", "", .))) %>%
  mutate_at(.vars = 3:14, .funs = function(.)ifelse(is.na(.), 0, .)) %>% 
  mutate(fatality_rate = round(total_deaths/total_cases, 4)) %>% 
  .[,-1]

# Population Density
world_population <- read_html("https://www.worldometers.info/world-population/") %>% 
  html_table(dec = ".") %>% 
  .[[5]] %>% 
  clean_names() 

names(world_population)[6:7] <- c("density_km2", 'land_area_km2')

world_population <- world_population %>% 
  mutate(country_or_dependency = case_when(country_or_dependency == "Czech Republic (Czechia)" ~ "Czechia",
                                           country_or_dependency == "United Arab Emirates" ~ "UAE",
                                           country_or_dependency == "St. Vincent & Grenadines" ~ "St. Vincent Grenadines",
                                           country_or_dependency == "DR Congo" ~ "DRC",
                                           country_or_dependency == "Central African Republic" ~ "CAR",
                                           country_or_dependency == "State of Palestine" ~ "Palestine",
                                           country_or_dependency == "Saint Barthelemy" ~ "St. Barth",
                                           country_or_dependency == "United Kingdom" ~ "UK",
                                           country_or_dependency == "United States" ~ "USA",
                                           country_or_dependency == "South Korea" ~ "S. Korea",
                                           country_or_dependency == "Sao Tome & Principe" ~ "Sao Tome and Principe",
                                           TRUE ~ as.character(country_or_dependency)),
         population_2020 = as.numeric(gsub(",", "", population_2020)),
         net_change = as.numeric(gsub(",", "", net_change)),
         density_km2 = as.numeric(gsub(",", "", density_km2)),
         land_area_km2 = as.numeric(gsub(",", "", land_area_km2)),
         migrants_net = as.numeric(gsub(",", "", migrants_net)),
         yearly_change = as.numeric(gsub(" %", "", yearly_change))/100,
         fert_rate = as.numeric(fert_rate),
         med_age = as.numeric(med_age),
         urban_pop_percent = as.numeric(gsub(" %", "", urban_pop_percent))/100,
         world_share = as.numeric(gsub(" %", "", world_share))/100) %>% 
  select(-1)
# 
# world_coord <- read_html("https://developers.google.com/public-data/docs/canonical/countries_csv") %>% 
#   html_table() %>% 
#   .[[1]] %>% 
#   filter(!is.na(latitude) & !is.na(longitude))
# # write_csv(world_coord, "data/world_coord.csv")

world_covid %>% 
  left_join(world_population, by = c("country" = "country_or_dependency")) %>% 
  filter(!is.na(population_2020)) %>% 
  write.csv("data/world_covid.csv", na = "", row.names = FALSE)
