# usulku isinya:
# 1. bar chart: Fatalyty rate di berbagai negara (v)
# 2. scatter plot: Hubungan antara jumlah kasus dengan kepadatan penduduk + Bubble Chart
# 3. trend/time series plot: banyaknya kejadian dari waktu ke waktu



library(rvest)
library(dplyr)
library(tidyr)
library(readr)
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
  clean_names()

write_csv(dailynational, "data/dailynational.csv")

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
  filter(!country %in% c("MS Zaandam", "Diamond Princess"))

world_covid <- world_covid %>% 
  mutate_at(.vars = 3:14, .funs = function(.)as.numeric(gsub("[[:punct:][:alpha:]]", "", .))) %>%
  mutate_at(.vars = 3:14, .funs = function(.)ifelse(is.na(.), 0, .)) %>% 
  mutate(fatality_rate = round(total_deaths/total_cases, 4))

write_csv(world_covid, "data/world_covid.csv")
