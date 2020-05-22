library(readr)

world_covid <- read_csv("data/world_covid.csv")
str(world_covid)
head(world_covid, n = 10)

dailynational <- read_csv("data/dailynational.csv")
str(dailynational)
head(dailynational, n = 10)

library(ggplot2)
library(scales)

##############################################################################
#                                                                            #
#                       Top 20 Fatality Rate Countries                       #
#                                                                            #
##############################################################################

# Grafik dasar
ggplot(data = world_covid, mapping = aes(x = country, y = fatality_rate)) + 
  geom_bar(stat = "identity")

# Flip barchart
ggplot(data = world_covid, mapping = aes(x = country, y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# Flip & sorted barchart
ggplot(data = world_covid, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# Flip - sorted - top 20 barchart
top20 <- world_covid %>% 
  arrange(desc(fatality_rate)) %>%
  head(n = 20)

ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# Custom title - axis label
ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Top 20 Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate")

# Custom axis scale 
ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Top 20 Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(1))

# Custom theme 
ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Top 20 Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme_minimal()

# Custom theme 
ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Top 20 Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme_minimal()

theme_set(theme_minimal())

# Change color & grid
ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  coord_flip() + 
  labs(title = "Top 20 Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

# Add label & y-limit 
ggplot(data = top20, mapping = aes(x = reorder(country, fatality_rate), y = fatality_rate)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  geom_text(aes(label = paste0(fatality_rate*100, "%")), size = 3, hjust = -0.1) + 
  coord_flip() + 
  labs(title = "Top 20 Fatality Rate Countries",
       x = "Country",
       y = "Fatality Rate") + 
  scale_y_continuous(labels = scales::percent_format(1), limits = c(0, 0.25)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

# By Total Cases 
## top 20 barchart
top20 <- world_covid %>% 
  arrange(desc(total_cases)) %>%
  head(n = 20)

ggplot(data = top20, mapping = aes(x = reorder(country, total_cases), y = total_cases)) + 
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  geom_text(aes(label = formatC(total_cases, big.mark = ",", decimal.mark = ".", format = "d")), size = 3, hjust = -0.1) + 
  coord_flip() + 
  labs(title = "Top 20 Total Cases Countries",
       x = "Country",
       y = "Total Cases") + 
  scale_y_continuous(labels = scales::number_format(big.mark = ","), limits = c(0, 1.8*10^6)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

##############################################################################
#                                                                            #
#                         Population vs Total Cases                          #
#                                                                            #
##############################################################################

# Hubungan jumlah kasus dgn kepadatan penduduk
ggplot(data = world_covid, mapping = aes(x = population, y = total_cases)) + 
  geom_point() 

# Opacity
ggplot(data = world_covid, mapping = aes(x = population, y = total_cases)) + 
  geom_point(alpha = 0.3) 

# x-axis format
ggplot(data = world_covid, mapping = aes(x = population, y = total_cases)) + 
  geom_point(alpha = 0.3) +
  scale_x_continuous(labels = scales::number_format())

# x-axis format
ggplot(data = world_covid, mapping = aes(x = population/10^6, y = total_cases/10^3)) + 
  geom_point(alpha = 0.3) +
  scale_x_continuous(labels = scales::number_format()) +
  scale_y_continuous(labels = scales::number_format())

# x-axis limit
ggplot(data = world_covid, mapping = aes(x = population/10^6, y = total_cases/10^3)) + 
  geom_point(alpha = 0.3) +
  scale_x_continuous(labels = scales::number_format(), limits = c(0, 400)) +
  scale_y_continuous(labels = scales::number_format(), limits = c(0, 400))

# Buble chart
ggplot(data = world_covid, mapping = aes(x = population/10^6, y = total_cases/10^3, size = round(fatality_rate*100, 2))) + 
  geom_point(alpha = 0.3) +
  scale_x_continuous(labels = scales::number_format(), limits = c(0, 400)) +
  scale_y_continuous(labels = scales::number_format(), limits = c(0, 400))

# Buble chart
ggplot(data = world_covid, mapping = aes(x = population/10^6, y = total_cases/10^3, size = round(fatality_rate*100, 2))) + 
  geom_point(alpha = 0.3, color = "coral") +
  scale_x_continuous(labels = scales::number_format(), limits = c(0, 400)) +
  scale_y_continuous(labels = scales::number_format(), limits = c(0, 400))

# Buble chart custom title
ggplot(data = world_covid, mapping = aes(x = population/10^6, y = total_cases/10^3, size = round(fatality_rate*100, 2))) + 
  geom_point(alpha = 0.3, color = "coral") +
  scale_x_continuous(labels = scales::number_format(), limits = c(0, 300)) +
  scale_y_continuous(labels = scales::number_format(), limits = c(0, 350)) +
  labs(title = "",
       x = "Population (millions)",
       y = "Total Cases (thousands)",
       size = "Fatality Rate (%)")

# Buble chart custom legend
ggplot(data = world_covid, mapping = aes(x = population/10^6, y = total_cases/10^3, size = round(fatality_rate*100, 2))) + 
  geom_point(alpha = 0.3, color = "coral") +
  scale_x_continuous(labels = scales::number_format()) +
  scale_y_continuous(labels = scales::number_format()) +
  labs(title = "Population vs Total Cases",
       x = "Population (millions)",
       y = "Total Cases (thousands)",
       size = "Fatality Rate (%)") + 
  theme(legend.position = "top")

##############################################################################
#                                                                            #
#                        Indonesia Daily Cases Trend                         #
#                                                                            #
##############################################################################

# Indonesia Trend                                                                            
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "cases")) + 
  geom_line(aes(y = daily_recovered, color = "recovered")) + 
  geom_line(aes(y = daily_deaths, color = "deaths"))

# Change color & line size
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) + 
  scale_color_manual(name = "Cases",
                     values = c("New" = "yellow", "Recovered" = "green", "Deaths" = "red"))

# Add title and axis title
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) + 
  scale_color_manual(name = "Cases",
                     values = c("New" = "yellow", "Recovered" = "green", "Deaths" = "red")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases")

# Change x-axis format
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) + 
  scale_color_manual(name = "Cases",
                     values = c("New" = "yellow", "Recovered" = "green", "Deaths" = "red")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m")


# Add trend line
dates <- dailynational$dates
data.ts <- dailynational$daily_cases

dts <- ts(data = data.ts, start = c(2020, as.numeric(format(dates[1], "%j"))), frequency = 365)

ks <- ksmooth(x = dates, y = dts, kernel = "normal", bandwidth = 10, x.points = dates)
dailynational$x <- ks$x
dailynational$y <- ks$y

ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) + 
  geom_line(aes(x = x, y = y, color = "Trend"), size = 0.8) + 
  scale_color_manual(name = "Cases",
                     values = c("New" = "yellow", "Recovered" = "green", "Deaths" = "red", "Trend" = "grey")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m")

# Custom legend & position
ggplot(data = dailynational, mapping = aes(x = dates)) + 
  geom_line(aes(y = daily_cases, color = "New"), size = 0.8) + 
  geom_line(aes(y = daily_recovered, color = "Recovered"), size = 0.8) + 
  geom_line(aes(y = daily_deaths, color = "Deaths"), size = 0.8) + 
  geom_line(aes(x = x, y = y, color = "Trend"), size = 0.8) + 
  scale_color_manual(name = "",
                     values = c("New" = "yellow", "Recovered" = "green", "Deaths" = "red", "Trend" = "grey")) + 
  labs(title = "Indonesia Daily Cases Trend",
       x = "Dates",
       y = "Daily Cases") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") + 
  theme(legend.position = "top")
