library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
# April 4th data
df1 <- read_csv("https://raw.githubusercontent.com/lsbastos/measles_US/refs/heads/main/data/data-table_2025_04_04.csv") |>
  mutate(data_as_of = "2025-04-04")

# April 4th data
df2 <- read_csv("https://raw.githubusercontent.com/lsbastos/measles_US/refs/heads/main/data/data-table_2025_04_18.csv") |>
  mutate(data_as_of = "2025-04-18")

# May 8th data

df3 <- read_csv("https://raw.githubusercontent.com/lsbastos/measles_US/refs/heads/main/data/data-table_2025_05_08.csv") |>
  mutate(data_as_of = "2025-05-08")

df <- bind_rows(df1, df2, df3)

df_wide <- df |> pivot_wider( id_cols = c("week_start", "week_end"),
                              names_from = "data_as_of",
                              values_from = "cases",
                              names_prefix = "cases_as_of_")

df_long_counts <- df_wide |>
  mutate(`2025-05-08` = `cases_as_of_2025-05-08` - `cases_as_of_2025-04-18`,
         `2025-04-18` = `cases_as_of_2025-04-18` - `cases_as_of_2025-04-04`,
         `2025-04-04` = `cases_as_of_2025-04-04`) |>
  select(week_end, `2025-05-08`, 
         `2025-04-18`,
         `2025-04-04`) |>
  pivot_longer(!week_end) |>
  rename(data_as_of = name) |>
  filter(week_end > "2025-01-01", week_end < "2025-04-08")

df_long_counts$data_as_of <- factor(df_long_counts$data_as_of,
                                    levels = c("2025-05-08", "2025-04-18", "2025-04-04"))

ggplot(df_long_counts) +
  geom_bar(aes(x = week_end, y = value, fill = data_as_of),
          stat = "identity") +
  xlab("Week ending date") +
  ylab("Number of cases") +
  ggtitle("Weekly cases in 2025 by rash onset date")+
  labs(fill = "Data as of date") + 
  theme_bw() +
  coord_cartesian(ylim = c(-1, 120))
