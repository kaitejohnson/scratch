# Explore potential right truncation in NSSP ED visit data
vintaged_datasets <- read_csv("https://raw.githubusercontent.com/reichlab/flu-metrocast/refs/heads/main/target-data/time-series.csv")

 TX<-vintaged_datasets |> filter(location == "texas")

ggplot(TX) +
  geom_line(aes(x = target_end_date, y = observation, color = factor(as_of)))

NYC<-vintaged_datasets |> filter(location == "nyc")

ggplot(NYC) +
  geom_line(aes(x = target_end_date, y = observation, color = factor(as_of)))

denver<-vintaged_datasets |> filter(location == "denver")

ggplot(denver) +
  geom_line(aes(x = target_end_date, y = observation, color = factor(as_of)))