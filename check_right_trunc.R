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

for(i in 1:length(unique(vintaged_datasets$location))){
  loc <- unique(vintaged_datasets$location)[i]
  sel_loc<-vintaged_datasets |> filter(location == loc)
  
  p <- ggplot(sel_loc) +
    geom_line(aes(x = target_end_date, y = observation, color = factor(as_of)))
  
  ggsave(plot = p, 
         filename = file.path("output",glue::glue("{loc}.png")))
}
