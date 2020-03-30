library("data.table")
library("tidyverse")

# Info
# https://www.backblaze.com/b2/hard-drive-test-data.html#downloading-the-raw-hard-drive-test-data
# https://data.world/scuttlemonkey/hard-drive-reliability-sample

# Get a List of all files named with a key word, say all `.csv` files
file_names <- list.files("data/drive_stats_2019", 
                        pattern="*.csv", 
                        full.names=TRUE)

data <- rbindlist(lapply(file_names,function(x) fread(input = x,
                                                    header = TRUE,
                                                    stringsAsFactors = FALSE,
                                                    select = c("date", "serial_number", "model","capacity_bytes","failure", "smart_9_raw") ))
                  )

data <- data %>%
  mutate(model = ifelse(grepl("^ST",model),'Seagate',str_extract(model, "^[^\\s]+")),
         capacity_bytes = round(capacity_bytes/10e11)
         )

## Group of data

max_hour_smart_9_raw <- max(data$smart_9_raw[!is.na(data$smart_9_raw)]) + 1


data_group <- data %>% 
  group_by(serial_number, model) %>% 
  summarise(TB = max(capacity_bytes),
            count_obs = n(),
            min_date = min(date),
            max_date = max(date),
            min_Hours = min(smart_9_raw),
            max_Hours = max(smart_9_raw),
            count_fail = sum(failure),
            fail       = max(failure),
            first_date_fail = min(ifelse(failure == 1, date,"2020-01-01")),
            first_hour_fail = min(ifelse(failure == 1, smart_9_raw, max_hour_smart_9_raw))
  )



## Independent variables

data_group <- data_group %>% 
  mutate(age = floor(min_Hours/24),
         study_time = ifelse(fail == 1,
                             floor((first_hour_fail - min_Hours)/24), 
                             floor((max_Hours - min_Hours)/24)) + 1 
  )

write.csv(x =  data_group,
          file = "output/data/data_group_2019.csv")


problem_raw <- data %>% filter(serial_number == "ZA153SZQ")

#ZCH07Q3L / ZA153SZQ

## save register for hiphotesis taken

write.csv(x =  problem_raw,
          file = "output/data/problem_HDD_2Fails.csv")


# Multiple register: 2AG8AVRY, 2AGKYLBY

problem_raw <- data_group %>% filter(serial_number == "2AG8AVRY")


fails <- data_group %>% filter(count_fail > 0)

#long_life <- data_group %>% filter(age + study_time > 90)

