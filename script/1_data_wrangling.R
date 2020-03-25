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
                                                    select = c("date", "serial_number", "model","capacity_bytes","failure") ))
                  )

data <- data %>%
  mutate(model = ifelse(grepl("^ST",model),'Seagate',str_extract(model, "^[^\\s]+")),
         capacity_bytes = round(capacity_bytes/10e11)
         )

data_group <- data %>% 
  group_by(serial_number, model, capacity_bytes) %>% 
  summarise(count_obs = n(),
            min_date = min(date),
            max_date = max(date),
            count_fail = sum(failure),
            fail       = max(failure),
            first_date_fail = min(ifelse(failure == 1,date,"2020-01-01")))


begin_study <- "2019-01-01"

data_group <- data_group %>% 
                mutate(age = difftime(min_date, begin_study, units = c("days")),
                       study_time = ifelse(fail == 1,
                                           difftime(first_date_fail, min_date, units = c("days")), 
                                           difftime(max_date, min_date, units = c("days")))   + 1
                       )

write.csv(x =  data_group,
          file = "output/data/data_group_2019.csv")


problem_raw <- data %>% filter(serial_number == "ZCH07Q3L")

#ZCH07Q3L / ZA153SZQ

## save register for hiphotesis taken

write.csv(x =  problem_raw,
          file = "output/data/problem_HDD.csv")


# Multiple register: 2AG8AVRY, 2AGKYLBY

problem <- data_group %>% filter(serial_number == "ZA153SZQ")


fails <- data_group %>% filter(count_fail > 0)

#long_life <- data_group %>% filter(age + study_time > 90)

