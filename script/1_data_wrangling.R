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
                                                    select = c("date", "serial_number", 
                                                               "model", "capacity_bytes",
                                                               "failure", "smart_9_raw",
                                                               "smart_5_normalized", 
                                                               "smart_10_normalized",
                                                               "smart_197_normalized",
                                                               "smart_198_normalized") 
                                                      )                 
                         )
                  )

# Modified columns capacity_bytes and models
data[, c("capacity_bytes", "model") := list(round(capacity_bytes/10e11),
                                            ifelse(grepl("^ST",model),
                                                   'Seagate',
                                                   str_extract(model, "^[^\\s]+")))]

# Number of NA

n_col <- dim(data)[2]
data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:n_col]


## Group of data using data table commands

max_hour_smart_9_raw <- as.integer(max(data$smart_9_raw[!is.na(data$smart_9_raw)]) + 1)

data_group <- data[, list(TB = max(capacity_bytes),
                          count_obs = .N,
                          min_date = min(date),
                          max_date = max(date),
                          min_Hours = min(smart_9_raw),
                          max_Hours = max(smart_9_raw),
                          count_fail = sum(failure),
                          fail       = max(failure),
                          first_date_fail = min(ifelse(failure == 1, date,"2020-01-01")),
                          first_hour_fail = min(ifelse(failure == 1, smart_9_raw, max_hour_smart_9_raw)),
                          mean_reallocated   = mean(smart_5_normalized),
                          mean_spin_retry    = mean(smart_10_normalized),
                          mean_current_pend  = mean(smart_197_normalized),
                          mean_uncorrectable = mean(smart_198_normalized)
                          ),
by =.(serial_number, model)]


head(data_group)
summary(data_group)

# Creation variables for survival models
# age: Count of hours of first power on measure in days
# study_time: Count of days between the first measure and the last measure
#             or measure of fail 

data_group <- data_group %>% 
  mutate(age = floor(min_Hours/24),
         study_time = ifelse(fail == 1,
                             floor((first_hour_fail - min_Hours)/24), 
                             floor((max_Hours - min_Hours)/24)) + 1 
  )

write.csv(x =  data_group,
          file = "output/data/data_group_2019.csv")


problem_raw_2Fails <- data %>% filter(serial_number == "ZA153SZQ")

#ZCH07Q3L / ZA153SZQ

## save register for hiphotesis taken

write.csv(x =  problem_raw_2Fails,
          file = "output/data/problem_HDD_2Fails.csv")

# Subset of fails

fails <- data_group %>% filter(count_fail > 0)

#long_life <- data_group %>% filter(age + study_time > 90)

