library("data.table")
library("tidyverse")

# Info
# https://www.backblaze.com/b2/hard-drive-test-data.html#downloading-the-raw-hard-drive-test-data
# https://data.world/scuttlemonkey/hard-drive-reliability-sample

# Get a List of all files named with a key word, say all `.csv` files
file_names <- list.files("data/subset", 
                         pattern="*.csv", 
                         full.names=TRUE)

data <- rbindlist(lapply(file_names,function(x) fread(input = x,
                                                      header = TRUE,
                                                      stringsAsFactors = FALSE
                                                      ))
)

str(data)

#data[, capacity_bytes := round(capacity_bytes/10e11)  ]
#data[, model := ifelse(grepl("^ST",model),'Seagate',str_extract(model, "^[^\\s]+"))  ]

data[, c("capacity_bytes", "model") := list(round(capacity_bytes/10e11),
                                            ifelse(grepl("^ST",model),'Seagate',str_extract(model, "^[^\\s]+")))]


# EQUIVALENT DPLYR
#data <- data %>%
#  mutate(model = ifelse(grepl("^ST",model),'Seagate',str_extract(model, "^[^\\s]+")),
#         capacity_bytes = round(capacity_bytes/10e11)
#  )


str(data)

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
                      first_hour_fail = min(ifelse(failure == 1, smart_9_raw, max_hour_smart_9_raw))
                      ),
                  by =.(serial_number, model)]

############################
#dplyr

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

############################

str(data_group)

data_group$model = as.factor(data_group$model)
data_group$serial_number = as.factor(data_group$serial_number)

summary(data_group$serial_number)


# add variables age + study_time 
data_group <- data_group %>% 
  mutate(age = floor(min_Hours/24),
         study_time = ifelse(fail == 1,
                             floor((first_hour_fail - min_Hours)/24), 
                             floor((max_Hours - min_Hours)/24)) + 1 
  )


## Survival analysis
library("survival")

# for survival

str(data_group)

data_group <-  data_group %>% 
  mutate(model = as.factor(model),
         serial_number = as.factor(serial_number),
         min_date = as.Date(min_date),
         max_date = as.Date(max_date),
         first_date_fail = as.Date(first_date_fail)
  ) %>% 
  filter(age >= 0)

summary(data_group)

attach(data_group)

# 1) Kaplan-Meier Global probababilities

# 1.1) Survival function 
surv_object_HDD <- Surv(age, age + study_time, fail)

km_survival_HDD <- survfit(surv_object_HDD ~ 1)

plot(km_survival_HDD, xlab="Time", ylab="Survival Probability")

detach(data_group)


library(simPH)



# time must be in units(not dates)

str(data_group)

data_group <- data_group %>% 
  mutate(end = age + study_time) 
  

summary(data_group$end - data_group$age)

str(data_group)

data_group$fail <- as.numeric(data_group$fail)

data_group <- SurvExpand(data_group, 
                      GroupVar = "serial_number",
                      Time = "age",
                      Time2 = "end",
                      event = "fail")


BaseVars <- c("qmv", "backlog")

# tvc create a long time interaction

GolubEUPData_tvc <- tvc(GolubEUPData_exp,
                        b = BaseVars, 
                        tvar = "end",
                        tfun = "log")