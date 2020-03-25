#Load Libraries

library("data.table")
library("tidyverse")
library("survival")
library("survminer")


## Load data

data_group <- fread(input = "output/data/data_group_2019.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

data_group <-  data_group %>% 
                mutate(model = as.factor(model),
                       serial_number = as.factor(serial_number),
                       min_date = as.Date(min_date),
                       max_date = as.Date(max_date),
                       first_date_fail = as.Date(first_date_fail)
                      ) %>% 
                select(-V1)


summary(data_group)


## Number of fails for day

fails <- data_group %>% filter(fail == 1)

ggplot(data = fails, aes(x = first_date_fail)) +
  geom_bar() +
  labs(title = "Number of fails by day") +
  xlab("Day of fail") +
  theme_classic()

## Models of HDD

ggplot(data = data_group, aes(model)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = model)) + 
  scale_y_continuous(labels=scales::percent, limits = c(0,0.8)) +
  ylab("relative frequencies") +
  labs(title = "Relative Frequency of Hard drive Models") +
  theme_classic()


ggplot(data = fails, aes(model)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = model)) + 
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  ylab("relative frequencies") +
  labs(title = "Relative Frequency of number of fails by Model") +
  theme_classic()

# Study time -- ofe

ggplot(data = data_group, aes(x=study_time)) +
  geom_histogram(position="identity", binwidth = 30) +
  labs(title = "Frequency histogram of number of study time days") +
  theme_classic()


## Survival analysis

attach(data_group)

# 1) Kaplan-Meier Global probababilities

# 1.1) Survival function 
surv_object_HDD <- Surv(age, age + study_time, fail)

km_survival_HDD <- survfit(surv_object_HDD ~ 1)

# Global option
print(km_survival_HDD)

# Graph
#ggsurvplot(km_survival_HDD, 
#           data = data_group, 
#           palette = "#2E9FDF",
#           ylim = c(0.975,1),
#           legend.lab = "All Models")

# Graph
ggsurvplot(
  km_survival_HDD, 
  data       = data_group, 
  ylim       = c(0.975,1),
  size       = 1,                   # change line size
  palette    = "#2E9FDF",           # custom color palettes
  conf.int   = TRUE,                # Add confidence interval
  risk.table = TRUE,                # Add risk table
  risk.table.col    = "strata",     # Risk table color by groups
  legend.lab        = "All Models", # Change legend labels
  risk.table.height = 0.25,         # Useful to change when you have multiple groups
  ggtheme           = theme_bw()    # Change ggplot2 theme
)

# 1.2) cumulative hazard

ggsurvplot(km_survival_HDD,
           data     = data_group,
           conf.int = TRUE,
           ggtheme  = theme_bw(),      # Change ggplot2 theme
           palette  = "#E7B800",
           fun      = "cumhaz")

# 2) Kaplan-Meier non-parametric analysis by model
km_survival_model <- survfit(surv_object_HDD ~ model)


ggsurvplot(km_survival_model, 
           data = data_group,
           ylim = c(0.95,1),
           legend.lab = c("DELLBOSS", "HGST", "Hitachi", "Seagate","TOSHIBA", "WDC"),
           risk.table = TRUE
           )

# 3) Nelson-Aalen non-parametric analysis
na_survival_HDD <- survfit(coxph(surv_object_HDD ~ 1), type = "aalen")
print(na_survival_HDD)


ggsurvplot(
  na_survival_HDD, 
  data       = data_group, 
  ylim       = c(0.975,1),
  size       = 1,                   # change line size
  palette    = "#2E9FDF",           # custom color palettes
  conf.int   = TRUE,                # Add confidence interval
  risk.table = TRUE,                # Add risk table
  risk.table.col    = "strata",     # Risk table color by groups
  legend.lab        = "All Models", # Change legend labels
  risk.table.height = 0.25,         # Useful to change when you have multiple groups
  ggtheme           = theme_bw()    # Change ggplot2 theme
)


# 4) Univariate Compute the Cox model
res_cox_hdd <- coxph(surv_object_HDD ~ model, data = data_group)
res_cox_hdd


detach(data_group)

