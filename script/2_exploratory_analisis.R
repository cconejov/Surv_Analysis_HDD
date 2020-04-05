#1) Load the packages

library("data.table")
library("tidyverse")

# Survival Analysis
library("survival")
library("survminer")

library("KMsurv")
library("simPH")

#2) Load the data/Modification

data_group <- fread(input = "output/data/data_group_2019.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    select = c("serial_number", 
                               "model", 
                               "TB",
                               "fail",
                               "first_date_fail",
                               "mean_reallocated",
                               "mean_spin_retry",
                               "mean_current_pend",
                               "mean_uncorrectable",
                               "age",
                               "study_time")
                    )

data_group <-  data_group %>% 
                mutate(model           = as.factor(model),
                       serial_number   = as.factor(serial_number),
                       first_date_fail = as.Date(first_date_fail)
                      ) %>% 
               filter(age >= 0,
                      mean_reallocated >= 0) %>% 
               droplevels()

# Create Variable: Median Age:
ageMed <- summary(data_group$age)["Median"]

data_group$age_Med <- data_group$age - ageMed

#############################################
# 3) Exploratory Analysis
#############################################

## 3.1) Summary of data_group
summary(data_group)

## 3.2) Detail age/study_time by fail
ggplot(data = data_group) +
  geom_point(aes(x = age, y = study_time, 
                 shape = as.factor(fail), color = as.factor(fail) ))

## 3.3) Number of fails by day

fails <- data_group %>% filter(fail == 1)

ggplot(data = fails, aes(x = first_date_fail, fill = model)) +
  geom_bar() +
  labs(title = "Number of fails by day") +
  xlab("Day of fail") +
  theme_classic()

## 3.4) Models of Hard disk and fails

ggplot(data = data_group, aes(x = model)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(fail))) + 
  scale_y_continuous(labels=scales::percent, limits = c(0,0.8)) +
  ylab("relative frequencies") +
  labs(title = "Relative Frequency of Hard drive Models",
       fill =  "Fail",
       x = "Model Hard Disk") +
  theme_classic()


## 3.5) Capacity of Hard disk and fails

ggplot(data = data_group, aes(x = as.factor(TB))) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(fail))) + 
  scale_y_continuous(labels=scales::percent, limits = c(0,0.8)) +
  ylab("relative frequencies") +
  labs(title = "Relative Frequency of Capacity of Hard drive Models",
       fill =  "Fail",
       x = "Capacity (TB)") +
  theme_classic()

## 3.6 Distribution of Age and fails

ggplot(data = data_group, (aes(x = age, fill = as.factor(fail)))) +
  geom_density(alpha = 0.7, color = NA) +
  labs(
    y = "Density",
    x = "Age (days)",
    fill = "Fail",
    title = "Age by Fail HD"  ) +
  theme_classic() +
  geom_vline(xintercept = 365, linetype = "dashed") +
  geom_text(x = 800, y = 0.002, label= "1 year \n operation")


# 3.7) Other variables

## 3.7.1) mean_reallocated

  ggplot(data = data_group, (aes(x = mean_reallocated, fill = as.factor(fail)))) +
  geom_density(alpha = 0.7, color = NA) +
  labs(
    y = "Density",
    fill = "Fail",
    title = "Reallocated Sectors Count",
    subtitle = "Low values are better"
  ) +
    theme_classic()

## 3.7.2) mean_spin_retry

  ggplot(data = data_group, (aes(x = mean_spin_retry, fill = as.factor(fail)))) +
    geom_density(alpha = 0.7, color = NA) +
    labs(
      y = "Density",
      fill = "Fail",
      title = "Spin Retry Count",
      subtitle = "Low values are better"
    ) +
    theme_classic()
  
## 3.7.3) mean_current_pend 
  
  ggplot(data = data_group, (aes(x = mean_current_pend, fill = as.factor(fail)))) +
    geom_density(alpha = 0.7, color = NA) +
    labs(
      y = "Density",
      fill = "Fail",
      title = "Current Pending Sector Count",
      subtitle = "Low values are better"
    ) +
    theme_classic()
  
  
## 3.7.4) mean_uncorrectable

  ggplot(data = data_group, (aes(x = mean_uncorrectable, fill = as.factor(fail)))) +
    geom_density(alpha = 0.7, color = NA) +
    labs(
      y = "Density",
      fill = "Fail",
      title = "Uncorrectable Sector Count",
      subtitle = "Low values are better"
    ) +
    theme_classic()

#############################################
# 4 Survival analysis
#############################################
  
attach(data_group)

# 1) Kaplan-Meier Global probababilities

# 1.1) Survival function 
surv_object_HDD <- Surv(age, age + study_time, fail)

km_survival_HDD <- survfit(surv_object_HDD ~ 1)

# Global option / Description of results
print(km_survival_HDD)

# Graph
#ggsurvplot(km_survival_HDD, 
#           data = data_group, 
#           palette = "#2E9FDF",
#           ylim = c(0.975,1),
#           legend.lab = "All Models")

#plot(km_survival_HDD)

# Graph
ggsurv <- ggsurvplot(
  km_survival_HDD, 
  data       = data_group, 
  ylim       = c(0.85,1),
  palette    = "#2E9FDF",           # custom color palettes
  conf.int   = TRUE,                # Add confidence interval
  risk.table = TRUE,                # Add risk table
  risk.table.col    = "strata",     # Risk table color by groups
  legend.lab        = "All Hard Disk Models", # Change legend labels
  risk.table.height = 0.25,         # Useful to change when you have multiple groups
  ggtheme           = theme_bw(),    # Change ggplot2 theme
  title             = "Kaplan-Meier Failure Estimates Hard Disk"
)  

# Drawing a vertical line at Expected lifetime

ggsurv$plot <- ggsurv$plot +
geom_vline(xintercept = 1825, linetype = "dashed") +
  geom_text(x = 2500, y = 0.95, label="Expected lifetime:\n 1825 days")

print(ggsurv)

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
           ylim = c(0.6, 1),
           legend.lab = c("HGST", "Hitachi", "Seagate","TOSHIBA", "WDC"),
           risk.table = TRUE
           )

# 3) Nelson-Aalen non-parametric analysis
na_survival_HDD <- survfit(coxph(surv_object_HDD ~ 1), type = "aalen")
print(na_survival_HDD)

ggsurvplot(
  na_survival_HDD, 
  data       = data_group, 
  ylim       = c(0.85,1),
  size       = 1,                   # change line size
  palette    = "#2E9FDF",           # custom color palettes
  conf.int   = TRUE,                # Add confidence interval
  risk.table = TRUE,                # Add risk table
  risk.table.col    = "strata",     # Risk table color by groups
  legend.lab        = "All Models", # Change legend labels
  risk.table.height = 0.25,         # Useful to change when you have multiple groups
  ggtheme           = theme_bw(),    # Change ggplot2 theme
  title             = "Nelson-Aalen Failure Estimates Hard Disk"
  )

# 4) PH COX Model

# 4.1) Univariate Cox regression

# 4.1.1) Validation and Diagnostics PH assumptions

res_cox_Age <- coxph(surv_object_HDD ~ age_Med , data = data_group)

test_cox_Age <- cox.zph(res_cox_Age)

test_cox_Age

# Conclusion: High p-value
# Test is not statistically significant for the age Med covariate.
# we can assume the proportional hazards.

# 4.1.2) Summary of results:

summary(res_cox_Age)

# 4.2) Multivariate Cox regression

# 4.2.1) Validation and Diagnostics PH assumptions

# a) Proportional hazards assumption

res_cox_Mult <- coxph(surv_object_HDD ~ age_Med + 
                                        mean_reallocated + 
                                        mean_spin_retry +
                                        mean_current_pend + 
                                        mean_uncorrectable,
                      data = data_group)


test_cox_mult <- cox.zph(res_cox_Mult)

test_cox_mult

# Conclusion: Test is not statistically significant for each of the covariates, 
# Global test is not statistically significant. 
# Therefore, we can assume the proportional hazards.

# b) Graphical diagnostic

# Scaled Schoenfeld residuals vs ime

ggcoxzph(test_cox_mult)

# 4.2.2) Summary of results:

summary(res_cox_Mult)




# 5) Simulated Cox PH model, constant covariates Compare the median age with other age range

# Simulated relative hazards of days on survival time for Hard Disk
library(simPH)

Sim1 <- coxsimLinear(res_cox_Age, b = "age_Med", Xj = seq(-500, 2500, by = 500))
      
      
simGG(Sim1, 
      xlab = "Days of Age from the Sample Median (497 days)",
      ylab = "Relative Hazard with Comparison\n to a 497 days old\n")

# Compare with https://www.backblaze.com/blog/how-long-do-disk-drives-last/

detach(data_group)
