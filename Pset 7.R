library(tidyverse)
library(lmtest)

data <- read_csv("problem7_data.csv")

cor(data["SP"], data["Bond"])

model <- lm("Bond ~ SP", data = data)
print(summary(model))
print(dwtest(model))

data <- data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  arrange(Date)

data <- data %>% 
  mutate(bond_pct = ((Bond - lag(Bond)) / lag(Bond)) * 100, 
         sp_pct = ((SP - lag(SP)) / lag(SP)) * 100) %>% 
  drop_na()

print(cor(data["sp_pct"], data["bond_pct"]))

model_pct <- lm("bond_pct ~ sp_pct", data = data)
print(summary(model_pct))
print(dwtest(model_pct))
