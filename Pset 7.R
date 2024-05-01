library(tidyverse)
library(lmtest)

# Question 6
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

# Question 7
assets <- data.frame(
  Asset = c("A", "B", "C"),
  Average_Return = c(0.082, 0.0966, 0.140),
  Standard_Deviation = c(0.112, 0.171, 0.250)
)

pAB <- -0.20
pAC <- -0.60
pBC <- 0.30

sd_port <- function(wA, wB, wC, sdA, sdB, sdC, pAB, pAC, pBC){
  term1 <- wA^2 * sdA^2
  term2 <- 2 * pAB * wA * wB * sdA * sdB
  term3 <- 2 * pAC * wA * wC * sdA * sdC
  term4 <- 2 * pBC * wB * wC * sdB * sdC
  term5 <- wB^2 * sdB^2
  term6 <- wC^2 * sdC^2
  
  variance <- sum(term1, term2, term3, term4, term5, term6)
  sd <- sqrt(variance)
  
  return(sd) 
}

sd_port(wA = 1/3, wB = 1/3, wC = 1/3, 
        sdA = assets[1,3], sdB = assets[2,3], sdC = assets[3,3],
        pAB = pAB, pAC = pAC, pBC = pBC)

sd_port(wA = 0.583, wB = 0, wC = 0.417, 
        sdA = assets[1,3], sdB = assets[2,3], sdC = assets[3,3],
        pAB = pAB, pAC = pAC, pBC = pBC)

sd_port(wA = 0.40, wB = 0.30, wC = 0.30, 
        sdA = assets[1,3], sdB = assets[2,3], sdC = assets[3,3],
        pAB = pAB, pAC = pAC, pBC = pBC)
