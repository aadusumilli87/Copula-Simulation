# Copula Simulation - 256 Scenarios 
# Load Packages
library(tidyverse)
library(fredr)
library(lubridate)
library(VineCopula)
library(copula)
# library(MASS)
# library(fitdistrplus)
# Set API Key
fredr::fredr_set_key('...') # Redacted
# Setup ---------------------------------------------------------------------
# Load Data with Fredr package
# S&P 500
SP500 <- fredr(
  series_id = 'SP500'
) %>% 
  select(date, value) %>% 
  rename(SP500 = value)

# VIX
VIX <- fredr(
  series_id = 'VIXCLS'
) %>% 
  select(date, value) %>% 
  rename(VIX = value)

#DCOILWTICO (Crude Oil)
Oil <- fredr(
  series_id = 'DCOILWTICO'
)%>% 
  select(date, value) %>% 
  rename(Oil = value)
# Gold 

#Gold <- fredr(
 # series_id = 'GOLDAMGBD228NLBM'
#)
# For whatever reason, this series does not work with the Fred API


# Will manually upload
Gold <- read.csv(
  'C:\\Users\\aadus\\OneDrive\\Documents\\GW\\Fall 2020\\Quantitative Risk Management\\Project\\Data\\GOLDAMGBD228NLBM.csv')
# Convert the Date to a Time Object
Gold$DATE <- mdy(Gold$DATE)


# Merge to a single dataframe - will reduce down to the intersection of dates through inner joins 
# We can choose a specific date range later
AllVars <- SP500 %>% 
  inner_join(y = VIX, by = 'date') %>% 
  inner_join(y = Oil, by = 'date') %>% 
  inner_join(y = Gold, by = c('date' = 'DATE')) %>% 
  rename(Gold = GOLDAMGBD228NLBM)


# Remove NA's - presence of missing values is preventing all data columns from being numeric types
AllVars <- AllVars[complete.cases(AllVars), ]
# Remove '.' values from the Gold Series
AllVars <- AllVars %>% 
  filter(Gold != '.') %>% 
  mutate(Gold = as.numeric(Gold)) 

# Calculate Log Differences
AllVarsDiff <- map_df(select(AllVars, 2:5) / lag(select(AllVars, 2:5)), log)
# Add Back the Dates
AllVarsDiff <- cbind(AllVars[, 1], AllVarsDiff)
# Remove NA Values
AllVarsDiff <- AllVarsDiff[complete.cases(AllVarsDiff), ]
# Filter Date Range
AllVarsDiff <- AllVarsDiff %>% 
  filter(date > as.Date('2019-09-01'))

# Export All Vars: 
#write.csv(AllVarsDiff, 
#  'C:\\Users\\aadus\\OneDrive\\Documents\\GW\\Fall 2020\\Quantitative Risk Management\\Project\\AllVarsDiff.csv')

# EDA ---------------------------------------------------------------------

# Summary Statistics
# We need the Mu and Sigma for each Asset return
SummaryStats <- data.frame(
  Mean = sapply(AllVarsDiff[, 2:5], mean),
  Sigma = sapply(AllVarsDiff[, 2:5], sd))
# Remove RowNames
SummaryStats <- SummaryStats %>% 
  rownames_to_column(var = 'Asset')

# What Marginal Distributions should we use?
# All Asset Distributions are long tailed (see 02_CopulaVisualization)
# Gaussian is a poor fit. T distribution with low DF appears superior for the marginals
# To estimate distribution parameters, we can use the optimization routine in MASS
# This fits parameters based on MLE
# S&P 500 - initial values are provided by the summary stats
SP_Marginal_Params <- MASS::fitdistr(x = AllVarsDiff$SP500, densfun = 't', 
  start = list(m = mean(AllVarsDiff$SP500), s = sd(AllVarsDiff$SP500), df = 2),
  lower = c(0, 0.001, 1))

# Test Other Parameters
result <- matrix(nrow = 5, ncol = 4)
for (i in 2:6)
tryCatch({
  params <- MASS::fitdistr(
    x = AllVarsDiff$SP500, densfun = 't', 
    start = list(m = mean(AllVarsDiff$SP500), s = sd(AllVarsDiff$SP500), df = i),
    lower = c(0, 0.001, 1))
  result[i, ] <- unlist(broom::glance(params))
  }, error = 'Optimization Failed')
# Format Results
SP_500Results <- data.frame(result)[2:5, ]
# Add Column Names
colnames(SP_500Results) <- c('Log Likelihood', 'AIC', 'BIC', 'Sample Size')
# Add DF column
SP_500Results <- SP_500Results %>% 
  mutate(DF = c(2:5)) %>% 
  select(5, 1:4)
# From these results, the T(2) distribution is the best fitting T-distribution
# s = 0.006, m = 0.0019

# Repeat for VIX
VIX_MarginalParams <- MASS::fitdistr(x = AllVarsDiff$VIX, densfun = 't', 
  start = list(m = mean(AllVarsDiff$VIX), s = sd(AllVarsDiff$VIX), df = 2),
  lower = c(0, 0.001, 1))
# Loop through params
#result_VIX <- matrix(nrow = 6, ncol = 4)
# for (i in 2:7)
  #tryCatch({
   # params <- MASS::fitdistr(
    #  x = AllVarsDiff$VIX, densfun = 't', 
     # start = list(m = mean(AllVarsDiff$VIX), s = sd(AllVarsDiff$VIX), df = i),
      #lower = c(0, 0.001, 1))
    #result_VIX[i, ] <- unlist(broom::glance(params))
  #}, error = 'Optimization Failed')
# For Vix, we get the same results regardless of initial DF value
# Best Params are: DF = 3.427, s = 0.05739, m = 0

# Repeat for Oil
Oil_MarginalParams <- MASS::fitdistr(x = AllVarsDiff$Oil, densfun = 't', 
  start = list(m = mean(AllVarsDiff$Oil), s = sd(AllVarsDiff$Oil), df = 2),
  lower = c(0, 0.001, 1))
# Test Other Parameters
# result_oil <- matrix(nrow = 5, ncol = 4)
# for (i in 2:6)
  # tryCatch({
    # params <- MASS::fitdistr(
     #  x = AllVarsDiff$Oil, densfun = 't', 
     #  start = list(m = mean(AllVarsDiff$Oil), s = sd(AllVarsDiff$Oil), df = i),
      # lower = c(0, 0.001, 1))
   #  result_oil[i, ] <- unlist(broom::glance(params))
 #  }, error = 'Optimization Failed')
# Running the loop produced the same result - for any starting value of DF, we get the same 
# Set of parameters
# Best Params: df = 1.588, m = 0.00151, s = 0.01578

# Repeat for Gold
Gold_MarginalParams <- MASS::fitdistr(x = AllVarsDiff$Gold, densfun = 't', 
  start = list(m = mean(AllVarsDiff$Gold), s = sd(AllVarsDiff$Gold), df = 2),
  lower = c(0, 0.001, 1))
# Best Params: df = 3.663, m = 0.000727, s = 0.0070897573

# T-Marginal Parameters
T.Marginals <- tibble(
  Asset = c('S&P 500', 'VIX', 'Oil Prices', 'Gold Prices'),
  `Degrees of Freedom` = c(2, 3.39, 1.36, 3.26),
  `Location Parameter` = c(0.002, 0, 0, 0.0006),
  `Scale Parameter` = c(0.008, 0.057, 0.0174, 0.007)
)

# Correlation Matrix
CorrelationMatrix <- data.frame(cor(AllVarsDiff[, 2:5], method = 'spearman'))
# This is the structure we have to preserve with the Copula


# Model  ------------------------------------------------------------------

# From the results above, the Gaussian Copula seems a poor fit
# Based on the long tails, the T distribution appears to be a better choice

# Create Matrix of Pseudo Observations
Pobs_Matrix <- pobs(as.matrix(AllVarsDiff[, 2:5]))
# Correlation Matrix of Pseudo-Observations
# The Psuedo-Observations 
cor(Pobs_Matrix, method = 'spearman') # This matches the actual data exactly

# Bi-Variate Fitting with VineCopula
# A more robust procedure is to fit the multi-variate copula as a 'cascade' of bi-variate copulas
# In General, the work flow goes like: 1. Structure Selection -> 2. Copula Selection -> 3. Estimation
# The VineCopula package provides the 'RVnineCopSelect' which selects the Vine Structure and Bi-Variate Copulas
# Based on optimizing AIC/BIC
# Copula Fit
Copula_Fit <- RVineStructureSelect(Pobs_Matrix, progress = TRUE)
# summary(Copula_Fit)
# Note - I originally fit a 'Naive' T-Copula, but the results, measured by info. criteria, 
# Were worse. Lower Log-Likelihood (276 vs 297), higher AIC, BIC

# Simulate the 256 10 Day Returns - 10 draws for each sim
Copula_sim <- data.frame(RVineSim(N = 2560, RVM = Copula_Fit))


# data.frame(cor(Copula_sim)) %>% View


# quite close to original data 
# These simulations are still in the uniform scale
# How do we scale them back to Log Returns? We need to use the marginal distributions
# Will go with a Normal Distribution, and the estimated T-distributions above
Simulation_Normal <- data.frame(mapply(qnorm, Copula_sim, mean = c(SummaryStats$Mean), sd = c(SummaryStats$Sigma)))

# T Marginals
Simulation_T <- data.frame(
  SP500 = (qt(Copula_sim$SP500, df = SP_Marginal_Params$estimate[3]) * 
             SP_Marginal_Params$estimate[2]) + SP_Marginal_Params$estimate[1],
  VIX = (qt(Copula_sim$VIX, df = VIX_MarginalParams$estimate[3]) * 
           VIX_MarginalParams$estimate[2]) + VIX_MarginalParams$estimate[1],
  Oil = (qt(Copula_sim$Oil, df = Oil_MarginalParams$estimate[3]) * 
           Oil_MarginalParams$estimate[2]) + Oil_MarginalParams$estimate[1],
  Gold = (qt(Copula_sim$Gold, df = Gold_MarginalParams$estimate[3]) * 
           Gold_MarginalParams$estimate[2]) + Gold_MarginalParams$estimate[1]
)

# Calculate the 10 Day Returns
# Define groupings of 10 observations
Simulation_Normal <- Simulation_Normal %>% 
  mutate(Simulation = rep(1:(n() / 10), each = 10))

# Repeat for T Simulations
Simulation_T <- Simulation_T %>% 
  mutate(Simulation = rep(1:(n() / 10), each = 10))

# Write a function to calculate 10 day returns
SimulatedChange <- function(initial_value, asset){
  Results = vector(mode = 'double', length = 11)
  Results[1] = initial_value
  for (i in 2:11){
    Results[i] <- Results[i - 1] *(1 + asset[i])
  } 
  TotalChange <- Results[10] - Results[1]
  Output <- list(TotalChange, Results)
  Output
}

# Calculate the 10 day changes - Normal
TenDayNormalSim <- Simulation_Normal %>% 
  group_by(Simulation) %>% 
  summarise(SP.10Day_Norm = SimulatedChange(initial_value = 3310.11, asset = SP500)[[1]],
            VIX.10Day_Norm = SimulatedChange(initial_value = 37.59, asset = VIX)[[1]],
            Oil.10Day_Norm = SimulatedChange(initial_value = 35.94, asset = Oil)[[1]],
            Gold.10Day_Norm = SimulatedChange(initial_value = 1876.85, asset = Gold)[[1]]
  )
       
# Ten Day Changes - T
TenDayTSim <- Simulation_T %>% 
  group_by(Simulation) %>% 
  summarise(SP.10Day_T = SimulatedChange(initial_value = 3310.11, asset = SP500)[[1]],
            VIX.10Day_T = SimulatedChange(initial_value = 37.59, asset = VIX)[[1]],
            Oil.10Day_T = SimulatedChange(initial_value = 35.94, asset = Oil)[[1]],
            Gold.10Day_T = SimulatedChange(initial_value = 1876.85, asset = Gold)[[1]]
  )

# Combine the Two Simulation DFs
TenDaySim <- cbind(TenDayNormalSim, TenDayTSim[, 2:5])

# Summary Stats of Simulated (Scaled) Numbers
# Normal
SummaryStats.Normal <- data.frame(
  Mean = sapply(Simulation_Normal[, 1:4], mean),
  Sigma = sapply(Simulation_Normal[, 1:4], sd))
# T
SummaryStats.T <- data.frame(
  Mean = sapply(Simulation_T[, 1:4], mean),
  Sigma = sapply(Simulation_T[, 1:4], sd))

# VAR Values (Percentiles)
# NOTE: What percentile to use? 0.01 or 0.99? Still unclear on this
VaR.Values <- sapply(TenDaySim[,2:9], quantile, probs = 0.01) 
# Convert to DF
VaR.Values <- data.frame(VaR.Values)
# Set RowNames
rownames(VaR.Values) <- c('S&P 500 (Normal)', 'VIX (Normal)', 
                          'WTI (Normal)', 'Gold (Normal)',
                          'S&P 500 (T)', 'VIX (T)', 
                          'WTI (T)', 'Gold (T)')

# Ten Day Change Summary Stats
TenDaySummaryT <- data.frame(
  Mean = sapply(TenDayTSim[, c(2:5)], mean),
  Std.Dev = sapply(TenDayTSim[, c(2:5)], sd),
  Median = sapply(TenDayTSim[, c(2:5)], median),
  Min = sapply(TenDayTSim[, c(2:5)], min),
  Max = sapply(TenDayTSim[, c(2:5)], max)
)

# Ten Day Normal Summary Stats
TenDaySummaryNormal <- data.frame(
  Mean = sapply(TenDayNormalSim[, c(2:5)], mean),
  Std.Dev = sapply(TenDayNormalSim[, c(2:5)], sd),
  Median = sapply(TenDayNormalSim[, c(2:5)], median),
  Min = sapply(TenDayNormalSim[, c(2:5)], min),
  Max = sapply(TenDayNormalSim[, c(2:5)], max)
)

# Export Summary Stats
write.csv(TenDaySummaryT,
  'C:\\Users\\aadus\\OneDrive\\Documents\\GW\\Fall 2020\\Quantitative Risk Management\\Project\\TenDaySummary_T.csv')

# Export Summary Stats
write.csv(TenDaySummaryNormal,
          'C:\\Users\\aadus\\OneDrive\\Documents\\GW\\Fall 2020\\Quantitative Risk Management\\Project\\TenDaySummary_Normal.csv')


# Backtesting -------------------------------------------------------------
# Estimate an alternative Copula and Conduct the Kupiec Test
# Do simpler models work better? Will estimate with a single copula, similar to assignment
# 'Naive' T Fit
tFit <- tCopula(dim = 4, dispstr = 'un')
# Naive Normal Fit
normalFit <- normalCopula(dim = 4, dispstr = 'un')

# Fit the Models
naiveFit.T <- fitCopula(tFit, data = Pobs_Matrix)
naiveFit.Normal <- fitCopula(normalFit, data = Pobs_Matrix)
# What is the goodness of fit? 
summary(naiveFit.T)
summary(naiveFit.Normal)
# Log Liklihood of 171.5 for T, 124 for Normal. Both worse than the VineCopula (182)

