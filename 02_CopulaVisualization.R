# Copula Project - Visualizations
library(tidyverse)
# Marginal Histograms
SP_Hist <- ggplot(AllVarsDiff) +
  geom_histogram(aes(SP500), binwidth = 0.001) +
  labs(x = 'First Difference of S&P 500') + 
  ylab(NULL) +
  theme_minimal()
# Symmetric, but long tailed. Does not look to fit a normal

# Vix
Vix_Hist <- ggplot(AllVarsDiff) +
  geom_histogram(aes(VIX), binwidth = 0.01) +
  labs(x = 'First Difference of VIX') + 
  ylab(NULL) +
  theme_minimal()
# Appears roughly symmetric, with slight right skew

# Oil
Oil_Hist <- ggplot(AllVarsDiff) +
  geom_histogram(aes(Oil), binwidth = 0.005) +
  labs(x = 'First Difference of Oil Prices (WTI)') + 
  ylab(NULL) +
  theme_minimal()
# Similar to S&P - symmetric, but long tails

# Gold
Gold_Hist <- ggplot(AllVarsDiff) +
  geom_histogram(aes(Gold), binwidth = 0.0025) +
  labs(x = 'First Difference of Gold Prices') + 
  ylab(NULL) +
  theme_minimal()

# Plot the 4 Histograms together: Using ggarrange from ggpubr package
Distributions <- ggpubr::ggarrange(SP_Hist, Vix_Hist, Oil_Hist, Gold_Hist, ncol = 2, nrow = 2)
# Add Labels
AllHists <- ggpubr::annotate_figure(Distributions, 
                                    top = ggpubr::text_grob('Visualizing Asset Returns: S&P 500, VIX, WTI (Crude Oil), Gold Fixing Price',
                                                            face = 'bold'),
                                    bottom = ggpubr::text_grob('Source: Federal Reserve Bank of St. Louis', hjust = 1, x = 1, size = 8))



# Correlation Plot - Reshape
Corrplot_Data <- CorrelationMatrix %>% 
  rownames_to_column(var = 'Asset') %>% 
  pivot_longer(!Asset, names_to = 'Asset2', values_to = 'Correlation')

# Plot
Corrplot <- ggplot(Corrplot_Data, aes(x = Asset, y = Asset2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = '#D02915', high = '#18874C', mid = '#F3E6F8', midpoint = 0,
                       name = 'Spearman Correlation') +
  labs(x = NULL, y = NULL, 
       title = 'Visualizing Correlation: S&P 500, VIX, WTI (Crude Oil), Gold Fixing Price',
       caption = 'Source: St. Louis Fed') +
  theme_minimal()



# Scatter Plots - For Bi-Variate Fitting
# S&P and VIX
SP_Vix <- ggplot(data = AllVarsDiff) +
  geom_point(aes(x = VIX, y = SP500), alpha = 0.5) +
  geom_smooth(aes(x = VIX, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(-0.06, 0.06)) +
  labs(title = 'S&P 500 and VIX Returns', x = 'VIX Returns', y = 'S&P 500 Returns') +
  theme_minimal()
# S&P and Oil
SP_Oil <- ggplot(data = AllVarsDiff) +
  geom_point(aes(x = Oil, y = SP500), alpha = 0.5) +
  geom_smooth(aes(x = Oil, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.3, 0.3), ylim = c(-0.08, 0.08)) + 
  labs(title = 'S&P 500 and WTI Returns', x = 'WTI Returns', y = 'S&P 500 Returns') +
  theme_minimal()
# S&P and Gold
SP_Gold <- ggplot(data = AllVarsDiff) +
  geom_point(aes(x = Gold, y = SP500), alpha = 0.5) +
  geom_smooth(aes(x = Gold, y = SP500), method = 'lm') +
  labs(title = 'S&P 500 and Gold Returns', x = 'Gold Returns', y = 'S&P 500 Returns') +
  theme_minimal()

# Vix and Oil
Vix_Oil <-  ggplot(data = AllVarsDiff) +
  geom_point(aes(x = VIX, y = Oil), alpha = 0.5) +
  geom_smooth(aes(x = VIX, y = Oil), method = 'lm') +
  labs(title = 'VIX and Oil Returns', x = 'VIX Returns', y = 'WTI Returns') +
  theme_minimal()

# Vix and Gold
Vix_Gold <-  ggplot(data = AllVarsDiff) +
  geom_point(aes(x = VIX, y = Gold), alpha = 0.5) +
  geom_smooth(aes(x = VIX, y = Gold), method = 'lm') +
  labs(title = 'VIX and Gold Returns', x = 'VIX Returns', y = 'Gold Returns') +
  theme_minimal()

# Gold and Oil
Oil_Gold <-  ggplot(data = AllVarsDiff) +
  geom_point(aes(x = Gold, y = Oil), alpha = 0.5) +
  geom_smooth(aes(x = Gold, y = Oil), method = 'lm') +
  labs(title = 'Gold and Oil Returns', x = 'Gold Returns', y = 'WTI (Oil) Returns') +
  theme_minimal()


# Comparison Plots - Copula Sims vs Actual Data
# Check the procedure - do the simulated returns seem similar to the actual returns?
# SP500/Vix
Sim_SP.VIX <- ggplot(Simulation_Normal) +
  geom_point(aes(x = VIX, y = SP500)) + 
  geom_smooth(aes(x = VIX, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(-0.06, 0.06)) +
  labs(x = 'Simulated VIX Return (Normal)', y = 'Simulated S&P Return (Normal)') + 
  theme_minimal()

# Repeat for T dist
Sim_T.SP.Vix <- ggplot(Simulation_T) +
  geom_point(aes(x = VIX, y = SP500)) +
  geom_smooth(aes(x = VIX, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(-0.06, 0.06)) +
  labs(x = 'Simulated VIX Return (T)', y = 'Simulated S&P Return (T)') + 
  theme_minimal()

# Show with Real Data
SP_Vix_Plots <- ggpubr::ggarrange(SP_Vix, Sim_SP.VIX, Sim_T.SP.Vix, nrow = 2, ncol = 2)

# SP500/Oil
Sim_SP.Oil <- ggplot(Simulation_Normal) +
  geom_point(aes(x = Oil, y = SP500)) + 
  geom_smooth(aes(x = Oil, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.3, 0.3), ylim = c(-0.08, 0.08)) +
  labs(x = 'Simulated WTI Return (Normal)', y = 'Simulated S&P Return (Normal)') + 
  theme_minimal()

# Repeat for T dist
Sim_T.SP.Oil <- ggplot(Simulation_T) +
  geom_point(aes(x = Oil, y = SP500)) +
  geom_smooth(aes(x = Oil, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.3, 0.3), ylim = c(-0.08, 0.08)) +
  labs(x = 'Simulated VIX Return (T)', y = 'Simulated S&P Return (T)') + 
  theme_minimal()

# Show with Real Data
SP_Oil_Plots <- ggpubr::ggarrange(SP_Oil, Sim_SP.Oil, Sim_T.SP.Oil, nrow = 2, ncol = 2)

# SP500/Gold

Sim_SP.Gold <- ggplot(Simulation_Normal) +
  geom_point(aes(x = Gold, y = SP500)) + 
  geom_smooth(aes(x = Gold, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.05, 0.05), ylim = c(-0.1, 0.1))  +
  labs(x = 'Simulated Gold Return (Normal)', y = 'Simulated S&P Return (Normal)') + 
  theme_minimal()

# Repeat for T dist
Sim_T.SP.Gold <- ggplot(Simulation_T) +
  geom_point(aes(x = Gold, y = SP500)) +
  geom_smooth(aes(x = Gold, y = SP500), method = 'lm') +
  coord_cartesian(xlim = c(-0.05, 0.05), ylim = c(-0.1, 0.1)) +
  labs(x = 'Simulated Gold Return (T)', y = 'Simulated S&P Return (T)') + 
  theme_minimal()

# Show with Real Data
SP_Gold_Plots <- ggpubr::ggarrange(SP_Gold, Sim_SP.Gold, Sim_T.SP.Gold, nrow = 2, ncol = 2)

# Vix/Oil
Sim_Vix.Oil <- ggplot(Simulation_Normal) +
  geom_point(aes(x = VIX, y = Oil)) + 
  geom_smooth(aes(x = VIX, y = Oil), method = 'lm') +
  coord_cartesian(xlim = c(-0.2, 0.4), ylim = c(-0.2, 0.4))  +
  labs(x = 'Simulated VIX Return (Normal)', y = 'Simulated WTI Return (Normal)') + 
  theme_minimal()

# Repeat for T dist
Sim_T.Vix.Oil <- ggplot(Simulation_T) +
  geom_point(aes(x = VIX, y = Oil)) +
  geom_smooth(aes(x = VIX, y = Oil), method = 'lm') +
  coord_cartesian(xlim = c(-0.2, 0.4), ylim = c(-0.2, 0.4)) +
  labs(x = 'Simulated VIX Return (T)', y = 'Simulated WTI Return (T)') + 
  theme_minimal()

# Repeat for T dist
Vix_Oil_Plots <- ggpubr::ggarrange(Vix_Oil, Sim_Vix.Oil, Sim_T.Vix.Oil, nrow = 2, ncol = 2)

# Vix/Gold
Sim_Vix.Gold <- ggplot(Simulation_Normal) +
  geom_point(aes(x = VIX, y = Gold)) + 
  geom_smooth(aes(x = VIX, y = Gold), method = 'lm') +
  coord_cartesian(xlim = c(-0.2, 0.4), ylim = c(-0.05, 0.05))  +
  labs(x = 'Simulated VIX Return (Normal)', y = 'Simulated Gold Return (Normal)') + 
  theme_minimal()

# Repeat for T dist
Sim_T.Vix.Gold <- ggplot(Simulation_T) +
  geom_point(aes(x = VIX, y = Gold)) +
  geom_smooth(aes(x = VIX, y = Gold), method = 'lm') +
  coord_cartesian(xlim = c(-0.2, 0.4), ylim = c(-0.05, 0.05)) +
  labs(x = 'Simulated VIX Return (T)', y = 'Simulated Gold Return (T)') + 
  theme_minimal()

# Repeat for T dist
Vix_Gold_Plots <- ggpubr::ggarrange(Vix_Gold, Sim_Vix.Gold, Sim_T.Vix.Gold, nrow = 2, ncol = 2)

# Gold/Oil
Sim_Gold.Oil <- ggplot(Simulation_Normal) +
  geom_point(aes(x = Gold, y = Oil)) + 
  geom_smooth(aes(x = Gold, y = Oil), method = 'lm') +
  coord_cartesian(xlim = c(-0.05, 0.05), ylim = c(-0.2, 0.4))  +
  labs(x = 'Simulated Gold Return (Normal)', y = 'Simulated WTI Return (Normal)') + 
  theme_minimal()

# Repeat for T dist
Sim_T.Gold.Oil <- ggplot(Simulation_T) +
  geom_point(aes(x = Gold, y = Oil)) +
  geom_smooth(aes(x = Gold, y = Oil), method = 'lm') +
  coord_cartesian(xlim = c(-0.05, 0.05), ylim = c(-0.2, 0.4)) +
  labs(x = 'Simulated Gold Return (T)', y = 'Simulated WTI Return (T)') + 
  theme_minimal()

# Repeat for T dist
Oil_Gold_Plots <- ggpubr::ggarrange(Oil_Gold, Sim_Gold.Oil, Sim_T.Gold.Oil, nrow = 2, ncol = 2)


# Distribution of ten day returns
# Define a function to pick binwidth - this is the Freedman Diaconis Rule
FD_Hist <- function(asset) {
  Bin.width = 2 * IQR(asset) * (length(asset) ^(-1/3))
  Bin.width
}
# SP500
SP500.NormReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = SP.10Day_Norm), binwidth = FD_Hist(TenDaySim$SP.10Day_Norm)) +
  labs(x = 'S&P 500 10 Day $ Return (Normal)', y = 'Frequency') +
  coord_cartesian(xlim = c(-1000, 1000)) +
  theme_minimal()
  
SP500.TReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = SP.10Day_T), binwidth = FD_Hist(TenDaySim$SP.10Day_T)) +
  labs(x = 'S&P 500 10 Day $ Return (T)', y = 'Frequency') +
  coord_cartesian(xlim = c(-1000, 1000)) +
  theme_minimal()

SP500.Hists <- ggpubr::ggarrange(SP500.NormReturns, SP500.TReturns, nrow = 1, ncol = 2)

# VIX
Vix.NormReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = VIX.10Day_Norm), binwidth = FD_Hist(TenDaySim$VIX.10Day_Norm)) +
  labs(x = 'VIX 10 Day $ Return (Normal)', y = 'Frequency') +
  coord_cartesian(xlim = c(-50, 50)) +
  theme_minimal()

Vix.TReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = VIX.10Day_T), binwidth = FD_Hist(TenDaySim$VIX.10Day_T)) +
  labs(x = 'VIX 10 Day $ Return (T)', y = 'Frequency') +
  coord_cartesian(xlim = c(-50, 50)) +
  theme_minimal()

Vix.Hists <- ggpubr::ggarrange(Vix.NormReturns, Vix.TReturns, nrow = 1, ncol = 2)

# Oil
Oil.NormReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = Oil.10Day_Norm), binwidth = FD_Hist(TenDaySim$Oil.10Day_Norm)) +
  labs(x = 'Oil 10 Day $ Return (Normal)', y = 'Frequency') +
  coord_cartesian(xlim = c(-30, 30)) +
  theme_minimal()

Oil.TReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = Oil.10Day_T), binwidth = FD_Hist(TenDaySim$Oil.10Day_T)) +
  labs(x = 'Oil 10 Day $ Return (T)', y = 'Frequency') +
  coord_cartesian(xlim = c(-30, 30)) +
  theme_minimal()

Oil.Hists <- ggpubr::ggarrange(Oil.NormReturns, Oil.TReturns, nrow = 1, ncol = 2)

# Gold
Gold.NormReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = Gold.10Day_Norm), binwidth = FD_Hist(TenDaySim$Gold.10Day_Norm)) +
  labs(x = 'Gold 10 Day $ Return (Normal)', y = 'Frequency') +
  coord_cartesian(xlim = c(-200, 200)) +
  theme_minimal()

Gold.TReturns <- ggplot(TenDaySim) +
  geom_histogram(aes(x = Gold.10Day_T), binwidth = FD_Hist(TenDaySim$Gold.10Day_T)) +
  labs(x = 'Gold 10 Day $ Return (T)', y = 'Frequency') +
  coord_cartesian(xlim = c(-200, 200)) +
  theme_minimal()

Gold.Hists <- ggpubr::ggarrange(Gold.NormReturns, Gold.TReturns, nrow = 1, ncol = 2)