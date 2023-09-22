
  # ===================================================================== #
  #  Release mortality in snapper Chrysophrys auratus                     #                                                                     #  
  #                                                                       #
  #  AUTHOR:       Jade Quinton Maggs                                     #
  #  EMAIL:        jademaggs@gmail.com / jade.maggs@niwa.co.nz            #
  #  UPDATED:      2023-08-23                                             #
  #  R VERSION:    v4.3.0                                                 #
  #                                                                       #
  # ===================================================================== #


# README ==================================================================

# Code provides various summaries of data collection and then moves on 
# to modelling the risk of death. 
# The script uses a fully portable working directory structure 
# containing an R-project file and four subdirectories: 
#
#   - code:   all code files
#   - docs:   documentation
#   - data:   data and any other inputs
#   - output: output figures, tables, etc 

# SETUP ===================================================================

# Erase all previous variables from memory
rm(list = ls())

# IMPORT LIBRARIES
library(dplyr)
library(ggbeeswarm)  # For beeswarm plots
library(ggpubr)  # To arrange ggplots
library(effects) # To extract and plot predictor effects
library(lattice) # Required to change trellis settings
library(splines) # for fitting natural cubic splines - ns()
library(lmtest)  # for lrtest() function

# IMPORT DATASETS
rm_data <- read.csv("data/release_mortality.csv")

# FORMAT DATA -------------------------------------------------------------

# Convert variables to ordered factors
levels(factor(rm_data$treatment))
rm_data$treatment <- factor(rm_data$treatment, 
                            levels = c('Control', 'Recreational'))

levels(factor(rm_data$capture_hook_site))
rm_data$capture_hook_site <- 
  factor(rm_data$capture_hook_site, levels = c('Control', 'Lip', 'Foul', 'Gut'))

levels(factor(rm_data$capture_depth_range))
rm_data$capture_depth_range <- 
  factor(rm_data$capture_depth_range, levels = c('Shallow', 'Medium', 'Deep'))

# Convert dates to proper format
rm_data$pickup_datetime <- strptime(rm_data$pickup_datetime, 
                                    format = "%Y-%m-%d %H:%M")

rm_data$net_release_datetime <-
  as.POSIXct(paste(rm_data$capture_date, rm_data$net_release_time), 
             format="%Y-%m-%d %H:%M")

rm_data$capture_datetime <-
  as.POSIXct(paste(rm_data$capture_date, rm_data$capture_time), 
             format="%Y-%m-%d %H:%M")

rm_data$retrieval_datetime <-
  as.POSIXct(paste(rm_data$retrieval_date, rm_data$retrieval_time), 
             format="%Y-%m-%d %H:%M")

# Create a concatenated string for season and treatment
rm_data$season_treatment <- 
  paste0(rm_data$season, ' (', 
         rm_data$treatment,')')

# Shorten the concatenation for plotting
rm_data$season_treatment <- gsub("Experiment", "Expt", rm_data$season_treatment)

# Create na ordered hook site factor variable
rm_data$hook_site <- factor(rm_data$capture_hook_site, c("Lip","Foul","Gut"))

# Separate out recreational fish records
rm_data_recr <- 
  subset(rm_data, treatment == 'Recreational') # Remove control fish


# Summarise data collection ================================================

# ****************
# Summary table
# ****************

# Capture depth range - summary table
rm_data %>% 
  group_by(treatment, season, capture_depth_range) %>% 
  summarise(min_depth = min(capture_depth_m), 
            max_depth = max(capture_depth_m))

# SST temperature
rm_data %>% 
  group_by(season_treatment, capture_depth_range) %>% 
  summarise(mean(water_temperature, na.rm = TRUE))

rm_data %>% 
  group_by(season) %>% 
  summarise(mean(water_temperature, na.rm = TRUE))
  
# Number of fish caught - summary table
rm_data %>% 
  group_by(treatment, season) %>% 
  tally()

rm_data %>% 
  group_by(treatment, season, capture_depth_range) %>% 
  tally()

rm_data %>% 
  group_by(treatment, season, capture_depth_range, capture_hook_site) %>% 
  tally()

# Number of mortalities - summary table
rm_data %>% 
  filter(fish_retrieval_fate == 'Died') %>% 
  group_by(treatment, season) %>% 
  tally()

rm_data %>% 
  filter(fish_retrieval_fate == 'Died') %>% 
  group_by(treatment, season, capture_depth_range) %>% 
  tally()

rm_data %>% 
  filter(fish_retrieval_fate == 'Died') %>% 
  group_by(treatment, season, capture_depth_range, capture_hook_site) %>% 
  tally()


# ****************
# Figure 3 a-b
# ****************

# Configure element sizes
text_size = 8
bee_cex = 0.5
bee_size = 0.1
anno_size = 4

# Distribution of snapper collection by treatment and capture depth
p1 <- 
  ggplot(data = rm_data, aes(x = season_treatment, y = capture_depth_m, colour = season_treatment)) +
  geom_beeswarm(cex = bee_cex, size = bee_size) +
  labs(x = 'Treatment', y = 'Capture depth (m)') +
  annotate("text", x = 0.5, y = 35, label = "a)", size = anno_size) +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = 'none')


# Length frequency of shallow caught snapper (compare control with treatment)
p2 <- 
  rm_data %>% 
  filter(capture_depth_range == 'Shallow') %>% 
  ggplot(aes(x = retrieval_length_cm, fill = treatment)) +
  geom_histogram(binwidth = 1, position=position_dodge(0.75)) +
  scale_fill_manual(values = c('darkgrey','black'), name = '', labels = c('Control','Treatment')) + 
  labs(x = 'Fork length (cm)', y = 'Frequency') +
  annotate("text", x = 5, y = 45.5, label = 'b)', size = anno_size) +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = c(0.85, 0.85), # Adjust legend position
        legend.justification = c(1, 1)) # Adjust legend justification

# Arrange plots on grid and save to output
ggarrange(p1, p2, ncol = 2, nrow = 1)
ggsave('output/figure3.tiff', plot = last_plot(), 
       device = "tiff", width = 200, height = 75, units = 'mm', dpi = 500,
       compression = 'lzw')

# Clean up
rm(p1, p2)

# *************************
# Figure 4 a-d
# *************************

# Configure element sizes
text_size = 10
bee_cex = 0.5

# Distribution of snapper by hook site and capture depth (collections)
p1 <- 
  ggplot(data = rm_data_recr, 
         aes(x = hook_site, y = capture_depth_m, colour = hook_site)) +
  geom_beeswarm(cex = bee_cex, size = 0.1) +
  labs(x = 'Hook site', y = 'Capture depth (m)') +
  annotate("text", x = 0.5, y = 35, label = "a)", size = 4) +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = 'none')

# Distribution of snapper by hook site and capture depth (mortalities)
p2 <- 
  ggplot(data = subset(rm_data_recr, fish_retrieval_fate == 'Died'), 
         aes(x = hook_site, y = capture_depth_m, colour = hook_site)) +
  geom_beeswarm(cex = bee_cex, size = 0.1) +
  labs(x = 'Hook site', y = 'Capture depth (m)') +
  annotate("text", x = 0.5, y = 35, label = "b)", size = 4) +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = 'none')

# Empirical cumulative distribution function plot for fork length (collections)
p3 <- 
  rm_data_recr %>% 
  ggplot(aes(retrieval_length_cm)) +
  stat_ecdf(aes(colour = capture_depth_range), geom = 'line') +
  scale_colour_manual(values = c('lightblue','blue','darkblue')) +
  labs(x = 'Fork length (cm)', y = 'Cumulative proportion') + 
  annotate("text", x = 0.5, y = 0.9, label = "c)", size = 4) +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = c(0.95, 0.65), 
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = 
          element_rect(color = "black", linewidth = 0.1, linetype = "solid")) 

# Empirical cumulative distribution function plot for fork length (mortalities)
p4 <- 
  rm_data_recr %>% 
  filter(fish_retrieval_fate == 'Died') %>% 
  ggplot(aes(retrieval_length_cm)) +
  stat_ecdf(aes(colour = capture_depth_range), geom = 'line') +
  scale_colour_manual(values = c('lightblue','blue','darkblue')) +
  labs(x = 'Fork length (cm)', y = 'Cumulative proportion') + 
  annotate("text", x = 0.5, y = 0.9, label = "d)", size = 4) +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = c(0.95, 0.65),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = 
          element_rect(color = "black", linewidth = 0.1, linetype = "solid"))

# Arrange plots on grid and save to output
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave('output/figure4.tiff', plot = last_plot(), 
       device = "tiff", width = 200, height = 150, units = 'mm', dpi = 500,
       compression = 'lzw')

# Clean up
rm(p1, p2, p3, p4)


# **********************************************************
# Now focus on treatment fish as all control fish survived
# **********************************************************


# ****************
# Figure 5 
# total transfer time
# ****************

# Convert transfer duration to number in the treatment dataset
rm_data_recr$total_transfer_time_num <- 
  as.numeric(substr(rm_data_recr$total_transfer_time, 4,5))

# Configure element sizes
text_size = 8
bee_cex = 0.5
bee_size = 0.1
anno_size = 4

# Plot fish retrieval fate (alive/dead) vs transfer time
ggplot(data = rm_data_recr, 
       aes(x = fish_retrieval_fate, y = total_transfer_time_num, 
           colour = fish_retrieval_fate)) +
  geom_beeswarm(cex = bee_cex, size = 0.1) +
  scale_colour_manual(values = c('darkred','darkgreen')) +
  labs(x = 'Fish retrieval fate', y = 'Total transfer time (minutes') +
  theme_bw() +
  theme(text = element_text(size = text_size),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(vjust=-5),
        axis.title.y = element_text(vjust=5),
        panel.grid = element_blank(),
        legend.position = 'none')

# Save the plot
ggsave('output/figure5.tiff', plot = last_plot(), 
       device = "tiff", width = 100, height = 75, units = 'mm', dpi = 500,
       compression = 'lzw')


# **********************
# Tabulate other factors
# **********************

# Stocking density
table(rm_data$retrieval_net_id, rm_data$season)
table(rm_data_recr$retrieval_net_id[rm_data_recr$fish_retrieval_fate == 'Died'], 
      rm_data_recr$season[rm_data_recr$fish_retrieval_fate == 'Died'])

# Time to death
sort((rm_data_recr$retrieval_datetime[rm_data_recr$fish_retrieval_fate == 'Died'] - 
        rm_data_recr$net_release_datetime[rm_data_recr$fish_retrieval_fate == 'Died'])/60)

# Float/sink
rm_data_recr %>%
  filter(fish_retrieval_fate == 'Died') %>%
  group_by(capture_depth_range, hook_site, retrieval_float_sink) %>%
  tally()

# Risk of death ============================================================

# Fit null model
m0 <- glm(I(fish_retrieval_fate == 'Died') ~ 1, data = rm_data_recr, family = binomial(logit))
AIC(m0)

# Fit maximal model
# not including net_id because correlated with capture depth
m1 <- 
  glm(I(fish_retrieval_fate == 'Died') ~ 
        hook_site * capture_depth_m * retrieval_length_cm * season,
      data = rm_data_recr,
      family = binomial(logit)) # log link not working

summary(m1)
anova(m1, test = 'LRT') # signif: hook site, depth
anova(m1, test = 'Chisq') # same result as LRT
AIC(m1) # 836.3

# Model selection using backward stepwise regression
# more difficult to miss the right model using backward stepwise  - Thomas Lumley
# forward better with large data sets, millions of records and variables
m2 <- step(m1, direction = 'backward') # minimising AIC
summary(m2)
anova(m2, test = 'LRT')
anova(m2, test = 'Chisq')
AIC(m2)  # 820.7

# AIC reduced from 836.3 to 820.7
# Now dig into remaining terms starting with interactions

# Check the affect of removing factors from step model
m3 <- update(m2, ~. - capture_depth_m:season) # drop interaction term
summary(m3)
lrtest(m2, m3) # makes little difference
anova(m2, m3, test = 'LRT')
anova(m2, m3, test = 'Chisq')
print(AIC(m3) - AIC(m2)) # AIC increased marginally from 820.7 to 821.4

# capture_depth_m:season interaction can be dropped

# Check the affect of removing factors from step model
m4 <- update(m2, ~. - hook_site:capture_depth_m)  # drop interaction term
summary(m4)
lrtest(m2, m4) # makes significant difference
anova(m2, m4, test = 'Chisq')
print(AIC(m4) - AIC(m2)) # AIC increased from 820.7 to 828.9

# hook_site:capture_depth must be retained

# Now look at main effects...

# Check the affect of removing factors from step model
m5 <- update(m2, ~. - season)
summary(m5)
lrtest(m2, m5) # makes significant difference
anova(m2, m5, test = 'Chisq')
print(AIC(m5) - AIC(m2)) # marginal increase 

# retain season for now

# Check the affect of removing factors from step model
m6 <- update(m2, ~. - retrieval_length_cm)
summary(m6)
lrtest(m2, m6) # makes little difference
anova(m2, m6, test = 'Chisq')
print(AIC(m6) - AIC(m2)) # marginal increase

# drop retrieval_length_cm term

# Now drop those terms found to make little difference 
m7 <- update(m2, ~. -capture_depth_m:season -retrieval_length_cm)
summary(m7)
lrtest(m2, m7) # Check difference between stepped model and current model
anova(m2, m7, test = 'Chisq') # season is no longer significant and can be dropped
AIC(m7); AIC(m2); AIC(m1) # Getting better 

# Now drop season 
m8 <- update(m7, ~. -season)
summary(m8)
anova(m8, test = 'Chisq') # All remaining terms significant
AIC(m8) # marginal increase

# Now try improving fit of capture depth

# First try a quadratic term on capture depth
m9 <- glm(I(fish_retrieval_fate == 'Died') ~ 
            hook_site + I(capture_depth_m^2) + hook_site:I(capture_depth_m^2),
          data = rm_data_recr,
          family = binomial(logit))

summary(m9)
anova(m8, m9, test = 'Chisq') # significance not showing???
AIC(m8); AIC(m9) # notable increase in AIC

# Now try a natural cubic spline on capture depth
m10 <- glm(I(fish_retrieval_fate == 'Died') ~ 
            hook_site + ns(capture_depth_m, df = 3) + hook_site:ns(capture_depth_m, df = 3),
          data = rm_data_recr,
          family = binomial(logit))

summary(m10)
anova(m10, test = 'Chisq')
anova(m9, m10, test = 'Chisq') # now a big difference between quadratic term and ns spline
anova(m8, m10, test = 'Chisq') # significant difference between simple term and ns spline
AIC(m8); AIC(m9); AIC(m10) # ns spline gives much better AIC value

# Try gams
library(mgcv) # GAM time

# Try gam with interaction effect
m11 <- 
  gam(I(fish_retrieval_fate == 'Died') ~ 
        hook_site + s(capture_depth_m) + s(capture_depth_m, by = hook_site),
      data = rm_data_recr, 
      family = binomial(logit))

summary.gam(m11)
par(mfrow = c(2,2))
par(mar = c(4,4,4,4) + 0.1)
plot.gam(m11) 

# Try gam with only main effects
m12 <- 
  gam(I(fish_retrieval_fate == 'Died') ~ 
        hook_site + s(capture_depth_m),
      data = rm_data_recr, 
      family = binomial(logit))

summary(m12)
par(mfrow = c(1,1))
par(mar = c(4,4,4,4) + 0.1)
plot.gam(m12)

# Retain m10 as minimum adequate model (final model)
fm <- m10

# **************************
# Estimates from final model
# **************************
AIC(glm(I(fish_retrieval_fate == 'Died') ~ hook_site, data = rm_data_recr, family = binomial(logit)))
AIC(glm(I(fish_retrieval_fate == 'Died') ~ hook_site + ns(capture_depth_m, df = 3), data = rm_data_recr, family = binomial(logit)))
AIC(glm(I(fish_retrieval_fate == 'Died') ~ hook_site + ns(capture_depth_m, df = 3) + hook_site:ns(capture_depth_m, df = 3), data = rm_data_recr, family = binomial(logit)))

# Extract coefficient estimates
coef_est <- summary(fm)$coefficients[, "Estimate"]

# Back-transform coefficient estimates using logistic function
odds_ratio <- exp(coef_est)
prob_ratio <- odds_ratio / (1 + odds_ratio)

# Print the results
print("Coefficient estimates (log-odds): ")
print(coef_est)
print("Coefficient estimates (odds ratio): ")
print(odds_ratio)
print("Coefficient estimates (probability ratio): ")
print(prob_ratio)


# **************************
# Fig. S1
# Effects of final model
# Hook site
# **************************

# Predict from final model
fm_eff_hs <- predictorEffect('hook_site', fm, 
                  xlevels=list(capture_depth_m=c(5,10,15,20,25,30,35,40)))
fm_eff_hs$model.matrix

# Set up tiff plotting device
tiff('output/figure6.tiff',  width = 200, height = 150, units = 'mm', 
     res = 500, compression = 'lzw')

# Set up grid for plotting
trellis.par.set(list(fontsize = list(text = 8)))

# Plot
plot(fm_eff_hs, xlab = 'Hook site', ylab = 'Probability of death', main = '',
     axes=list(y=list(type="response")))

# Close plot device
dev.off()

# **************************
# Fig. 6
# Effects of final model
# Capture depth
# **************************

# Predict from final model
fm_eff_cd <- predictorEffect('capture_depth_m', fm)
fm_eff_cd$model.matrix

# Set up tiff plotting device
tiff('output/figure7.tiff',  width = 200, height = 75, units = 'mm', res = 500, compression = 'lzw')

# Set up grid for plotting
trellis.par.set(list(fontsize = list(text = 8))) # Set plot size after png()

#Plot
plot(fm_eff_cd, xlab = 'Capture depth (m)', ylab = 'Probability of death', main = '',
     axes = list(y = list(type = "response")))

# Close plot device
dev.off()

# **************************
# Table 6
# Tabulate predictions from 
# final model
# **************************

# Set up dataframe for predictions with variable hook site and depth
df <- data.frame(hook_site = rep(c('Lip','Foul','Gut'), each = 8),
                 capture_depth_m = rep(seq(5, 40, 5), times = 3))

# Predict from final model
df$death_fm <- predict(fm, df, type = 'response')

# Write to csv
write.table(df, file = 'output/predictions.csv', sep = ',', row.names = FALSE)

# END OF FILE =============================================================

