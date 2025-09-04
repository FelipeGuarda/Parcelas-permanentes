# Load required packages
library(tidyverse)
library(ggpubr)
library(car)
library(emmeans)
library(multcomp)

# Read data
growth_data <- read.csv("gr.model.data.csv")

growth_data <- growth_data %>%
  mutate(parc_elev = factor(parc_elev),
         growth_period = factor(growth_period),
         nombre = factor(nombre))

# Check structure
str(growth_data)
summary(growth_data)


# Summary statistics
growth_data %>%
  group_by(parc_elev, growth_period) %>%
  summarise(mean_growth = mean(growth_rate),
            sd_growth = sd(growth_rate),
            n = n())

# Visualize distributions
ggplot(growth_data, aes(x = growth_rate, fill = growth_period)) +
  geom_histogram(bins = 30) +
  facet_wrap(~parc_elev) +
  theme_minimal() +
  labs(title = "Growth Rate Distribution by Elevation and Period")

# Boxplot visualization
ggplot(growth_data, aes(x = parc_elev, y = growth_rate, fill = growth_period)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Growth Rate by Elevation and Period",
       x = "Elevation", y = "Growth Rate")


## Fit the model with interaction
model <- glm(growth_rate ~ growth_period * parc_elev, 
             family = gaussian(),
             data = growth_data)

# Model summary
summary(model)

# ANOVA table
anova(model, type = "III")


##Model diagnositc
# Check assumptions
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Normality test
shapiro.test(residuals(model))

# Homoscedasticity test
ncvTest(model)   #not running

# Check for outliers
influenceIndexPlot(model)    #not running



##Post-hoc analysis
# Post-hoc comparisons with Tukey adjustment
emmeans_results <- emmeans(model, pairwise ~ growth_period * parc_elev, adjust = "tukey")
summary(emmeans_results)

# Compact letter display
cld(emmeans_results, Letters = letters, adjust = "tukey")  ##no cahco



##visualization of the results
# Create a summary dataframe for plotting
summary_data <- growth_data %>%
  group_by(parc_elev, growth_period) %>%
  summarise(mean_growth = mean(growth_rate),
            se_growth = sd(growth_rate)/sqrt(n()),
            .groups = 'drop')

# Interaction plot
ggplot(summary_data, aes(x = parc_elev, y = mean_growth, 
                         group = growth_period, color = growth_period)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_growth - se_growth, 
                    ymax = mean_growth + se_growth), 
                width = 0.1) +
  theme_minimal() +
  labs(title = "Interaction Between Elevation and Growth Period",
       x = "Elevation", y = "Mean Growth Rate",
       color = "Growth Period") +
  scale_color_manual(values = c("#E69F00", "#56B4E9"))




#########################################################################
#########################################################################
##### model glm with elevation, periods a. species
#########################################################################


#Modelfitting
# Fit the full model with species and all interactions
full_model <- glm(growth_rate ~ growth_period + parc_elev + nombre,
                  family = gaussian(),
                  data = growth_data)
summary(full_model)

# More parsimonious model (all 2-way interactions)
main_model <- glm(growth_rate ~ (growth_period + parc_elev + nombre)^2,
                  family = gaussian(),
                  data = growth_data)
summary(main_model)

# Compare models
anova(main_model, full_model, test = "Chisq")

# Select the appropriate model (we'll use main_model if 3-way interaction isn't significant)
final_model <- if(anova(main_model, full_model, test = "Chisq")$`Pr(>Chi)`[2] < 0.05) {
  full_model
} else {
  main_model
}

# Model summary
summary(final_model)

# ANOVA table
car::Anova(final_model, type = "III")
anova(final_model, type = "III")



##Model dianostic

# Set up plotting area
par(mfrow = c(2, 2))

# Residual plots
plot(final_model)

# Q-Q plot with confidence envelope
car::qqPlot(residuals(final_model), main = "Q-Q Plot")

# Residuals vs. fitted with smoother
ggplot(data.frame(fitted = fitted(final_model),
                  resid = residuals(final_model)),
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values", y = "Residuals")

# Check homogeneity of variance
car::ncvTest(final_model)

# Check for influential observations
car::influencePlot(final_model)

# Reset plotting area
par(mfrow = c(1, 1))


##post-hoc analysis
# Create emmeans object
emm_options(pbkrtest.limit = 5000) # Increase limit if needed

# Compare growth periods within each elevation and species
emmeans_species <- emmeans(final_model, 
                           specs = pairwise ~ growth_period | parc_elev + nombre,
                           adjust = "tukey")

# Compare elevations within each growth period and species
emmeans_elev <- emmeans(final_model,
                        specs = pairwise ~ parc_elev | growth_period + nombre,
                        adjust = "tukey")

# Compare species within each elevation and growth period
emmeans_nombre <- emmeans(final_model,
                          specs = pairwise ~ nombre | parc_elev + growth_period,
                          adjust = "tukey")

# Summary of comparisons
summary(emmeans_species)
summary(emmeans_elev)
summary(emmeans_nombre)

# Compact letter display for visualization
cld_data <- cld(emmeans(final_model, ~ growth_period * parc_elev * nombre),
                Letters = letters,
                adjust = "sidak")   #tukey is not provide
#Error in CLD(emmeans(final_model, ~growth_period * parc_elev * nombre),  : 
#could not find function "CLD"


###VIsualization

# Create summary data including species
summary_data_species <- growth_data %>%
  group_by(parc_elev, growth_period, nombre) %>%
  summarise(mean_growth = mean(growth_rate, na.rm = TRUE),
            se_growth = sd(growth_rate, na.rm = TRUE)/sqrt(n()),
            n = n(),
            .groups = 'drop') %>%
  left_join(cld_data, by = c("parc_elev", "growth_period", "nombre"))

# Interaction plot by species
ggplot(summary_data_species, 
       aes(x = parc_elev, y = emmean, 
           group = interaction(growth_period, nombre),
           color = nombre, linetype = growth_period)) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.3), size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, position = position_dodge(width = 0.3)) +
  geom_text(aes(y = upper.CL + 0.05, label = .group),
            position = position_dodge(width = 0.3),
            show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Growth Rate by Elevation, Period, and Species",
       x = "Elevation", y = "Mean Growth Rate",
       color = "Species", linetype = "Growth Period") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Faceted plot by species
ggplot(summary_data_species, 
       aes(x = parc_elev, y = emmean, 
           group = growth_period, color = growth_period)) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_text(aes(y = upper.CL + 0.05, label = .group),
            show.legend = FALSE) +
  facet_wrap(~ nombre) +
  theme_minimal() +
  labs(title = "Growth Rate Patterns by Species",
       x = "Elevation", y = "Mean Growth Rate",
       color = "Growth Period") +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





