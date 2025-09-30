## Adv. Data Analytics - Mixed Effects Model Assignment 04
# SSS 27 SEPT 2025

# load in libraries
library(ggplot2)
library(dplyr)
library(arm)
library(ggfortify)
library(lme4)
library(emmeans)
library(ggtext)
library(patchwork)
library(performance)
library(flexplot)
library(ggthemes)
library(lmerTest)

# load in the dataset
strikes <- read.csv("strike_data.csv")

# look at the data
glimpse(strikes)

# variables to use:
# temp_treatment
# snake
# neck_max_vel
# max_gape_vel

# make snake (ID) and treatment temperature factors
strikes$snake <- as.factor(strikes$snake)


# plot the data
ggplot(strikes, aes(temp_treatment, fill=snake)) +
  geom_bar(color="black") +
  scale_y_continuous(breaks=seq(10, 25, 35)) +
  ggtitle(label = "Unbalanced Sampling Plot",
          subtitle = "n = 23 snakes; 5 temp. treatments; 295 total tests") +
  theme_calc()
  
# note that 21 of the trials led to no strike; and Gretel never struck for future 0's in sum stats
  
# plot the changes across temperature treatments for each snake
plot_treat.line <- ggplot(strikes, aes(x=temp_treatment, y=neck_max_vel, group=snake, color=snake, shape=snake)) + 
  geom_point(size=4, position = position_dodge2(width=.33, preserve = "total")) +
  scale_y_continuous() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Hotter = Faster? Crotalus spp. Strike Speeds",
       x= "Temperature Treatment (°C)", 
       y = "Maximum Velocity of Strike (m/s)") +
  theme_calc()

#box plot of raw data by machine; does not adjust for repeated measures of individuals
plot_treat.box <- ggplot(strikes, aes(x=temp_treatment, y=neck_max_vel, group = temp_treatment, color=temp_treatment)) + 
  geom_boxplot() +
  scale_y_continuous() +
  theme(legend.position="none") +
  labs(title="Uneven Sampling: Tests",
       subtitle = "Raw data",
       x= "Temperature Treatment(°C)", 
       y = "Maximum Velocity of Strike (m/s)")+
  theme_minimal()

#box plot of raw data by person
plot_snake.box <- ggplot(strikes, aes(x=snake, y=neck_max_vel, group = snake, color=snake)) + 
  geom_boxplot() +
  scale_y_continuous() +
  theme(legend.position="none") +
  labs(title="Uneven Sampling: Snakes",
       subtitle = "Raw data",
       x= "Snake", 
       y = "Maximum Velocity of Strike (m/s)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_treat.line/(plot_treat.box + plot_snake.box) #uses library(patchwork) for plot organization)

# summarise data
treatment_means <- strikes %>%
  group_by(temp_treatment) %>%
  filter(!is.na(neck_max_vel)) %>%
  summarise(mean_spd = mean(neck_max_vel),
            se_rating=sd(neck_max_vel)/sqrt(n()))
treatment_means

# run a mixed model for the different temperature treatments
# the fixed effect is the temperature 
# the random effect is the snake


# Replace NA with 0 in all numeric columns
strikes_clean <- replace(strikes, is.na(strikes), 0)

strikes_clean$temp_treatment <- as.factor(strikes_clean$temp_treatment)


mixed_treatment <- lmer(neck_max_vel ~ temp_treatment * snake + (1|snake), data = strikes_clean)

#mixed_treatment.spp <- lmer(neck_max_vel ~ temp_treatment * snake + (1|snake/species), data = strikes_clean)
anova(mixed_treatment)

#anova(mixed_treatment.spp)
# temperature treatment has an effect
# but also the interaction of the snake and the temperature can explain the variance

# summarise the model
summary(mixed_treatment)

# of note in the table is the removal of all tests which did not lead to a strike (new total 277)
# also note that 23 snakes now being used bc Gretel has been kicked out


# check the model
performance::check_model(mixed_treatment)

# overall looks good! wth the exception of collinearity

# fit the estimated marginal means from the model; mean each temp. treatment
mixed_treatment_emm <- emmeans(mixed_treatment, "temp_treatment")
mixed_treatment_emm

# for each snake
mixed_snake_emm <- emmeans(mixed_treatment, "snake")
mixed_snake_emm

# fit the emmeans from the model for snake*treatment
emmeans(mixed_treatment, "temp_treatment", "snake") 

# other way around temp*snake
emmeans(mixed_treatment, "snake", "temp_treatment")


# raw means vs the model adjusted estimated marginal means

strikes_emm <- as.data.frame(mixed_treatment_emm)
strikes_emm
treatment_means

#model-adjusted means (emm) are different than raw means; adjusted for the same snake going through multiple trials


# model selection for random variation between species AND snake

ggplot(strikes, aes(x=temp_treatment, y=neck_max_vel, group=species, color=species, shape=species)) + 
  geom_point(size=4, position = position_dodge2(width = 0.5, preserve = "total")) +
  scale_y_continuous() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="C.o. helleri vs C. scutulatus",
       subtitle = "n = 23, helleri = 12, scutulatus = 11",
       x= "Temperature Treatment (°C)", 
       y = "Maximum Velocity of Strike (m/s)") +
  theme_minimal()

mixed_treatment.spp1 <- lmer(neck_max_vel ~ temp_treatment + (1|snake), data = strikes_clean) # same slopes; random intercepts
mixed_treatment.spp2 <- lmer(neck_max_vel ~ temp_treatment + (species|snake), data = strikes_clean) # random intercepts; random slopes

summary(mixed_treatment.spp1)
summary(mixed_treatment.spp2)

flexplot::visualize(mixed_treatment.spp1, plot="model", sample=23) +  
  scale_x_discrete(limits = c("15", "20", "25", "30", "35")) +          # same slopes
  ggtitle("random intercepts-same slopes") +
  labs(subtitle = "Formula: Max Velocity ~ Temperature Treatment + (1|Snake)")

flexplot::visualize(mixed_treatment.spp2, plot="model", sample=23) +
  scale_x_discrete(limits = c("15", "20", "25", "30", "35")) +          # random intercepts; random slopes
  ggtitle("random intercepts-random slopes") +
  labs(subtitle = "Formula: Max Velocity ~ Temperature Treatment + (Species | Snake)")

# compare the models summaries 
arm::display(mixed_treatment.spp1)
arm::display(mixed_treatment.spp2)

strikes_parameters <- parameters::compare_parameters(mixed_treatment.spp1, mixed_treatment.spp2)

performance::compare_performance(mixed_treatment.spp1, mixed_treatment.spp2)




mixed_treatment_emm <- emmeans(mixed_treatment, "temp_treatment")
data_emm <- as.data.frame(mixed_treatment_emm)

data_emm
ggplot(data_emm, aes(x=temp_treatment, y=emmean)) + 
  geom_point(size=4) +
  scale_y_continuous(limits = c(2, 3.8)) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2) +
  labs(title="Hotter = Faster? Rattlesnake Strike Speeds", x= "Temperature Treatment (C)", y = "Max Strike Velocity (m/s)") +
  geom_point(data = treatment_means, size = 4, x= treatment_means$temp_treatment, y = treatment_means$mean_spd, color = "gray") +
  theme_calc() +
  theme(legend.position = "none")

