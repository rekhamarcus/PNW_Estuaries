#methods figure script

library(dplyr)
library(ggplot2)
library(tidyterra)
library(terra) #for working with spatial data
library(sf) #for working with spatial data
library(basemaps)#for basemaps 
library(ggspatial)
library(zoo) #for yearmonth
library(scales) #for density plot colors
library(lubridate)

setwd("C:/Users/rekha/OneDrive - University of Victoria/Wetlands")

#concept - change in mean

x <- seq(from = -5, to = 30, by = 0.05)
df <- data.frame(
  scenario = factor(rep(c("historical", "future"), each = 701)),
  temp = round(c(rnorm(x, mean = 7, sd = 5),
                 rnorm(x, mean = 15, sd = 5))))
  
ggplot(df, aes(x = temp, fill = scenario)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp)- 5, sd = sd(df$temp)), geom = "area", fill = "#457b9d", alpha = 0.7, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 5, sd = sd(df$temp)), geom = "area", fill = "#e63946", alpha = 0.7, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp)- 5, sd = sd(df$temp))) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 5, sd = sd(df$temp))) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  geom_vline(data = df, aes(xintercept = mean(df$temp)- 5, color = "mean"), linetype = "dashed", color = "black") +
  geom_vline(data = df, aes(xintercept = mean(df$temp) + 5, color = "mean"), linetype = "dashed", color = "black") +
  geom_segment(aes(x = mean(df$temp)- 5, y = 0.01, xend = mean(df$temp) + 5, yend = 0.01), arrow = arrow()) +
  geom_text(label = "Mean Change", x = 11, y = 0.014, colour = "black", fontface = "bold", size = 8) +
  xlab("Variable") +
  ylab("Frequency") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(full = "transparent", color = "black", size = 1), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.position = c(0.9, 0.8))
  
ggsave("Figures/change_mean.png",
       width = 9,
       height = 7, 
       dpi = 600)

#concept - change in variability

df <- data.frame(
  scenario = factor(rep(c("historical", "future"), each = 701)),
  temp = round(c(rnorm(x, mean = 7, sd = 5),
                 rnorm(x, mean = 9, sd = 8))))

ggplot(df, aes(x = temp, fill = scenario)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) - 1), geom = "area", fill = "#457b9d", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) - 1), geom = "area", fill = "#457b9d", xlim = c((mean(df$temp)), (mean(df$temp)) + sd(df$temp)), alpha = 0.9, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) + 5), geom = "area", fill = "#e63946", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) + 5), geom = "area", fill = "#e63946", xlim = c((mean(df$temp)), (mean(df$temp)) + (sd(df$temp) + 4)), alpha = 0.9, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) - 1)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) + 5)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  geom_segment(aes(x = mean(df$temp), y = 0.037, xend = mean(df$temp) + (sd(df$temp)), yend = 0.037)) +
  geom_segment(aes(x = mean(df$temp), y = 0.036, xend = mean(df$temp), yend = 0.038)) +
  geom_segment(aes(x = mean(df$temp) + (sd(df$temp)), y = 0.036, xend = mean(df$temp) + (sd(df$temp)), yend = 0.038)) +
  geom_segment(aes(x = mean(df$temp), y = 0.01, xend = mean(df$temp) + (sd(df$temp) + 4), yend = 0.01)) +
  geom_segment(aes(x = mean(df$temp), y = 0.009, xend = mean(df$temp), yend = 0.011)) +
  geom_segment(aes(x = mean(df$temp) + (sd(df$temp) + 4), y = 0.009, xend = mean(df$temp) + (sd(df$temp) + 4), yend = 0.011)) +
  geom_segment(aes(x = mean(df$temp) + 2, y = 0.037, xend = mean(df$temp) + 2, yend = 0.01), arrow = arrow()) +
  geom_text(label = "Variability", x = 7, y = 0.023, colour = "black", fontface = "bold", size = 8) +
  geom_text(label = "Change", x = 7, y = 0.019, colour = "black", fontface = "bold", size = 8) +
  xlab("Variable") +
  ylab("Frequency") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(full = "transparent", color = "black", size = 1), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 15, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.9, 0.8))

 ggsave("Figures/change_sd.png",
       width = 9,
       height = 7, 
       dpi = 600)


#concept - change in magnitude of worst case scenario
 
#scatterplot of year data

x <- seq(from = -5, to = 30, by = 0.535)
 
from <- as.Date("01-1-01 12:00:00 EDT", "%d-%m-%y")
to <- as.Date("31-12-05 12:00:00 EDT", "%d-%m-%y")
times <- seq.Date(from, to, 28)
 
t.df <- data.frame(date = times, 
                   year = year(times),
                   temp = as.numeric(rnorm(66, mean = 7, sd = 5)))
                   
tmax <- t.df %>%
  group_by(year) %>%
  summarise(max = max(temp))

ggplot() +
   geom_point(data = t.df, aes(x = date, y = temp)) +
   scale_y_continuous(expand = expansion(mult = 0)) +
   scale_x_continuous(expand = expansion(mult = 0.1)) +
   scale_x_date(date_labels = "%Y") + 
  xlab("Year") +
  ylab("Temperature") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.85, 0.8))
   
 
#evd diagram

ggplot(df, aes(x = temp)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", xlim = c((mean(df$temp)), (mean(df$temp)) + sd(df$temp)), alpha = 0.9) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#e63946", xlim = c((mean(df$temp)) + 2*sd(df$temp), 35), alpha = 0.9) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp))) +
  geom_vline(data = df, aes(xintercept = mean(df$temp), color = "mean"), linetype = "dashed", color = "black") + #mean
  geom_segment(aes(x = mean(df$temp), y = 0.01, xend = mean(df$temp) + (sd(df$temp)), yend = 0.01)) + #stdev
  geom_segment(aes(x = mean(df$temp), y = 0.009, xend = mean(df$temp), yend = 0.011)) + #stdev end
  geom_segment(aes(x = mean(df$temp) + (sd(df$temp)), y = 0.009, xend = mean(df$temp) + (sd(df$temp)), yend = 0.011)) + #stdev end
  geom_curve(data = df, aes(x = 18, y = 0.03, xend = 22, yend = 0.01), curvature = 0.1, angle = 90, color = "black", arrow = arrow()) + #shape curve arrow
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0, xend = (mean(df$temp)) + 2*sd(df$temp), yend = 0.008)) + #95th p
  geom_segment(aes(x = 27, y = 0.001, xend = 31, yend = 0.011)) + #ES label line
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0.008, xend = 30, yend = 0.02)) + #95th p label line
  geom_segment(aes(x = 19.5, y = 0.02, xend = 25, yend = 0.03)) + #shape curve label line
  geom_text(label = expression(paste("Mean ( ", mu, " )")), x = 9, y = 0.02, colour = "black") +
  geom_text(label = expression(paste("Standard Deviation ( ", sigma, " )")), x = 14, y = 0.007, colour = "black") +
  geom_text(label = expression(paste("Curve ( ", xi, " )")), x = 27, y = 0.03, colour = "black") +
  geom_text(label = "Return Period", x = 33, y = 0.02, colour = "black") +
  geom_text(label = "Expected Shortfall", x = 34, y = 0.011, colour = "black") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Variable Maxima") +
  ylab("Frequency") +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(-5, 40)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(full = "transparent", color = "black", size = 1), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.85, 0.8))

ggsave("Figures/evd.png",
       width = 9.3,
       height = 6.6, 
       dpi = 600)

#shift in percentile diagram

ggplot(df, aes(x = temp, fill = scenario)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2), geom = "area", fill = "#e63946", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2), geom = "area", fill = "#e63946", xlim = c((mean(df$temp) + 3) + 2*(sd(df$temp) + 2), 40), alpha = 0.9, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", xlim = c((mean(df$temp)) + 2*sd(df$temp), 40), alpha = 0.9, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp))) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2)) +
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0, xend = (mean(df$temp)) + 2*sd(df$temp), yend = 0.008)) + #95th p
  geom_segment(aes(x = (mean(df$temp) + 3) + 2*(sd(df$temp) + 2), y = 0, xend = (mean(df$temp) + 3) + 2*(sd(df$temp) +2), yend = 0.0062)) + #95th p
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0.003, xend = (mean(df$temp) + 3) + 2*(sd(df$temp) + 2), yend = 0.003), arrow = arrow()) +
  geom_segment(aes(x = 26, y = 0.003, xend = 30, yend = 0.02)) + #95th p
  geom_text(label = "Change in", x = 34, y = 0.026, colour = "black", size = 8, fontface = "bold") +
  geom_text(label = "Magnitude of", x = 34, y = 0.023, colour = "black", size = 8, fontface = "bold") +
  geom_text(label = "Extremes", x = 34, y = 0.02, colour = "black", size = 8, fontface = "bold") +
   scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Variable") +
  ylab("Frequency") +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(-5, 40)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(full = "transparent", color = "black", size = 1),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 15, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.9, 0.8))

ggsave("Figures/shift_95p.png",
       width = 9,
       height = 7, 
       dpi = 600)

#shift in extremes diagram

ggplot(df, aes(x = temp)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2), geom = "area", fill = "#e63946", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2), geom = "area", fill = "#e63946", xlim = c((mean(df$temp) + 3) + 2*(sd(df$temp) + 2), 40), alpha = 0.9, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", xlim = c((mean(df$temp)) + 2*sd(df$temp), 40), alpha = 0.9, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp))) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2)) +
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0, xend = (mean(df$temp)) + 2*sd(df$temp), yend = 0.008)) + #95th p
  geom_segment(aes(x = (mean(df$temp) + 3) + 2*(sd(df$temp) + 2), y = 0, xend = (mean(df$temp) + 3) + 2*(sd(df$temp) +2), yend = 0.0062)) + #95th p
  geom_segment(aes(x = 26, y = 0.001, xend = 30, yend = 0.02)) + 
  geom_segment(aes(x = 34, y = 0.001, xend = 30, yend = 0.02)) + 
  geom_text(label = "Percent Change", x = 32, y = 0.0225, colour = "black", fontface = "bold", size = 5) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Temperature") +
  ylab("Frequency") +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(-5, 40)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        c 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.85, 0.8))

ggsave("Figures/shift_ES.png",
       width = 9,
       height = 7, 
       dpi = 600)

#plots for graphical abstract ------------------------------------------------

#concept - change in mean

x <- seq(from = -5, to = 30, by = 0.05)
df <- data.frame(
  scenario = factor(rep(c("historical", "future"), each = 701)),
  temp = round(c(rnorm(x, mean = 7, sd = 5),
                 rnorm(x, mean = 15, sd = 5))))

ggplot(df, aes(x = temp, fill = scenario)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp)- 5, sd = sd(df$temp)), geom = "area", fill = "#457b9d", alpha = 0.7, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 5, sd = sd(df$temp)), geom = "area", fill = "#e63946", alpha = 0.7, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp)- 5, sd = sd(df$temp))) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 5, sd = sd(df$temp))) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  geom_vline(data = df, aes(xintercept = mean(df$temp)- 5, color = "mean"), linetype = "dashed", color = "black") +
  geom_vline(data = df, aes(xintercept = mean(df$temp) + 5, color = "mean"), linetype = "dashed", color = "black") +
  geom_segment(aes(x = mean(df$temp)- 5, y = 0.01, xend = mean(df$temp) + 5, yend = 0.01), arrow = arrow()) +
  geom_text(label = "Mean", x = -10, y = 0.053, colour = "black", fontface = "bold", size = 8) +
  geom_text(label = "Change", x = -10, y = 0.05, colour = "black", fontface = "bold", size = 8) +
  xlim(-20, 40) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.position = c(0.2, 0.7))

ggsave("Figures/change_mean_abstract.png",
       width = 9,
       height = 7, 
       dpi = 600)

#concept - change in variability

df <- data.frame(
  scenario = factor(rep(c("historical", "future"), each = 701)),
  temp = round(c(rnorm(x, mean = 7, sd = 5),
                 rnorm(x, mean = 9, sd = 8))))

ggplot(df, aes(x = temp, fill = scenario)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) - 1), geom = "area", fill = "#457b9d", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) - 1), geom = "area", fill = "#457b9d", xlim = c((mean(df$temp)), (mean(df$temp)) + sd(df$temp)), alpha = 0.9, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) + 5), geom = "area", fill = "#e63946", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) + 5), geom = "area", fill = "#e63946", xlim = c((mean(df$temp)), (mean(df$temp)) + (sd(df$temp) + 4)), alpha = 0.9, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) - 1)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp) + 5)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  geom_segment(aes(x = mean(df$temp), y = 0.037, xend = mean(df$temp) + (sd(df$temp)), yend = 0.037)) +
  geom_segment(aes(x = mean(df$temp), y = 0.036, xend = mean(df$temp), yend = 0.038)) +
  geom_segment(aes(x = mean(df$temp) + (sd(df$temp)), y = 0.036, xend = mean(df$temp) + (sd(df$temp)), yend = 0.038)) +
  geom_segment(aes(x = mean(df$temp), y = 0.01, xend = mean(df$temp) + (sd(df$temp) + 4), yend = 0.01)) +
  geom_segment(aes(x = mean(df$temp), y = 0.009, xend = mean(df$temp), yend = 0.011)) +
  geom_segment(aes(x = mean(df$temp) + (sd(df$temp) + 4), y = 0.009, xend = mean(df$temp) + (sd(df$temp) + 4), yend = 0.011)) +
  geom_segment(aes(x = mean(df$temp) + 2, y = 0.037, xend = mean(df$temp) + 2, yend = 0.01), arrow = arrow()) +
  geom_text(label = "Variability", x = -10, y = 0.063, colour = "black", fontface = "bold", size = 8) +
  geom_text(label = "Change", x = -10, y = 0.059, colour = "black", fontface = "bold", size = 8) +
  xlim(-20, 40) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.position = c(0.2, 0.7))

ggsave("Figures/change_sd_abstract.png",
       width = 9,
       height = 7, 
       dpi = 600)

#shift in percentile diagram

ggplot(df, aes(x = temp, fill = scenario)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2), geom = "area", fill = "#e63946", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2), geom = "area", fill = "#e63946", xlim = c((mean(df$temp) + 3) + 2*(sd(df$temp) + 2), 40), alpha = 0.9, aes(colour = "Future")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#457b9d", xlim = c((mean(df$temp)) + 2*sd(df$temp), 40), alpha = 0.9, aes(colour = "Historical")) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp))) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp) + 3, sd = sd(df$temp) + 2)) +
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0, xend = (mean(df$temp)) + 2*sd(df$temp), yend = 0.008)) + #95th p
  geom_segment(aes(x = (mean(df$temp) + 3) + 2*(sd(df$temp) + 2), y = 0, xend = (mean(df$temp) + 3) + 2*(sd(df$temp) +2), yend = 0.0062)) + #95th p
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0.003, xend = (mean(df$temp) + 3) + 2*(sd(df$temp) + 2), yend = 0.003), arrow = arrow()) +
  #geom_segment(aes(x = 26, y = 0.003, xend = 30, yend = 0.02)) + #95th p
  geom_text(label = "Change in", x = -10, y = 0.057, colour = "black", size = 8, fontface = "bold") +
  geom_text(label = "Magnitude of", x = -10, y = 0.053, colour = "black", size = 8, fontface = "bold") +
  geom_text(label = "Extremes", x = -10, y = 0.049, colour = "black", size = 8, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(-5, 40)) +
  xlim(-20, 40) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(), 
        legend.text = element_text(size = 15, face = "bold"),
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.position = c(0.2, 0.7))

ggsave("Figures/shift_95p_abstract.png",
       width = 9,
       height = 7, 
       dpi = 600)

#outdated -------------------------------------------------------------------------

source("PNW_Estuaries/Functions/crop.estuary.R") #function to crop data files to a single estuary
source("PNW_Estuaries/Functions/block.maxima.R") #function to extract block maxima from historical data

#read in shapefiles
estuaries <- readRDS("Data/Shapefiles/estuaries.rds")

#subset region - FRE (391), boundary bay/Nickomekl/Serpentine River Complex (218), campbell river (361) (BC), 
#drayton harbour (1001), birch bay (1002), point roberts (1000), lummi/nooksack (1003) (WA) 
border.reg <- estuaries[which(estuaries$EST_ID %in% c(391, 218,361, 1000, 1001, 1002, 1003)),]

#results data
ssp370.2041.2070 <- readRDS('Data/Results/ssp370.2041.2070.rds')

#crop data to region
results.border <- ssp370.2041.2070[ssp370.2041.2070$EST_ID %in% border.reg$EST_ID,]
results.border <- left_join(border.reg, results.border, by = "EST_ID")

#plot historical mean temp (a) ---------------------------------------------------

heat <- rev(c("#d33f6a", "#e1704c", "#e99a2c", "#e8c33c", "#e2e6bd"))

#read in files
tas <- list.files('Data/Temperature/CHELSA_monthly_timeseries_historical', pattern = ".tif", full.names = T)
tas.proj <- list.files('Data/Temperature/CHELSA_monthly_projections', pattern = ".tif", full.names = T)
bio4 <- list.files('Data/Temperature/CHELSA_seasonality_projections', pattern = ".tif", full.names = T)

tmean <- list()
tsd <- list()
tproj <- list()
tseas <- list()

for(i in 1:nrow(border.reg)){
  
  #crop to each estuary in subset
  tas.estuary <- crop.estuary(files = tas, shape = border.reg[i,])
  tas.proj.estuary <- crop.estuary(files = tas.proj, shape = border.reg[i,])
  bio4.estuary <- crop.estuary(files = bio4, shape = border.reg[i,])
  
  #extract mean temperature from historical data
  t <- app(tas.estuary, fun = "mean")
  
  #extract sd from historical data
  sd <- app(tas.estuary, fun = "sd")
  
  #save outputs
  tmean[[i]] <- mean(values(t),na.rm = TRUE)
  tsd[[i]] <- mean(values(sd), na.rm = TRUE)
  tproj[[i]] <- mean(values(tas.proj.estuary), na.rm = TRUE)
  tseas[[i]] <- mean(values(bio4.estuary), na.rm = TRUE)
  
}

tmean <- as.data.frame(tmean) %>%
  pivot_longer(cols = 1:7)
means.border <- cbind(tmean$value, border.reg)
tsd <- as.data.frame(tsd) %>%
  pivot_longer(cols = 1:7)
means.border <- cbind(tsd$value, means.border)
tproj <- as.data.frame(tproj) %>%
  pivot_longer(cols = 1:7)
means.border <- cbind(tproj$value, means.border)
tseas <- as.data.frame(tseas) %>%
  pivot_longer(cols = 1:7)
means.border <- cbind(tseas$value, means.border)
 
#plot mean temperature
ggplot(means.border, aes(fill = (as.numeric(tmean.value)/10) - 273.15)) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Mean Temperature", subtitle = "1979 - 2019") + 
  scale_fill_gradientn(colours = heat, name = "Temperature (C)") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.temp.historical.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot projected mean (b)------------------------------------------------------

ggplot(means.border, aes(fill = as.numeric(tproj.value))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Mean Future Temperature", subtitle = "2020 - 2100, all climate scenarios average") + 
  scale_fill_gradientn(colours = heat, name = "Temperature (c)") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.temp.proj.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot percent change in mean (c) ----------------------------------------------------

#results data
ssp370.2041.2070 <- readRDS('Data/Results/ssp370.2041.2070.rds')

#crop data to region
results.border <- ssp370.2041.2070[ssp370.2041.2070$EST_ID %in% border.reg$EST_ID,]
results.border <- left_join(border.reg, results.border, by = "EST_ID")

ggplot(results.border, aes(fill = as.numeric(mean.tas))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Percent Change in Temperature", subtitle = "SSP3 7.0, 2041-2070") + 
  scale_fill_gradientn(colours = heat, name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.tempchange.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot historical seasonality (d) -------------------------------------------------

var <- rev(c("#392d69", "#4e3a7f", "#624795", "#7754ac", "#8c61c2", "#a06ed8", "#b57bee"))

#plot mean sd
ggplot(means.border, aes(fill = as.numeric(tsd.value))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Mean Variation in Temperature", subtitle = "1979 - 2019") + 
  scale_fill_gradientn(colours = var, name = "Standard Deviation") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.temp.hist.sd.png",
       width = 5,
       height = 5, 
       dpi = 600)


#plot projected seasonality (e) ---------------------------------------------------

#plot mean sd
ggplot(means.border, aes(fill = as.numeric(tseas.value)/10)) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Projected Variation in Temperature", subtitle = "2020 - 2100, all climate scenarios average") + 
  scale_fill_gradientn(colours = var, name = "Standard Deviation") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.temp.seas.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot percent change in seasonality (f) ------------------------------------------

ggplot(results.border, aes(fill = as.numeric(SD.tas))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Percent Change in Variability", subtitle = "SSP3 7.0, 2041-2070") + 
  scale_fill_gradientn(colours = var, name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.seaschange.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot histograms to visualize data distribution (g) ---------------------------

fre <- readRDS('Data/Shapefiles/fraser_river_estuary.rds')
fre <- st_transform(fre, crs = st_crs("EPSG:4326"))

fre.tas <- crop.estuary(files = tas, shape = fre)

df.tas <- as.data.frame(fre.tas, xy = TRUE)
df.tas <- pivot_longer(df.tas, cols = c(3:493))

#plot distribution of fraser river only 
ggplot(df.tas, aes(x = (value/10) - 273.15, fill = after_stat(x))) +
  geom_bar(width = 0.1) +
  scale_fill_gradientn(colours = heat) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  ggtitle(label = "Recorded Temperatures in the Fraser River Estuary", subtitle = "1979 - 2019") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("fre.temp.hist.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot temperature maxima (h) ---------------------------------------------------

extremeheat <- rev(c("#461220", "#8c2f39", "#b23a48","#d33f6a", "#fcb9b2", "#fed0bb"))

#extract temperature maxima
t <- as.data.frame(minmax(fre.tas))
maxt <- t[2,]

x <- pivot_longer(maxt, cols = c(1:491))
x$year <- substr(x$name,
                 start = nchar(x$name) - nchar('yyyy_V.2.1') + 1,
                 stop = nchar(x$name) - nchar('_V.2.1'))

tas.max <- group_by(x, year) %>%
  summarise(tasmax = (max(value)/10) - 273.15) #convert units from K*10 to C

ggplot(tas.max, aes(x = tasmax, fill = after_stat(x))) +
  geom_bar(width = 0.2) +
  scale_fill_gradientn(colours = extremeheat) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  ggtitle(label = "Maximum Yearly Recorded Temperatures in the Fraser River Estuary", subtitle = "1979 - 2019") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("fre.tasmax.hist.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot distribution of future data (i) ---------------------------------------------   

fre.tas.proj <- crop.estuary(files = tas.proj, shape = fre)

df.tas.proj <- as.data.frame(fre.tas.proj, xy = TRUE)
df.tas.proj <- pivot_longer(df.tas.proj, cols = c(3:110))

ggplot(df.tas.proj, aes(x = value, fill = after_stat(x))) +
  geom_bar(width = 0.1) +
  scale_fill_gradientn(colours = heat) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  ggtitle(label = "Projected Temperatures in the Fraser River Estuary", subtitle = "2020 - 2100, all climate scenarios average") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("fre.temp.proj.png",
       width = 5,
       height = 5, 
       dpi = 600)

#plot percent change in extremes (g) ----------------------------------------------

ggplot(results.border, aes(fill = as.numeric(ES0.95.tas))) +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(color = "black", linewidth = 0.5) +
  labs(title = "Percent Change in Magnitude of Extreme Heat", subtitle = "SSP3 7.0, 2041-2070") + 
  scale_fill_gradientn(colours = extremeheat, name = "Percent Change") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("border.extremechange.ssp370.2041.2070.png",
       width = 5,
       height = 5, 
       dpi = 600)


#plot block maxima -------------------------------------------------------------

df.tas$date <- as.Date(paste(substr(df.tas$name,
                                       start = nchar(df.tas$name) - nchar('yyyy_V.2.1') + 1,
                                       stop = nchar(df.tas$name) - nchar('_V.2.1')),
                              substr(df.tas$name,
                              start = nchar(df.tas$name) - nchar('mm_yyyy_V.2.1') + 1,
                              stop = nchar(df.tas$name) - nchar('_yyyy_V.2.1')), "01", sep = "-"))

df.tas$date <- as.yearmon(df.tas$date)

tas.max$date <- as.yearmon(as.Date(paste(tas.max$year, "07", "01", sep = "-"), format = "%Y-%m-%d"))

ggplot() +
  geom_point(data = df.tas, aes(x = date, y = (value/10) - 273.15, alpha = 0.2)) +
  geom_point(data = tas.max, aes (x = date, y = tasmax, color = "#d33f6a")) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(1979, 2020, by = 1)) +
  ggtitle(label = "Recorded Temperatures in the Fraser River Estuary", subtitle = "1979 - 2019") +
  xlab("Time") +
  ylab("Temperature (C)") +
  theme_classic() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_line(color = "black", size = 0.5, linetype = 2), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Figures/Methods/block_maxima_FRE.png",
       width = 12,
       height = 6,
       dpi = 600)

 
#plot evd plot of maxima --------------------------------------------------------

ggplot(tas.max, aes(x = tasmax)) +
  geom_density(aes(fill = "#d33f6a")) +
  geom_vline(xintercept = 20, ) +
  geom_text(label = "Return Period", x = 20, y = 0.35) +
  geom_area(aes(x = stage(tasmax, after_stat = scales::oob_censor(x, c(20, 20.7)))), stat = "density") + 
  geom_text(label = "Estimated Shortfall", x = 20.2, y = 0.1, colour = "white") +
  #scale_fill_gradientn(colours = heat) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  ggtitle(label = "Maximum Yearly Recorded Temperatures in the Fraser River Estuary", subtitle = "1979 - 2019") +
  xlab("Temperature (C)") +
  ylab("Frequency") +
  theme_classic() + 
  theme(legend.position = "none")

ggsave("Figures/Methods/density_tasmax_FRE.png",
       width = 12,
       height = 5, 
       dpi = 600)


