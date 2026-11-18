##'File name: Methods_figures
##'Author: Rekha Marcus
##'Thesis: Chapter 1
##'
##'This script includes code used to generate conceptual figures 2 and 3 for Chapter 1
##'of the thesis, as well as the figures for the graphical abstract of Chapter 1,
##'which demonstrate changes in the shape of distributions as well as the construction 
##'of an extreme value distribution using the block maxima approach.

#load packages ----------------------------------------------------------------

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

#Figure 2 ---------------------------------------------------------------------
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
  geom_text(label = "Mean Change", x = 8, y = 0.014, colour = "black", fontface = "bold", size = 5) +
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
  geom_text(label = "Variability \nChange", x = 5, y = 0.023, colour = "black", fontface = "bold", size = 5) +
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
 
 #concept - change in extremes
 
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
   geom_text(label = "Change in Extremes", x = 32, y = 0.0225, colour = "black", fontface = "bold", size = 5) +
   scale_y_continuous(expand = expansion(mult = 0)) +
   xlab("Variable") +
   ylab("Frequency") +
   scale_x_continuous(expand = expansion(mult = 0), limits = c(-5, 40)) +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
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
         legend.position = c(0.85, 0.8))
 
 ggsave("Figures/shift_ES.png",
        width = 9,
        height = 7, 
        dpi = 600)
 
#Figure 3 ---------------------------------------------------------------------
 
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

tmax <- t.df[t.df$temp %in% tmax$max,]

ggplot() +
  geom_vline(data = t.df, aes(xintercept = as.Date("01-1-02 12:00:00 EDT", "%d-%m-%y")), linetype = "dashed", color = "gray") + 
  geom_vline(data = t.df, aes(xintercept = as.Date("01-1-03 12:00:00 EDT", "%d-%m-%y")), linetype = "dashed", color = "gray") + 
  geom_vline(data = t.df, aes(xintercept = as.Date("01-1-04 12:00:00 EDT", "%d-%m-%y")), linetype = "dashed", color = "gray") + 
  geom_vline(data = t.df, aes(xintercept = as.Date("01-1-05 12:00:00 EDT", "%d-%m-%y")), linetype = "dashed", color = "gray") + 
  geom_point(data = t.df, aes(x = date, y = temp), size = 2) +
  geom_point(data = tmax, aes(x = date, y = temp), size = 3, color = "#e63946") +
  xlab("Year") +
  ylab("Variable") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.85, 0.8))

ggsave("Figures/year_BM_scatterplot.png",
       width = 9.3,
       height = 6.6, 
       dpi = 600)
 
#evd diagram

ggplot(df, aes(x = temp)) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#e63946", alpha = 0.6) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#e63946", xlim = c((mean(df$temp)), (mean(df$temp)) + sd(df$temp)), alpha = 0.9) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp)), geom = "area", fill = "#e63946", xlim = c((mean(df$temp)) + 2*sd(df$temp), 35), alpha = 0.9) +
  stat_function(fun = dnorm, args = c(mean = mean(df$temp), sd = sd(df$temp))) +
  geom_vline(data = df, aes(xintercept = mean(df$temp), color = "mean"), linetype = "dashed", color = "black") + #mean
  geom_segment(aes(x = mean(df$temp), y = 0.01, xend = mean(df$temp) + (sd(df$temp)), yend = 0.01)) + #stdev
  geom_segment(aes(x = mean(df$temp), y = 0.009, xend = mean(df$temp), yend = 0.011)) + #stdev end
  geom_segment(aes(x = mean(df$temp) + (sd(df$temp)), y = 0.009, xend = mean(df$temp) + (sd(df$temp)), yend = 0.011)) + #stdev end
  geom_curve(data = df, aes(x = 18, y = 0.03, xend = 22, yend = 0.01), curvature = 0.1, angle = 90, color = "black", arrow = arrow()) + #shape curve arrow
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0, xend = (mean(df$temp)) + 2*sd(df$temp), yend = 0.008)) + #95th p
  geom_segment(aes(x = 25, y = 0.001, xend = 31, yend = 0.011)) + #ES label line
  geom_segment(aes(x = (mean(df$temp)) + 2*sd(df$temp), y = 0.008, xend = 30, yend = 0.02)) + #95th p label line
  geom_segment(aes(x = 19.5, y = 0.02, xend = 25, yend = 0.03)) + #shape curve label line
  geom_text(label = expression(paste("Mean ( ", mu, " )")), x = 9, y = 0.02, colour = "black", size = 7) +
  geom_text(label = expression(paste("Standard Deviation ( ", sigma, " )")), x = 14, y = 0.007, colour = "black", size = 7) +
  geom_text(label = expression(paste("Curve ( ", xi, " )")), x = 28.5, y = 0.03, colour = "black", size = 7) +
  geom_text(label = "Return Period", x = 33, y = 0.021, colour = "black", size = 7) +
  geom_text(label = "Expected Shortfall", x = 34, y = 0.013, colour = "black", size = 7) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  xlab("Variable Maxima") +
  ylab("Frequency") +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(-5, 40)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", color = "black", size = 1), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.85, 0.8))

ggsave("Figures/evd.png",
       width = 9.3,
       height = 6.6, 
       dpi = 600)

#Graphical abstract -----------------------------------------------------------

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