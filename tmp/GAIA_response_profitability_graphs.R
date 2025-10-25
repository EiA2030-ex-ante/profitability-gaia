
# illustrate profitability of lime investments using graphed responses and inverse price ratio

library(tidyverse)
library(data.table)



# response estimates 
mydat <- read.csv("tmp/predicted_means_allcrops_soils_jc.csv")

mydat <- mydat %>%
  filter(crop=="maize")
# sort ascending lime treatments per site
mydat <- mydat[order(mydat$col, mydat$lime_tha),]
mydat


mydat <- mydat %>%
  group_by(site, crop) %>%
  mutate(cyield = emmean[lime_tha == 0],
         ydif = emmean - cyield) %>%
  ungroup() 

# # if you didn't already have control yields defined for all rows of each group, you could calculate as follows:
# # calculate yield effects relative to baseline
# mydat$refyld <- NA
# mydat[mydat$lime_tha==0.0,]$refyld <- mydat[mydat$lime_tha==0.0,]$emmean
# 
# dt <- as.data.table(mydat)
# # Calculate the minimum value of 'tmp' for each group defined in 'site'
# #dt[, .(MinValue = min(tmp, na.rm = TRUE)), by = site]
# dt[, refyld := min(refyld, na.rm = TRUE), by = site]


# calculate the difference between each treatment yield and the control for that group
#mydat$ydif <- mydat$emmean - mydat$cyield

mydat$x <- mydat$lime_tha
mydat$y <- mydat$ydif


#define prices 
## maize USD/MT	
p_m <- 150	
## lime USD/MT	
p_l <- 100	
## ratio p_m/p_l
pratio <- p_m / p_l


fun1 <- function(x) (y = x/pratio)

# x <- 0:10
# y <- fun1(x)
# df.pr <- data.frame(x,y)
x <- 0:8
y <- fun1(x)
df.pr <- data.frame(x,y)


ggplot(mydat, aes(x, y), ylim(0, 3), xlim(0, 8)) +
  geom_line(data = mydat[mydat$site=="Jimma",], col = "green4") + geom_point(data = mydat[mydat$site=="Jimma",], col = "green4", size=3) +
  geom_line(data = mydat[mydat$site=="Mbozi",], col = "blue") + geom_point(data = mydat[mydat$site=="Mbozi",], col = "blue", size=3) +
  geom_line(data = mydat[mydat$site=="Geita",], col = "orange") + geom_point(data = mydat[mydat$site=="Geita",], col = "orange", size=3) +
  geom_line(data = mydat[mydat$site=="Nyaruguru",], col = "yellow") + geom_point(data = mydat[mydat$site=="Nyaruguru",], col = "yellow", size=3) +
  geom_line(data = mydat[mydat$site=="Ngororero",], col = "purple") + geom_point(data = mydat[mydat$site=="Ngororero",], col = "purple", size=3) +
  geom_line(data = mydat[mydat$site=="Burera",], col = "brown") + geom_point(data = mydat[mydat$site=="Burera",], col = "brown", size=3) +
  geom_ribbon(data = df.pr, ymin=-Inf, aes(ymax=y), fill='red', alpha=0.2) +
  geom_ribbon(data = df.pr, aes(ymin=y), ymax=Inf, fill='green', alpha=0.2) +
  coord_cartesian(ylim = c(0,3), xlim = c(0, 8)) +
  ylab("Yield response attributed to liming (MT/HA)") + xlab("Lime application rate (MT/HA)") +
  geom_text(data=annotation, aes( x=l1, y=l2, label=label), 
            color="white", 
            size=3 , angle=0, fontface="bold" )

plt_1 <- ggplot(mydat, aes(x, y, color = site)) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(data = df.pr, aes(x = x, ymin = -Inf, ymax = y), fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  geom_ribbon(data = df.pr, aes(x = x, ymin = y, ymax = Inf), fill = "green", alpha = 0.2, inherit.aes = FALSE) +
  scale_color_viridis_d(option = "A") +
  #coord_cartesian(xlim = c(0, 8), ylim = c(0, 3)) +
  labs(
    x = "Lime application rate (MT/HA)",
    y = "Yield response attributed to liming (MT/HA)",
    color = "Site"
  ) +
  theme_minimal()

plt_1

plotly::ggplotly(plt_1)

