# -----------------------------------------------------------------------------------------------
# iAssess figure all assessments per stock
#
# 25/09/2017 taken from ices SAG data plot
# -----------------------------------------------------------------------------------------------

setwd("D:/Dropbox/ICES Assessment database")

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours

# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

# Load dataset
load(file="rdata/sagdb.RData")

# ---------------------------------------------------------------------------------------------
# plots of assessments by assessment year and stock and fisheries guild (for a single variable)
# ---------------------------------------------------------------------------------------------

# To do: convert this into a function that can plot the different variables. I have struggled with this a lot but 
# without success. 1/8/2017

x <-
  sagdb %>% 
  filter(assessmentyear >= 1990) %>% 
  filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
  filter(fisheriesguild %in% c("crustacean","elasmobranch","pelagic","demersal","benthic")) %>%
  mutate(fishstockold = ifelse(is.na(fishstockold)|fishstockold=="", fishstocknew, fishstockold)) %>% 
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  data.frame() %>% 
  mutate(fishstockold = factor(fishstockold), 
         fishstockold = factor(fishstockold, levels = rev(levels(fishstockold)))) %>% 
  
  # here is the bit to add the colour variable
  mutate(source = factor(source))

# define colour scale
myColors        <- brewer.pal(length(levels(x$source)),"Set1")
names(myColors) <- levels(x$source)

p1 <-
  filter(x, fisheriesguild %in% c("crustacean","elasmobranch")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = " ", y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2a <-
  filter(x, fisheriesguild %in% c("pelagic")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = NULL, y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2b <-
  filter(x, fisheriesguild %in% c("benthic")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2 <- plot_grid(p2a + 
                  theme(legend.position="none", 
                        axis.title.y=element_blank(), 
                        axis.text.x=element_blank(), 
                        plot.margin = unit(c(10, 5, 1, 5), "mm")), 
                p2b + 
                  theme(legend.position="none", 
                        axis.title.y=element_blank(), 
                        plot.margin = unit(c(1, 5, 5, 5), "mm")),
                ncol=1,align="v",
                rel_heights = c(4.7, 2))

p3 <-
  filter(x, fisheriesguild %in% c("demersal")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(y = "fishstock", x=" " ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE)

plot_grid(p1 + theme(legend.position="none") + theme(axis.title.y=element_blank()), 
          p2 ,
          p3 ,
          ncol=3,
          align = 'v', rel_widths = c(1,1,1.2))

# unique(sagdb$fisheriesguild)
# unique(x$stockpublishnote)
# unique(x$assessmentcat)
# unique(x$status)
