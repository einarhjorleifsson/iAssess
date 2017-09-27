# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph plotting
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- get_dropbox()
setwd(paste(dropboxdir, "/ICES Assessment database", sep=""))

# load the data
load(file="rdata/sagdb.RData")


# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

d <-
  sagdb %>% 
  # filter(fishstocknew %in% c("her-noss", "whb-comb","mac-nea","hom-west"),
  # filter(fishstockold %in% c("mac-nea","mac-nea-bench","mac-nea-old")) %>% 
  # filter(fishstockold %in% c("hom-west","hom-west-bench")) %>% 
  
  # filter(grepl("hom-west", fishstockold) ) %>% 
  filter(grepl("mac-nea", fishstockold) ) %>% 
  # filter(grepl("whb", fishstockold) ) %>% 
  # filter(grepl("noss", fishstockold) ) %>% 
  
  # filter(fishstocknew %in% c("her-47d3"),
  # filter(grepl("hom.27.2a", fishstocknew)) %>% 
  # filter(grepl("her.27.3a", fishstocknew)) %>% 
  ungroup() %>% 
  filter(year             >  1980, 
         assessmentyear   >  2003,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, fishstock, fishstockold, fishstocknew, 
         recruitment:lowrecruitment, f:lowf, ssb:lowssb,
         assessmenttype2, assessmentmodel) %>% 
  mutate(assessmenttype2 = ifelse(assessmentyear == max(assessmentyear),"last",assessmenttype2)) %>% 
  mutate(fishstock = gsub("-bench","",fishstock, fixed=TRUE),
         fishstock = gsub("-old","",fishstock, fixed=TRUE)) %>% 
  mutate(tyear     = ifelse(assessmenttype2 == "assess", as.character(assessmentyear), NA),
         tyear     = ifelse(assessmenttype2 == "last", paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(assessmenttype2 == "old", paste(assessmentyear,"-O",sep="") ,tyear),
         tyear     = ifelse(assessmenttype2 == "bench", paste(assessmentyear,"-B",sep="") ,tyear)) %>% 
  data.frame()

# get the last assessment year and stock name
lastyear        <- unique(unlist(select(filter(d, assessmenttype2=="last"), assessmentyear)))
last <-
  d %>% 
  filter(assessmenttype2 == "last") %>% 
  select(fishstock, year, lastssb = ssb, lastf=f, lastr = recruitment)

# scale to last year ?
d <-
  d %>% 
  left_join(last, by=c("fishstock","year")) %>% 
  mutate(recruitment = recruitment/lastr,
         ssb         = ssb/lastssb,
         f           = f / lastf)


# plot ssb
p1 <-
  d %>% 
  filter(!is.na(ssb)) %>%  
  
  ggplot(aes(year,ssb, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype2, size=assessmenttype2, linetype=assessmenttype2) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype2), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  


# plot f
p2 <-
  d %>% 
  filter(!is.na(f)) %>%  
  
  ggplot(aes(year,f, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype2, size=assessmenttype2, linetype=assessmenttype2) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype2), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "F")   +
  facet_grid(fishstock ~ .)

plot_grid(p1 + theme(legend.position = "none", axis.title      = element_blank()), 
          p2 + theme(axis.title      = element_blank()),
          ncol=2, align = 'h', rel_widths = c(3,3))
