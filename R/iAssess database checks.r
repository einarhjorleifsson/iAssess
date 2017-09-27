# -----------------------------------------------------------------------------------------------
# ICES SAG checker
#
# 10/08/2017 created code from ICES SAG download.r
# 25/09/2017 cleaned up; only checking code left
# -----------------------------------------------------------------------------------------------

setwd("D:/Dropbox/ICES Assessment database")

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots

# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

load(file="rdata/sagdb.RData")

# filter on methods
sagdb %>% 
  filter(assessmentmodel %in% c("xsa","ica","vpa","ls","sam","xsm", "tsa", "trends only", "surba","sms")) %>% 
  View()

# check assessmentypes used
sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number() == 1) %>% 
  filter(!is.na(assesstype)) %>% 
  select(fishstock, assessmentyear, stockpublishnote, status, assesstype) %>% 
  View()

# check stocksizedescriptions used
sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number() == 1) %>% 
  group_by(stocksizedescription) %>% 
  summarise(n = n()) %>% 
  arrange(stocksizedescription) %>% 
  View()

filter(sagdb, is.na(commonname)) %>% View()

sort(unique(sagdb$stocksizeunits))
filter(sagdb, is.na(fishstocknew)) %>% View()
filter(sagdb, is.na(commonname)) %>% View()
filter(sagdb, is.na(stocksizeunits)) %>% View()
filter(sagdb, stocksizedescription == "stock size" & stocksizeunits == "tonnes") %>% View()
filter(sagdb, grepl("ratio", stocksizeunits)) %>% View()

filter(sagdownload, grepl("cod-arct", fishstockold)) %>% View()
filter(sagexcel, grepl("cod-arct", fishstockold)) %>% View()
filter(sagexcel_toadd, grepl("cod-arct", fishstockold)) %>% View()
filter(sagdb, grepl("cod-arct", fishstockold)) %>% group_by(assessmentyear) %>% filter(row_number()==1) %>% 
  arrange(assessmentyear) %>% View()

filter(sagdb, grepl("mac-nea", fishstockold), assessmentyear==2013) %>% View()
filter(sagexcel_toadd, grepl("her-47d3", fishstock), assessmentyear==2017) %>% View()
filter(sagdownload, grepl("mac-nea", fishstock), assessmentyear==2013) %>% View()

