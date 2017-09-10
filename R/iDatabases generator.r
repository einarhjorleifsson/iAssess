# -----------------------------------------------------------------------------------------------
# IDatabases generator.r
#
# iSpecies  : properties of species (names etc.)
# iStock    : properties of stocks and assessment
# iAssess   : stock assessment database (by stock, assessmentyear and year and date)
# iAdvice   : ICES advice and advice basis
# iForecast : short term forecasts
# iManage   : management actions and implementation
# iKeys     : transferring names of stocks
# 
# stocklist : stock list with stock description and advice status by assessment year - integrate into iStock
#
# 30/03/2017 first coding during HAWG
# 07/07/2017 adapted for downloading as csv file
# 14/07/2017 integrated the new and old stocknames; only data download and storage as rdata files
# 20/07/2017 thorough checking of databases and making sure all items are filled and OK
# 01/08/2017 added a number of diagnostic graphs; redone the excel database for nephrops
# 01/08/2017 added the stock assessment methods from stock advice database
# 10/08/2017 moved checking routines to separate code
# 11/08/2017 adapted for R3.4.1 and Tidyverse
# 14/08/2017 added assessmentmodel and assessmenttype categories
# 04/09/2017 updated for western horse mackerel 2017
# 06/09/2017 added automatic link to dropbox folder
# 10/09/2017 split all data into new databases: iAssess, iAdvice, iManage, iForecast, iSpecies, iStock
# -----------------------------------------------------------------------------------------------

# ICES Stock database
# library(devtools)
# devtools::install_github("ices-tools-prod/icesSD")
# devtools::install_github("ices-tools-prod/icesSAG")
library(icesSD)  # ICES Stock database
library(icesSAG)  # ICES Stock Assessment Graphs

# which functions in a package?
# lsf.str("package:icesSAG")
# lsf.str("package:icesSD")

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- get_dropbox()
setwd(paste(dropboxdir, "/ICES Assessment database", sep=""))


# -----------------------------------------------------------------------------------------
# set the year
# -----------------------------------------------------------------------------------------

myyear <- 0   # use 0 for all years

# -----------------------------------------------------------------------------------------
# read stock names (for lookup old names and new names)
# -----------------------------------------------------------------------------------------

stocknames <-
  read.csv("D:/Dropbox/ICES Assessment database/ICES StocksOldCodesNewCodesEGs.csv", stringsAsFactors = FALSE) %>% 
  select(1,2) %>% 
  setNames(., c("fishstockold","fishstock")) 

# -----------------------------------------------------------------------------------------
# get ICES stock database and create iRename object (for renaming between new and old stockkeylabels)
# -----------------------------------------------------------------------------------------

t <-
  icesSD::getSD() %>%
  lowcase %>% 
  group_by(stockkeylabel) %>% 
  filter(row_number() == 1) %>% 
  select(stockkey, stockkeylabel, stockkeydescription, previousstockkey, previousstockkeylabel) 

# write.csv(t, file="downloads/icessd_raw.csv")
# filter(t, stockkeylabel == "ang-78ab")
# filter(t, grepl("^an", stockkeylabel)) %>% View()
# range(t$stockkey, na.rm=TRUE)

# old assessment codes
told <-
  t %>% 
  filter(is.na(previousstockkey)) %>% 
  ungroup() %>% 
  select(stockkey, stockkeylabel) %>% 
  arrange(stockkey)

# filter(x, stockkeylabel == "ang-78ab")

# old stock description names
oldnames <- 
  readxl::read_excel(path="downloads/ICES old names.xlsx", col_names=TRUE, col_types="text") 

# new assessment codes
tnew <-
  t %>% 
  filter(!is.na(previousstockkey)) %>% 
  arrange(stockkey)

# create iRename dataset
iRename <-
  told %>% 
  left_join(tnew, by=c("stockkeylabel" = "previousstockkeylabel")) %>% 
  ungroup() %>% 
  mutate(stockkey = ifelse(!is.na(stockkey.y), stockkey.y, stockkey.x) ) %>% 
  select(stockkey, stockkeylabel, stockkeydescription ) %>% 
  data.frame() %>% 
  
  # bind with new assessment codes
  rbind(data.frame(select(tnew, stockkey, stockkeylabel, stockkeydescription))) %>% 
  
  # add historic names
  left_join(oldnames, by=c("stockkeylabel")) %>% 
  mutate(stockkeydescription = ifelse(!is.na(stockkeydescription.y), stockkeydescription.y, stockkeydescription.x)) %>% 
  select(stockkey, stockkeylabel, stockkeydescription ) %>% 
  
  # fix problem with missing stockkey
  mutate(stockkey = ifelse(stockkeylabel == "sal-wgc", 999999, stockkey)) %>% 
  
  # add fao code
  mutate(speciesfaocode = substr(stockkeylabel,1,3)) %>% 
  
  
  arrange(stockkeylabel, stockkey)

# filter(x, grepl("ang|ank|anf|anb", stockkeylabel)) %>% View()
# filter(told, grepl("ang|ank|anf|anb", stockkeylabel)) %>% View()

save(iRename, file="rdata/iRename.RData")

rm(t, tnew, told, oldnames)

# -----------------------------------------------------------------------------------------
# read the species database (from excel, not from ICES SAG because more information added to excel version)
# -----------------------------------------------------------------------------------------

iSpecies <-
  readxl::read_excel(path="downloads/species_list.xlsx", col_names=TRUE, col_types="text") %>%
  mutate_at(c("speciescommonname","trophicguild","fisheriesguild","sizeguild"), 
            funs(tolower)) %>%
  group_by(speciesfaocode, speciesscientificname, speciescommonname) %>%
  arrange(speciesfaocode) 

save(iSpecies, file="rdata/iSpecies.RData")
# load(file="rdata/iSpecies.RData")

# -----------------------------------------------------------------------------------------
# read ICES advice database and split up in different parts (iAdvice, iForecast, iManage, ...)
# -----------------------------------------------------------------------------------------

t <- 
  readxl::read_excel(path="../ICES advice database/ICES scientific advice database.xlsx", col_names=TRUE, col_types="text", 
                     trim_ws=TRUE) %>%
  lowcase %>% 
  rename(stockkeylabel = stockices, 
         managementyear = year,
         assessmentyear = assessyear) %>% 
  mutate_at(c("advisedlandingsmax","advisedcatchmax","tal","tac","officiallandings","iceslandings","icesindustrialbycatch",
              "icesdiscards","icescatch","fsqymin1","ssbymin1","fadvmax","fmax","f01","fmed","f35spr","flim","fpa","fmsy",
              "blim","bpa","msybtrig"), 
            funs(as.numeric)) %>%
  mutate_at(c("managementyear", "assessmentyear","firstyearofdata","ncpueseries","nsurveyseries"), 
            funs(as.integer)) %>% 
  left_join(iRename, by="stockkeylabel")
  
# filter(t, grepl("ivvi", stockkeylabel)) %>% View()

# Generate iAdvice
iAdvice <-
  t %>% 
  filter(!is.na(advicebasis)) %>% 
  filter(!is.na(stockkey)) %>% 
  select(stockkey, stockkeylabel, stockkeydescription, speciesfaocode, managementyear, advicebasis, advisedlandings, advisedcatch,
         advisedlandingsmax, advisedcatchmax) %>% 
  data.frame()

# Generate iManage
iManage <-
  t %>% 
  select(stockkey, stockkeylabel, stockkeydescription, stockarea, stocksubarea, 
         managementyear, managementauthority, taccode, tacarea, multispeciesmanagement, 
         stockmanagement, aggregateat, aggregated, 
         tal, tac, officiallandings, iceslandings, icesindustrialbycatch, icesdiscards, icescatch) %>% 
  data.frame()

# Generate iStock (part 1)
iStock_part1 <-
  t %>% 
  filter(!is.na(assessmentyear)) %>% 
  select(stockkey, stockkeylabel, 
         assessmentyear, assessmentmodel = assessmodel, expertgroup=wg, firstyearofdata, ncpueseries, nsurveyseries, 
         assessmenttype = assesstype, assessmentcomment = assesscomment, datacategory=dlscategory,
         fmax, f01, fmed, f35spr, flim, fpa, fmsy, blim, bpa, msybtrigger=msybtrig) %>% 
  data.frame()

# Generate iForecast
iForecast <-
  t %>% 
  filter(!is.na(assessmentyear)) %>% 
  left_join(iRename, by="stockkeylabel") %>% 
  filter(!is.na(stockkey)) %>% 
  select(stockkey, stockkeylabel, managementyear, fsqpreviousyear=fsqymin1, ssbpreviousyear = ssbymin1, fadvmax) %>% 
  data.frame()

rm(t)
         
# -------------------------------------------------------------------------------------------------
# Generate iStock (by stock, assessment year and date) - information on stock assessment properties
# -------------------------------------------------------------------------------------------------

# Get stock database
t <-
  icesSD::getSD() %>%
  lowcase %>% 
  select(-stockkey) %>% 
  left_join(iRename, by="stockkeylabel") %>% 
  select(stockkey, stockkeylabel, assessmentyear = activeyear, expertgroup, advicedraftinggroup, datacategory, 
         yearoflastassessment, assessmentfrequency, yearofnextassessment, assessmentmodel2 = assessmenttype, advicereleasedate, advicecategory, 
         advicetype, useofdiscardsinadvice, pabufferapplied, published, sectionnumber, assessmentkey, modifieddate)

# filter(t, grepl("ivvi", stockkeylabel)) %>% View()

# Get stock list with published/not published information
u <-
  icesSAG::getListStocks(year=0) %>% 
  lowcase %>%
  select(stockkeylabel, assessmentyear, status)  

# Combine everything in iStock object
iStock <-
  iStock_part1 %>% 
  full_join(t, by=c("stockkey","stockkeylabel","assessmentyear")) %>%
  full_join(u, by=c("stockkeylabel","assessmentyear")) %>% 
  mutate_at(c("assessmentmodel","assessmentmodel2","advicecategory","advicetype","useofdiscardsinadvice","pabufferapplied", "status"),
            funs(tolower)) %>% 
  mutate(expertgroup      = ifelse(!is.na(expertgroup.y),  expertgroup.y,  expertgroup.x),
         datacategory     = ifelse(!is.na(datacategory.y), datacategory.y, datacategory.x),
         assessmentmodel2 = ifelse(assessmentmodel2 == "na", NA, assessmentmodel2),
         
         assessmenttype   = ifelse(grepl("explo",assessmentmodel), "exploratory",assessmenttype),
         assessmenttype   = ifelse(grepl("trends",assessmentmodel), "trends",assessmenttype),
         
         assessmentmodel  = ifelse(is.na(assessmentmodel), assessmentmodel2, assessmentmodel),
         assessmentmodel  = ifelse(grepl("(xsa"   , assessmentmodel, fixed=TRUE), "xsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("sxsa"    , assessmentmodel, fixed=TRUE), "sxsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flxsa" , assessmentmodel, fixed=TRUE), "xsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(ica"   , assessmentmodel, fixed=TRUE), "ica"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flica" , assessmentmodel, fixed=TRUE), "ica"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(sam"    , assessmentmodel, fixed=TRUE), "sam"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flsam" , assessmentmodel, fixed=TRUE), "sam"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(tsa"   , assessmentmodel, fixed=TRUE), "tsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(adapt" , assessmentmodel, fixed=TRUE), "adapt" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(ss3"   , assessmentmodel, fixed=TRUE), "ss3"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(stock synthesis 3", assessmentmodel, fixed=TRUE), "ss3"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(gadget" , assessmentmodel, fixed=TRUE), "gadget", assessmentmodel),
         assessmentmodel  = ifelse(grepl("(asap"  , assessmentmodel, fixed=TRUE), "asap"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(amish"  , assessmentmodel, fixed=TRUE), "amish" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(aspic"  , assessmentmodel, fixed=TRUE), "aspic" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(mycc"   , assessmentmodel, fixed=TRUE), "mycc"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("multi-year catch curve"   , assessmentmodel, fixed=TRUE), "mycc"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(aspic"  , assessmentmodel, fixed=TRUE), "aspic" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("aarts"  , assessmentmodel, fixed=TRUE), "aarts_poos" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(cbbm"  , assessmentmodel, fixed=TRUE), "cbbm" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(scaa"  , assessmentmodel, fixed=TRUE), "scaa" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(sms"  , assessmentmodel, fixed=TRUE), "sms" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(tasacs"  , assessmentmodel, fixed=TRUE), "tasacs" , assessmentmodel) ) %>% 
         
         # NEED TO FINALIZE THIS LIST !!
  
  arrange(stockkey, assessmentyear, stockkeylabel) %>% 
  select(stockkey, stockkeylabel, 
         assessmentyear, assessmentmodel, expertgroup, advicedraftinggroup, 
         firstyearofdata, ncpueseries, nsurveyseries, 
         assessmenttype, assessmentcomment, datacategory,
         yearoflastassessment, assessmentfrequency, yearofnextassessment, advicereleasedate,
         advicetype, useofdiscardsinadvice, pabufferapplied, published, status, sectionnumber, assessmentkey, modifieddate,
         fmax, f01, fmed, f35spr, flim, fpa, fmsy, blim, bpa, msybtrigger) 

# iStock %>% 
#   group_by(assessmentmodel) %>% 
#   filter(grepl("ang", stockkeylabel, fixed=TRUE) ) %>% 
#   select(stockkey, stockkeylabel, assessmentyear, assessmentmodel, assessmenttype) %>% 
#   View()
 
# iStock %>% 
#   group_by(assessmentmodel) %>% 
#   filter(row_number() == 1) %>% 
#   select(stockkey, stockkeylabel, assessmentyear, assessmentmodel, assessmenttype) %>% 
#   write.csv(., file="downloads/assessmodel.csv")

save(iStock, file="rdata/iStock.RData")
# load(file="rdata/stockdb.RData")

rm(u,t, iStock_part1)

# -----------------------------------------------------------------------------------------
# get reference points
# -----------------------------------------------------------------------------------------

# t <-
#   getSAG(stock=NULL, year=0, data="refpts", combine=TRUE)
#   # write.csv(t, file="downloads/refpoints.csv", row.names=FALSE)
#   # write.csv(data.frame(names(t)), file="downloads/refpoints_fields.csv", row.names=FALSE)
#   # t <- read.csv(file="downloads/refpoints.csv", stringsAsFactors = FALSE)
# 
# refpoints <-   
#   t %>% 
#   lowcase %>% 
#   rename(fishstock = stockkeylabel) %>% 
#   dplyr::select(fishstock, assessmentyear,
#                 flim, blim, fmsy, msybtrigger, fpa, bpa, 
#                 fmgt=fmanagement, bmgt=bmanagement,
#                 recruitmentage) %>% 
#   mutate_at(vars(flim:bmgt), funs(as.numeric)) 

# save(refpoints, file="rdata/refpoints.RData")
load(file="rdata/refpoints.RData")


# -----------------------------------------------------------------------------------------
# Download standard graph data and combine with previous datasets
# -----------------------------------------------------------------------------------------

# sagdownload <-
#   icesSAG::getSAG(stock=NULL, year=0, data="summary", combine=TRUE) %>%
#   lowcase()
# write.csv(sagdownload, file="downloads/sagdownload.csv", row.names=FALSE)

sagdownload <- 
  read.csv(file="downloads/sagdownload.csv", stringsAsFactors =FALSE) %>% 
  lowcase %>% 
  dplyr:: select(fishstock, assessmentyear, year,
                 recruitment, highrecruitment, lowrecruitment,
                 ssb, highssb, lowssb,
                 f, highf, lowf, 
                 catches, landings, discards,
                 fage, 
                 units, 
                 stocksizedescription, stocksizeunits,
                 fishingpressuredescription, fishingpressureunits,
                 stockpublishnote) %>% 
  
  # add fao code if missing
  mutate(faocode = substr(fishstock,1,3)) %>% 
  
  # dealing with old and new stocknames
  left_join(stocknames, by = c("fishstock" = "fishstockold")) %>% 
  rename(fishstocknew = fishstock.y) %>% 
  left_join(stocknames, by = c("fishstock")) %>% 
  mutate(fishstocknew = ifelse(is.na(fishstocknew) & !is.na(fishstockold), fishstock, fishstocknew),
         fishstockold = ifelse(is.na(fishstockold) & !is.na(fishstocknew), fishstock, fishstockold),
         fishstockold = ifelse(is.na(fishstockold) & is.na(fishstocknew) , fishstock, fishstockold)) %>% 
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription","fishingpressureunits"), funs(tolower)) %>% 
  
  
  # join relevant datasets
  left_join(refpoints, by=c("fishstock","assessmentyear")) %>% 
  left_join(stocklist, by=c("fishstock","assessmentyear")) %>% 
  left_join(stockdb  , by=c("fishstock","assessmentyear")) %>% 
  left_join(speciesdb, by=c("faocode")) %>% 
  left_join(iad,       by=c("fishstock","assessmentyear")) %>% 
  
  mutate(source   = "download", 
         flim     = ifelse(!is.na(flim.x), flim.x, flim.y),
         fpa      = ifelse(!is.na(fpa.x),  fpa.x, fpa.y),
         fmsy     = ifelse(!is.na(fmsy.x), fmsy.x, fmsy.y),
         blim     = ifelse(is.na(blim.x),  blim.x, blim.y),
         bpa      = ifelse(is.na(bpa.x),   bpa.x, bpa.y),
         msybtrig = ifelse(is.na(msybtrigger),  msybtrigger, msybtrig),
         assessmentmodel = ifelse(!is.na(assessmentmodel.y), assessmentmodel.y, assessmentmodel.x) ) %>% 
  
  select(-flim.x, -flim.y, -fpa.x, -fpa.y, -fmsy.x, -fmsy.y, -blim.x, -blim.y, -bpa.x, -bpa.y, -msybtrigger, 
         -assessmentmodel.x, -assessmentmodel.y) %>% 
  
  data.frame() 

# Extract list of fishstock and assessment years from SAG database
sagdownload_unique <-
  sagdownload %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  select(fishstock, assessmentyear)

# -----------------------------------------------------------------------------------------
# read old excel SAG database
# -----------------------------------------------------------------------------------------

sagexcel <-
  readxl::read_excel("ICES Assessment Summary database.xlsx",
             sheet = "DATA",
             col_names = TRUE,
             col_types = "text",
             skip = 0) %>%
  lowcase %>%
  rename(assessmentyear = assyear) %>% 
  mutate(fishstock = tolower(fishstock)) %>%
  mutate_at(vars("year","assessmentyear"), funs(as.integer)) %>% 
  mutate_at(vars("lowrecruitment", "recruitment","highrecruitment",
                 "lowssb","ssb","highssb",
                 "lowf", "f","highf",
                 "landings","catches","discards","ibc",
                 "flim","fpa","fmsy", "fmanagement",
                 "blim","bpa","msybtrigger","bmanagement",
                 "recruitmentage","recruitmentlength"), funs(as.numeric)) %>% 
  select(-contains("custom")) %>% 
  rename(scientificname = speciesname,
         commonname     = sgname,
         fmgt           = fmanagement,
         bmgt           = bmanagement,
         faocode        = species) %>% 
  
  # dealing with old and new stocknames
  left_join(stocknames, by = c("fishstock" = "fishstockold")) %>% 
  rename(fishstocknew = fishstock.y) %>% 
  left_join(stocknames, by = c("fishstock")) %>% 
  mutate(fishstocknew = ifelse(is.na(fishstocknew) & !is.na(fishstockold), fishstock, fishstocknew),
         fishstockold = ifelse(is.na(fishstockold) & !is.na(fishstocknew), fishstock, fishstockold),
         fishstockold = ifelse(is.na(fishstockold) & is.na(fishstocknew) , fishstock, fishstockold)) %>% 
  
  
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription","fishingpressureunits",
                 "commonname", "scientificname"), funs(tolower)) %>% 
  select(-assessmentkey, -icesareas, -report, -(unitofrecruitment:tbiomassunit), 
         -catcheslandingsunits, -officiallandings, -(ibc:yieldssb), -(flandings:funallocated), 
         -(flength:area), -scientificname, -commonname) %>% 
  filter(year <= assessmentyear) %>% 
  
  # add fao code if missing
  mutate(faocode = substr(fishstock,1,3)) %>% 
  
  # add species information
  left_join(speciesdb, by=c("faocode")) %>% 
  
  # add ICES Advice Database information
  select(-blim, -bpa, -msybtrigger,-flim, -fpa, -fmsy) %>% 
  left_join(iad, by=c("fishstock","assessmentyear")) %>% 
  
  mutate(source = "excel") 
  

# Extract list of fishstock and assessment years from old database
sagexcel_unique <-
  sagexcel %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  select(fishstock, assessmentyear)

# sagexcel_overview <-
#   sagexcel %>% 
#   group_by(fishstock, assessmentyear) %>% 
#   summarise(nssb = sum(!is.na(ssb)),
#          nf   = sum(!is.na(f)),
#          nrec = sum(!is.na(recruitment)),
#          nlan = sum(!is.na(landings))) %>% 
#   select(fishstock, assessmentyear, nlan, nrec, nssb, nf)
# 
# write.csv(sagexcel_overview, file="excel_overview.csv")



# -----------------------------------------------------------------------------------------
# Construct the excel sag dataset to add to the downloaded SAG dataset, 
# i.e. unique combinations of fishstock and assessmentyear
# -----------------------------------------------------------------------------------------

sagexcel_toadd <-
  setdiff(sagexcel_unique,sagdownload_unique) %>% 
  left_join(sagexcel, by=c("fishstock","assessmentyear")) %>% 
  data.frame()

# setdiff(names(sagexcel),names(sagdownload)) 
# setdiff(names(sagdownload),names(sagexcel)) 

# -----------------------------------------------------------------------------------------
# Add the unique data in the excel sag dataset to the downloaded sag dataset and do cleaning up
# -----------------------------------------------------------------------------------------

sagdb <- 
  rbind.all.columns(sagdownload, sagexcel_toadd) %>%  

  # corrections to stock size descriptions
  mutate(
    stocksizedescription = gsub("stock size: "                 ,""                   , stocksizedescription),
    stocksizedescription = gsub("indices"                      ,"index"              , stocksizedescription),
    stocksizedescription = gsub("indicator"                    ,"index"              , stocksizedescription),
    stocksizedescription = gsub("^biomass$"                    ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^biomass index$"              ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^evhoe biomass index$"        ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^stock size index: abundance$","abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^abundance$"                  ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^density$"                    ,"density index"      , stocksizedescription),
    stocksizedescription = gsub("^tsb$"                        ,"total biomass"      , stocksizedescription),
    stocksizedescription = gsub("^total abundance index$"      ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^index$"                      ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^stock abundance$"            ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^density index$"              ,"abundance index"    , stocksizedescription),
    
    stocksizedescription = gsub("stock size index: biomass \\(ages 1-8\\)"   ,"total biomass index"      , stocksizedescription),
    stocksizedescription = gsub("stock size index: german survey"            ,"total biomass index"      , stocksizedescription),
    stocksizedescription = gsub("stock size index: smoothed greenland index" ,"total biomass index"      , stocksizedescription),
    
    stocksizedescription = ifelse(grepl("tv",stocksizedescription), "abundance index", stocksizedescription),
    stocksizedescription = ifelse(grepl("ssb & b",stocksizedescription) , "ssb", stocksizedescription),
    stocksizedescription = ifelse(grepl("total biomass/bmsy",stocksizedescription) , "b/bmsy", stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "stock size" & stocksizeunits == "tonnes", "ssb", stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "stock size" & grepl("kg/",stocksizeunits), "total biomass index", stocksizedescription),
    stocksizedescription = ifelse(grepl("relative", stocksizeunits, fixed=TRUE) & is.na(stocksizedescription), "total biomass index", stocksizedescription)
  ) %>% 
  
  # corrections to stock units
  mutate(
    stocksizeunits = gsub("stock size: "                 ,""                   , stocksizeunits),
    stocksizeunits = gsub(" ", "", stocksizeunits),
    stocksizeunits = gsub("density(burrows/m2)", "burrows/m2", stocksizeunits, fixed=TRUE),
    stocksizeunits = gsub("cpue(kg/1000hooks)", "kg/1000hooks", stocksizeunits, fixed=TRUE),
    stocksizeunits = gsub("^abundance$", "millions", stocksizeunits),
    stocksizeunits = gsub("na(ratio)", "relative", stocksizeunits, fixed=TRUE),
    stocksizeunits = ifelse(grepl("kg/h", stocksizeunits, fixed=TRUE), "kg/hour", stocksizeunits),
    stocksizeunits = ifelse(grepl("n/h", stocksizeunits, fixed=TRUE), "n/hour", stocksizeunits)
  ) %>% 
  
  # corrections to fishing pressure descriptions
  mutate(
    fishingpressuredescription = gsub("fishing pressure: ",""  , fishingpressuredescription),
    fishingpressuredescription = gsub(" ",""  , fishingpressuredescription),
    fishingpressuredescription = gsub("f&hr","f"  , fishingpressuredescription, fixed=TRUE),
    fishingpressuredescription = gsub("fishingpressure","f"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("finwinterrings","f"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("weightedf","f"  , fishingpressuredescription), 
    
    fishingpressuredescription = gsub("harvestrate","hr"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("relativehr","hr/index"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("hrindex","hr/index"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("relativeexploitationrate","hr/index"  , fishingpressuredescription), 
    
    fishingpressuredescription = ifelse(grepl("ages",fishingpressuredescription), "f", fishingpressuredescription),
    fishingpressuredescription = ifelse(grepl("null",fishingpressuredescription), NA, fishingpressuredescription),
    
    fishingpressureunits       = ifelse(grepl("relative",fishingpressuredescription) & is.na(fishingpressureunits), "relative", fishingpressureunits ),
    fishingpressuredescription = ifelse(grepl("relative",fishingpressuredescription) , "fproxy", fishingpressuredescription )
  ) %>% 
  

  # corrections to fishing pressure units
  mutate(
    fishingpressureunits = gsub(" ",""  , fishingpressureunits),
    fishingpressureunits = gsub("peryear","year-1"  , fishingpressureunits),
    fishingpressureunits = gsub("%","percent"  , fishingpressureunits),
    fishingpressureunits = gsub("^f$","year-1"  , fishingpressureunits),
    fishingpressureunits = gsub("^catch/biomass$","relative"  , fishingpressureunits),

    fishingpressureunits = ifelse(grepl("cm",fishingpressureunits), "year-1",fishingpressureunits) ,
    fishingpressureunits = ifelse(grepl("null",fishingpressureunits), NA,fishingpressureunits) ,
    fishingpressureunits = ifelse(grepl("ratio",fishingpressureunits), "relative",fishingpressureunits) 
  ) %>% 
  
  # Add assessment type2 category (assess, bench, old, alt, explore)
  # CHECK: Should I remove the labels from the fishstock variable  ??
  mutate(
    assessmenttype2 = ifelse(grepl("-bench$" , fishstock), "bench", "assess"),
    assessmenttype2 = ifelse(grepl("-old$"   , fishstock), "old"  , assessmenttype2),
    assessmenttype2 = ifelse(grepl("-alt$"   , fishstock), "alt"  , assessmenttype2)
  ) %>% 
  
  # correction due to missing units
  mutate(
    stocksizeunits       = ifelse(stocksizedescription=="b/bmsy" & is.na(stocksizeunits),"relative",stocksizeunits),  
    fishingpressureunits = ifelse(fishingpressuredescription=="f/fmsy" & is.na(fishingpressureunits),"relative",fishingpressureunits)
  ) %>% 
  
  # corrections to the assignments of specific stocks and years
  mutate(
    stocksizedescription       = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"b/bmsy"  ,stocksizedescription),
    stocksizeunits             = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"relative",stocksizeunits),
    fishingpressuredescription = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"f/fmsy"  ,fishingpressuredescription),
    fishingpressureunits       = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"relative",fishingpressureunits)
  ) %>% 
  
  # remove double series (e.g. mac-nea 2013 is twice in the sag download)
  group_by(fishstock, assessmentyear, year) %>% 
  filter(row_number() == 1) %>% 
  
  # convert to lowercase
  mutate_at(vars(assessmenttype), funs(tolower)) %>% 
  
  # remove empty variables
  select(-recruitmentlength)


save(sagdb, file="rdata/sagdb.RData")
# load(file="rdata/sagdb.RData")



