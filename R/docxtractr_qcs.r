# --------------------------------------------------------------------------------------------
# Read word tables
#
# 05/10/2017 coded up the reading in of all data files; datafiles converted to docx format; all superscripts removed prior to reading
# 05/10/2017 converted to iAssess github
# --------------------------------------------------------------------------------------------

library(docxtractr)
library(tidyverse)
library(stringr)

# make list of filenames
file.list <- c(list.files(path       = "data/qcs-docs-docx/", 
                          pattern    = "docx",
                          recursive  = TRUE, 
                          full.names = TRUE,   
                          ignore.case= TRUE) )

# read files from filename list
# i <- 47
# j <- 1
for (i in 1:length(file.list)) {
  
  # extract stock name
  stock <- gsub(".qcs.docx","", file.list[i], fixed=TRUE)
  stock <- gsub("((?:[^/]*/)*)(.*)","\\2", stock)
  
  print(paste0(i,stock,file.list[i],sep=" - "))
  
  # set docx object
  docx <- read_docx(path=file.list[i])
  # docx_tbl_count(docx)
  # docx_describe_tbls(docx)
  
  # read tables
  tmp <- docx_extract_all_tbls(docx, guess_header = FALSE, trim = TRUE)  
  
  # convert tables to data frames
  for (j in 1:length(tmp)) {
    
    # print(j)
    
    t       <- tmp[j] %>%  as.data.frame()
    longvar <- t[1,1]
    nc      <- ncol(t)
    nr      <- nrow(t)
    head    <- t[3,2:nc]
    rows    <- t[4:nr,1]  %>% data.frame()
    names(rows) <- "assessmentyear"
    t <- 
      t[4:nr,2:nc] %>% 
      setNames(head) %>% 
      cbind(rows) %>% 
      gather(key=year, value=value, 1:(nc-1)) %>% 
      mutate(stock          = tolower(stock), 
             longvar        = longvar, 
             value          = as.numeric(gsub("\\s+","",value)),
             assessmentyear = substr(as.character(assessmentyear), 1,4),
             assessmentyear = as.integer(assessmentyear)) %>% 
      filter(!is.na(value), value != "")
    
    if (j == 1) { data <- t
    } else      { data <- rbind(data,t) }
    
  } # end of j for loop
  
  if (i == 1) { qcsdata <- data
  } else      { qcsdata <- rbind(qcsdata,data) }
  
} #end of i for loop  


# check and convert TO BE DONE
t <-
  qcsdata %>% 
  mutate(
    year    = as.integer(year),
    stock   = gsub(".qcs.docx","",stock), 
    stock   = gsub("anb-89","anb-8c9a", stock),
    stock   = gsub("anp-89","anp-8c9a", stock),
    stock   = gsub("cod-coast","cod-coas", stock),
    stock   = gsub("had-icel","had-iceg", stock),
    stock   = gsub("had-irisde3","had-iris", stock),
    stock   = gsub("her-2532excgor","her-2532-gor", stock),
    stock   = gsub("mac-wes","mac-nea", stock),
    
    
    longvar = tolower(longvar),
    longvar = gsub("  "," ", longvar), 
    var     = ifelse(grepl("average f"  , longvar), "f"  , NA),
    var     = ifelse(grepl("spawning"   , longvar), "ssb", var),
    var     = ifelse(grepl("recruitment", longvar), "r"  , var),
    var     = ifelse(grepl("fishable"   , longvar), "fb" , var),
    
    unit    = ifelse(var=="f"                                  , "year-1"                            , NA),
    unit    = ifelse(var=="ssb" & grepl("\\(.+\\)$"  , longvar), gsub(".+\\((.+)\\)$","\\1", longvar), unit),
    unit    = ifelse(var=="fb"  & grepl("\\(.+\\)$"  , longvar), gsub(".+\\((.+)\\)$","\\1", longvar), unit),
    unit    = ifelse(             grepl("unit: (.+)$", longvar), gsub(".+unit: (.+)$","\\1", longvar), unit),
    unit    = ifelse(var=="r" & stock=="cod-2532"              , "thousands"                         , unit),  
    unit    = ifelse(var=="r" & stock=="cod-iceg"              , "millions"                          , unit),  
    unit    = ifelse(var=="r" & stock=="ple-celt"              , "thousands"                         , unit),  
    unit    = ifelse(var=="r" & stock=="hke-soth"              , "check"                             , unit),
    
    unit    = gsub("[^[:alnum:]]", "", unit), 
    unit    = gsub("^000s$|^000$","thousands", unit),
    unit    = gsub("^000 000s$|000000s","millions" , unit),
    unit    = gsub("^000t$|^000stonnes$|^000tonnes$","thousand tonnes" , unit),
    unit    = gsub("000million$","billions" , unit),
    unit    = gsub("^t$", "tonnes",unit),
    
    
    age     = ifelse(var=="f" & grepl("\\(.+\\)$", longvar), gsub(".+\\((.+)\\)$","\\1", longvar), NA),
    age     = gsub(" +", "", age), 
    age     = gsub(",,u", "", age), 
    age     = gsub(",u", "", age), 
    age     = gsub(",w", "", age), 
    age     = ifelse(var=="f" & stock=="anb-78ab" & assessmentyear <= 2000, "4-8" , age),
    age     = ifelse(var=="f" & stock=="anb-78ab" & assessmentyear  > 2000, "6-10", age),
    age     = ifelse(var=="f" & stock=="anp-78ab" & assessmentyear <= 2000, "3-7", age),
    age     = ifelse(var=="f" & stock=="anp-78ab" & assessmentyear  > 2000, "3-8", age),
    age     = ifelse(var=="f" & stock=="cod-347d" & assessmentyear <= 2002, "2-8", age),
    age     = ifelse(var=="f" & stock=="cod-347d" & assessmentyear  > 2002, "2-4", age),
    age     = ifelse(var=="f" & stock=="her-irls"                          ,"2-7" , age ),
    
    age     = ifelse(var=="r" & grepl(".+\\(age (.)\\).+", longvar), gsub(".+\\(age (.)\\).+","\\1", longvar), age),
    age     = ifelse(var=="r" & grepl(".+\\(age (.) \\).+", longvar), gsub(".+\\(age (.) \\).+","\\1", longvar), age),
    age     = ifelse(var=="r" & stock=="anb-78ab" & assessmentyear <= 2000, "1", age),
    age     = ifelse(var=="r" & stock=="anb-78ab" & assessmentyear  > 2000, "2", age),
    age     = ifelse(var=="r" & stock=="anp-78ab" & assessmentyear <= 2000, "0", age),
    age     = ifelse(var=="r" & stock=="anp-78ab" & assessmentyear  > 2000, "1", age),
    age     = ifelse(var=="r" & stock=="cod-2532", "2"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stock=="her-irls", "1"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stock=="mgw-78"  , "1"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stock=="ple-celt", "1"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stock=="whg-47d" , "check", age)
  )

# unique(t$stock)
# unique(t$unit)
# unique(t$age)
# 
# filter(t, var=="r"&is.na(unit)) %>% group_by(stock, assessmentyear) %>%  filter(row_number()==1) %>%  View()
# filter(t, var=="r"&is.na(age)) %>% group_by(stock, assessmentyear) %>%  filter(row_number()==1) %>%  View()
# filter(t, stock=="her-3a22") %>% View()
# filter(t, grepl("1990", age)) %>%  View()
# filter(t, grepl("\\(.+\\)$", var)) %>% distinct(var) %>% View()

setf   <- t %>% filter(var=="f")   %>% select(stock, assessmentyear, year, var, f=value, f_unit=unit, f_age=age)
setr   <- t %>% filter(var=="r")   %>% select(stock, assessmentyear, year, var, r=value, r_unit=unit, r_age=age)
setssb <- t %>% filter(var=="ssb") %>% select(stock, assessmentyear, year, var, ssb=value, ssb_unit=unit, ssb_age=age)
setfb  <- t %>% filter(var=="fb")  %>% select(stock, assessmentyear, year, var, fb=value, fb_unit=unit, fb_age=age)

qcsdata <-
  select(setr, stock, assessmentyear, year, r, r_unit, r_age) %>% 
  full_join(select(setssb, stock, assessmentyear, year, ssb, ssb_unit), 
            by=c("stock","assessmentyear","year")) %>% 
  full_join(select(setfb, stock, assessmentyear, year, fb, fb_unit), 
            by=c("stock","assessmentyear","year")) %>% 
  full_join(select(setf, stock, assessmentyear, year, f, f_unit, f_age) , 
            by=c("stock","assessmentyear","year")) %>% 
  arrange(stock, assessmentyear, year) 

  # save dataset
save(qcsdata, file="rdata/qcsdata.RData")



# plot overview
qcsdata %>%
  select(stock, assessmentyear, year, r, ssb, f) %>% 
  gather(key=variable, value=value, r:f) %>%
  mutate(stock = factor(stock), 
         stock = factor(stock, levels = rev(levels(stock)))) %>% 
           
  ggplot(aes(x=assessmentyear, y=stock)) +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        legend.position  = "none",
        legend.direction = "vertical",
        legend.title     = element_blank()) +
  geom_point(aes(colour = stock)) +
  scale_y_discrete(position="right") +
  labs(y = "fishstock", x=" " ) +
  facet_wrap(~variable)


