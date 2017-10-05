# --------------------------------------------------------------------------------------------
# Read word tables
#
# 05/10/2017 coded up the reading in of all data files; datafiles converted to docx format; all superscripts removed prior to reading
# 05/10/2017 converted to iAssess github
# --------------------------------------------------------------------------------------------

library(docxtractr)
library(tidyverse)

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
             longvar            = longvar, 
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
unique(qcsdata$longvar)

qcsdata <-
  qcsdata %>% 
  mutate(
    longvar = tolower(longvar),
    longvar = gsub("  "," ", longvar), 
    var     = ifelse(grepl("average f"  , longvar), "f"  , NA),
    var     = ifelse(grepl("spawning"   , longvar), "ssb", var),
    var     = ifelse(grepl("recruitment", longvar), "r"  , var),
    var     = ifelse(grepl("fishable"   , longvar), "fb" , var),
    
    unit    = ifelse(var=="ssb" & grepl("\\(.+\\)$"  , longvar), gsub(".+\\((.+)\\)$","\\1", longvar), NA),
    unit    = ifelse(var=="fb"  & grepl("\\(.+\\)$"  , longvar), gsub(".+\\((.+)\\)$","\\1", longvar), unit),
    unit    = ifelse(             grepl("unit: (.+)$", longvar), gsub(".+unit: (.+)$","\\1", longvar), unit),
    
    age     = ifelse(var=="f" & grepl("\\(.+\\)$", longvar), gsub(".+\\((.+)\\)$","\\1", longvar), NA) )

filter(t, is.na(var1)) %>% View()
filter(t, stock=="her-3a22") %>% View()
filter(t, grepl("tonnes", var)) %>% distinct(var) %>% View()
filter(t, grepl("\\(.+\\)$", var)) %>% distinct(var) %>% View()
unique(t$unit)

# save dataset
save(qcsdata, file="rdata/qcsdata.RData")










