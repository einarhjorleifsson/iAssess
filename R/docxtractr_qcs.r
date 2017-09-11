# --------------------------------------------------------------------------------------------
# Read word tables
# --------------------------------------------------------------------------------------------

library(docxtractr)
library(tidyverse)

real_world <- read_docx("D:/GIT/oldqcs/qcs-docs/Cod-2224.qcs.doc")
docx_tbl_count(real_world)
docx_describe_tbls(real_world)
t <- docx_extract_all_tbls(real_world, guess_header = FALSE, trim = TRUE)

t1 <- t[1] %>% as.data.frame()

var <- t1[1,1]
nc  <- ncol(t1)
nr  <- nrow(t1)
head <- t1[3,2:nc]
rows <- t1[4:nr,1] %>% data.frame()
names(rows) <- "assessmentyear"
data <- t1[4:nr,2:nc] %>% 
  setNames(head) %>% 
  cbind(rows) %>% 
  gather(key=year, value=value, 1:(nc-1)) %>% 
  mutate(var = var, 
         assessmentyear = substr(as.character(assessmentyear), 1,4),
         assessmentyear = as.integer(assessmentyear)) %>% 
  filter(!is.na(value), value != "")







