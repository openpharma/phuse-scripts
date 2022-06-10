# read cdiscpilot01 study sdtm data

library(haven)
library(stringr)

studydata <- sapply(list.files(path='data/sdtm/cdiscpilot01', pattern = '.xpt$', full.names = TRUE), read_xpt, simplify = FALSE)
names(studydata) <- str_replace(basename(names(studydata)), '.xpt$', '')
