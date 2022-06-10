# read cdiscpilot01 study sdtm data

library(haven)
library(stringr)

sdtm <- sapply(list.files(path='data/sdtm/cdiscpilot01', pattern = '.xpt$', full.names = TRUE), read_xpt, simplify = FALSE)
names(sdtm) <- str_replace(basename(names(sdtm)), '.xpt$', '')

adam <- sapply(list.files(path='data/adam/cdiscpilot01', pattern = '.xpt$', full.names = TRUE), read_xpt, simplify = FALSE)
names(adam) <- str_replace(basename(names(adam)), '.xpt$', '')
