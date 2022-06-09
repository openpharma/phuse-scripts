# create adsl from sdtm data

library(tidyr)

xx <- dm %>% 
  select(-c("DOMAIN", "RFXSTDTC", "RFXENDTC", "RFICDTC", "RFPENDTC", "DTHDTC", "ARMCD", "ACTARMCD", "ACTARM", "COUNTRY",  "DMDTC", "DMDY")) %>% 
  right_join(pivot_wider(data = studydata$suppdm, id_cols = USUBJID, names_from = QNAM, values_from = QVAL, values_fill = 'N')) %>% 
  rename(saffl=SAFETY, ittfl=ITT, efffl=EFFICACY, comp8fl=COMPLT8, comp16fl=COMPLT16, comp24fl=COMPLT24 ) %>% 
  left_join((ds %>% filter(DSSEQ==1) %>% select(USUBJID, DSDECOD) %>% mutate(dcreascd=str_to_title(DSDECOD), .keep='unused')), by='USUBJID')
