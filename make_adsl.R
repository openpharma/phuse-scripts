# create adsl from sdtm data

library(tidyr)
library(dplyr)
library(lubridate)

adsl <- sdtm$dm %>%
  mutate(TRTSDT=as_date(RFXSTDTC),
         TRTEDT=as_date(RFXENDTC),
         TRTDUR=1+time_length(TRTEDT-TRTSDT, unit='days'),
         RFENDT=as_date(RFENDTC)
         ) %>%
  select(-c("DOMAIN", "RFXSTDTC", "RFXENDTC", "RFICDTC", "RFPENDTC", "DTHDTC", "ARMCD", "ACTARMCD", "ACTARM", "COUNTRY",  "DMDTC", "DMDY")) %>%
  right_join(pivot_wider(data = sdtm$suppdm, id_cols = USUBJID, names_from = QNAM, values_from = QVAL, values_fill = 'N')) %>%
  rename(SAFFL=SAFETY, ITTFL=ITT, EFFFL=EFFICACY, COMP8FL=COMPLT8, COMP16FL=COMPLT16, COMP24FL=COMPLT24 ) %>%
  left_join((sdtm$ds %>%
               filter(DSSEQ==1) %>%
               select(USUBJID, DSDECOD) %>%
               mutate(DCREASCD=str_to_title(DSDECOD)) %>%
               rename(DCDECOD=DSDECOD)
               ), by='USUBJID') %>%
  left_join((sdtm$ex %>%
               mutate(dur=1+EXENDY-EXSTDY) %>%
               group_by(USUBJID) %>%
               summarise(CUMDOSE=sum(EXDOSE*dur), TRTDUR=sum(dur)) %>%
               mutate(AVGDD=CUMDOSE/TRTDUR)
               ))
