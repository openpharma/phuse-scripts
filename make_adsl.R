# create adsl from sdtm data

library(tidyr)
library(dplyr)
library(lubridate)

adsl <- sdtm$dm %>%
  mutate(TRTSDT=as_date(RFXSTDTC),
         TRTEDT=coalesce(as_date(RFXENDTC), as_date(RFENDTC)),
         TRTDUR=1+time_length(interval(TRTSDT, TRTEDT), unit='days'),
         RFENDT=as_date(RFENDTC),
         AGEGR1=cut(AGE, breaks = c(0,65, 80, Inf), labels = c('<65', '65-80', '>80'))
         ) %>%
  select(-c("DOMAIN", "RFXSTDTC", "RFXENDTC", "RFICDTC", "RFPENDTC", "DTHDTC", "ARMCD", "ACTARMCD", "ACTARM", "COUNTRY",  "DMDTC", "DMDY")) %>%
  right_join(pivot_wider(data = sdtm$suppdm, id_cols = USUBJID, names_from = QNAM, values_from = QVAL, values_fill = 'N'),
             by='USUBJID') %>%
  rename(SAFFL=SAFETY, ITTFL=ITT, EFFFL=EFFICACY, COMP8FL=COMPLT8, COMP16FL=COMPLT16, COMP24FL=COMPLT24 ) %>%
  left_join(sdtm$ds %>%
              filter(DSSEQ==1) %>%
              mutate(DCREASCD = if_else(DSTERM=='PROTOCOL ENTRY CRITERIA NOT MET', 'I/E NOT MET', DSDECOD)) %>%
              mutate(DCREASCD = str_to_title(recode(DCREASCD, `STUDY TERMINATED BY SPONSOR` = 'Sponsor Decision',
                                                    `WITHDRAWAL BY SUBJECT` = 'Withdrew Consent'))) %>%
              select(USUBJID, DSDECOD, DCREASCD) %>%
              rename(DCDECOD = DSDECOD),
            by='USUBJID') %>%
  left_join(left_join(sdtm$dm, sdtm$ex, by='USUBJID') %>% mutate(trtend=coalesce(as_date(EXENDTC), as_date(RFXENDTC))) %>%
               mutate(dur=1+time_length(interval(as_date(EXSTDTC), trtend), unit = 'days')) %>%
               group_by(USUBJID) %>%
               summarise(CUMDOSE=coalesce(sum(EXDOSE*dur),0),
                         AVGDD=round(coalesce(CUMDOSE/sum(dur), 0), digits=1)),
            by='USUBJID') %>%
  left_join(sdtm$vs %>%
              select(USUBJID, VISITNUM, VSTESTCD, VSSTRESN) %>%
              filter(VISITNUM %in% c(1,3) & VSTESTCD %in% c('HEIGHT', 'WEIGHT')) %>%
              pivot_wider(id_cols = c(USUBJID, VISITNUM), names_from = VSTESTCD, values_from = VSSTRESN) %>%
              group_by(USUBJID) %>%
              fill(HEIGHT, WEIGHT) %>%
              filter(row_number()==n()) %>%
              mutate(BMIBL = round(WEIGHT/((HEIGHT/100) * (HEIGHT/100)), 1),
                     WEIGHTBL=round(WEIGHT, 1),
                     HEIGHTBL=round(HEIGHT, 1),
                     BMIBLGR1=cut(BMIBL, breaks=c(0,25,30,Inf), labels=c('<25', '25-30', '>=30'))) %>%
              select(USUBJID, BMIBL, BMIBLGR1, WEIGHTBL, HEIGHTBL),
            by='USUBJID') %>%
  left_join(sdtm$qs %>%
              filter(QSCAT=='MINI-MENTAL STATE') %>%
              group_by(USUBJID) %>%
              summarise(MMSETOT=sum(QSSTRESN)),
            by='USUBJID') %>%
  left_join(sdtm$sc %>%
              select(USUBJID, SCSTRESN) %>%
              rename(EDUCLVL=SCSTRESN),
            by='USUBJID') %>%
  left_join(sdtm$mh %>%
              filter(MHCAT=='PRIMARY DIAGNOSIS') %>%
              mutate(DISONSDT=as_date(MHSTDTC)) %>%
              select(USUBJID, DISONSDT)) %>%
  mutate(DURDIS=round(time_length(interval(DISONSDT, as_date(RFSTDTC)), unit = 'month'),1),
         DURDSGR1=cut(DURDIS, breaks=c(0, 12, Inf), labels=c('<12', '>=12')))

