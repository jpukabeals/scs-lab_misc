# Design for oat variety trial
# see protocol https://docs.google.com/document/d/14cXVcnGX5cSMi8vUFYPR4rm79tsqQONXJi0BCnhIYZQ/edit#heading=h.z39udv4mbnc8
# 26Apr2024

c("sumo",
  "pearl",
  "rushmore",
  "streaker",
  "reins",
  "buffalo") -> entry

library(agricolae)

design.rcbd(
  trt = entry,
  r=4,
  seed=314,
  first=F
) -> fieldbook

library(tidyverse)

entry %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  rename(entry_code = rowid,
         entry= value) %>% 
  relocate(entry_code, .after=entry) -> entry_code_df

fieldbook$book %>% 
  left_join(entry_code_df) %>% 
  write.table(
    "clipboard",
    row.names = F
  )
