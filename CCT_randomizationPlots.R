# Design for companion crop trial
# see protocol https://docs.google.com/document/d/1i3j-DW1ga3VARF9xl9Lo_VC0O4qMf_YmHcoQZodZ_24/edit
# 26Apr2024

c("iwg",
  "wheat",
  "barley",
  "oat",
  "iwg+wheat",
  "iwg+barley",
  "iwg+oat") -> trt

library(agricolae)

design.rcbd(
  trt=trt,
  r=4,
  seed=314,
  first = F
) -> fieldbook

write.table(
  fieldbook$book,
  "clipboard",
  row.names = F
)
fieldbook$book 


library(tidyverse)

trt %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  rename(trt_code = rowid,
         trt= value) %>% 
  relocate(trt_code, .after=trt) -> trt_code_df

fieldbook$book %>% 
  left_join(trt_code_df) %>% 
  write.table(
    
    "clipboard",
    row.names = F
  )
