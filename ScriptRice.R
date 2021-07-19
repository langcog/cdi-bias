library(wordbankr)
library(tidyverse)
library(ggrepel)
library(dbplyr)
library(mirt) 
library(here)
library(kableExtra)
library(gridExtra)
library(printr)
library(parallel)
library(tidyr)



d_demo <- 
  get_administration_data(language = "English (American)", form = "WG")

d_long_wg <- get_instrument_data(language="English (American)", form = "WG") %>%
  mutate(produces = as.numeric(value == "produces")) # we'll start with production
# yes = understands; no = does not understand/produce, produces = produces (and understands)

en_wg <- get_item_data(language="English (American)", form="WG") %>%
  filter(type=="word") %>% select(-complexity_category, -item_id)

# subjects not producing any words yet (can't fit model with them)
bad_Ss = d_demo[which(d_demo$production==0),]$data_id

d_long_wg <- d_long_wg %>% left_join(en_wg %>% select(num_item_id, definition)) %>%
  filter(!is.na(definition))

d_mat <- d_long_wg %>% select(data_id, definition, produces) %>% 
  pivot_wider(id_cols = data_id, names_from = definition, values_from = produces)

d_demo <- d_demo %>% 
  filter(production!=0, !is.na(sex)) %>% # can't fit children not producing words, or with NA sex in group model
  arrange(data_id)

# there are more IDs in d_mat than in d_demo; remove those
d_mat <- d_mat %>% filter(is.element(data_id, d_demo$data_id)) %>% arrange(data_id)

sids = d_mat$data_id
d_mat$data_id = NULL
d_mat = data.matrix(d_mat)
row.names(d_mat) = sids

table(en_wg$category)

sex_group = as.character(d_demo$sex)

# ToDo: define ses_group ("high" for mom_ed > X, "low") (choose X ..high school?)

#mg1 <- multipleGroup(d_mat, 1, group = sex_group, SE = T) 
# Error: Multiple Group model will not be identified without proper constraints (groups contain missing data patterns where item responses have been completely omitted or, alternatively, the number of categories within each group is not equal to the total number of categories)
#mg2 <- multipleGroup(d_mat, model = 1, group = sex_group, technical=list(NCYCLES=1000), 
#invariance = 'slopes', verbose = T)

itemnames <- colnames(d_mat)
anchor_items = c(9,12,95,87,13)
#wont work if any of the items have only one response category aka all 0's or all 1's
model <- multipleGroup(d_mat, 1, group = sex_group, SE = TRUE,
                       invariance = c(itemnames[anchor_items], 'free_means', 'free_var'))
dif_items = setdiff(1:ncol(d_mat), anchor_items)
resulta1d <- DIF(model, c('a1', 'd'), plotdif = TRUE, items2test=itemnames[dif_items])
save(file = here("result_smDIF.Rds"), resulta1d)
