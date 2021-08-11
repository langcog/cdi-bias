# LOAD PACKAGES -----------------------------------------------------------
library(tidyverse)
library(difR)
library(mirt)
library(janitor)
library(here)
#data("verbal")

# gets WS production data and demo variables (ToDo: stitch with WG production data)
get_production_data <- function() {
  require(wordbankr)
  
  d_long_ws <- get_instrument_data(language="English (American)", form = "WS") %>%
    mutate(produces = as.numeric(value == "produces")) # we'll start with production
  
  en_ws <- get_item_data(language="English (American)", form="WS") %>%
    filter(type=="word") %>% select(-complexity_category, -item_id)
  
  d_demo <- get_administration_data(language = "English (American)", form = "WS")
  
  # subjects not producing any words yet (can't fit model with them)
  bad_Ss = d_demo[which(d_demo$comprehension==0),]$data_id # 9 bad production Ss
  
  d_long_ws <- d_long_ws %>% left_join(en_ws %>% select(num_item_id, definition)) %>%
    filter(!is.na(definition))
  
  d_mat <- d_long_ws %>% select(data_id, definition, produces) %>% 
    pivot_wider(id_cols = data_id, names_from = definition, values_from = produces)
  
  d_long_ws <- d_long_ws %>% select(-value, -num_item_id)
  
  save(d_mat, d_demo, en_ws, d_long_ws, file=here("data/en_ws_production.Rdata"))
}

# run once
#get_production_data()

# load saved CDI:WS data  -------------------------------------------------------
load(here("data/en_ws_production.Rdata")) # d_demo, d_mat, en_wg (items)

d_demo <- d_demo %>% 
  filter(comprehension!=0, !is.na(sex)) %>% # can't fit children not producing words, or with NA sex in group model
  arrange(data_id) %>%
  mutate(eth_group = ifelse(ethnicity=="White", "White", "Nonwhite"),
         ses_group = 
           ifelse(is.element(mom_ed, c("None", "Primary", "Some Secondary", "Secondary")), "low", "high"))
  

# there are more IDs in d_mat than in d_demo; remove those
d_mat <- d_mat %>% filter(is.element(data_id, d_demo$data_id)) %>% arrange(data_id)

# only 4090 kids with demographic variables

sids = d_mat$data_id
d_mat$data_id = NULL
d_mat = data.matrix(d_mat)
row.names(d_mat) = sids

sex_group = as.character(d_demo$sex)
ses_group = as.character(d_demo$ses_group)
eth_group = as.character(d_demo$eth_group)

# MIRT FUNCTION HELPERS ---------------------------------------------------
fit_mod_intuitive <- function(data, groups){
  multipleGroup(data, 1, itemtype = "Rasch", groups, invariance = "free_var", SE = TRUE, verbose = T)
}

mod_intuitive_to_draws_df <- function(mod){
  par_draws <- MASS::mvrnorm(n = 10000, mu = extract.mirt(mod, "parvec"), Sigma = extract.mirt(mod, "vcov"))
  par_draws <- par_draws[ , str_detect(colnames(par_draws), "d")]
  draws_df <- tibble(run = 1:nrow(par_draws))
  stopifnot(ncol(par_draws) %% 2 == 0)
  n_items <- ncol(par_draws) / 2
  for (i in 1:n_items) {
    draws_df[[paste0("item", i)]] <- par_draws[ , i] - par_draws[ , i + n_items]
  }
  draws_df
}

draws_df_to_logit_plot <- function(draws_df){
  draws_df %>%
    select(-run) %>%
    gather(var, val) %>%
    mutate(var = paste0("Item ", add_zero(parse_number(as.character(var))))) %>%
    ggplot(aes(x = val, y = var)) +
    ggridges::geom_density_ridges() +
    labs(x = "", y = "")
}

# CREATE GLIMMER PLOT ----------------------------------------------------------
plot_glimmer <- function(mod_intuitive, item_names, plotName='') {
  draws_df <- mod_intuitive %>% mod_intuitive_to_draws_df()
  names(draws_df) = c("run", item_names)
  
  
  p <- draws_df %>%
    clean_names() %>%
    gather(var, val, -run) %>%
    mutate(var = fct_reorder(var, val)) %>%
    ggplot(aes(x = val, y = var)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept=0), linetype='dashed', alpha=.5) +
    labs(x = "", y = "") + theme_classic()
  if(plotName!='') ggsave(paste0(plotName,'.pdf'), 
                          width=8, height=80, # .2 * length(item_names)
                          limitsize = F) 
  return(p)
}


# RUN MODELS
run_once <- function() {
  mod_intuitive_sex <- fit_mod_intuitive(d_mat, sex_group)
  mod_intuitive_ses <- fit_mod_intuitive(d_mat, ses_group)
  mod_intuitive_eth <- fit_mod_intuitive(d_mat, eth_group)

  save(mod_intuitive_sex, mod_intuitive_eth, mod_intuitive_ses,
       file="data/glimmer_prodWS_models.Rds")
}

load("data/glimmer_prodWS_models.Rds")

# GLIMMER TIME
plot_glimmer(mod_intuitive_sex, colnames(d_mat), plotName="GLIMMER_sex_prodWS")
plot_glimmer(mod_intuitive_ses, colnames(d_mat), plotName="GLIMMER_ses_prodWS")
plot_glimmer(mod_intuitive_eth, colnames(d_mat), plotName="GLIMMER_eth_prodWS")


