# LOAD PACKAGES -----------------------------------------------------------
library(tidyverse)
library(difR)
library(mirt)
library(janitor)
library(here)
#data("verbal")

# load saved CDI:WG data  -------------------------------------------------------
load(here("data/en_wg_comp.Rdata")) # d_demo, d_mat, en_wg (items)

d_demo <- d_demo %>% 
  filter(comprehension!=0, !is.na(sex)) %>% # can't fit children not producing words, or with NA sex in group model
  arrange(data_id)

# there are more IDs in d_mat than in d_demo; remove those
d_mat <- d_mat %>% filter(is.element(data_id, d_demo$data_id)) %>% arrange(data_id)

sids = d_mat$data_id
d_mat$data_id = NULL
d_mat = data.matrix(d_mat)
row.names(d_mat) = sids

sex_group = as.character(d_demo$sex)
ses_group = as.character(d_demo$ses_group)

# MIRT FUNCTION HELPERS ---------------------------------------------------
fit_mod_intuitive <- function(data, groups){
  multipleGroup(data, 1, itemtype = "Rasch", groups, invariance = "free_var", SE = TRUE, verbose = FALSE)
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
                          width=8, height=50, # .2 * length(item_names)
                          limitsize = F) 
  return(p)
}


# RUN MODELS
mod_intuitive_sex <- fit_mod_intuitive(d_mat, sex_group)
mod_intuitive_ses <- fit_mod_intuitive(d_mat, ses_group)


# GLIMMER TIME
plot_glimmer(mod_intuitive_sex, colnames(d_mat), plotName="GLIMMER_sex_compWG")
plot_glimmer(mod_intuitive_ses, colnames(d_mat), plotName="GLIMMER_ses_compWG")


