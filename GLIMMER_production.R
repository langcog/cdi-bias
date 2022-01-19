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
  bad_Ss = d_demo[which(d_demo$production==0),]$data_id # 9 bad production Ss
  
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
  filter(production!=0) %>% # can't fit children not producing words, or with NA sex in group model
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
plot_glimmer <- function(mod_intuitive, item_names, items_to_plot, plotName='', width=8, height=80) {
  draws_df <- mod_intuitive %>% mod_intuitive_to_draws_df()
  names(draws_df) = c("run", item_names)
  
  
  p <- draws_df %>% 
    clean_names() %>%
    gather(var, val, -run) %>%
    filter(is.element(var, items_to_plot)) %>%
    mutate(var = fct_reorder(var, val)) %>%
    ggplot(aes(x = val, y = var)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept=0), linetype='dashed', alpha=.5) +
    labs(x = "", y = "") + theme_classic()
  if(plotName!='') ggsave(paste0(plotName,'.pdf'), 
                          width=width, height=height, # .2 * length(item_names)
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
plot_glimmer(mod_intuitive_sex, colnames(d_mat), colnames(d_mat), plotName="GLIMMER_sex_prodWS")
plot_glimmer(mod_intuitive_ses, colnames(d_mat), colnames(d_mat), plotName="GLIMMER_ses_prodWS")
plot_glimmer(mod_intuitive_eth, colnames(d_mat), colnames(d_mat), plotName="GLIMMER_eth_prodWS")

# small glimmer plots for paper
plot_glimmer(mod_intuitive_sex, colnames(d_mat), 
             items_to_plot = c("vagina","tights","dress_object","doll","necklace","pretty","underpants","purse","baby",
                 "bear","hug","some","over","cookie","tiny","why", # middling bias
                 "truck","police","dump","firetruck","bat","hammer","tractor","vroom","penis"), # male bias
             plotName="smGLIMMER_sex_prodWS", height=4.5, width=4)

plot_glimmer(mod_intuitive_ses, colnames(d_mat), 
             items_to_plot = c("gum","walker","so","hate","soda_pop","each","candy","why","can_object", # low-SES bias
                               "game","egg","pizza","touch","crayon","rain","dirty", # middling bias
                               "daddy","cockadoodledoo","moo","duck","woof_woof","uh_oh","vroom","quack_quack","grrr"), 
             plotName="smGLIMMER_ses_prodWS", height=4.5, width=4)

plot_glimmer(mod_intuitive_eth, colnames(d_mat), 
             items_to_plot = c("sofa","so","trash","gum","walker","wish","give_me_five","finish","if", # non-white bias
                               "hello","quiet","church","leg","dish","story","bicycle",
                               "moo","bye","uh_oh","all_gone","quack_quack","pets_name","grrr","mommy","daddy"), 
             plotName="smGLIMMER_eth_prodWS", height=4.5, width=4)

# histograms of item difficulty differences

#mod_p <- summary(mod_intuitive_sex)

extract_group_df <- function(group_model, groups=c("Male","Female")) {
  Mit = as_tibble(coef(group_model, simplify=T)[[groups[1]]]$items) %>%
    mutate(definition = rownames(coef(group_model, simplify=T)[[groups[1]]]$items),
           group1 = groups[1],
           group2 = groups[2]) %>%
    select(-g, -u) %>% 
    rename(d_g1 = d)
  Fit = as_tibble(coef(group_model, simplify=T)[[groups[2]]]$items) %>%
    mutate(definition = rownames(coef(group_model, simplify=T)[[groups[2]]]$items)) %>%
    select(-g, -u) %>%
    rename(d_g2 = d)
  
  combo <- Mit %>% left_join(Fit) %>%
    mutate(d_diff = d_g2 - d_g1,
           d_diff_abs = abs(d_diff)) 
  return(combo)
}

get_extreme_item_difficulty_differences <- function(mm) {
  #yfit <- dnorm(mm$d_diff, mean = mean(mm$d_diff), sd = sd(mm$d_diff)) 
  mm <- mm %>% mutate(d_diff = d_g2 - d_g1)
  max_dif = mean(mm$d_diff) + 2*sd(mm$d_diff)
  min_dif = mean(mm$d_diff) - 2*sd(mm$d_diff)
  mm <- mm %>% mutate(extreme = ifelse((d_diff > max_dif) | (d_diff < min_dif), T, F))
  print(paste("mininum difference:",min_dif, "maximum difference:",max_dif))
  return(mm)
}

item_difficulty_difference_histogram <- function(mm, withNormal=F) {
  #yfit <- dnorm(mm$d_diff, mean = mean(mm$d_diff), sd = sd(mm$d_diff)) 
  mm <- mm %>% mutate(d_diff = d_g2 - d_g1)
  max_dif = mean(mm$d_diff) + 2*sd(mm$d_diff)
  min_dif = mean(mm$d_diff) - 2*sd(mm$d_diff)
  mm <- mm %>% mutate(extreme = ifelse((d_diff > max_dif) | (d_diff < min_dif), T, F))
  
  p <- mm %>% ggplot(aes(x=d_diff)) + # , fill=extrem
    geom_histogram(aes(y =..density..), alpha=.7) + theme_classic() +
    geom_vline(aes(xintercept=median(d_diff)), linetype="dashed") +
    geom_vline(aes(xintercept=max_dif), linetype="dashed", color="red") +
    geom_vline(aes(xintercept=min_dif), linetype="dashed", color="red") +
    xlab(paste(mm$group2, "-", mm$group1, "Item Difficulty"))
  
  if(withNormal) {
  p <- p + stat_function(fun = dnorm, alpha=.6, 
                         args = list(mean = mean(mm$d_diff), sd = sd(mm$d_diff)))
  }
  return(p)
}

mm_sex <- extract_group_df(mod_intuitive_sex, groups=c("Male","Female"))
sex_hist <- item_difficulty_difference_histogram(mm_sex)

mm_ses <- extract_group_df(mod_intuitive_ses, groups=c("high","low"))
ses_hist <- item_difficulty_difference_histogram(mm_ses)

mm_eth <- extract_group_df(mod_intuitive_eth, groups=c("Nonwhite","White"))
eth_hist <- item_difficulty_difference_histogram(mm_eth)

require(ggpubr)
ggarrange(sex_hist, ses_hist, eth_hist, nrow=1)
ggsave(file="item_DIF_hist_thresh.pdf", width=8, height=3.5)

sex_histn <- item_difficulty_difference_histogram(mm_sex, withNormal = T)
ses_histn <- item_difficulty_difference_histogram(mm_ses, withNormal = T)
eth_histn <- item_difficulty_difference_histogram(mm_eth, withNormal = T)

ggarrange(sex_histn, ses_histn, eth_histn, nrow=1)
ggsave(file="item_DIF_hists_withNormals.pdf", width=8, height=3.5)




# constrained multiGroup model, allowing only variance between groups to vary
constrained_sex <- multipleGroup(d_mat, 1, itemtype = "Rasch", 
                                 sex_group, invariance = c("intercepts","free_var"), SE = TRUE, verbose = T)

constrained_ses <- multipleGroup(d_mat, 1, itemtype = "Rasch", 
                                 ses_group, invariance = c("intercepts","free_var"), SE = TRUE, verbose = T)

constrained_eth <- multipleGroup(d_mat, 1, itemtype = "Rasch", 
                                 eth_group, invariance = c("intercepts","free_var"), SE = TRUE, verbose = T)


# AOAA-OAT: all others as anchors - one at a time
# remove the item showing most DIF (based on chisq LR test) from the anchor set, 
# re-fit, iterate until no new items display DIF
AOAA_OAT <- function(d_mat, group, fname=c()) {
  mirtCluster()
  constrained <- multipleGroup(d_mat, 1, itemtype = "Rasch", 
                               group, invariance = c("intercepts","free_var"), SE = TRUE, verbose = T)
  items_removed <- tibble()
  difPresent = TRUE
  while(difPresent) {
    # figure out which item shows the most DIF (by LRT--chisq)
    difm <- DIF(constrained, which.par=c('d'), scheme='drop') # ~20 minutes per item..
    most_dif_indx = which(difm$X2==max(difm$X2))
    if(difm[most_dif_indx,]$p<.01) {
      items_removed = bind_rows(items_removed, 
                                bind_cols(word=row.names(difm)[most_dif_indx], difm[most_dif_indx,]))
      col_to_remove = which(colnames(d_mat)==row.names(difm)[most_dif_indx])
      print(paste("removing",row.names(difm)[most_dif_indx]))
      # remove that item, fit the constrained model again
      constrained <- multipleGroup(d_mat[,-col_to_remove], 1, itemtype = "Rasch", 
                                       group, invariance = c("intercepts","free_var"), SE = TRUE, verbose = T)
      if(length(fname)>0) save(constrained, items_removed, file=paste0("AOAA-OAT_",fname,".Rdata"))
    } else {
      difPresent = FALSE
    }
  }
  return(list(model=constrained, items_removed=items_removed, dif_stats=difm))
}

AOAA_OAT(d_mat, ses_group, "ses")
AOAA_OAT(d_mat, sex_group, "sex")
AOAA_OAT(d_mat, eth_group, "race")

# drop uses a constrained model (e.g. same group means) and then tests whether dropping
dif_itemN = DIF(mod_intuitive_sex, which.par=c('d'), scheme='drop', items2test=1:2)

dif_itemN = DIF(mod_intuitive_sex, 'd', scheme='add', items2test = 1)
# Error in constrain[[i]] :  attempt to select less than one element in integerOneIndex