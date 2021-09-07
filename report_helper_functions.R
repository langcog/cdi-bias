# MIRT FUNCTION HELPERS ---------------------------------------------------
#returns the difference between the number of times a words is spoke to boys vs. girls
freq_dif <- function(data, target_word){
  boy <- sum(data[(data$target_child_sex == "male")&(data$gloss == target_word),]$count, na.rm=TRUE)
  girl <- sum(data[(data$target_child_sex == "female")&(data$gloss == target_word),]$count, na.rm=TRUE)
  return(boy-girl)
}

fit_mod_intuitive <- function(data, groups){
  multipleGroup(data, 1, itemtype = "Rasch", groups, invariance = "free_var", SE = TRUE, verbose = T)
}

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

get_extreme_item_difficulty_differences <- function(mm, SD=2) {
  #yfit <- dnorm(mm$d_diff, mean = mean(mm$d_diff), sd = sd(mm$d_diff)) 
  mm <- mm %>% mutate(d_diff = d_g2 - d_g1)
  max_dif = mean(mm$d_diff) + SD*sd(mm$d_diff)
  min_dif = mean(mm$d_diff) - SD*sd(mm$d_diff)
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

# ideal: show top 5, bottom 5, and random/middle 5
plot_glimmer <- function(mod_intuitive, item_names) {
  draws_df <- mod_intuitive %>% mod_intuitive_to_draws_df()
  names(draws_df) = c("run", item_names)

  p_sample <- draws_df %>%
    clean_names() %>%
    gather(var, val, -run) %>%
    mutate(var = fct_reorder(var, val)) 
  
  word_avg <- p_sample %>% group_by(var) %>%
    summarise(mean_val = mean(val)) %>%
    arrange(mean_val)
  
  middle_index = floor(nrow(word_avg) / 2) # actual middle?

  mid <- sample(6:(nrow(word_avg)-5), 5, replace=F)
  desired_words <- bind_rows(word_avg %>% head(5),
                             word_avg[mid,], # might sample head/tail sample_n(5)
                             word_avg %>% tail(5))
  
  p <- p_sample %>% filter(is.element(var, desired_words$var)) %>%
    ggplot(aes(x = val, y = var)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept=0), linetype='dashed', alpha=.5) +
    labs(x = "", y = "") + theme_classic()
  
  return(p)
}

## childes_new
lang_map <- read_csv("resources/language_map.csv")

normalize_language <- function(language) {
  language %>% str_replace(" ", "_") %>% str_to_lower()
}

convert_lang_stemmer <- function(lang, method = "snowball") {
  lang_map %>% filter(wordbank == lang) %>% pull(get(method))
}

transforms <- list(
  function(s) str_replace_all(s, "(.*) \\(.*\\)", "\\1"), # foo (bar) -> foo
  function(s) str_replace_all(s, " ", "_"), # foo bar -> foo_bar
  function(s) str_replace_all(s, " ", "+"), # foo bar -> foo+bar
  function(s) str_replace_all(s, "(.+) \\1", "\\1") # (foo) bar -> bar
  #   function(x) paste0(x, "e+moi"),
  #   function(x) paste0(x, "e-moi"),
  #   function(x) paste0(x, "+moi"),
  #   function(x) paste0(x, "-moi"),
  #   function(x) paste0(x, "ent"),
  #   function(x) paste0(x, "e-l"),
  #   function(x) paste0(x, "e-toi"),
  #   function(x) paste0(x, "-l"),
  #   function(x) paste0(x, "-toi"),
  #   function(x) paste0(x, "es-tu"),
  #   function(x) paste0(x, "s+tu"),
  #   function(x) paste0(x, "s-moi")
)

build_special_case_map <- function(lang) {
  norm_lang <- normalize_language(lang)
  special_case_file <- glue("resources/{norm_lang}.csv")
  if (file.exists(special_case_file)) {
    a<-read_csv(special_case_file, col_names = FALSE) %>%
      rename(uni_lemma = X1, definition = X2) %>%
      pivot_longer(-c(uni_lemma, definition),
                   names_to = "x", values_to = "option") %>%
      filter(!is.na(option)) %>%
      select(-x) %>%
      mutate(language = lang)
  }
  else{
    a<-data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("uni_lemma", "definition", "option", "language")))) %>%
      mutate(language = lang, uni_lemma = as.character(uni_lemma), definition = as.character(definition), option = as.character(option) )
  }
  return(a)
}

build_options <- function(language, word, special_cases) {
  opts <- c(word, special_cases)
  opts <- c(opts, word %>% str_split("[,/]") %>% unlist()) # "foo, bar", "foo/bar"
  opts <- c(opts, map(transforms, function(t) t(opts)))
  opts <- opts %>% unlist() %>% unique() %>% str_trim()
  stemmer_lang <- convert_lang_stemmer(language)
  if (!is.na(stemmer_lang)) opts <- c(opts, stem(opts, stemmer_lang))
  opts <- opts %>% unique()
}

# construct a mapping from CDI items to various potential realizations of them
# in CHILDES
build_uni_lemma_map <- function(uni_lemmas) {
  special_case_map <- unique(uni_lemmas$language) %>%
    map_df(build_special_case_map) %>%
    group_by(language, uni_lemma, definition) %>%
    summarise(special_cases = list(option))
  
  uni_lemmas %>%
    unnest(items) %>%
    left_join(special_case_map) %>%
    mutate(option = pmap(list(language, definition, special_cases),
                         build_options)) %>%
    select(language, uni_lemma, option) %>%
    unnest(option)
}

##stemmer code move later

# Gets stems for a list of words in a given language.
# Uses Snowball by default (with special case for Croatian, which
# uses Steven Koch's implementation of the Zagreb Stemmer)
# Also allows for hunspell as an alternative method

stem <- function(words, language, method = "snowball") {
  
  if (method == "snowball") {
    
    if (language %in% SnowballC::getStemLanguages()) {
      SnowballC::wordStem(words, language)
      
    } else if (language == "croatian") {
      chunk_size <- 1000
      word_chunks <- split(words, ceiling(seq_along(words) / chunk_size))
      map(word_chunks, function(word_chunk) {
        system2("python",
                args = c("scripts/croatian.py", sprintf('"%s"', word_chunk)),
                stdout = TRUE)
      }) %>% unlist()
      
    } else {
      warning(sprintf("language %s not in list of stemmable languages",
                      language))
      words
    }
    
  } else if (method == "hunspell") {
    
    Sys.setenv(DICPATH = here("resources", "dicts"))
    
    if (language %in% hunspell::list_dictionaries()) {
      lapply(words, function(word) {
        stem <- hunspell::hunspell_stem(word, dictionary(language))[[1]]
        return(if (length(stem) == 0) word else stem[1])
      }) %>% unlist()
    } else {
      warning(sprintf("language %s not in list of stemmable languages",
                      language))
      words
    }
    
  } else {
    warning(sprintf("invalid stemming method %s",
                    method))
    words
  }
}