pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)

source(here::here("HRD","Recover_data.R"))

raw_hrd = read.csv(here::here("HRD","Data","raw_hrd.csv")) %>% filter(session == 1)

df = prep_data(raw_hrd) %>% filter(Modality == "Intero")


df = df %>% arrange(s,nTrials)

datastan = list(Y = df$y,
                N = nrow(df),
                S = length(unique(df$s)),
                S_id = df$s,
                X = df$x)


mod_norm_prior = cmdstanr::cmdstan_model(here::here("HRD","Normal.stan"))

fit_norm <- mod_norm_prior$sample(
  data = datastan,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  refresh = 50,
  adapt_delta = 0.95,
  max_treedepth = 12
)


fit_norm$save_object(here::here("HRD","stanmodel","Normal_intero.RDS"))


### Extero

raw_hrd = read.csv(here::here("HRD","Data","raw_hrd.csv")) %>% filter(session == 1)

df = prep_data(raw_hrd) %>% filter(Modality == "Extero")


df = df %>% arrange(s,nTrials)

datastan = list(Y = df$y,
                N = nrow(df),
                S = length(unique(df$s)),
                S_id = df$s,
                X = df$x)


mod_norm_prior = cmdstanr::cmdstan_model(here::here("HRD","Normal.stan"))

fit_norm <- mod_norm_prior$sample(
  data = datastan,
  iter_sampling = 1000,
  iter_warmup = 1000,
  chains = 4,
  parallel_chains = 4,
  refresh = 50,
  adapt_delta = 0.95,
  max_treedepth = 12
)


fit_norm$save_object(here::here("HRD","stanmodel","Normal_extero.RDS"))







############### extracting

source(here::here("HRD","Recover_data.R"))

raw_hrd = read.csv(here::here("HRD","Data","raw_hrd.csv")) %>% filter(session == 1)

df = prep_data(raw_hrd) %>% filter(Modality == "Intero")


df = df %>% arrange(s,nTrials)

Normal <- readRDS(here::here("HRD","stanmodel","Normal_intero.RDS"))


parameters = Normal$summary(c("beta","alpha","lapse"))


parameters_intero = parameters %>% mutate(idx = str_extract(variable, "\\d+"),
                      variable = str_remove(variable, "\\[\\d+\\]")) %>% 
  mutate(Modality = "Intero")

df = df %>% mutate(idx = as.character(s))

df_sum = df %>% mutate(ACC = ifelse(x > 0 & y == 1, 1,
                             ifelse(x < 0 & y == 1, 0,
                                    ifelse(x > 0 & y == 0, 0,
                                           ifelse(x < 0 & y == 0, 1,
                                                  ifelse(x == 0, 0,NA)))))) %>% 
    group_by(participant_id,idx) %>% 
    summarize(mean_confidence = mean(as.numeric(Confidence), na.rm = T),
              sd_confidence = sd(as.numeric(Confidence), na.rm = T),
              mean_RT = mean(as.numeric(rt), na.rm = T),
              sd_RT = sd(as.numeric(rt), na.rm = T),
              Accuracy = sum(ACC) / n())


parameters_intero = inner_join(df_sum,
           parameters_intero %>% select(variable,mean,q5,q95,idx)) %>% rename(estimated_mean = mean, estimated_q5 = q5, estimated_q95 = q95) %>% 
  pivot_wider(names_from = "variable",values_from = c("estimated_mean","estimated_q5","estimated_q95"))

# test
# subjects = seq(1,10,1)
# df %>% filter(s %in% subjects) %>% ggplot(aes(x = x , y = y)) + 
#   geom_point()+facet_wrap(~participant_id)
# 
# parameters_intero %>% filter(idx %in% subjects)


write.csv(parameters_intero,here::here("HRD","Hierarchical_HRD_Intero.csv"))


source(here::here("HRD","Recover_data.R"))

raw_hrd = read.csv(here::here("HRD","Data","raw_hrd.csv")) %>% filter(session == 1)

df = prep_data(raw_hrd) %>% filter(Modality == "Extero")


df = df %>% arrange(s,nTrials)

Normal_extero <- readRDS(here::here("HRD","stanmodel","Normal_extero.RDS"))


parameters_extero = Normal_extero$summary(c("beta","alpha","lapse"))

parameters_extero = parameters_extero %>% mutate(idx = str_extract(variable, "\\d+"),
                                          variable = str_remove(variable, "\\[\\d+\\]")) %>% 
  mutate(Modality = "Extero")

df = df %>% mutate(idx = as.character(s))

df_sum = df %>% mutate(ACC = ifelse(x > 0 & y == 1, 1,
                                    ifelse(x < 0 & y == 1, 0,
                                           ifelse(x > 0 & y == 0, 0,
                                                  ifelse(x < 0 & y == 0, 1,
                                                         ifelse(x == 0, 0,NA)))))) %>% 
  group_by(participant_id,idx) %>% 
  summarize(mean_confidence = mean(as.numeric(Confidence), na.rm = T),
            sd_confidence = sd(as.numeric(Confidence), na.rm = T),
            mean_RT = mean(as.numeric(rt), na.rm = T),
            sd_RT = sd(as.numeric(rt), na.rm = T),
            Accuracy = sum(ACC) / n())


parameters_extero = inner_join(df_sum,
                               parameters_extero %>% select(variable,mean,q5,q95,idx)) %>% 
  rename(estimated_mean = mean, estimated_q5 = q5, estimated_q95 = q95) %>% 
  pivot_wider(names_from = "variable",values_from = c("estimated_mean","estimated_q5","estimated_q95"))



# test
subjects = seq(1,20,1)
df %>% filter(s %in% subjects) %>% ggplot(aes(x = x , y = y)) + geom_point()+facet_wrap(~participant_id)
parameters_extero%>% filter(idx %in% subjects) %>% .$estimated_mean_alpha


write.csv(parameters_intero,here::here("HRD","Hierarchical_HRD_Extero.csv"))

