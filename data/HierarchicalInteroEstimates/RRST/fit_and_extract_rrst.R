

pacman::p_load(cmdstanr, tidyverse,posterior, bayesplot, tidybayes, furrr)

source(here::here("RRST","utility.R"))

raw_rrst = read.csv(here::here("RRST","Data","raw_rrst.csv"))

bad_ids = raw_rrst %>% group_by(id) %>% summarize(Acc = sum(ResponseCorrect) / n()) %>% filter(Acc < 0.5)

raw_rrst %>% filter(id == 263) %>% mutate(Confidence = as.numeric(Confidence)) %>% 
  pivot_longer(cols = c("ResponseCorrect","DecisionRT","Confidence")) %>% 
  ggplot(aes(x = StimulusLevel, y = value, col = as.factor(Decision)))+
  facet_wrap(~name, ncol = 1, scales = "free")+
  geom_jitter(width = 0.05, height = 0.1)+geom_smooth()

raw_rrst %>% mutate(Confidence = as.numeric(Confidence)) %>% 
  pivot_longer(cols = c("ResponseCorrect","DecisionRT","Confidence")) %>% group_by(StimulusLevel,name) %>% 
  summarize(mean_resp = mean(value, na.rm = T), sd_resp = sd(value, na.rm = T)/sqrt(n())) %>% 
  ggplot(aes(x = StimulusLevel, y = mean_resp))+
  facet_wrap(~name, ncol = 1, scales = "free")+geom_smooth()+
  geom_pointrange(aes(x = StimulusLevel, y = mean_resp, ymin = mean_resp - 2 * sd_resp, ymax = mean_resp + 2 * sd_resp))


data = raw_rrst %>% mutate(Confidence = as.numeric(Confidence),
                           DecisionRT = as.numeric(DecisionRT)) %>% 
  mutate(Confidence  = Confidence/100) %>% 
  filter(DecisionRT > 0.1) %>% drop_na()%>% 
  arrange(id) %>% select(id,participant_id, StimulusLevel,DecisionRT, Confidence, ResponseCorrect)

data = data %>% filter(!id %in% bad_ids$id)%>%mutate(id = as.numeric(as.factor(id)))%>%arrange(id)

id_identify = data %>% select(id, participant_id)

data = data %>%
  group_by(id) %>%
  mutate(minRT = min(DecisionRT))

datastan = list(T = nrow(data),
                S = length(unique(data$id)),
                S_id = as.numeric(data$id),
                X = data %>% .$StimulusLevel,
                Y = data %>% .$ResponseCorrect
                # RT = data %>% .$DecisionRT,
                # minRT = data %>% .$minRT,
                # Conf = data %>% .$Confidence
                
)

mod = cmdstanr::cmdstan_model(here::here("RRST","pure_RRST_noguess.stan"))

fit_norm <- mod$sample(
  data = datastan,
  iter_sampling = 5000,
  iter_warmup = 5000,
  chains = 4,
  init = 0,
  parallel_chains = 4,
  refresh = 100,
  adapt_delta = 0.99,
  max_treedepth = 15
)



#fit_norm$save_object(here::here("RRST_noguess.RDS"))


data_sum = data %>% rename(subject = id) %>% 
  group_by(subject) %>% mutate(Confidence = Confidence*100) %>% 
  summarize(mean_confidence = mean(as.numeric(Confidence), na.rm = T),
            sd_confidence = sd(as.numeric(Confidence), na.rm = T),
            mean_RT = mean(as.numeric(DecisionRT), na.rm = T),
            sd_RT = sd(as.numeric(DecisionRT), na.rm = T),
            Accuracy = sum(ResponseCorrect) / n())


# Extracting RRST

fit <- readRDS("~/Extracting VMP stuff/RRST/stanmodel/RRST_noguess.RDS")

params = c("beta","lapse","alpha")

params = fit$summary(params)

params = params %>% mutate(subject = as.numeric(str_extract(variable, "\\d+(?=\\])")),
                           variable = str_replace(variable, "\\[\\d+\\]", ""))

parameters = inner_join(data_sum,
                        params %>% select(variable,mean,q5,q95,subject) %>% 
  rename(estimated_mean = mean, estimated_q5 = q5, estimated_q95 = q95) %>% 
  pivot_wider(names_from = "variable",values_from = c("estimated_mean","estimated_q5","estimated_q95")),
                        by = "subject")


parameters_id = inner_join(id_identify %>% rename(subject = id) %>% distinct(),parameters,by = "subject")


write.csv(parameters_id,here::here("RRST","Hierarchical_RRST_subjectlevel_estimates.csv"))
