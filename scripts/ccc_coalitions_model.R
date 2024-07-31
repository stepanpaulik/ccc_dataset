library(tidyverse)
library(furrr)
plan(multisession, workers = parallel::detectCores() - 2)

write = FALSE

# Load and filter the data relevant for the analysis
data_metadata = read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>% 
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), "None", "At least 1")) %>%
  filter(between(year(date_decision), 2004, 2022)) %>%
  filter(grounds == "merits" | formation == "Plenum") %>%
  filter(grounds != "procedural")

data_judges = read_rds("../data/ccc_database/rds/ccc_judges.rds")

# 
data_compositions = data_metadata %>%
  select(doc_id, composition) %>%
  unnest(composition)

data_dissents = read_rds("../data/ccc_database/rds/ccc_separate_opinions.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)

data_metadata_temp = read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>% rename(date_decision_meta = date_decision,
                                                                                      date_submission_meta = date_submission) %>%
  select(judge_rapporteur_name, date_decision_meta, date_submission_meta, grounds, type_decision)

data = left_join(data_compositions, data_dissents, by = join_by(doc_id, judge_id == dissenting_judge_id)) %>%
  mutate(dissenting_judge_name = if_else(condition = is.na(dissenting_judge_name), true = 0, false = 1)) %>%
  rename(separate_opinion = dissenting_judge_name) %>%
  left_join(., data_metadata %>% select(doc_id, date_decision, subject_proceedings, grounds, subject_register, formation, type_decision, type_proceedings, concerned_acts, concerned_constitutional_acts, type_verdict, citations)) %>%
  mutate(n_concerned_acts = if_else(is.na(concerned_acts), 0, lengths(concerned_acts)),
         n_concerned_constitutional_acts = str_count(string = as.character(concerned_constitutional_acts), pattern = "čl")) %>%
  rowwise() %>%
  mutate(n_citations = length(unique(unlist(citations)))) %>%
  ungroup() %>%
  mutate(separate_opinion_nested = future_pmap(., function(doc_id, judge_name, date_decision, ...) data_judges %>%
                                                 rename(judge_name.y = judge_name) %>%
                                                 filter(date_decision >= judge_term_start & date_decision <= judge_term_end & judge_name == judge_name.y) %>%
                                                 select(judge_gender, judge_uni, judge_degree, judge_profession, judge_term_start, judge_term_end, judge_term_court))) %>%
  unnest(c(separate_opinion_nested)) %>%
  mutate(across(where(is.character), ~as_factor(.))) %>%
  group_by(doc_id) %>%
  filter(n() %in% c(3)) %>%
  filter(!any(judge_term_court %in% c("1st"))) %>%
  arrange(judge_name) %>%
  ungroup() %>%
  mutate(,grounds = fct_rev(grounds)) %>%
  select(-where(is.list)) %>%
  select(-dissenting_group)

data_term_temp = data %>% 
  group_by(doc_id) %>%
  count(judge_term_court) %>%
  pivot_wider(names_from = judge_term_court, values_from = n) %>%
  mutate(across(everything(), ~replace_na(., replace = 0))) %>%
  summarise(panel_term = if_else(`3rd` > `2nd`, "3rd", "2nd") %>%
              as_factor())

data = data %>%
  left_join(., data_term_temp) %>%
  left_join(., read_rds("../data/ccc_database/rds/ccc_texts.rds") %>%
              mutate(text = unlist(text)) %>%
              group_by(doc_id) %>%
              summarise(length_decision = str_length(text)))

# COALITIONS --------------------------------------------------------------
coalition_one = c("Kateřina Šimáčková", "Vojtěch Šimíček", "Ludvík David", "Jaromír Jirsa", "David Uhlíř", "Jiří Zemánek", "Tomáš Lichovník", "Jan Filip", "Milada Tomková", "Pavel Šámal")
coalition_two = c("Radovan Suchánek","Vladimír Sládeček","Josef Fiala","Jan Musil","Jaroslav Fenyk","Pavel Rychetský")

data_coalition = data %>%
  filter(formation != "Plenum") %>%
  group_by(doc_id) %>%
  filter(all(judge_name %in% c(coalition_one, coalition_two))) %>%
  ungroup() %>%
  mutate(coalition = if_else(judge_name %in% coalition_one, 1, 0)) %>%
  group_by(doc_id) %>%
  summarise(
    coalition = sum(coalition),
    coalition = case_when(coalition == 3 ~ "full_coal",
                          coalition == 0 ~ "full_coal",
                          coalition == 1 | 2 ~ "mixed_coal") %>% as_factor(),
    separate_opinion = if_else(any(separate_opinion == 1), 1, 0)) %>%
  mutate(separate_opinion = as_factor(separate_opinion)) %>%
  ungroup() %>%
  mutate(coalition = fct_rev(coalition))

rm(data_metadata_temp)
rm(data_term_temp)



# MODEL - COALITIONS ------------------------------------------------------
model_coalitions = logistic_reg() %>%
  set_engine("glm") %>%
  fit(separate_opinion ~ coalition,
      data = data_coalition) %>%
  extract_fit_engine()

modelsummary::modelsummary(model_coalitions,
                           estimate = "{estimate}{stars}",
                           statistic = "({std.error})",
                           stars = TRUE)

plot_coalitions = model_coalitions %>%
  tidy(conf.int = TRUE) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(x = "Term", y = "Estimate")
plot_coalitions

if(write == TRUE){
  write_rds(model_coalitions, file = "../ccc_dataset/report/model_coalitions.rds")
  write_rds(plot_coalitions, file = "../ccc_dataset/report/plot_coalitions.rds")
}
