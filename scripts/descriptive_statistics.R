library(tidyverse); theme_set(theme_minimal())
library(patchwork)


# case-level variables ----------------------------------------------------
data_metadata = read_rds("../data/ccc_dataset/ccc_metadata.rds") %>% 
  mutate(presence_dissent = if_else(is.na(as.character(dissenting_opinion)), "None", "At least 1"),
         merits_admissibility = case_when(
           str_detect(as.character(type_verdict), "vyhověno|zamítnuto") ~ "merits",
           str_detect(as.character(type_verdict), "procesní") & !str_detect(as.character(type_verdict), "vyhověno|zamítnuto|odmítnutno") ~ "procedural",
           .default = "admissibility"))
data_dissents = read_rds("../data/ccc_dataset/ccc_separate_opinions.rds")

data_metadata %>%
  filter(merits_admissibility %in% c("merits", "admissibility")) %>%
  group_by(year(date_submission)) %>%
  summarise(caseload = n(),
            avg_length = mean(length_proceeding)) %>%
  filter(avg_length < 400) %>%
  ggplot(aes(y = caseload, x = avg_length)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(x = "Average length of proceedings before the CCC", y = "Yearly caseload of the CCC")
  

caseload = data_metadata %>%
  mutate(merits_admissibility = case_when(
    str_detect(as.character(type_verdict), "vyhověno|zamítnuto") ~ "merits",
    str_detect(as.character(type_verdict), "procesní") & !str_detect(as.character(type_verdict), "vyhověno|zamítnuto|odmítnutno") ~ "procedural",
    .default = "admissibility")) %>%
  ggplot(aes(x = year(date_submission), fill = merits_admissibility)) +
  geom_bar(position = position_stack(reverse = TRUE))  +
  scale_x_continuous(breaks = seq(1991, 2023, 2)) +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = NULL, fill = "Type of verdict")
  
dissents_distribution_judges = data_dissents %>%
  group_by(doc_id) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 9, 1))

dissents_distribution_opinions = data_dissents %>%
  group_by(doc_id) %>%
  summarise(n = length(unique(dissenting_group))) %>%
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  labs(x = "Number of dissenting opinions", y = "Count")


dissents_prevalence = data_metadata %>%
  filter(type_decision == "Nález" | formation == "Plenum") %>%
  ggplot(aes(x = forcats::fct_infreq(presence_dissent))) +
  geom_bar() +
  labs(x = NULL, y = NULL)
  

# judge-level variables ---------------------------------------------------
data_judges = read_rds(file = "../data/ccc_dataset/ccc_judges.rds")
data_clerks = read_rds("../data/ccc_dataset/ccc_clerks.rds")

alma_mater = data_judges %>%
  ggplot(aes(x = judge_uni, fill = judge_uni)) +
  scale_fill_brewer(palette="Pastel1") +
  geom_bar() +
  labs(x = NULL, y = NULL)

average_age = data_judges %>%
  mutate(age = year(judge_term_start) - judge_yob) %>%
  ggplot(aes(x = age)) +
  geom_density() +
  labs(x = "Age (years) of a justice at the start of their term", y = NULL)
average_age 

gender_judges = data_judges %>%
  ggplot(aes(x = judge_term_court, fill = judge_gender, color = judge_gender)) +
  geom_bar(position = "dodge", show.legend = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_brewer(palette="Pastel1", direction = -1) +
  scale_fill_brewer(palette="Pastel1", direction = -1) +
  labs(x = "Terms of the CCC", y = NULL, subtitle = "Justices") +
  theme(legend.title = element_blank()) 
gender_judges

gender_clerks = data_clerks %>%
  distinct(clerk_name, .keep_all = TRUE) %>%
  ggplot(aes(x = clerk_gender, fill = clerk_gender)) +
  scale_color_brewer(palette="Pastel1", direction = -1) +
  scale_fill_brewer(palette="Pastel1", direction = -1) +
  geom_bar() +
  theme(legend.title = element_blank(),
          axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL, subtitle = "Clerks")
gender_clerks

gender_clerks_judges = data_clerks %>%
  distinct(clerk_name, .keep_all = TRUE) %>% 
  left_join(., data_judges %>% select(judge_name, judge_gender)) %>%
  ggplot(aes(x = clerk_gender, fill = clerk_gender)) +
  scale_color_brewer(palette="Pastel1", direction = -1) +
  scale_fill_brewer(palette="Pastel1", direction = -1) +
  geom_bar() +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL, subtitle = "Clerks") +
  facet_wrap(~judge_gender)

rm(list=ls(pattern="^data_"))
save.image(file = "report/descriptive_statistics.RData")
