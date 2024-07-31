library(tidyverse); theme_set(theme_minimal())
library(patchwork)
library(skimr)

# Majority of the tables/figures created here are not used in the final paper. They served as a way to do preliminary exploration of the data for me.

# load data ---------------------------------------------------------------
data_metadata = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |> 
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), "None", "At least 1"))
data_dissents = read_rds("../data/ccc_database/rds/ccc_separate_opinions.rds")

data_judges = read_rds(file = "../data/ccc_database/rds/ccc_judges.rds") |> 
  filter(judge_term_court != "4th")
data_clerks = read_rds("../data/ccc_database/rds/ccc_clerks.rds") |> 
  filter(judge_name %in% data_judges$judge_name[data_judges$judge_term_court != "4th"])


# PLOTS -------------------------------------------------------------------
# Create various plots for the final paper
data_metadata |>
  filter(grounds %in% c("merits", "admissibility")) |>
  group_by(year(date_submission)) |>
  summarise(caseload = n(),
            avg_length = mean(length_proceeding)) |>
  filter(avg_length < 400) |>
  ggplot(aes(y = caseload, x = avg_length)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(x = "Average length of proceedings before the CCC", y = "Yearly caseload of the CCC")

caseload = data_metadata |>
  ggplot(aes(x = year(date_decision), fill = grounds)) +
  geom_bar(position = position_stack(reverse = TRUE))  +
  scale_x_continuous(breaks = seq(1991, 2023, 2)) +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = NULL, fill = "Type of verdict")
caseload

# judge-level variables ---------------------------------------------------
alma_mater = data_judges |>
  ggplot(aes(x = judge_uni, fill = judge_uni)) +
  scale_fill_brewer(palette="Pastel1") +
  geom_bar() +
  labs(x = NULL, y = NULL, fill = "Alma Mater")
alma_mater

starting_age = data_judges |>
  mutate(age = year(judge_term_start) - judge_yob) |>
  ggplot(aes(x = age)) +
  geom_histogram() +
  geom_density(aes(y = after_stat(density)*110)) +
  labs(x = "Age (years) of a justice at the start of their term", y = NULL)
starting_age 


# SUMMARY TABLE -----------------------------------------------------------
summary_table = tibble()
summary_table = bind_rows(read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
                            summarise(
                              table = "ccc_metadata",
                              obeservation = "decision",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_texts.rds") %>%
                            summarise(
                              table = "ccc_texts",
                              obeservation = "text",
                              n_observations = n(), 
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_compositions.rds") %>%
                            summarise(
                              table = "ccc_compositions",
                              obeservation = "sitting justice",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_references.rds") %>%
                            summarise(
                              table = "ccc_references",
                              obeservation = "reference",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_separate_opinions.rds") %>%
                            summarise(
                              table = "ccc_metadata",
                              obeservation = "separate opinion",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_subject_matter.rds") %>%
                            summarise(
                              table = "ccc_subject_matter",
                              obeservation = "subject matter",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_parties.rds") %>%
                            summarise(
                              table = "ccc_parties",
                              obeservation = "party",
                              n_observations = n(),n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_verdicts.rds") %>%
                            summarise(
                              table = "ccc_verdicts",
                              obeservation = "verdict",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_judges.rds") %>%
                            summarise(
                              table = "ccc_judges",
                              obeservation = "judge",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          read_rds("../data/ccc_database/rds/ccc_clerks.rds") %>%
                            summarise(
                              table = "ccc_clerks",
                              obeservation = "clerk-judge",
                              n_observations = n(),
                              n_variables = ncol(.)),
                          )


# VIGNETTE - CLERKS -------------------------------------------------------
gender_judges = data_judges |>
  group_by(judge_term_court, judge_gender) |>
  summarise(n = n()) |>
  group_by(judge_term_court) |>
  mutate(pct = round(n/sum(n), digit = 2)) |>
  ungroup() |>
  ggplot(aes(x = judge_term_court, y = pct, fill = judge_gender)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_fill_brewer(palette="Greys") +
  labs(x = "Terms of the CCC", y = NULL, subtitle = "Justices") +
  theme(legend.title = element_blank()) 
gender_judges

gender_clerks = data_clerks |>
  distinct(clerk_name, .keep_all = TRUE) |>
  group_by(clerk_gender) |>
  summarise(n = n()) |>
  mutate(pct = round(n/sum(n), digit = 2)) |>
  ggplot(aes(x = clerk_gender, y = pct, fill = clerk_gender)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_fill_brewer(palette="Greys") +
  theme(legend.title = element_blank(),
          axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL, subtitle = "Clerks")
gender_clerks

gender_distribution = gender_judges + gender_clerks + theme(axis.text.y = element_blank())
ggsave("report/The_Czech_Constitutional_Court_Database_files/figure-latex/gender_distribution.eps", gender_distribution, device = "eps")

gender_clerks_judges = data_clerks |>
  distinct(clerk_name, .keep_all = TRUE) |> 
  left_join(data_judges |> select(judge_name, judge_gender, judge_term_court) |> distinct(judge_name, .keep_all = TRUE)) |>
  filter(judge_term_court != "4th") |>
  group_by(judge_gender, clerk_gender) |>
  summarise(n = n()) |>
  mutate(pct = round(n/sum(n), digit = 2)) |>
  ggplot(aes(x = clerk_gender, y = pct, fill = clerk_gender)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_fill_brewer(palette="Greys") +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~judge_gender)
gender_clerks_judges
ggsave("report/The_Czech_Constitutional_Court_Database_files/figure-latex/gender_clerks_judges.eps", gender_clerks_judges, device = "eps")
ggsave("presentation_plot.png", gender_clerks_judges)

female_clerks = read_csv(file = "data/educ_uoe_grad10_page_linear.csv") |>
  summarise(ratio = mean(OBS_VALUE))

female_clerks$type = "Graduates"

female_clerks$n = read_csv(file = "data/educ_uoe_grad02_page_linear.csv") |> summarise(n = mean(OBS_VALUE)) |> pluck(1,1) |> round(digits = 0)

female_clerks = female_clerks |>
  bind_rows(data_clerks |>
              summarise(ratio = 100*nrow(data_clerks |> filter(clerk_gender == "F") |> distinct(clerk_name))/
                          (length(unique(data_clerks$clerk_name))),
                        n = length(unique(data_clerks$clerk_name))) |>
              mutate(type = "Clerks")) |>
  bind_rows(data_judges |>
             summarise(ratio = 100*nrow(data_judges |> filter(judge_gender == "F") |> distinct(judge_name))/
                         (length(unique(data_judges$judge_name))),
                       n = length(unique(data_judges$judge_name))) |>
             mutate(type = "Judges")) |>
  relocate(type) |>
  mutate(ratio = round(ratio, digits = 1))

female_clerks

z_test = function(p1, p2, n1, n2, alpha = 0.025){
  p_hat = (p1+p2)/(n1+n2)
  
  standard_error = sqrt(p_hat*(1-p_hat)*(1/n1+1/n2))
  
  z = ((p1-p2)-0)/standard_error
  
  critical_value1 = qnorm(alpha, lower.tail=TRUE)
  critical_value2 = qnorm(alpha, lower.tail=FALSE)
  
  if(z > critical_value2 | z < critical_value1){
    print(paste0("Reject null hypothesis. The Z value was: ", z, "The critical value for ", alpha, " was ", critical_value1, " and ", critical_value2))
  } else{print("Null hypothesis couldn't be rejected")}
}

z_test(p1 = female_clerks$ratio[1], p2 = female_clerks$ratio[2], n1 = female_clerks$n[1], n2 = female_clerks$n[2])
z_test(p1 = female_clerks$ratio[1], p2 = female_clerks$ratio[3], n1 = female_clerks$n[1], n2 = female_clerks$n[3])


# VIGNETTE - COALITIONS ---------------------------------------------------
source("../ccc_separate_opinions/scripts/load_data.R")

coalition_one = c("Kateřina Šimáčková", "Vojtěch Šimíček", "Ludvík David", "Jaromír Jirsa", "David Uhlíř", "Jiří Zemánek", "Tomáš Lichovník", "Jan Filip", "Milada Tomková", "Pavel Šámal")
coalition_two = c("Radovan Suchánek","Vladimír Sládeček","Josef Fiala","Jan Musil","Jaroslav Fenyk","Pavel Rychetský")

data_coalition = data |>
  filter(formation != "Plenum") |>
  group_by(doc_id) |>
  filter(all(judge_name %in% c(coalition_one, coalition_two))) |>
  ungroup() |>
  mutate(coalition = if_else(judge_name %in% coalition_one, 1, 0)) |>
  group_by(doc_id) |>
  summarise(
    coalition = sum(coalition),
    coalition = case_when(coalition == 3 ~ "full_coal",
                          coalition == 0 ~ "full_coal",
                          coalition == 1 | 2 ~ "mixed_coal") |> as_factor(),
    separate_opinion = if_else(any(separate_opinion == 1), 1, 0)) |>
  ungroup()

coalition_tab = data_coalition |>
  group_by(coalition) |>
  summarise(n_separate_opinion = sum(separate_opinion),
    n = n(),
    ratio = n_separate_opinion/n,
    percent = scales::label_percent(accuracy = 0.01)(ratio)) |>
  mutate(coalition = c("Mixed", "Full"))

# The two-sided z-test
z_test_result = prop.test(x = c(coalition_tab$n_separate_opinion[1], coalition_tab$n_separate_opinion[2]), n = c(coalition_tab$n[1], coalition_tab$n[2]), correct = F)
z_test_result$estimate
z_test_result$p.value

rm(list=ls(pattern="^data"))
save.image(file = "report/descriptive_statistics.RData")

