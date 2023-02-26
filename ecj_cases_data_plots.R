xfun::pkg_attach2("textrank", "wordcloud", "ggplot2", "tidyverse", "RMySQL", "lubridate", "tm", "RColorBrewer")
matches_joined <- read.csv("data/matches_joined.csv")
data_metadata <- read.csv("data/decisions_US_metadata.csv")
ecj_judgments <- read.csv("data/CJEU-database-platform-csv-v-0-1/CJEU-database-platform-judgments-v-0-1.csv")

# Amount of ECJ References by Judge Rapporteur
cc_third <- c("David Ludvík", "Fenyk Jaroslav", "Fiala Josef", "Filip Jan", "Jirsa Jaromír", "Rychetský Pavel", "Šámal Pavel", "Šimáčková Kateřina", "Šimíček Vojtěch", "Sládeček Vladimír", "Suchánek Radovan", "Tomková Milada", "Uhlíř David", "Zemánek Jiří", "Lichovník Tomáš", "Musil Jan")

matches_joined %>% 
  group_by(judge_rapporteur.y, year_cc) %>% 
  summarize(count = n()) %>% 
  {. ->> matches_joined_judges} # this saves the count data

matches_joined_judges_total <- data_metadata %>% 
  group_by(judge_rapporteur, year(date_decision)) %>% 
  summarize(count_total = n()) %>%
  rename(
    judge_rapporteur.y = "judge_rapporteur",
    year_cc = "year(date_decision)")

matches_joined_judges <- left_join(matches_joined_judges, matches_joined_judges_total) %>%
  mutate(freq = count / count_total)

ggplot(data = subset(matches_joined_judges, subset = judge_rapporteur.y %in% cc_third & year_cc > 2013), mapping = aes(x = year_cc , y = count)) +
  geom_line() +
  facet_wrap(~judge_rapporteur.y, ncol = 4) + labs(x = NULL, y = NULL,
                                                   title = "Amount of ECJ references by judge rapporteur",
                                                   caption = "Source: Authors",
                                                   subtitle = "Only the 3rd CC between 2014 and 2022 is included")

ggplot(data = subset(matches_joined_judges, subset = judge_rapporteur.y %in% cc_third & year_cc > 2013), mapping = aes(x = year_cc , y = freq)) +
  geom_line() +
  facet_wrap(~judge_rapporteur.y, ncol = 4) + labs(x = NULL, y = NULL,
                                                   title = "Amount of ECJ references by judge rapporteur relative to their yearly caseload",
                                                   caption = "Source: Authors",
                                                   subtitle = "Only the 3rd CC between 2014 and 2022 is included") + 
  scale_y_continuous(labels = scales::percent)

# Total N cases per year
matches_joined_count_total <- data_metadata %>% 
  group_by(year_cc) %>% 
  summarize(count_total = n())

# Data prep for freq/outcome histogram
matches_joined  %>% 
  group_by(outcome, year_cc) %>% 
  summarize(count = n(),
            count_distinct = n_distinct(doc_id)) %>%
  {. ->> matches_joined_outcome}

matches_joined_freq_relative <- data_metadata %>% 
  group_by(outcome, year_cc) %>% 
  summarize(count_relative = n())

matches_joined_outcome <- left_join(matches_joined_freq_relative, matches_joined_outcome)
matches_joined_outcome <- left_join(matches_joined_outcome, matches_joined_count_total)
matches_joined_outcome$count <- matches_joined_outcome$count %>% replace_na(0)

matches_joined_outcome <- matches_joined_outcome %>%
  group_by(year_cc) %>% 
  mutate(freq = count/count_relative,
         freq_total = count/count_total,
         freq_distinct = count_distinct/count_relative)

ggplot(data = matches_joined_outcome, mapping = aes(x = year_cc, y = count, fill = outcome)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  labs(
    title = "Amount of ECJ References by Outcome of the CC Case",
    subtitle = "Amount in Absolute Terms",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Outcome")

ggplot(data = matches_joined_outcome, mapping = aes(x = year_cc, y = count, fill = outcome)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  labs(
    title = "Amount of ECJ References by Outcome of the CC Case",
    subtitle = "Amount in Absolute Terms",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Outcome")

ggplot(data = matches_joined_outcome, mapping = aes(x = year_cc, y = freq, fill = outcome)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Amount of ECJ References by Outcome of the CC Case",
    subtitle = "Amount in frequency relative to the sum of CC cases in a given year
    of a given outcome",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Outcome")

ggplot(data = matches_joined_outcome, mapping = aes(x = year_cc, y = freq_distinct, fill = outcome)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  labs(
    title = "Amount of ECJ References by Outcome of the CC Case",
    subtitle = "Amount in Relative Frequency of Distinct CC Cases",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Outcome")

ggplot(data = matches_joined_outcome, mapping = aes(x = year_cc, y = freq_total, fill = outcome)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Amount of ECJ References by Outcome of the CC Case",
    subtitle = "Amount in frequency relative to the sum of CC cases in a given year",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Outcome")

# Data prep for freq/type of proceedings histogram
matches_joined  %>% 
  mutate(type_proceedings = case_when(
    grepl("stížnostech", type_proceedings) ~ "Concrete",
    !grepl("stížnostech", type_proceedings) ~ "Abstract"
  )) %>%
  group_by(type_proceedings, year_cc) %>% 
  summarize(count = n(),
            count_distinct = n_distinct(doc_id)) %>%
  {. ->> matches_joined_type_proceedings}

matches_joined_type_proceedings_freq_relative <- subset(data_metadata, type_proceedings %in% c("O ústavních stížnostech", "O zrušení zákonů a jiných právních předpisů", "O souladu mezinárodních smluv")) %>% 
  mutate(type_proceedings = case_when(
    grepl("stížnostech", type_proceedings) ~ "Concrete",
    !grepl("stížnostech", type_proceedings) ~ "Abstract"
  )) %>%
  group_by(type_proceedings, year_cc) %>% 
  summarize(count_relative = n())

matches_joined_type_proceedings <- left_join(matches_joined_type_proceedings_freq_relative, matches_joined_type_proceedings)
matches_joined_type_proceedings <- left_join(matches_joined_type_proceedings, matches_joined_count_total)
matches_joined_type_proceedings$count <- matches_joined_type_proceedings$count %>% replace_na(0)


matches_joined_type_proceedings <- matches_joined_type_proceedings %>%
  group_by(year_cc) %>% 
  mutate(freq = count/count_relative,
         freq_total = count/count_total,
         freq_distinct = count_distinct/count_relative)

ggplot(data = matches_joined_type_proceedings, mapping = aes(x = year_cc, y = count, fill = type_proceedings)) +
  geom_col() + 
  scale_fill_brewer(palette="Pastel1") +
  labs(
    title = "Amount of ECJ References by Type of CC Review",
    subtitle = "Absolute Amount",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Type of Review")

ggplot(data = matches_joined_type_proceedings, mapping = aes(x = year_cc, y = freq, fill = type_proceedings)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Amount of ECJ References by Type of CC Review",
    subtitle = "Amount in frequency relative to the sum of CC cases in a given year
    of a given outcome",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Type of Review")

ggplot(data = matches_joined_type_proceedings, mapping = aes(x = year_cc, y = freq_distinct, fill = type_proceedings)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Amount of ECJ References by Type of CC Review",
    subtitle = "Amount in frequency relative of distinct CC cases",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Type of Review")

ggplot(data = matches_joined_type_proceedings, mapping = aes(x = year_cc, y = freq_total, fill = type_proceedings)) +
  geom_col() +
  scale_fill_brewer(palette="Pastel1") +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Amount of ECJ References by Type of CC Review",
    subtitle = "Amount in frequency relative to the sum of CC cases in a given year",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "Type of Review")

# Formation of ECJ
matches_joined  %>%
  mutate(formation = case_when(
    grepl("Chamber" , formation) & !grepl("Grand Chamber", formation) ~ "Chamber",
    grepl("Grand Chamber", formation) ~ "Grand Chamber",
    grepl("Plenary Assembly", formation) ~ "Plenary Assembly",
    grepl("single judge", formation) ~ "Single Judge",
  )) %>%
  group_by(formation, year_cc) %>% 
  summarize(count = n()) %>%
  ggplot(data = ., mapping = aes(x = year_cc, y = count, fill = formation)) +
  geom_col() + 
  scale_fill_brewer(palette="Pastel1") +
  labs(
    title = "Amount of ECJ References by ECJ Formation",
    subtitle = "Absolute Amount",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "ECJ Formation")


# Most cited decisions
matches_joined  %>%
  group_by(case_name) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(30) %>%
  ggplot(data = ., mapping = aes(x = reorder(case_name, count), y = count)) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Top 20 Cited ECJ Decisions",
    caption = "Source: Authors",
    x = NULL,
    y = NULL)

# Subject matter
matches_joined  %>%
  group_by(subject_matter_category) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10) %>%
  ggplot(mapping = aes(x = reorder(subject_matter_category, count), y = count)) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Top 10 Cited ECJ Subject Matters",
    caption = "Source: Authors",
    x = NULL,
    y = NULL)

# ECJ Judge Rapporteur
matches_joined  %>%
  group_by(judge_rapporteur.x) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15) %>%
  ggplot(mapping = aes(x = reorder(judge_rapporteur.x, count), y = count)) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Top 15 Cited ECJ Judge Rapporteurs",
    caption = "Source: Authors",
    x = NULL,
    y = NULL)

# Referring Member States
matches_joined  %>%
  filter(!referring_member_state == "not applicable") %>%
  group_by(referring_member_state) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15) %>%
  ggplot(mapping = aes(x = reorder(referring_member_state, count), y = count)) +
  geom_point() +
  coord_flip() +  labs(
    title = "Top 15 Cited ECJ Referring Member States",
    caption = "Source: Authors",
    x = NULL,
    y = NULL)

# Type of ECJ Proceedings
matches_joined  %>%
  mutate(procedure_abb = case_when(
    grepl("reference", procedure) ~ "Reference for a Preliminary Ruling",
    grepl("fulfill", procedure) ~ "Action for Failrure to Fulfill Obligations",
    grepl("annulment", procedure) ~ "Action for Annulment",
    TRUE ~ "Other")) %>%
  group_by(procedure_abb, year_cc) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15) %>%
  ggplot(mapping = aes(x = year_cc, y = count, fill = procedure_abb)) +
  geom_col() + 
  scale_fill_brewer(palette="Pastel1") +
  labs(
    title = "Amount of ECJ References by ECJ Type of Procedure",
    subtitle = "Absolute Amount",
    caption = "Source: Authors",
    x = "Year of the CC Decision",
    y = NULL,
    fill = "ECJ Type of Procedure")

ggplot(matches_joined, aes(x = year_cc, y = proceeding_year)) + 
  geom_bin2d() + 
  scale_fill_gradient(low = "purple", high = "white") +
  labs(x = "Year of the referring CC decision", y = "Year of the cited ECJ decision",
       title = "Heatmap of the Dates of Reference",
       fill = "Count",
       caption = "Source: Authors")

# Compare means
matches_joined %>% 
  group_by(doc_id, outcome) %>%
  summarize(count = n()) %>%
  group_by(outcome) %>%
  summarize(
    mean = mean(count, na.rm = TRUE),
      sd = sd(count, na.rm = TRUE)) %>%
  {.  ->> matches_joined_outcome_mean}

matches_joined_outcome_mean <- matches_joined %>% 
  group_by(outcome) %>%
  summarize(
    count = n()
  ) %>% 
  left_join(., matches_joined_outcome_mean)

matches_joined %>%
  group_by(doc_id, outcome) %>%
  summarize(count = n()) %>%
{.  ->> matches_joined_outcome_mean_analysis}

with(matches_joined_outcome_mean_analysis, shapiro.test(count[outcome == "granted"]))

res <- wilcox.test(count ~ outcome, data = matches_joined_outcome_mean_analysis,
                   exact = FALSE)
res

ggplot(matches_joined_outcome_mean_analysis, mapping = aes(x = outcome, y = count, fill = outcome)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel1") +
  coord_cartesian(ylim = c(0,15)) +
  theme(legend.position="none") +
  labs(
    x="Outcome",
    y="Number of Citations in a Case"
  )

# Difference in means type of proceedings
matches_joined %>% 
  mutate(type_proceedings = case_when(
    grepl("stížnostech", type_proceedings) ~ "Concrete",
    !grepl("stížnostech", type_proceedings) ~ "Abstract"
  )) %>%
  group_by(doc_id, type_proceedings) %>%
  summarize(count = n()) %>%
  group_by(type_proceedings) %>%
  summarize(
    mean = mean(count, na.rm = TRUE),
    sd = sd(count, na.rm = TRUE)) %>%
  {.  ->> matches_joined_type_proceedings_mean}

matches_joined_type_proceedings_mean <- matches_joined %>% 
  mutate(type_proceedings = case_when(
    grepl("stížnostech", type_proceedings) ~ "Concrete",
    !grepl("stížnostech", type_proceedings) ~ "Abstract"
  )) %>%
  group_by(type_proceedings) %>%
  summarize(
    count = n()
  ) %>% 
  left_join(., matches_joined_type_proceedings_mean)

matches_joined %>%
  group_by(doc_id, type_proceedings) %>%
  mutate(type_proceedings = case_when(
    grepl("stížnostech", type_proceedings) ~ "Concrete",
    !grepl("stížnostech", type_proceedings) ~ "Abstract"
  )) %>%
  summarize(count = n()) %>%
  {.  ->> matches_joined_type_proceedings_mean_analysis}

ggplot(matches_joined_type_proceedings_mean_analysis, mapping = aes(x = type_proceedings, y = count, fill = type_proceedings)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel1") +
  coord_cartesian(ylim = c(0,15)) +
  theme(legend.position="none") +
  labs(
    x="Type of Proceedings",
    y="Number of Citations in a Case"
  )

with(matches_joined_type_proceedings_mean_analysis, shapiro.test(count[type_proceedings == "Abstract"]))
with(matches_joined_type_proceedings_mean_analysis, shapiro.test(count[type_proceedings == "Concrete"]))
  


  # # Relative formation
# matches_joined  %>%
#   mutate(formation = case_when(
#     grepl("Chamber" , formation) & !grepl("Grand Chamber", formation) ~ "Chamber",
#     grepl("Grand Chamber", formation) ~ "Grand Chamber",
#     grepl("Plenary Assembly", formation) ~ "Plenary Assembly",
#     grepl("single judge", formation) ~ "Single Judge",
#   )) %>%
#   group_by(formation, proceeding_year) %>% 
#   summarize(count = n()) %>%
#   {. ->> matches_joined_formation}
# 
# ecj_judgments  %>%
#   mutate(formation = case_when(
#     grepl("Chamber" , formation) & !grepl("Grand Chamber", formation) ~ "Chamber",
#     grepl("Grand Chamber", formation) ~ "Grand Chamber",
#     grepl("Plenary Assembly", formation) ~ "Plenary Assembly",
#     grepl("single judge", formation) ~ "Single Judge",
#   )) %>%
#   group_by(formation, proceeding_year) %>% 
#   summarize(count_total = n()) %>%
#   {. ->> matches_joined_formation_total}
# 
# matches_joined_formation <- left_join(matches_joined_formation, matches_joined_formation_total)
# 
# matches_joined_formation %>% mutate(
#   freq = count/count_total
# ) %>%
#   {. ->> matches_joined_formation}
# 
# ggplot(data = matches_joined_formation, mapping = aes(x = proceeding_year, y = freq, fill = formation)) +
#   geom_col() + 
#   scale_fill_brewer(palette="Pastel1") +
#   labs(
#     title = "Amount of ECJ References by ECJ Formation",
#     subtitle = "Absolute Amount",
#     caption = "Source: Authors",
#     x = "Year of the CC Decision",
#     y = NULL,
#     fill = "ECJ Formation") +
#   scale_y_continuous(labels = scales::percent)
#   
# # Wordcloud
# 
# wordcloud_text <- matches_joined$field_register %>% TermDocumentMatrix() %>% 
#   as.matrix()
# wordcloud_text <- sort(rowSums(wordclodu_text),decreasing=TRUE) %>%
#   data.frame(word = names(.),freq=.)
# 
# wordcloud(words = wordcloud_text$word, freq = wordcloud_text$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
