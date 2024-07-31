library(tidyverse)
library(patchwork)

# Load data ---------------------------------------------------------------
data_metadata = read_rds("../data/ccc_database/rds/ccc_metadata.rds")
data_dissents = read_rds("../data/ccc_database/rds/ccc_separate_opinions.rds")
data_compositions = read_rds("../data/ccc_database/rds/ccc_compositions.rds")
data_separate_opinions = read_rds("../data/ccc_database/rds/ccc_separate_opinions.rds")


# Compositions check ------------------------------------------------------
# Counts number of decisions with certain number of judges on the bench
check_compositions = data_compositions |>
  group_by(doc_id) |>
  count() |>
  right_join(data_metadata |> select(doc_id, year_decision, formation)) |>
  mutate(n = replace_na(n, 0),
         term = case_when(between(year_decision, 1993, 1993+10) ~ "1st",
                          between(year_decision, 2004, 2004+10) ~ "2nd",
                          between(year_decision, 2014, 2014+10) ~ "3rd"),
         formation = if_else(formation == "Plenum", "Plenum", "Panel"))
check_compositions

# Plot of the previously wrangled data
check_compositions_fig_panel = check_compositions |>
  filter(formation == "Panel") |>
  ggplot(aes(x = n)) +
  geom_bar() +
  facet_wrap(~term) +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  labs(x = NULL,
       y = NULL) +
  ggtitle("Chambers")
check_compositions_fig_panel

check_compositions_fig_plenum = check_compositions |>
  filter(formation == "Plenum") |>
  ggplot(aes(x = n)) +
  geom_bar() +
  facet_wrap(~term) +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  labs(x = "# justices found in the text of a decision",
       y = NULL) +
  ggtitle("Plenum")
check_compositions_fig_plenum

check_compositions_fig = check_compositions_fig_panel / check_compositions_fig_plenum + plot_annotation(tag_levels = 'A')
check_compositions_fig


check_compositions_tab = check_compositions |>
  group_by(formation, term) |>
  summarise(incorrect = length(n[n %in% c(0,2)]),
            correct = length(n[n %in% c(1,3,5:15)]),
            proportion = scales::label_percent(accuracy = 0.01)(incorrect/(correct+incorrect))) |>
  arrange(term)

check_compositions_tab

# Separate opinions check -------------------------------------------------
check_separate_opinions = data_separate_opinions |>
  left_join(data_metadata |> select(doc_id, year_decision, formation)) |>
  mutate(correct = if_else(is.na(dissenting_group), "Incorrect", "Correct"),
         term = case_when(between(year_decision, 1993, 1993+10) ~ "1st",
                          between(year_decision, 2004, 2004+10) ~ "2nd",
                          between(year_decision, 2014, 2014+10) ~ "3rd"),
         formation = if_else(formation == "Plenum", "Plenum", "Panel")) |>
  group_by(formation, term, correct) |>
  count() |> 
  pivot_wider(names_from = correct, values_from = n) |>
  ungroup() |>
  mutate(across(everything(), replace_na, replace = 0)) |>
  mutate(proportion = scales::label_percent(accuracy = 0.01)(Incorrect/(Correct+Incorrect))) |>
  arrange(term)
check_separate_opinions

rm(list=ls(pattern="^data"))
save.image(file = "report/validity_check.RData")



