xfun::pkg_attach2("tidyverse", "tidytext", "ggplot2", "progress", "tm", "foreach", "jsonlite", "rapport")

# Load data
load("data/US_texts.RData")
load("data/US_metadata.RData")
load("data/US_texts_paragraphs.RData")
load("data/US_dissents.RData")
load("data/US_judges.RData")

vady <- data_metadata %>% filter(grepl(pattern = "neodstraněné vady", x = .$type_verdict))