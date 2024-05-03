library(codebookr)
library(tidyverse)

ccc_datasets = xlsx::read.xlsx("report/codebook.xlsx", 1)
ccc_variables = xlsx::read.xlsx("report/codebook.xlsx", 2)

# create a codebook
codebookr::create_codebook(
  file_path = "report/ccc_database_codebook.tex",
  datasets_input = ccc_datasets,
  variables_input = ccc_variables,
  title_text = "The Czech Constitutional Court Database \\\\ The CCC Database",
  version_text = "1.0",
  footer_text = "The Czech Constitutional Court Database \\hspace{5pt} | \\hspace{5pt} ANONYMIZED",
  author_names = "ANONYMIZED",
  theme_color = "#6e537e",
  heading_font_size = 30,
  subheading_font_size = 10,
  title_font_size = 16,
  table_of_contents = TRUE,
  include_variable_type = TRUE
)


