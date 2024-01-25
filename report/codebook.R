library(codebookr)

# view example data
View(codebookr::euip_datasets)
View(codebookr::euip_variables)

# create a codebook
codebookr::create_codebook(
  file_path = "report/codebook.tex",
  datasets_input = euip_datasets,
  variables_input = euip_variables,
  title_text = "The Czech Constitutional Court Dataset \\\\ The CCC Dataset",
  version_text = "1.0",
  footer_text = "The Czech Constitutional Court Dataset \\hspace{5pt} | \\hspace{5pt} Štěpán Paulík",
  author_names = "Štěpán Paulík",
  theme_color = "#FFF0F7",
  heading_font_size = 30,
  subheading_font_size = 10,
  title_font_size = 16,
  table_of_contents = TRUE,
  include_variable_type = TRUE
)
