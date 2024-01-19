library(codebookr)

# view example data
View(codebookr::euip_datasets)
View(codebookr::euip_variables)

# create a codebook
codebookr::create_codebook(
  file_path = "codebook.pdf",
  datasets_input = euip_datasets,
  variables_input = euip_variables,
  title_text = "The European Union Infringement Procedure \\\\ (EUIP) Database",
  version_text = "1.0",
  footer_text = "The EUIP Database Codebook \\hspace{5pt} | \\hspace{5pt} Joshua C. Fjelstul, Ph.D.",
  author_names = "Joshua C. Fjelstul, Ph.D.",
  theme_color = "#4D9FEB",
  heading_font_size = 30,
  subheading_font_size = 10,
  title_font_size = 16,
  table_of_contents = TRUE,
  include_variable_type = TRUE
)
