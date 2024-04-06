rmarkdown::render("report/The_Czech_Constitutional_Court_Database.Rmd", output_file="The_Czech_Constitutional_Court_Database.pdf",
                  bookdown::pdf_document2(template = stevetemplates::templ_article2(), 
                                          keep_tex = FALSE,
                                          latex_engine = "xelatex",
                                          toc = TRUE, number_sections = TRUE))

