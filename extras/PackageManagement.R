# Create manual and vignettes
unlink("extras/Trajectories.pdf")
system("R CMD Rd2pdf ./ --output=extras/Trajectories.pdf")

rmarkdown::render("vignettes/Trajectories-vignette.Rmd",
                  output_file = "../inst/doc/Trajectories-vignette.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE, toc_depth = 3,
                                          number_sections = TRUE))
