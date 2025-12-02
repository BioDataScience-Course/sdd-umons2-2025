# Purl-knit SDD I

# - Extract important R code from the R Markdown files using purl in an R script
# - Knit it to PDF using purl
# - Place these files in ...

files_noext <- c("01-reg-lineaire")

for (f in files_noext) {
  rmdfile <- paste0(f, ".Rmd")
  rfile <- paste0(f, "-b.R")
  mdfile <- paste0(f, "-b.md")
  htmlfile <- paste0(f, "-b.html")

  # Make sure old files are removed
  unlink(rfile)
  unlink(mdfile)
  unlink(htmlfile)

  # Extract R code using purl
  knitr::purl(rmdfile, output = rfile, documentation = 0L)

  # Eliminate multiple empty lines in this R script
  rlines <- readLines(rfile)
  rlines_clean <- rlines[!grepl("^\\s*$", rlines) |
    c(TRUE, diff(grepl("^\\s*$", rlines)) != 0)]
  writeLines(rlines_clean, rfile)

  # Knit with spin to PDF
  knitr::spin(rfile, precious = FALSE)
  unlink(mdfile)
}
