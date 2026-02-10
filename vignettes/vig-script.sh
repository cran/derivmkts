#!/bin/bash

Rscript -e "rmarkdown::render('derivmkts-vignette.Rmd', output_format=c('bookdown::pdf_document2', 'bookdown::html_document2'))"
