# All clear!
rm(list = ls())

# libraries
library(tidyverse)
library(stringr)
library(pdftools)
library(here)
library(writexl)
library(tesseract)
library(testthat)
library(here)

###################### rename_pdf_files ########################################
rename_pdf_files <- function(directory) {
  # List all files in the directory with full paths
  files <- list.files(directory, full.names = TRUE)

  # Loop through each file
  for (file in files) {
    base <- basename(file)
    # Use a regular expression to check for names like "100-671-517-fs-l1"
    # The regex looks for three groups of digits separated by hyphens followed by "-fs-l1"
    m <- regexpr("^([0-9]+-[0-9]+-[0-9]+-fs-l1)", base)

    if (m[1] != -1) {
      # if a match is found
      # Extract the matched prefix
      prefix <- regmatches(base, m)
      # Construct the new file name: "pdf-" + the prefix + ".pdf"
      new_name <- paste0("pdf-", prefix, ".pdf")
      new_path <- file.path(directory, new_name)

      # Rename the file
      if (file.rename(file, new_path)) {
        cat("Renamed", base, "to", new_name, "\n")
      } else {
        cat("Failed to rename", base, "\n")
      }
    }
  }
}

########################## EXAMPLE USE -----------------------------------------
# rename_pdf_files(here("pdfs"))

###################### process_pdf_files #######################################
# Function that renames PDFs based on a regex and then reads their text content
process_pdf_files <- function(directory) {
  # Rename files in the directory if they match the naming pattern
  rename_pdf_files(directory)

  # List all PDF files in the directory after renaming
  pdf_files <- list.files(directory, pattern = "\\.pdf$", full.names = TRUE)

  # Read the text from each PDF file using pdftools
  pdf_contents <- lapply(pdf_files, pdf_text)

  # Name the list with the base file names for easier reference
  names(pdf_contents) <- basename(pdf_files)

  return(pdf_contents)
}

########################## EXAMPLE USE -----------------------------------------
# pdf_texts <- process_pdf_files(here("pdfs"))
