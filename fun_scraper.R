# All clear!
rm(list = ls())

# libraries
library(tidyverse)
library(stringr)
library(pdftools)
library(here)
library(writexl)

################## FUNCTION TO STRIP ONE DOC ###################################
#######################################################################
# FUNCTION: extract_questions_from_pdf
#
# Description:
#   This function processes a PDF file to extract questions and their
#   corresponding answer lines from OCR text. It combines OCR text
#   across all pages, identifies candidate questions based on whether
#   a line ends with a '?' or is immediately followed by a line containing
#   both "Yes" and "No", and then extracts the first subsequent line that
#   contains both "Yes" and "No" as the answer. The function further
#   applies regex rules to determine a 'yn' (yes/no) flag:
#
#     1. "Yes" is considered checked if followed by any of these markers:
#        (x), («), ««), (««), or «)
#
#     2. If "No" is followed by © (after optional whitespace), this is
#        interpreted as an indication that "Yes" is checked.
#
#     3. Otherwise, if "No" is followed by any of the checked markers,
#        it is interpreted as "No" being checked.
#
#   The output is a tibble (data frame) with columns for the page number,
#   question text, answer text, and the computed 'yn' value.
#
# Usage:
#   qa_df <- extract_questions_from_pdf("path/to/your/pdf_file.pdf")
#
# Parameters:
#   pdf_file_path : string
#       The file path to the PDF document to be processed.
#
# Returns:
#   A tibble with columns:
#     - page: Page number from which the question was extracted.
#     - question: The extracted question text.
#     - answer: The corresponding answer line.
#     - yn: A flag ("y" or "n") indicating whether Yes or No was checked,
#           determined by the specified regex rules.
#######################################################################
extract_questions_from_pdf <- function(pdf_file_path) {
  # Extract OCR text for all pages at once.
  # pdf_ocr_text returns a character vector (one element per page).
  ocr_pages <- pdf_ocr_text(pdf_file_path)
  total_pages <- length(ocr_pages)

  # Combine the pages into a single data frame of lines with a page number indicator.
  combined_lines <- map2_dfr(ocr_pages, seq_along(ocr_pages),
                             ~ tibble(page = .y,
                                      line = str_split(.x, "\n")[[1]] %>%
                                        str_trim() %>%
                                        discard(~ .x == "")))

  # We'll extract candidate questions from the combined text.
  # Candidate questions are either:
  # 1. Lines that end with a "?"
  # 2. Lines that are immediately followed by a line that contains both "Yes" and "No"
  lines <- combined_lines$line

  # Identify indices by two methods:
  question_indices_1 <- grep("\\?$", lines)
  question_indices_2 <- which(sapply(1:(length(lines)-1), function(i) {
    str_detect(lines[i+1], "Yes") & str_detect(lines[i+1], "No")
  }))
  question_indices <- sort(unique(c(question_indices_1, question_indices_2)))

  # Define a helper function to extract the answer line.
  # It will search forward from the candidate question index until it finds a line containing both "Yes" and "No".
  extract_answer <- function(idx, lines) {
    for (i in (idx + 1):length(lines)) {
      # If we encounter a new question candidate, break out.
      if (str_detect(lines[i], "\\?$")) break
      if (str_detect(lines[i], "Yes") & str_detect(lines[i], "No")) {
        return(lines[i])
      }
    }
    return(NA_character_)
  }

  # Extract questions and answers.
  questions <- lines[question_indices]
  answers <- map_chr(question_indices, ~ extract_answer(.x, lines))

  # For the page value, we take the page number from our combined_lines for the question line.
  pages <- combined_lines$page[question_indices]

  # Build the data frame
  qa_df <- tibble(
    page = pages,
    question = questions,
    answer = answers
  )

  # Now add the yn column using your regex rules:
  # 1. "Yes" is checked if followed by one of: (x), («), ««), (««), or «)
  # 2. If "No" is followed by © (after optional whitespace), then that implies yes is checked.
  # 3. Otherwise, "No" is checked if followed by one of the checked patterns.
  qa_df <- qa_df %>%
    mutate(yn = case_when(
      str_detect(answer, regex("Yes\\s*(\\(x\\)|\\(«\\)|««\\)|\\(««\\)|«\\))", ignore_case = TRUE)) ~ "y",
      str_detect(answer, regex("No\\s*©", ignore_case = TRUE)) ~ "y",
      str_detect(answer, regex("No\\s*(\\(x\\)|\\(«\\)|««\\)|\\(««\\)|«\\))", ignore_case = TRUE)) ~ "n",
      TRUE ~ NA_character_
    ))

  return(qa_df)
}

########################## USAGE ################################################
# List all PDF files in the "pdfs" folder
pdf_files <- list.files(path = here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty list to store results for each file
results_list <- list()

# Loop through each PDF file, run the extraction function,
# and store the results in a list with a unique sheet name.
for (i in seq_along(pdf_files)) {
  cat("Processing file:", pdf_files[i], "\n")
  # Use the base name of the PDF (without extension) as the sheet name.
  sheet_name <- tools::file_path_sans_ext(basename(pdf_files[i]))
  results_list[[sheet_name]] <- extract_questions_from_pdf(pdf_files[i])
}

# Save the results to an Excel workbook where each sheet corresponds to one PDF file's results
library(writexl)
write_xlsx(results_list, path = here("results.xlsx"))

cat("All files processed. Results saved in results.xlsx\n")
