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

extract_header_info <- function(pdf_file) {
  # Extract the entire PDF as plain text (one element per page)
  pdf_pages <- pdf_text(pdf_file)

  # Combine pages into one text string and split into individual lines
  all_text <- paste(pdf_pages, collapse = "\n")
  lines <- unlist(strsplit(all_text, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  # Find the starting marker ("Completed by:") and the ending marker ("Please review")
  start_idx <- grep("^Completed by:", lines)
  end_idx <- grep("^Please review", lines)

  if (length(start_idx) == 0) {
    stop("Start marker 'Completed by:' not found.")
  }
  if (length(end_idx) == 0) {
    stop("End marker 'Please review' not found.")
  }

  # Use the first occurrence of each marker
  start_idx <- start_idx[1]
  end_idx <- end_idx[1]

  if (start_idx >= end_idx) {
    stop("Start marker occurs after end marker.")
  }

  # Extract lines from the start marker up to (but not including) the end marker
  block_lines <- lines[start_idx:(end_idx - 1)]
  header_block <- paste(block_lines, collapse = " ")

  # Use a regular expression with capturing groups to parse the three pieces.
  pattern <- "Completed by:\\s*(.*?)\\s*Workflow step:\\s*(.*?)\\s*QA form submitted\\s*(.*)"
  m <- regexec(pattern, header_block)
  matches <- regmatches(header_block, m)

  if (length(matches) > 0 && length(matches[[1]]) >= 4) {
    completed_by <- matches[[1]][2]
    workflow <- matches[[1]][3]
    qa_submitted <- matches[[1]][4]
  } else {
    completed_by <- workflow <- qa_submitted <- NA
  }

  # Return the file name as the first column along with the header fields
  data.frame(
    file = basename(pdf_file),
    completed_by = completed_by,
    workflow = workflow,
    qa_submitted = qa_submitted,
    stringsAsFactors = FALSE
  )
}

################################# Example Usage --------------------------------
# Assume your PDFs are stored in a "pdfs" subdirectory of your project:
# pdf_file <- list.files(here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)[1]
# header_df <- extract_header_info(pdf_file)
# print(header_df)

########################## extract_pdf_data ####################################
extract_questions_context <- function(pdf_file) {
  # --- Extract PDF data with font info enabled from all pages ---
  pdf_pages <- pdf_data(pdf_file, font_info = TRUE)

  # --- Helper: Reconstruct lines from a page using the "font_name" column ---
  get_lines_from_page <- function(page_df) {
    # Group words by a rounded y coordinate to form lines
    page_df$line_y <- round(page_df$y, 1)
    page_df <- page_df[order(page_df$line_y, page_df$x), ]
    # Concatenate words on the same line
    lines_text <- tapply(page_df$text, page_df$line_y, paste, collapse = " ")
    # Flag a line as bold if any word's font_name contains "Bold" or "Medi" (but not "Ital")
    lines_bold <- tapply(page_df$font_name, page_df$line_y, function(x) {
      any(
        grepl("Bold|Medi", x, ignore.case = TRUE) &
          !grepl("Ital", x, ignore.case = TRUE)
      )
    })
    data.frame(
      text = as.character(lines_text),
      is_bold = as.logical(lines_bold),
      stringsAsFactors = FALSE
    )
  }

  # Process all pages and combine into one data frame
  lines_list <- lapply(pdf_pages, get_lines_from_page)
  all_lines <- do.call(rbind, lines_list)
  lines_text <- all_lines$text
  lines_bold <- all_lines$is_bold

  # --- Remove header block ---
  # Assume header block ends with a line starting with "Please review"
  header_end <- grep("^Please review", lines_text)
  if (length(header_end) > 0) {
    question_area <- lines_text[(header_end[1] + 1):length(lines_text)]
    question_area_bold <- lines_bold[(header_end[1] + 1):length(lines_bold)]
  } else {
    question_area <- lines_text
    question_area_bold <- lines_bold
  }

  # --- Define the new start point ---
  # The start is the first bold section that comes immediately before a response.
  # A response is defined as either:
  #   - a non-bold line containing a "Yes No" prompt, or
  #   - a non-bold line starting with "Please provide any other information"
  question_start <- NA
  for (i in seq_along(question_area)) {
    if (question_area_bold[i]) {
      if ((i + 1) <= length(question_area)) {
        if (
          (!question_area_bold[i + 1] &&
            (grepl("Yes\\s*No", question_area[i + 1], ignore.case = TRUE) ||
              grepl(
                "^Please provide any other information",
                question_area[i + 1]
              )))
        ) {
          question_start <- i
          break
        }
      } else {
        question_start <- i
        break
      }
    }
  }
  if (is.na(question_start)) {
    question_start <- 1
  }

  # Restrict the question area to start at our new start point
  q_text <- question_area[question_start:length(question_area)]
  q_bold <- question_area_bold[question_start:length(question_area_bold)]

  # Optionally, if there's a stop marker ("Please provide any other information"), limit processing up to that line.
  stop_marker <- grep("^Please provide any other information", q_text)
  if (length(stop_marker) > 0) {
    q_text <- q_text[1:(stop_marker[1] - 1)]
    q_bold <- q_bold[1:(stop_marker[1] - 1)]
  }

  # --- Extract questions and their context ---
  # New heuristic:
  #   - A question is defined as a contiguous block of bold lines.
  #   - Then, if the next line is non-bold and is a response indicator (Yes No or text prompt),
  #     all subsequent non-bold lines until the next bold block are collected as context.
  questions <- list()
  contexts <- list()

  i <- 1
  while (i <= length(q_text)) {
    if (q_bold[i]) {
      # Accumulate all contiguous bold lines as the question
      question_lines <- character(0)
      while (i <= length(q_text) && q_bold[i]) {
        question_lines <- c(question_lines, q_text[i])
        i <- i + 1
      }
      current_question <- paste(question_lines, collapse = " ")
      current_context <- NA

      # If the next line is non-bold and is a response indicator, collect subsequent non-bold lines as context.
      if (
        (i) <= length(q_text) &&
          !q_bold[i] &&
          (grepl("Yes\\s*No", q_text[i], ignore.case = TRUE) ||
            grepl("^Please provide any other information", q_text[i]))
      ) {
        # Skip the response indicator line.
        i <- i + 1
        context_lines <- character(0)
        while (i <= length(q_text) && !q_bold[i]) {
          context_lines <- c(context_lines, q_text[i])
          i <- i + 1
        }
        if (length(context_lines) > 0)
          current_context <- paste(context_lines, collapse = " ")
      }

      questions[[length(questions) + 1]] <- current_question
      contexts[[length(contexts) + 1]] <- current_context
    } else {
      i <- i + 1
    }
  }

  # --- Build the output data frame ---
  result <- list(file = basename(pdf_file))
  for (k in seq_along(questions)) {
    result[[paste0("q", k)]] <- questions[[k]]
    result[[paste0("context", k)]] <- contexts[[k]]
  }

  as.data.frame(result, stringsAsFactors = FALSE)
}

################################# Example Usage --------------------------------
# Assume your PDFs are stored in the "pdfs" subdirectory.
# pdf_files <- list.files(here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)
# first_pdf_file <- pdf_files[1]
# qc_df <- extract_questions_context(first_pdf_file)
# print(qc_df)

########################## extract_answers_from_pdf ############################
extract_answers_by_question <- function(pdf_file) {
  # Use OCR to extract text from all pages.
  ocr_pages <- pdf_ocr_text(pdf_file)
  full_text <- paste(ocr_pages, collapse = "\n")

  # Split into lines and clean whitespace.
  lines <- str_trim(unlist(str_split(full_text, "\n")))
  lines <- lines[lines != ""]

  answers <- list()
  i <- 1
  while (i <= length(lines)) {
    # Case 1: A candidate Yes/No answer line (contains both "Yes" and "No").
    if (
      str_detect(lines[i], regex("Yes", ignore_case = TRUE)) &&
        str_detect(lines[i], regex("No", ignore_case = TRUE))
    ) {
      answers[[length(answers) + 1]] <- lines[i]
      i <- i + 1
    } else if (
      # Case 2: A candidate textbox answer that begins with the prompt.
      str_detect(
        lines[i],
        regex(
          "^Please provide any other information that you think may be relevant",
          ignore_case = TRUE
        )
      )
    ) {
      answer_block <- lines[i]
      i <- i + 1
      # Continue concatenating subsequent non-empty lines until a blank is found or a new candidate answer is detected.
      while (
        i <= length(lines) &&
          lines[i] != "" &&
          !(str_detect(lines[i], regex("Yes", ignore_case = TRUE)) &&
            str_detect(lines[i], regex("No", ignore_case = TRUE))) &&
          !str_detect(
            lines[i],
            regex("^Please provide any other information", ignore_case = TRUE)
          )
      ) {
        answer_block <- paste(answer_block, lines[i], sep = " ")
        i <- i + 1
      }
      answers[[length(answers) + 1]] <- answer_block
    } else {
      i <- i + 1
    }
  }

  # Build the output data frame with the file name as the first column.
  result <- data.frame(file = basename(pdf_file), stringsAsFactors = FALSE)
  for (k in seq_along(answers)) {
    result[[paste0("question_", k)]] <- answers[[k]]
  }

  # pivot longer to make the next step easier
  result <- result %>%
    pivot_longer(
      cols = starts_with("question_"),
      names_to = "question_number",
      values_to = "answer",
      values_drop_na = TRUE
    )

  return(result)
}

################################# Example Usage --------------------------------
# List all PDF files in the "pdfs" subdirectory.
pdf_files <- list.files(here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)

# For testing, process the first PDF file.
first_pdf_file <- pdf_files[1]
answers_df <- extract_answers_by_question(first_pdf_file)
print(answers_df)
