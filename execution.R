########################### BROAD STRATEGY #####################################
# 1. READ IN ALL FILES FROM PDF FOLDER
# 2. NAME THE FILES AS THE CODE AT THE START OF THE FILE
# 3. USE TRADITIONAL PDF TOOLS TO EXTRACT ALL THE RELEVANT TEXT INTO DF
# 4. USE OCR TO EXTRACT ANY CHECKMARK RESPONSES
# 5. JOIN BY FILE NAME AND QUESTION NUMBER
# META: ONE FUNCTION FOR PDFTOOLS, ONE FOR OCR, LOOP TO APPEND DF
################################################################################
# python librares
py_require("pdf2image")
py_require("pytesseract")
py_require("pandas")

# load in python script
source_python("extract_answers_by_question_pytesseract.py")

# Note you might want to use to rename a batch of files to better names to begin:
pdf_texts <- process_pdf_files(here("pdfs"))

########################## SINGLE USE ##########################################
# list file names
pdf_files <- list.files(here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)

# get one
first_pdf_file <- pdf_files[1]

# get header box
header_df <- extract_header_info(first_pdf_file)

# get questions and context
qc_df <- extract_questions_context(first_pdf_file)
answers_df <- extract_answers_by_question(first_pdf_file)

# get processed answers
processed_answer_df <- answers_df |>
  mutate(processed_answer = sapply(answer, process_answer)) |>
  rename(field = question_number)

# join to questions and context df
joined_qa_df <- qc_df |>
  left_join(processed_answer_df, by = c("file", "field"))

# Python-based OCR answers
py_answers_df <- extract_answers_by_question_pytesseract(first_pdf_file) %>%
  rename(field = question_number)

# Join them in
joined_qa_df <- joined_qa_df %>%
  left_join(py_answers_df, by = c("file", "field"))

# question box
qbox_df <- extract_text_box_df(first_pdf_file, debug = TRUE)

# bind_rows to final df
final_single_df <- bind_rows(header_df, joined_qa_df, qbox_df)

############################ AT SCALE ##########################################
# List all PDF file names in the "pdfs" folder
pdf_files <- list.files(here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty list to store results for each PDF
final_list <- list()

# Loop through each PDF file
for (i in seq_along(pdf_files)) {
  pdf_file <- pdf_files[i]

  # Get header information
  header_df <- extract_header_info(pdf_file)

  # Get questions/context and answers by question
  qc_df <- extract_questions_context(pdf_file)
  answers_df <- extract_answers_by_question(pdf_file)

  # Process the answers and rename question_number column to field
  processed_answer_df <- answers_df %>%
    mutate(processed_answer = sapply(answer, process_answer)) %>%
    rename(field = question_number)

  # Join questions/context with processed answers
  joined_qa_df <- qc_df %>%
    left_join(processed_answer_df, by = c("file", "field"))

  # Python-based OCR
  py_answers_df <- extract_answers_by_question_pytesseract(pdf_file) %>%
    rename(field = question_number)

  # Join Python answers in
  joined_qa_df <- joined_qa_df %>%
    left_join(py_answers_df, by = c("file", "field"))

  # Extract the text box
  qbox_df <- extract_text_box_df(pdf_file, debug = TRUE)

  # Combine all parts into one data frame for this PDF
  combined_df <- bind_rows(header_df, joined_qa_df, qbox_df)

  # Store the combined data frame in the list
  final_list[[i]] <- combined_df
}

# Bind all PDF results together into one final data frame
final_combined_df <- bind_rows(final_list)
