########################### BROAD STRATEGY #####################################
# 1. READ IN ALL FILES FROM PDF FOLDER
# 2. NAME THE FILES AS THE CODE AT THE START OF THE FILE
# 3. USE TRADITIONAL PDF TOOLS TO EXTRACT ALL THE RELEVANT TEXT INTO DF
# 4. USE OCR TO EXTRACT ANY CHECKMARK RESPONSES
# 5. CARRY OUT SAME THING IN PYTHON TO CROSS-VALIDATE
# 6. WRITE A HEURISTIC FOR SCORING THE OCR OUTPUT
# 7. MEASURE PERFORMANCE COMPARED TO HUMAN-SCORED SUBSET
# 8. TWEAK HEURISTIC UNTIL PERFORMANCE SATISFACTORY
################################################################################
# python librares
py_require("pdf2image")
py_require("pytesseract")
py_require("pandas")

# load in python script
source_python("extract_answers_by_question_pytesseract.py")

# load in real answers
real_answers <- read.csv(file = "real_answers.csv")

# Note you might want to use to rename a batch of files to better names to begin:
pdf_texts <- process_pdf_files(here("pdfs"))

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
  answers_df <- extract_answers_by_question(pdf_file) |>
    rename(field = question_number)

  # Python-based OCR answers
  py_answers_df <- extract_answers_by_question_pytesseract(pdf_file) |>
    rename(field = question_number)

  # join python and r answers
  answers_df <- answers_df |>
    left_join(py_answers_df, by = c("file", "field"))

  # Process the answers and rename question_number column to field
  processed_answer_df <- answers_df |>
    mutate(processed_answer = mapply(process_answer, answer, py_answer))

  # Join questions/context with processed answers
  joined_qa_df <- qc_df %>%
    left_join(processed_answer_df, by = c("file", "field"))

  # Extract the text box
  qbox_df <- extract_text_box_df(pdf_file, debug = TRUE)

  # Combine all parts into one data frame for this PDF
  combined_df <- bind_rows(header_df, joined_qa_df, qbox_df)

  # join in real answers
  combined_df <- combined_df |>
    left_join(real_answers, by = c("file", "field")) |>
    mutate(
      q_count = case_when(
        str_detect(field, "question") ~ 1,
        TRUE ~ NA_integer_
      )
    ) |>
    mutate(
      score = case_when(
        processed_answer == real_answer ~ 1,
        TRUE ~ NA_integer_
      )
    )

  # Store the combined data frame in the list
  final_list[[i]] <- combined_df
}

# Ensure that py_answer is a character in each data frame before binding
final_list <- lapply(final_list, function(df) {
  if ("py_answer" %in% names(df)) {
    df <- df %>% mutate(py_answer = as.character(py_answer))
  }
  df
})


# Bind all PDF results together into one final data frame
final_combined_df <- bind_rows(final_list)

# check performance
performance <- final_combined_df |>
  summarise(
    num_questions = sum(q_count, na.rm = TRUE), # Sum of q_count for total questions
    total_score = sum(score, na.rm = TRUE), # Sum of score for total correct responses
    accuracy = (total_score / num_questions) * 100 # Compute accuracy percentage
  )

# Print a formatted summary
cat(glue(
  "Total Questions: {performance$num_questions}\n",
  "Correct Responses: {performance$total_score}\n",
  "Accuracy: {round(performance$accuracy, 2)}%"
))

####### iteration zone #####
####### this dataframe helps critique the heuristic and improve ----------------
failures <- final_combined_df |>
  filter(q_count == 1 & is.na(score)) |>
  select(contains("answer"))
