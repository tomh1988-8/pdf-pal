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

########################## SINGLE USE ##########################################
# list file names
pdf_files <- list.files(here("pdfs"), pattern = "\\.pdf$", full.names = TRUE)

# get one
first_pdf_file <- pdf_files[1]

# get header box
header_df <- extract_header_info(first_pdf_file)

# get questions and context
qc_df <- extract_questions_context(first_pdf_file)
answers_df <- extract_answers_by_question(first_pdf_file) |>
  rename(field = question_number)

# Python-based OCR answers
py_answers_df <- extract_answers_by_question_pytesseract(first_pdf_file) |>
  rename(field = question_number)

# join python and r answers
answers_df <- answers_df |>
  left_join(py_answers_df, by = c("file", "field"))

# get processed answers
processed_answer_df <- answers_df |>
  mutate(processed_answer = mapply(process_answer, answer, py_answer))

# join to questions and context df
joined_qa_df <- qc_df |>
  left_join(processed_answer_df, by = c("file", "field"))

# question box
qbox_df <- extract_text_box_df(first_pdf_file, debug = TRUE)

# bind_rows to final df
final_single_df <- bind_rows(header_df, joined_qa_df, qbox_df)

# join in real answers
final_single_df <- final_single_df |>
  left_join(real_answers, by = c("file", "field"))
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

# check performance
performance <- final_single_df |>
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
