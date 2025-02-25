########################### BROAD STRATEGY #####################################
# 1. READ IN ALL FILES FROM PDF FOLDER
# 2. NAME THE FILES AS THE CODE AT THE START OF THE FILE
# 3. USE TRADITIONAL PDF TOOLS TO EXTRACT ALL THE RELEVANT TEXT INTO DF
# 4. USE OCR TO EXTRACT ANY CHECKMARK RESPONSES
# 5. JOIN BY FILE NAME AND QUESTION NUMBER
# META: ONE FUNCTION FOR PDFTOOLS, ONE FOR OCR, LOOP TO APPEND DF
################################################################################

########################### READ IN FILES ######################################
pdf_texts <- process_pdf_files(here("pdfs"))
