import os
import pytesseract
import pandas as pd

# Point pytesseract to your Tesseract executable:
# Make sure you include "tesseract.exe" at the end if on Windows.
pytesseract.pytesseract.tesseract_cmd = r"C:\Users\TomHun\AppData\Local\Programs\Tesseract-OCR\tesseract.exe"

from extract_answers_by_question_pytesseract import extract_answers_by_question_pytesseract

def test_ocr(pdf_file):
    answers_df = extract_answers_by_question_pytesseract(pdf_file)
    print(answers_df)

if __name__ == "__main__":
    # Example usage
    pdf_file = "pdfs/pdf-100-591-792.pdf"
    test_ocr(pdf_file)

    
  # python test_extract.py
