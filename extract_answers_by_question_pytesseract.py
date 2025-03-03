import os
import re
import pdf2image
import pytesseract
import pandas as pd
from PIL import Image

# Force the path to tesseract.exe 
pytesseract.pytesseract.tesseract_cmd = r"C:\Users\TomHun\AppData\Local\Programs\Tesseract-OCR\tesseract.exe"

def extract_answers_by_question_pytesseract(pdf_file: str) -> pd.DataFrame:
    """
    1) Convert each page of 'pdf_file' to an image (via pdf2image, dpi=300).
    2) Use pytesseract to extract text lines.
    3) Only capture lines that:
        - Start with "Yes" and total words <= 6, OR
        - Start with "No" and total words <= 6, OR
        - Start with "Not Applicable" and total words <= 6.
    4) Also capture lines for the text-box prompt:
       'Please provide any other information...'
       (gather subsequent lines until we hit another "answer" or an empty line).
    5) Return a tidy DataFrame with columns: [file, question_number, answer].
    """

    # 1) Convert PDF pages to images
    images = pdf2image.convert_from_path(pdf_file, dpi=300)

    # 2) OCR each page, accumulate lines
    all_lines = []
    for img in images:
        ocr_text = pytesseract.image_to_string(img)
        lines = [ln.strip() for ln in ocr_text.splitlines() if ln.strip()]
        all_lines.extend(lines)

    # 3) Define a helper to check if a line meets the "Yes/No/Not App" rule
    def is_short_answer_line(line: str) -> bool:
        words = line.split()
        if len(words) == 0:
            return False

        # total words <= 6 means up to 6 words total
        if len(words) <= 6:
            first_word = words[0].lower()

            # Case A: "Yes" ...
            if first_word == "yes":
                return True

            # Case B: "No" ...
            if first_word == "no":
                return True

            # Case C: "Not Applicable" (two-word check)
            if first_word == "not" and len(words) > 1:
                if words[1].lower() == "applicable":
                    return True

        return False

    answers = []
    i = 0
    while i < len(all_lines):
        line = all_lines[i]

        if is_short_answer_line(line):
            # This line meets the short "Yes/No/Not applicable" rule
            answers.append(line)
            i += 1

        # text-box prompt: "Please provide any other information..."
        elif re.search(r'(?i)^please provide any other information that you think may be relevant',
                       line):
            # gather subsequent lines until you hit a new short answer line or blank
            answer_block = line
            i += 1
            while i < len(all_lines):
                next_line = all_lines[i]
                if not next_line:
                    # blank line -> break
                    break
                if is_short_answer_line(next_line):
                    # next line is a short answer line -> stop
                    break
                if re.search(r'(?i)^please provide any other information', next_line):
                    # another "please provide" prompt -> stop
                    break

                answer_block += " " + next_line
                i += 1

            answers.append(answer_block)

        else:
            i += 1

    # 4) Build final DF: wide -> long
    result = {'file': os.path.basename(pdf_file)}
    for idx, ans in enumerate(answers, start=1):
        result[f'question_{idx}'] = ans

    df_wide = pd.DataFrame([result])
    df_long = df_wide.melt(
        id_vars='file',
        var_name='question_number',
        value_name='py_answer'
    )
    df_long.dropna(subset=['py_answer'], inplace=True)

    return df_long
