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
    3) Only capture lines that satisfy one of the following:
         - (Original) Start with "Yes" or "No" or "Not Applicable" (the first word is "not" and the second is "applicable") and total words <= 6.
         - (New) Contain a token matching exactly "Yes" (with a capital Y) and a token matching exactly "No" (with a capital N) and have 5 words or less.
         - (New) Contain tokens for "Yes" and "No", include the phrase "not applicable" (case-insensitive), and have less than 8 words.
    4) Also capture lines for the text-box prompt:
       'Please provide any other information that you think may be relevant'
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

    # 3) Define a helper to check if a line meets the answer criteria.
    def is_short_answer_line(line: str) -> bool:
        words = line.split()
        if not words:
            return False

        # Original logic: if the line has up to 6 words and starts with "Yes", "No", or "Not Applicable"
        if len(words) <= 6:
            first_word = words[0].lower()
            if first_word == "yes":
                return True
            if first_word == "no":
                return True
            if first_word == "not" and len(words) > 1 and words[1].lower() == "applicable":
                return True

        # New logic: check for tokens that match exactly "Yes" and "No"
        yes_found = any(re.match(r'^Yes\b', token) for token in words)
        no_found = any(re.match(r'^No\b', token) for token in words)

        # Condition 1: line has 5 words or less with both tokens
        if len(words) <= 5 and yes_found and no_found:
            return True

        # Condition 2: line has less than 8 words, both tokens, and contains the phrase "not applicable"
        if len(words) < 8 and yes_found and no_found and re.search(r'(?i)not applicable', line):
            return True

        return False

    answers = []
    i = 0
    while i < len(all_lines):
        line = all_lines[i]

        if is_short_answer_line(line):
            # This line meets one of the answer rules.
            answers.append(line)
            i += 1

        # Text-box prompt: "Please provide any other information that you think may be relevant"
        elif re.search(r'(?i)^please provide any other information that you think may be relevant', line):
            answer_block = line
            i += 1
            while i < len(all_lines):
                next_line = all_lines[i]
                if not next_line:
                    break
                if is_short_answer_line(next_line):
                    break
                if re.search(r'(?i)^please provide any other information', next_line):
                    break

                answer_block += " " + next_line
                i += 1

            answers.append(answer_block)
        else:
            i += 1

    # 4) Build final DataFrame: wide -> long
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
