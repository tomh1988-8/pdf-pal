##### Strategy #######
- No attribute detected using traditional pdf co-ordinate mapping
- Therefore, use OCR (Optical Character Recognition) instead
- Essentially, save each file as picture and use fancy ML-derived library to pick up the text from it
- Write a function to do it for one file
- the loop through all the files in a target folder
- as part of loop, save each unique set of results (one per file) to a different sheet in Excel WB



###### LOOK OUT FOR #########
- I see that the file names of the real files are very long, this might break the Excel bit as you can't have more than 30 chars for a sheet name
- If a Yes No is after a page break, you'll have an extra line in the results, delete these in Excel or R
- the regex section of the function can be built on everytime you find a new exception
- IF the function doesn't generalise well to other sheets that means a couple more exceptions need to be added, happy to add them or show you how to.

