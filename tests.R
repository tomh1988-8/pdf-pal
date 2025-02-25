###################### rename_pdf_files ########################################
# Assume rename_pdf_files is defined in your project. If not, source it:
# source("path/to/your/rename_pdf_files.R")

###################### TEST 1 --------------------------------------------------
# Test 1: Verify that files matching the naming pattern are renamed
# and files that do not match remain unchanged.
test_that("rename_pdf_files renames files with the matching naming format", {
  # Create a temporary directory for testing
  temp_dir <- tempfile("test_dir")
  dir.create(temp_dir)

  # Files that should be renamed (match the regex pattern)
  f1 <- file.path(
    temp_dir,
    "100-671-517-fs-l1-english-reading-workshop-24042024-ver75b-20250224095550.pdf"
  )
  f2 <- file.path(
    temp_dir,
    "100-671-518-fs-l1-english-reading-workshop-24042024-ver75b-20250224095550.pdf"
  )

  # Files that should remain unchanged (do not match the pattern)
  f3 <- file.path(temp_dir, "pdf1.pdf")
  f4 <- file.path(temp_dir, "random.pdf")

  # Create the dummy files
  file.create(f1)
  file.create(f2)
  file.create(f3)
  file.create(f4)

  # Run the renaming function
  rename_pdf_files(temp_dir)

  # Expected new file names based on the prefix extracted by the regex
  new_f1 <- file.path(temp_dir, "pdf-100-671-517-fs-l1.pdf")
  new_f2 <- file.path(temp_dir, "pdf-100-671-518-fs-l1.pdf")

  # Check that files with matching pattern have been renamed
  expect_true(file.exists(new_f1))
  expect_true(file.exists(new_f2))

  # Check that the original files are no longer present
  expect_false(file.exists(f1))
  expect_false(file.exists(f2))

  # Check that files not matching the pattern remain unchanged
  expect_true(file.exists(f3))
  expect_true(file.exists(f4))

  # Cleanup: Remove the temporary directory and its contents
  unlink(temp_dir, recursive = TRUE)
})

###################### TEST 2 --------------------------------------------------
# Test 2: Verify that the function handles an empty directory without error.
test_that("rename_pdf_files handles an empty directory without error", {
  # Create an empty temporary directory for testing
  temp_dir <- tempfile("empty_dir")
  dir.create(temp_dir)

  # The function should not throw an error when the directory is empty
  expect_silent(rename_pdf_files(temp_dir))

  # Cleanup: Remove the temporary directory
  unlink(temp_dir, recursive = TRUE)
})


###################### process_pdf_files #######################################
###################### TEST 1 --------------------------------------------------
# Test 1: Verify that process_pdf_files renames matching PDF files,
# reads their content, and returns a named list with character vectors.
test_that("process_pdf_files renames matching PDFs and reads their text", {
  # Create a temporary directory for testing
  temp_dir <- tempfile("test_dir")
  dir.create(temp_dir)

  # Create a PDF file that should be renamed (matching pattern)
  f1_original <- file.path(
    temp_dir,
    "100-671-517-fs-l1-english-reading-workshop-24042024-ver75b-20250224095550.pdf"
  )
  pdf(f1_original, width = 4, height = 4)
  plot(1:10, main = "Test PDF Content")
  dev.off()

  # Create a PDF file that does NOT match the pattern
  f2 <- file.path(temp_dir, "pdf2.pdf")
  pdf(f2, width = 4, height = 4)
  plot(1:10, main = "Non-matching PDF")
  dev.off()

  # Run the combined function
  contents <- process_pdf_files(temp_dir)

  # Expected new file name for the matching file after renaming
  new_f1 <- file.path(temp_dir, "pdf-100-671-517-fs-l1.pdf")

  # Verify that the matching file has been renamed and the original is gone
  expect_true(file.exists(new_f1))
  expect_false(file.exists(f1_original))

  # Check that the returned list includes both the renamed and non-renamed files
  expect_true("pdf-100-671-517-fs-l1.pdf" %in% names(contents))
  expect_true("pdf2.pdf" %in% names(contents))

  # Verify that the content from each PDF is read as a character vector
  expect_type(contents[["pdf-100-671-517-fs-l1.pdf"]], "character")
  expect_type(contents[["pdf2.pdf"]], "character")

  # Optionally, check that the text from the renamed PDF contains our known string
  text_content <- paste(contents[["pdf-100-671-517-fs-l1.pdf"]], collapse = " ")
  expect_true(grepl("Test PDF Content", text_content))

  # Cleanup: Remove the temporary directory and its contents
  unlink(temp_dir, recursive = TRUE)
})

###################### TEST 2 --------------------------------------------------
# Test 2: Verify that process_pdf_files handles an empty directory without error.
test_that("process_pdf_files handles an empty directory without error", {
  temp_dir <- tempfile("empty_dir")
  dir.create(temp_dir)

  # The function should run silently on an empty directory and return an empty list
  expect_silent({
    contents <- process_pdf_files(temp_dir)
    expect_length(contents, 0)
  })

  # Cleanup: Remove the temporary directory
  unlink(temp_dir, recursive = TRUE)
})
