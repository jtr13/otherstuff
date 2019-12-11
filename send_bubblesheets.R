# ZipGrade.com, Click Quizzes then quiz name
# Under Quiz Statistics:
# download .csv standard format
# download .pdfs Page for Each Paper
# use https://www.pdf2go.com/split-pdf to split .pdfs
# rename .pdf folder "ScanSheetsA" and move to this dir

library(tidyverse)
students <- read_csv("quiz-MidtermBFall2019-standard20180510.csv") %>%
  select(uni = `External Id`, name = `First Name`, numcorrect = `Num Correct`) %>%
  rownames_to_column("testnum") %>%
  mutate(grade = numcorrect + 1) %>%
  select(-numcorrect)


# rename pdfs

# 1. get rid of stuff after (and including) hyphen

files <- paste0("ScanSheetsB/", list.files("ScanSheetsB/"))
file.rename(from = files, to = str_replace_all(files, "-.*", ".pdf"))

# 2. change files to include uni
file.rename(from = paste0("ScanSheetsB/MidtermBFall2019_", students$testnum, ".pdf"),
            to = paste0("ScanSheetsB/midterm_", students$uni, ".pdf"))

# based on: https://github.com/jennybc/send-email-with-r

subject <- "Midterm bubble sheet"
email_from <- "Joyce Robbins <jtr13@columbia.edu>"
body <- "<p>Hi %s,</p>
<p>Here is your graded midterm bubble sheet.</p>
<p>Your grade is %s.</p>
<p>Please see CourseWorks announcements for additional grading information.</p>
<br>
Sent from R with <a href=\"https://gmailr.r-lib.org\">gmailr</a>"

emaildata <- students %>%
  transmute(To = paste0(uni, "@columbia.edu"),
            From = email_from,
            Subject = subject,
            body = sprintf(body, name, grade),
            attachment = paste0("ScanSheetsB/midterm_", uni, ".pdf"))

write_csv(emaildata, "composed-emailsB.csv")

library(gmailr)
gm_auth_configure(path="EDAV.json")

# from https://gmailr.r-lib.org/ and

# https://stackoverflow.com/questions/41865159/r-purrrpmap-how-to-refer-to-input-arguments-by-name

email_messages <- emaildata %>%
  slice(71:85) %>%
  pmap(~gm_mime() %>%
         gm_to(..1) %>%  # To
         gm_from(..2) %>%   # From
         gm_subject(..3) %>%  # Subject
         gm_html_body(..4) %>%  # body
         gm_attach_file(..5) %>%   # attachment
         gm_create_draft())

# based on: https://github.com/jennybc/send-email-with-r

# draft
safe_send_draft <- safely(gm_send_draft)
sent_drafts <- email_messages %>%
  map(safe_send_draft)

# send
# safe_send_message <- safely(gm_send_message)
# sent_mail <- email_messages %>%
#  map(safe_send_message)

errors <- sent_drafts %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_drafts[errors]



