# ZipGrade.com, Click Quizzes then quiz name
# Under Quiz Statistics:
# download .csv standard format
# download .pdfs Page for Each Paper in single pdf (only check first of three options)
# use https://www.pdf2go.com/split-pdf to split .pdfs (Click Split All)
# set the name of the .pdf folder and make sure it's in this dir

### DOWNLOAD AND RENAME BUBBLE SHEETS ###

pdffolder <- "EDAVMidtermFall20/"

library(tidyverse)

students <- read_csv("quiz-EDAVMidtermFall20-standard20180510.csv") %>%
  select(uni = `External Id`, name = `First Name`, numcorrect = `Num Correct`) %>%
  rownames_to_column("testnum") %>%
  mutate(grade = numcorrect) %>%
  select(-numcorrect)

write_csv(students, "EDAVstudents.csv")

# rename pdfs

# 1. get rid of stuff after (and including) hyphen

files <- paste0(pdffolder, list.files(pdffolder))
file.rename(from = files, to = str_replace_all(files, "-.*", ".pdf"))

# 2. change files to include uni -- check that it's done correctly
file.rename(from = paste0("EDAVMidtermFall20/EDAVMidtermFall20_", students$testnum, ".pdf"),
            to = paste0("EDAVMidtermFall20/midterm_", students$uni, ".pdf"))

### COMPOSE EMAILS ###

# based on: https://github.com/jennybc/send-email-with-r

subject <- "Midterm bubble sheet"
email_from <- "Joyce Robbins <jtr13@columbia.edu>"
body <- "<p>Hi %s,</p>
<p>Here is your graded midterm bubble sheet.</p>
<p>Your grade is %s out of 96.</p>
<br>
Sent from R with <a href=\"https://gmailr.r-lib.org\">gmailr</a>"

students <- read_csv("EDAVstudents.csv")

emaildata <- students %>%
  transmute(To = paste0(uni, "@columbia.edu"),
            From = email_from,
            Subject = subject,
            body = sprintf(body, name, grade),
            attachment = paste0(pdffolder, "midterm_", uni, ".pdf"))

# this is for reference only
write_csv(emaildata, "composed-emailsB.csv")

### CREATE EMAIL DRAFTS ###

# NOTES
# gm_drafts() shows drafts in your actual draft folder
# gm_create_draft() adds a draft to your actual draft folder
# gm_send_draft() sends a draft
# ex. g <- gm_create_draft(test_email); gm_send_draft(g)
# or you can send an email directly without creating a draft:
# gm_send_message(test_email)

# run once (supposedly) https://stackoverflow.com/a/62651398/5314416

library(tidyverse)
library(gmailr)
gm_auth_configure(path = "EDAV2.json")

# Get the .json file if needed, instructions here: https://github.com/r-lib/gmailr

# from https://gmailr.r-lib.org/ and

# https://stackoverflow.com/questions/41865159/r-purrrpmap-how-to-refer-to-input-arguments-by-name

emaildata <- read_csv("composed-emailsB.csv")

email_messages <- emaildata %>%
  slice(41:50) %>%  # gets stuck unless I do small batches
  pmap(~gm_mime() %>%
         gm_to(..1) %>%  # To
         gm_from(..2) %>%   # From
         gm_subject(..3) %>%  # Subject
         gm_html_body(..4) %>%  # body
         gm_attach_file(..5) %>%   # attachment
         gm_create_draft())

# based on: https://github.com/jennybc/send-email-with-r

# draft
# safe_send_draft <- safely(gm_send_draft)

# sent_drafts <- email_messages %>%
#  map(safe_send_draft)

# UNCOMMENT TO SEND
# safe_send_message <- safely(gm_send_message)
# sent_mail <- email_messages %>%
#  map(safe_send_message)

errors <- sent_drafts %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_drafts[errors]



