# Load Libraries

library(XML)
library(rvest)
library(stringr)

# Read url page
smart_parsed <- read_html("https://en.wikipedia.org/wiki/S.M.A.R.T.", encoding = "UTF-8")
tables <- html_table(smart_parsed, fill = TRUE)

# Extract S.M.A.R.T table.
smart_table_code <- tables[[3]]

# Take four columns. Description is extensive
smart_table_code <- smart_table_code[,1:4]

# Change columns names
colnames(smart_table_code) <- c("ID","Attribute","Ideal","Crucial")

# Substract the first three digits as ID
smart_table_code$ID <- str_remove(substr(smart_table_code$ID,1,nchar(smart_table_code$ID)-4), "^0+")

# Leave only crucial variables and variable Power-On Hours.
detail_code <- smart_table_code[smart_table_code$Crucial != "" | smart_table_code$ID == "9", 1:3]

# Delete [:digit:] pattern

detail_code$Attribute <- str_replace_all(detail_code$Attribute, "[^[A-Za-z()]]", " ") %>%
                            str_replace_all(.,"[ ]+", " ")

