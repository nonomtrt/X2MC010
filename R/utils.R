library(tidyverse)
library(tidytext)

extract_encryption <- function(f) {
  f <- strsplit(f, "/")
  f
  f <- map_chr(f, 2)
  f <- strsplit(f, "ciphertxt_")
  f <- map_chr(f, 2)
  f <- strsplit(f, "\\.")
  map_chr(f, 1)
}
extract_encryption(f)
