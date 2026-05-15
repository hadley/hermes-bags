# Pass 3: parse every cleaned product HTML into one row per bag.
#
# Reads 02-html-product/*.html (class-only markup from 02-clean.R) and writes
# 03-hermes-bags.parquet. All fields sourced from class-tagged elements.

library(rvest)
library(purrr)
library(dplyr)
library(tibble)
library(nanoparquet)

files <- list.files("02-html-product", pattern = "\\.html$", full.names = TRUE)
message("parsing ", length(files), " files")

extract_one <- function(file) {
  doc <- read_html(file, encoding = "UTF-8")

  price_text <- doc |> html_element(".price") |> html_text()
  price <- as.integer(readr::parse_number(price_text))

  tibble(
    title             = doc |> html_element("h2.product_title") |> html_text(),
    price             = price,
    short_description = doc |> html_element(".woocommerce-product-details__short-description") |> html_text(trim = TRUE),
    description       = doc |> html_element(".tab-content.tab-description") |> html_text(trim = TRUE),
    size              = doc |> html_element(".tab-content.tab-size") |> html_text(trim = TRUE)
  )
}
rows <- map(files, extract_one, .progress = TRUE)

write_parquet(rows |> list_rbind(), "03-hermes-bags.parquet")
