# Pass 2: strip each raw product HTML down to just the product information.
#
# Reads 01-html-product/*.html (full WordPress page with header/footer/scripts/
# styles/related products) and writes 02-html-product/*.html containing just
# the title, price, short description, and the .resp-tabs-container block.
# Only `class` and `id` attributes survive.
#
# Assumes that the files are all generated from a CMS so they have exactly
# the same structure and no error handling is needed.

library(rvest)
library(xml2)
library(purrr)

raw_dir <- "01-html-product"
out_dir <- "02-html-product"
dir.create(out_dir, showWarnings = FALSE)

prune_attrs <- function(doc) {
  for (n in xml_find_all(doc, "//*")) {
    keep <- c(class = xml_attr(n, "class"), id = xml_attr(n, "id"))
    keep <- keep[!is.na(keep)]
    xml_set_attrs(n, keep)
  }
  doc
}

# libxml2's "format" save option only re-indents element-only content, so any
# whitespace text node from the source HTML blocks reformatting of its parent.
# Strip whitespace-only text nodes outright; for mixed-content text, collapse
# whitespace runs that contain \t or \n (those are source indentation; real
# inline spaces are single 0x20 chars).
normalize_whitespace <- function(doc) {
  xml_remove(xml_find_all(doc, "//text()[normalize-space()='']"))
  for (t in xml_find_all(doc, "//text()")) {
    v <- xml_text(t)
    v2 <- sub("^[ \t\n]*[\t\n][ \t\n]*", "", v)
    v2 <- sub("[ \t\n]*[\t\n][ \t\n]*$", "", v2)
    v2 <- gsub("[ \t\n]*[\t\n][ \t\n]*", " ", v2)
    if (!identical(v2, v)) xml_text(t) <- v2
  }
  doc
}

clean_one <- function(file_in, file_out) {
  doc <- read_html(file_in)

  html_str <- paste0(
    "<!DOCTYPE html>",
    "<html><body>",
    "<div>",
    as.character(html_elements(doc, "h2.product_title")),
    as.character(html_elements(doc, ".single-product-price")),
    as.character(html_elements(doc, ".description")),
    as.character(html_elements(doc, ".resp-tabs-container")),
    "</div>",
    "</body></html>"
  )

  cleaned <- prune_attrs(normalize_whitespace(read_html(html_str)))

  # Force nice indenting
  write_xml(
    cleaned,
    file_out,
    options = c("format", "as_xml", "no_declaration")
  )
}

paths_in <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE)
paths_out <- file.path(out_dir, basename(paths_in))

map2(paths_in, paths_out, clean_one, .progress = "Cleaning")
