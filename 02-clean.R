# Pass 2: strip each raw product HTML down to just the product information.
#
# Reads 01-html-product/*.html (full WordPress page with header/footer/scripts/
# styles/related products) and writes 02-html-product/*.html containing only
# what 03-extract.R needs. All attributes except `class` are stripped; data
# that lived in attributes (gtm4wp JSON, tab ids) is re-emitted as semantic
# class-tagged elements.

library(rvest)
library(xml2)
library(jsonlite)

raw_dir <- "01-html-product"
out_dir <- "02-html-product"
dir.create(out_dir, showWarnings = FALSE)

serialize <- function(x) if (length(x) == 0 || is.na(x)) "" else as.character(x)

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

prune_attrs <- function(doc) {
  for (n in xml_find_all(doc, "//*")) {
    cls <- xml_attr(n, "class")
    if (is.na(cls)) {
      xml_set_attrs(n, character(0))
    } else {
      xml_set_attrs(n, c(class = cls))
    }
  }
  doc
}

gtm_to_html <- function(json_str) {
  if (length(json_str) == 0 || is.na(json_str)) {
    return("")
  }
  dat <- tryCatch(fromJSON(json_str), error = function(e) NULL)
  if (is.null(dat)) {
    return("")
  }
  fields <- c(
    sku = "sku",
    `item-id` = "item_id",
    `stock-status` = "stockstatus",
    `stock-level` = "stocklevel",
    category = "item_category"
  )
  parts <- vapply(
    names(fields),
    function(cls) {
      val <- dat[[fields[[cls]]]]
      if (is.null(val) || length(val) == 0) {
        return("")
      }
      sprintf('<span class="%s">%s</span>', cls, escape_html(as.character(val)))
    },
    character(1)
  )
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return("")
  }
  paste0('<div class="product-data">', paste(parts, collapse = ""), "</div>")
}

inner_html <- function(el) {
  if (is.na(el)) {
    return("")
  }
  paste(vapply(xml_contents(el), as.character, character(1)), collapse = "")
}

wrap_tab <- function(doc, sel, cls) {
  el <- html_element(doc, sel)
  if (is.na(el)) {
    return("")
  }
  sprintf('<div class="tab-content %s">%s</div>', cls, inner_html(el))
}

clean_one <- function(file_in, file_out) {
  doc <- read_html(file_in)

  product_div <- html_element(doc, "div[id^='product-'].product")
  product_classes <- if (!is.na(product_div)) {
    html_attr(product_div, "class")
  } else {
    ""
  }
  gtm_val <- html_attr(
    html_element(doc, "input[name='gtm4wp_product_data']"),
    "value"
  )

  body_bits <- c(
    sprintf('<div class="%s">', product_classes),
    gtm_to_html(gtm_val),
    serialize(html_element(doc, "h2.product_title")),
    serialize(html_element(doc, ".single-product-price")),
    serialize(html_element(
      doc,
      ".woocommerce-product-details__short-description"
    )),
    wrap_tab(doc, "#tab-custom_tab1", "tab-description"),
    wrap_tab(doc, "#tab-custom_tab2", "tab-size"),
    wrap_tab(doc, "#tab-custom_tab3", "tab-delivery"),
    "</div>"
  )

  html_str <- paste(
    c("<!DOCTYPE html>", "<html><body>", body_bits, "</body></html>"),
    collapse = "\n"
  )
  cleaned <- prune_attrs(read_html(html_str))
  write_html(cleaned, file_out, format = TRUE)
}

files <- list.files(raw_dir, pattern = "\\.html$", full.names = TRUE)
message("cleaning ", length(files), " files")

for (f in files) {
  out <- file.path(out_dir, basename(f))
  if (file.exists(out)) {
    next
  }
  tryCatch(clean_one(f, out), error = function(e) {
    message("  failed: ", basename(f), " – ", conditionMessage(e))
  })
}

message("done.")
