# Pass 1: download every Hermès product page from loveluxury.co.uk via chromote.
#
# Cloudflare blocks headless + libcurl requests, so we drive a HEADED Chrome
# you launch yourself, and attach chromote to it.
#
# --- one-time setup --------------------------------------------------------
# In a terminal, launch Chrome with remote debugging enabled:
#
#   rm -rf /tmp/lluxchrome
#   "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
#     --remote-debugging-port=9333 \
#     '--remote-allow-origins=*' \
#     --user-data-dir=/tmp/lluxchrome \
#     --no-first-run --no-default-browser-check &
#

library(chromote)
library(rvest)
library(purrr)

base_url <- "https://loveluxury.co.uk/shop/hermes/"
listing_dir <- "01-html-listing"
product_dir <- "01-html-product"
urls_file <- "01-product-urls.txt"
debug_port <- 9333

dir.create(listing_dir, showWarnings = FALSE)
dir.create(product_dir, showWarnings = FALSE)

# --- attach to the running Chrome ------------------------------------------

chrome <- ChromeRemote$new(host = "127.0.0.1", port = debug_port)
b <- ChromoteSession$new(parent = Chromote$new(browser = chrome))

b$Page$navigate(base_url)
cat(
  "\n>>> Solve the Cloudflare challenge in the Chrome window if shown,\n",
  "    wait until the Hermès listing is visible, then press <Return>.\n",
  sep = ""
)
readline()

fetch <- function(url, settle = 2) {
  b$Page$navigate(url)
  b$Page$loadEventFired(timeout_ = 60)
  Sys.sleep(settle)
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  if (grepl("Just a moment|cf-challenge|Enable JavaScript and cookies", html)) {
    cat("\n>>> Cloudflare challenge detected. Solve it, then press <Return>.\n")
    readline()
    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML"
    )$result$value
  }
  html
}

# --- 1a. listing pages -----------------------------------------------------

listing_url <- function(page) {
  if (page == 1) base_url else paste0(base_url, "page/", page, "/")
}

n_pages_total <- function(file) {
  doc <- read_html(file)
  nums <- doc |>
    html_elements("a.page-numbers") |>
    html_attr("href") |>
    stringr::str_match("/page/(\\d+)/") |>
    _[, 2] |>
    as.integer()
  max(c(1L, nums), na.rm = TRUE)
}

page1 <- file.path(listing_dir, "page-001.html")
if (!file.exists(page1)) {
  message("saving listing page 1")
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  writeLines(html, page1)
}
n_pages <- n_pages_total(page1)
message("total listing pages: ", n_pages)

for (p in seq_len(n_pages)) {
  dest <- file.path(listing_dir, sprintf("page-%03d.html", p))
  if (file.exists(dest)) {
    next
  }
  message("listing page ", p, "/", n_pages)
  writeLines(fetch(listing_url(p)), dest)
}

# --- 1b. extract product URLs ---------------------------------------------

extract_product_urls <- function(file) {
  doc <- read_html(file)
  hrefs <- doc |>
    html_elements("a.product-loop-title") |>
    html_attr("href")
  hrefs <- sub("#.*$", "", hrefs)
  hrefs <- hrefs[grepl("^https://loveluxury\\.co\\.uk/shop/hermes-", hrefs)]
  unique(hrefs)
}

listing_files <- list.files(
  listing_dir,
  pattern = "\\.html$",
  full.names = TRUE
)
product_urls <- unique(unlist(map(listing_files, extract_product_urls)))
writeLines(product_urls, urls_file)
message(
  "found ",
  length(product_urls),
  " product URLs (saved to ",
  urls_file,
  ")"
)

# --- 1c. product pages -----------------------------------------------------

slug_to_file <- function(url) {
  s <- sub("^https://loveluxury\\.co\\.uk/shop/", "", url)
  s <- sub("/$", "", s)
  s <- gsub("/", "_", s, fixed = TRUE)
  file.path(product_dir, paste0(s, ".html"))
}

for (i in seq_along(product_urls)) {
  url <- product_urls[i]
  dest <- slug_to_file(url)
  if (file.exists(dest)) {
    next
  }
  message(sprintf("[%d/%d] %s", i, length(product_urls), basename(dest)))
  html <- tryCatch(fetch(url, settle = 1.5), error = function(e) {
    message("  failed: ", conditionMessage(e))
    NULL
  })
  if (!is.null(html)) writeLines(html, dest)
}

message("done.")
