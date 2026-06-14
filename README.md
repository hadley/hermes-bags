# Hermès bags

A dataset of 2,447 Hermès handbags scraped from [Love Luxury](https://loveluxury.co.uk/shop/hermes/). For each bag we capture 21 columns of data including the asking price, the physical dimensions, and a set of fields parsed from the listing text: model, size, colour, leather, hardware, stamp year, condition, and whether it ships as a full set / with a receipt.

- **[hermes-bags.parquet](hermes-bags.parquet)** — the final cleaned dataset.
- **[data-dict.yaml](data-dict.yaml)** — data dictionary describing every column,
  with types, examples, and a glossary of resale jargon.

## Process

The pipeline runs in four stages:

- **[01-download.R](01-download.R)** — scrape every Hermès product page from Love
  Luxury. Drives a headed Chrome via [chromote](https://rstudio.github.io/chromote/), saving the raw listing and product HTML.
- **[02-clean.R](02-clean.R)** — strip each raw product page down to just the
  title, price, and description blocks, keeping only `class`/`id` attributes.
- **[03-extract.R](03-extract.R)** — parse the cleaned HTML into one row per bag,
  writing `03-hermes-bags.parquet`.
- **[04-enrich.R](04-enrich.R)** — extract model / size / colour / leather /
  hardware from each title with an LLM ([ellmer](https://ellmer.tidyverse.org/) + Anthropic batch API), simplify
  colours and leathers to everyday English, parse the dimensions, blind stamp,
  year, condition, receipt, and full-set fields via regex, and join everything
  back into the final `hermes-bags.parquet`.

Much of the code in this repository was written by [Claude code](https://claude.com/claude-code).
