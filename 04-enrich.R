# Pass 4: extract model / size / colour / leather / hardware from each unique
# bag title using ellmer's batch structured-output API, simplify colour and
# leather names to everyday English, then join all derived fields back onto
# the full bag table.
#
# Stages:
#   (a) batch-call the LLM once per unique title  →  04-parsed-titles.parquet
#   (b) batch-call the LLM to simplify each unique colour / leather component
#       →  04-simplified-colours.parquet, 04-simplified-leathers.parquet
#   (c) join (a) and (b) back to the full table   →  04-hermes-bags.parquet

library(ellmer)
library(nanoparquet)
library(dplyr)
library(tibble)

d <- read_parquet("03-hermes-bags.parquet")

titles <- d$title |> unique() |> na.omit() |> as.character()
message("unique titles: ", length(titles))

# ---- structured schema ----------------------------------------------------

type_bag <- type_object(
  model = type_string(
    paste(
      "Hermès bag model name, including any sub-variant qualifier.",
      "Examples: 'Birkin', 'Kelly', 'Mini Kelly', 'Kelly Pochette',",
      "'Constance', 'Constance Slim', 'Picotin', 'Evelyne', '24/24',",
      "'Herbag', 'Lindy', 'Bolide', 'Garden Party', 'Jypsiere'.",
      "Leave null if the title is not a recognisable Hermès handbag."
    ),
    required = FALSE
  ),
  size = type_integer(
    paste(
      "Numeric size of the bag in cm (commonly 18, 20, 25, 28, 30, 35, 40).",
      "Leave null if the model has no numeric size or none is given."
    ),
    required = FALSE
  ),
  colour = type_string(
    paste(
      "Colour name(s), possibly multi-word.",
      "Examples: 'Noir', 'Black', 'Bleu Royal', 'Gris Etain',",
      "'Etoupe', 'Rouge H', 'Gold', 'Craie', 'Vert Vertigo'.",
      "If the bag has multiple colours (e.g. a tricolour or bi-colour bag),",
      "list them all separated by commas, with the primary/dominant colour first,",
      "e.g. 'Noir, Gold' or 'Vert Emerald, Vert Titien, Rose Azalee'.",
      "Leave null if no colour is given."
    ),
    required = FALSE
  ),
  leather = type_string(
    paste(
      "Leather or material type.",
      "Examples: 'Togo', 'Epsom', 'Swift', 'Clemence', 'Chèvre',",
      "'Box', 'Crocodile', 'Alligator', 'Ostrich', 'Lizard', 'Ardennes',",
      "'Barenia', 'Evercolor', 'Canvas', 'Toile'.",
      "If the bag uses multiple leathers (e.g. a combination of Togo and Swift),",
      "list them all separated by commas, with the primary/dominant leather first,",
      "e.g. 'Togo, Swift' or 'Chèvre, Box, Crocodile'.",
      "Leave null if not specified."
    ),
    required = FALSE
  ),
  hardware = type_string(
    paste(
      "Hardware finish.",
      "Examples: 'Gold', 'Palladium', 'Rose Gold', 'Permabrass',",
      "'Ruthenium', 'Brushed Gold', 'Brushed Palladium', 'Electrum'.",
      "Leave null if not specified."
    ),
    required = FALSE
  )
)

# ---- (a) batch extraction --------------------------------------------------

chat <- chat_anthropic(
  model = "claude-haiku-4-5-20251001",
  system_prompt = paste(
    "You extract structured fields from Hermès handbag titles.",
    "Each user message is a single bag title (e.g. 'Hermès Birkin 30",
    "Noir Togo Palladium Hardware'). Return the model, numeric size,",
    "colour, leather, and hardware. Use null for any field that cannot",
    "be determined from the title alone."
  )
)

# Anthropic's Message Batches API: ~50% cheaper than parallel calls,
# completes asynchronously (usually minutes; up to 24h).
extracted <- batch_chat_structured(
  chat = chat,
  prompts = as.list(titles),
  type = type_bag,
  path = "04-extracted-titles.json"
)

parsed_titles <- tibble(title = titles) |>
  bind_cols(as_tibble(extracted)) |>
  select(-any_of(".error")) |>
  rename(model_size = size)

write_parquet(parsed_titles, "04-parsed-titles.parquet")
message("wrote 04-parsed-titles.parquet (", nrow(parsed_titles), " rows)")

# ---- (b) simplify colours and leathers to everyday English ----------------

split_components <- function(x) {
  out <- unlist(strsplit(x, "\\s*,\\s*"))
  out <- trimws(out)
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

all_colours <- split_components(parsed_titles$colour)
all_leathers <- split_components(parsed_titles$leather)
message("unique colour components: ", length(all_colours))
message("unique leather components: ", length(all_leathers))

type_simple_colour <- type_object(
  simple = type_string(
    paste(
      "A simple, common English colour word a non-expert would recognise.",
      "Choose one of: 'black', 'white', 'red', 'blue', 'green', 'yellow',",
      "'orange', 'purple', 'pink', 'brown', 'grey', 'gold', 'silver',",
      "'beige', 'tan'. Use 'multi' for multi-colour names, 'other' if none fit."
    )
  )
)

type_simple_leather <- type_object(
  simple = type_string(
    paste(
      "The animal or material source in plain English that a non-expert would recognise.",
      "Map each Hermès leather to its source: most leathers (Togo, Epsom, Swift,",
      "Clemence, Box, Barenia, Evercolor, Ardennes, Vache Hunter, ...) → 'cow'.",
      "Chèvre, Mysore → 'goat'. Crocodile → 'crocodile'. Alligator → 'alligator'.",
      "Ostrich → 'ostrich'. Lizard → 'lizard'. Canvas, Toile → 'canvas'.",
      "Use 'other' if unsure."
    )
  )
)

chat_simple <- chat_anthropic(
  model = "claude-haiku-4-5-20251001",
  system_prompt = paste(
    "You simplify Hermès colour or leather names to plain English.",
    "Each user message is one term. Return its simplified form."
  )
)

colour_simple <- batch_chat_structured(
  chat = chat_simple,
  prompts = as.list(all_colours),
  type = type_simple_colour,
  path = "04-simplified-colours.json"
)
leather_simple <- batch_chat_structured(
  chat = chat_simple,
  prompts = as.list(all_leathers),
  type = type_simple_leather,
  path = "04-simplified-leathers.json"
)

colour_lookup <- tibble(
  colour = all_colours,
  colour_simple = colour_simple$simple
)
leather_lookup <- tibble(
  leather = all_leathers,
  leather_simple = leather_simple$simple
)

write_parquet(colour_lookup, "04-simplified-colours.parquet")
write_parquet(leather_lookup, "04-simplified-leathers.parquet")

# ---- (c) join back to the full table ---------------------------------------

# Also parse the cm dimensions from `size` (still a regex job — it's reliable).
dims <- stringr::str_match(
  d$size,
  "(\\d+(?:\\.\\d+)?)\\s*[xX×]\\s*(\\d+(?:\\.\\d+)?)\\s*[xX×]\\s*(\\d+(?:\\.\\d+)?)"
)

first_component <- function(x) {
  trimws(stringr::str_extract(x, "^[^,]+"))
}

# Hermès blind stamp: a single letter giving the year of manufacture. The same
# letter means different years in different eras, so detect square-shape stamps
# (1997-2014) and otherwise assume the modern no-shape era (2015-). The listings
# usually also state a year next to the stamp (e.g. "B Stamp 2023"), but ~15% of
# the time that is the seller's listing/purchase year and contradicts the stamp
# letter (e.g. "W Stamp 2025" when W is always 2024), so we trust the letter, not
# the written year. The modern run below is confirmed by the dominant
# letter/year pairing across the listings.
stamp_to_year <- function(letter, square) {
  modern <- c(
    T = 2015L,
    X = 2016L,
    A = 2017L,
    C = 2018L,
    D = 2019L,
    Y = 2020L,
    Z = 2021L,
    U = 2022L,
    B = 2023L,
    W = 2024L,
    K = 2025L,
    G = 2026L
  )
  square_era <- c(
    A = 1997L,
    B = 1998L,
    C = 1999L,
    D = 2000L,
    E = 2001L,
    F = 2002L,
    G = 2003L,
    H = 2004L,
    I = 2005L,
    J = 2006L,
    K = 2007L,
    L = 2008L,
    M = 2009L,
    N = 2010L,
    O = 2011L,
    P = 2012L,
    Q = 2013L,
    R = 2014L
  )
  pick <- ifelse(
    square,
    square_era[letter],
    coalesce(modern[letter], square_era[letter])
  )
  as.integer(unname(pick))
}

# The stamp letter is the single standalone letter just before "stamp" /
# "square stamp", e.g. "... Gold Hardware / B Stamp 2023".
extract_stamp <- function(x) {
  toupper(stringr::str_match(x, "(?i)\\b([a-z])\\b\\s+(?:square\\s+)?stamp")[,
    2
  ])
}

classify_condition <- function(x) {
  s <- tolower(x)
  case_when(
    is.na(s) ~ NA_character_,
    stringr::str_detect(
      s,
      "box[- ]?fresh|as new|never used|unworn|brand new"
    ) ~ "Box Fresh",
    stringr::str_detect(
      s,
      "excellent\\s+pre[- ]?loved|excellent preloved"
    ) ~ "Excellent Pre-Loved",
    stringr::str_detect(
      s,
      "pre[- ]?loved|preloved|pre[- ]?owned"
    ) ~ "Pre-Loved",
    TRUE ~ NA_character_
  )
}

# 3-state: TRUE / FALSE / NA. Negative context (without / no / excluding /
# minus / except) must be immediately before "receipt" — so "with the receipt
# and without the dust bag" still counts as having a receipt.
has_phrase <- function(x, pos, neg) {
  s <- coalesce(tolower(x), "")
  pos_hit <- stringr::str_detect(s, pos)
  neg_hit <- stringr::str_detect(s, neg)
  case_when(
    neg_hit ~ FALSE,
    pos_hit ~ TRUE,
    TRUE ~ NA
  )
}

classify_receipt <- function(x) {
  has_phrase(
    x,
    pos = "(with|including|copy|and|plus)\\s+(?:the\\s+|a\\s+|copy\\s+|original\\s+)*rec(?:ei|ie)pt",
    neg = "(without|exclud\\w*|no|minus|except)\\s+(?:the\\s+|a\\s+)?rec(?:ei|ie)pt"
  )
}

classify_full_set <- function(x) {
  has_phrase(
    x,
    pos = "\\bfull[- ]set\\b",
    neg = "\\b(not\\s+(?:a\\s+)?full[- ]set|partial\\s+set|incomplete\\s+set)\\b"
  )
}

out <- d |>
  left_join(parsed_titles, by = "title") |>
  mutate(
    width = as.integer(round(as.numeric(dims[, 2]))),
    height = as.integer(round(as.numeric(dims[, 3]))),
    depth = as.integer(round(as.numeric(dims[, 4]))),
    colour_primary = first_component(colour),
    leather_primary = first_component(leather),
    # The stamp lives mostly in short_description; fall back to description.
    stamp = coalesce(
      extract_stamp(short_description),
      extract_stamp(description)
    ),
    year = stamp_to_year(
      stamp,
      stringr::str_detect(
        coalesce(short_description, ""),
        "(?i)square\\s+stamp"
      ) |
        stringr::str_detect(coalesce(description, ""), "(?i)square\\s+stamp")
    ),
    condition = classify_condition(short_description),
    has_receipt = classify_receipt(description),
    full_set = classify_full_set(description)
  ) |>
  left_join(colour_lookup, by = c(colour_primary = "colour")) |>
  left_join(leather_lookup, by = c(leather_primary = "leather")) |>
  select(-size) |>
  relocate(colour_primary, colour_simple, .after = colour) |>
  relocate(leather_primary, leather_simple, .after = leather)

write_parquet(out, "hermes-bags.parquet")
message(
  "wrote hermes-bags.parquet (",
  nrow(out),
  " rows, ",
  ncol(out),
  " cols)"
)
