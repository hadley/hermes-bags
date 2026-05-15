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
  simple_colour = colour_simple$simple
)
leather_lookup <- tibble(
  leather = all_leathers,
  simple_leather = leather_simple$simple
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
    width_cm = as.integer(round(as.numeric(dims[, 2]))),
    height_cm = as.integer(round(as.numeric(dims[, 3]))),
    depth_cm = as.integer(round(as.numeric(dims[, 4]))),
    primary_colour = first_component(colour),
    primary_leather = first_component(leather),
    condition = classify_condition(short_description),
    has_receipt = classify_receipt(description),
    full_set = classify_full_set(description)
  ) |>
  left_join(colour_lookup, by = c(primary_colour = "colour")) |>
  left_join(leather_lookup, by = c(primary_leather = "leather"))

write_parquet(out, "04-hermes-bags.parquet")
message(
  "wrote 04-hermes-bags.parquet (",
  nrow(out),
  " rows, ",
  ncol(out),
  " cols)"
)
