library(stringr)

# Social media icons configuration
get_social_icons <- function() {
  list(
    linkedin = str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>"),
    github   = str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>"),
    bluesky  = str_glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
  )
}

# Non-TidyTuesday variant: source/note line + social row, no "#TidyTuesday
# Week N" text (this isn't a weekly-challenge piece).
create_social_caption_standalone <- function(source_text, note_text = NULL) {
  icons <- get_social_icons()
  social_text <- str_glue(
    "{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest"
  )
  if (is.null(note_text)) {
    str_glue("Source: {source_text}<br>{social_text}")
  } else {
    str_glue("{note_text}<br>Source: {source_text}<br>{social_text}")
  }
}
