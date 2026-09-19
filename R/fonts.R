library(showtext)
library(here)

# Font configuration function
setup_fonts <- function() {
  # Add Font Awesome — PATH ASSUMES this file has been copied into this
  # project (eia/fonts/6.6.0/...). It will NOT resolve automatically from
  # here::here() in a different .Rproj. Comment out / skip if not present;
  # social-icon captions in 13_final_visual.R are optional and gated on
  # this succeeding.
  fa_path <- here::here("fonts", "6.6.0", "Font Awesome 6 Brands-Regular-400.otf")
  if (file.exists(fa_path)) {
    font_add("fa6-brands", fa_path)
  } else {
    message("Font Awesome file not found at: ", fa_path,
            " — social-icon glyphs will not render. Copy the font file into ",
            "this project if you want the social caption row.")
  }

  # Google Fonts — Steven Ponce brand system (v1.0, May 2026)
  # Display / titles:
  font_add_google("Big Shoulders", regular.wt = 800, family = "title_1") # Main
  font_add_google("Big Shoulders", regular.wt = 100, family = "title_2") # Facet charts

  # UI / chart labels / body:
  font_add_google("DM Sans", regular.wt = 400, family = "subtitle")
  font_add_google("DM Sans", regular.wt = 400, family = "body")
  font_add_google("DM Sans", regular.wt = 400, family = "text")

  # Mono / captions / data labels: JetBrains Mono — tabular numerals, crisp
  font_add_google("JetBrains Mono", regular.wt = 400, family = "caption")

  showtext_auto(enable = TRUE)
  showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)
}

get_font_families <- function() {
  list(
    title_1 = "title_1",
    title_2 = "title_2",
    subtitle = "subtitle",
    text = "text",
    caption = "caption"
  )
}
