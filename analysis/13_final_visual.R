# =============================================================================
# 13_final_visual.R
#
#
# SCOPE NOTE -----------------------------------------------------------------
# Visual design pass over 12's validated geometry-only prototypes, built on
# Steven's actual brand system (R/fonts.R, R/base_theme.R) rather than a
# generic ggplot theme. Does NOT reopen analytical decisions (06-11) or
# geographic validation (12) — inherits both by sourcing 12 directly.
#
# Headline (confirmed, not re-derived here):
#   "During the highest-demand U.S. hours of 2025, Tacoma and Seattle relied
#    more heavily on net imports, while AECI switched from net exporter to
#    net importer."
# BPAT and SWPP are the explanatory visual layer.
#
# AUDIENCE DECISION (resolved): professional portfolio piece that must stay
# legible on LinkedIn — NOT an energy-industry technical report. BA codes
# remain the analytical labeling system. Two quiet orientation labels
# (WASHINGTON, MISSOURI) answer "where are these systems?" without turning the
# backdrop into a reference map. Full BA names (BPAT = Bonneville Power
# Administration, SWPP = Southwest Power Pool, AECI = Associated Electric
# Cooperative) live in the accompanying post, not in the chart.
#
# Social-icon caption row: ENABLED (fonts folder confirmed present).
# Depends on the Font Awesome file actually resolving via R/fonts.R's
# existence check — if it's missing, icon glyphs won't render but the
# plain text (handles, source, SIKE note) still will.
#
# Depends on:
#   analysis/12_geography_and_chart_prototype.R (sourced directly)
#   R/fonts.R, R/base_theme.R (your brand system, copied into this project)
# =============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(here)
library(stringr)
library(ggtext)

source(here("R", "fonts.R"))
source(here("R", "base_theme.R"))
source(here("R", "social_icons.R"))
source(here("analysis", "12_geography_and_chart_prototype.R"))
# Inherits: anchors, nw_arcs, central_arcs, nw_anchor_subset,
# central_anchor_subset, compute_padded_bbox(), sike_caption

setup_fonts()
fonts <- get_font_families()  # create_base_theme() reads this as a global

# Revised wording per review — overridden here (styling layer) rather than
# editing 12 (which stays scoped to geometry/analysis, not caption prose).
sike_caption <- paste0(
  "SIKE (+0.3 pp; 2.1% of AECI's attributed change) is omitted because ",
  "boundary geometry was unavailable but remains in the underlying analysis."
)

# --- 0. Palette -------------------------------------------------------------
# Rust + Slate + Teal (this portfolio's own archived guidance for
# geopolitical/economic data — avoids the political-color associations
# flagged for burgundy/blue in this context), passed through YOUR
# get_theme_colors(palette = ...) convention rather than a standalone list.

colors <- get_theme_colors(
  palette = list(
    col_driver = "#9C5A2C",         # rust — increases dependence / the flip
    col_countervailing = "#426C7A"  # teal-slate — offsets it
  )
)
col_driver <- colors$palette$col_driver
col_countervailing <- colors$palette$col_countervailing

# Orientation-label gray: a light warm neutral, deliberately well below the
# state-outline stone (#7A7068) so it reads as backdrop, never as data.
# If it disappears after LinkedIn's image compression, step it darker
# (e.g. "#A9A399") rather than adding weight or size.
col_state_label <- "#B5AFA6"

# --- 1. Geographic context ---------------------------------------------------
# Real US state boundaries (maps::map -> sf), NOT BA territory polygons —
# ba_boundaries_valid is used only upstream for anchor computation, never
# passed into rendering. Pre-cropped to each panel's bbox with st_crop(),
# not just clipped at render time, so nothing near the edge gets an
# unintended jagged cutoff and nothing bleeds past the panel into the
# caption area below (the actual cause of the earlier render's bleed —
# clip = "off" on coord_sf let background geometry escape its bbox, not a
# wrong data source).

old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)  # GEOS's planar engine tolerates maps::map()'s known
# self-intersections better than s2 does — st_make_valid()
# alone under s2 didn't fully resolve them here. Restored
# below; doesn't affect anything upstream (12's BA
# boundaries were already handled separately, in a
# projected CRS that sidesteps s2 entirely).

us_states_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |>
  sf::st_set_crs(4326) |>
  sf::st_make_valid()

stopifnot(all(sf::st_is_valid(us_states_sf)))

bbox_to_sf <- function(bbox_list) {
  sf::st_bbox(c(xmin = bbox_list$xlim[1], xmax = bbox_list$xlim[2],
                ymin = bbox_list$ylim[1], ymax = bbox_list$ylim[2]),
              crs = 4326)
}

nw_bbox <- compute_padded_bbox(nw_anchor_subset)
central_bbox <- compute_padded_bbox(central_anchor_subset)

nw_states <- sf::st_crop(us_states_sf, bbox_to_sf(nw_bbox))
central_states <- sf::st_crop(us_states_sf, bbox_to_sf(central_bbox))

sf::sf_use_s2(old_s2)  # restore — don't leave s2 off for the rest of the session

# --- 2. Per-panel label resolution -------------------------------------------
# annotate()-style hardcoded offsets, not ggrepel (per your own carry-forward
# note that ggrepel silently drops labels it can't place). Offsets are a
# starting point for visual iteration, targeted at the two collisions flagged
# in 12's review (BPAT/TPWR, BPAT/SCL) — not a blanket nudge on every label.

nw_arcs <- nw_arcs |>
  mutate(
    label_x = (lon_from + lon_to) / 2,
    label_y = (lat_from + lat_to) / 2,
    # BPAT/SCL offset held as-is (confirmed acceptable). BPAT/TPWR pushed
    # further below-right per review — previous offset still collided.
    label_x = case_when(
      counterparty == "BPAT" & target == "TPWR" ~ label_x + 0.30,
      counterparty == "BPAT" & target == "SCL" ~ label_x - 0.20,
      TRUE ~ label_x
    ),
    label_y = case_when(
      counterparty == "BPAT" & target == "TPWR" ~ label_y - 0.18,
      counterparty == "BPAT" & target == "SCL" ~ label_y - 0.02,
      TRUE ~ label_y
    )
  )

central_arcs <- central_arcs |>
  mutate(label_x = (lon_from + lon_to) / 2, label_y = (lat_from + lat_to) / 2)

# Pooled max |contribution| across BOTH panels — used as the shared upper limit
# of the linewidth scale in build_final_panel().
shared_max_contrib <- max(abs(c(nw_arcs$contribution_pp, central_arcs$contribution_pp)))

# --- 3. Editorial color scale -------------------------------------------------

driver_scale <- scale_color_manual(
  values = c("Driver (increases dependence/flip)" = col_driver,
             "Countervailing (offsets it)" = col_countervailing),
  name = NULL
)

# --- 4. Panel composition, built on YOUR theme system ------------------------

weekly_theme_elements <- theme(
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  # Legend intentionally suppressed: the subtitle already states the encoding
  # (rust = increased dependence, slate = offsets it; width = |contribution|),
  # so a legend would only repeat it and add clutter to the map panels.
  legend.position = "none",
  plot.title = element_text(family = fonts$title_2, face = "bold", size = rel(1.15),
                            color = colors$title, margin = margin(b = 4)),
  # element_textbox_simple(), not element_text()/element_markdown() — per
  # your own note, element_markdown() does not auto-wrap long text, which
  # is exactly what caused the SIKE caption clipping at the panel edge.
  plot.caption = ggtext::element_textbox_simple(
    width = unit(1, "npc"), family = fonts$caption, size = rel(0.55),
    color = colors$caption, hjust = 0, margin = margin(t = 6)
  )
)

panel_theme <- extend_weekly_theme(create_base_theme(colors), weekly_theme_elements)

build_final_panel <- function(arc_data, anchor_subset, state_backdrop, title,
                              node_label_overrides = NULL, state_label = NULL) {
  bbox <- compute_padded_bbox(anchor_subset)
  
  # Default: nudge every node label straight up by a fixed fraction of the
  # panel's y-range. node_label_overrides (a tibble of code/dx/dy) replaces
  # that default for specific codes that need a different direction — e.g.
  # TPWR moving above-LEFT instead of straight up, to clear its own +11.7 pp
  # value label sitting below-right of the same point.
  default_dy <- diff(bbox$ylim) * 0.035
  anchor_subset <- anchor_subset |>
    mutate(label_dx = 0, label_dy = default_dy)
  
  if (!is.null(node_label_overrides)) {
    anchor_subset <- anchor_subset |>
      rows_update(node_label_overrides, by = "code")
  }
  
  p <- ggplot() +
    geom_sf(data = state_backdrop, fill = colors$background, color = colors$subtitle,
            linewidth = 0.3, inherit.aes = FALSE)
  
  # Quiet state-orientation label. Added directly after the backdrop and
  # BEFORE the flow/point/label layers, so it sits beneath everything else.
  # Body font (fonts$text), regular weight, uppercase, smaller than the BA
  # codes (3.4), light neutral gray. One label per panel, placed in empty
  # map space — see state_label definitions below the panel calls.
  if (!is.null(state_label)) {
    p <- p + annotate(
      "text", x = state_label$x, y = state_label$y, label = state_label$label,
      hjust = state_label$hjust, vjust = state_label$vjust,
      size = 2.5, family = fonts$text, fontface = "plain", color = col_state_label
    )
  }
  
  p +
    geom_curve(
      data = arc_data,
      aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,
          linewidth = abs(contribution_pp), color = direction),
      curvature = 0.2, alpha = 0.88, lineend = "round"
    ) +
    geom_point(data = anchor_subset, aes(x = lon, y = lat), size = 2.2, color = colors$title) +
    geom_text(data = anchor_subset, aes(x = lon + label_dx, y = lat + label_dy, label = code),
              size = 3.4, family = fonts$text, fontface = "bold", color = colors$title) +
    geom_text(
      data = arc_data,
      aes(x = label_x, y = label_y, label = sprintf("%+.1f pp", contribution_pp)),
      size = 3.0, family = fonts$caption, color = colors$title
    ) +
    driver_scale +
    # SHARED limits across both panels: each panel is its own ggplot, so an
    # unspecified scale trains separately per panel and would draw each panel's
    # largest arc at the same width (+11.7 pp NW == +8.3 pp Central). Fixing
    # limits to the pooled max makes width comparable across panels, which the
    # subtitle ("Line width = |contribution|") promises.
    scale_linewidth_continuous(range = c(0.6, 4.2), limits = c(0, shared_max_contrib),
                               guide = "none") +
    # clip = "on" (the default) — NOT "off". "off" was the actual cause of
    # the earlier render's background bleeding past the panel into the
    # caption area. Labels are kept inside the bbox via compute_padded_bbox's
    # margin + the explicit per-code offsets above, not by disabling clip.
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, clip = "on") +
    labs(title = title, color = NULL, x = NULL, y = NULL) +
    panel_theme
}

tpwr_override <- tibble(code = "TPWR", label_dx = -0.22, label_dy = 0.14)

# State-label placement (positions are a starting point for the visual pass).
# NW: the whole panel sits inside Washington, so the label is anchored to the
#     empty lower-right corner of the bbox (right-aligned, 4% in from the
#     edges) — clear of the BPAT-TPWR-SCL cluster and the +11.7 pp label.
# Central: anchored in data coordinates inside Missouri's empty southwest
#     interior, between the SWPP->AECI arc above and the AECI->SPA arc and its
#     +1.7 pp label to the right, and away from the Kansas border on the left.
nw_state_label <- list(
  label = "WASHINGTON",
  x = nw_bbox$xlim[2] - 0.04 * diff(nw_bbox$xlim),
  y = nw_bbox$ylim[1] + 0.04 * diff(nw_bbox$ylim),
  hjust = 1, vjust = 0
)
central_state_label <- list(
  label = "MISSOURI", x = -93.6, y = 37.6, hjust = 0.5, vjust = 0.5
)

p_nw_final <- build_final_panel(nw_arcs, nw_anchor_subset, nw_states,
                                "Pacific Northwest: BPAT as shared driver",
                                node_label_overrides = tpwr_override,
                                state_label = nw_state_label)
p_central_final <- build_final_panel(central_arcs, central_anchor_subset, central_states,
                                     "Central: SWPP-led AECI network",
                                     state_label = central_state_label)

combined <- p_nw_final + p_central_final + plot_layout(widths = c(0.85, 1.15))

# --- 5. Annotation layer ------------------------------------------------------

methodology_text <- str_wrap(str_glue(
  "EIA Form 930 (interchange, demand) via EIA Open Data API. ",
  "State boundaries: R maps package (maps::map(\"state\")). ",
  "BA anchor locations derived from Esri Policy Maps \"Balancing Authority ",
  "Energy Summary\" (boundary vintage not stated by publisher \u2014 not a ",
  "confirmed 2025 EIA snapshot). ",
  "Findings validated across lower/midpoint/upper reconciliation scenarios and ",
  "1%/5%/10% stress thresholds."
), width = 160)

icons <- get_social_icons()
social_text <- str_glue(
  "{icons$linkedin} stevenponce &bull; {icons$bluesky} sponce1 &bull; {icons$github} poncest"
)

# SIKE note (moved here from the Central panel's own caption — captions
# belong to the outer patchwork layout, not an individual sf panel), then
# source line, then social row. "Source:" appears exactly once.
source_caption <- str_glue("{sike_caption}<br>Source: {methodology_text}<br>{social_text}")



final_figure <- combined +
  plot_annotation(
    title = "Tacoma, Seattle, and AECI leaned harder on imports during 2025's highest-demand hours",
    subtitle = str_wrap(paste0(
      "During the top 5% of U.S. hours by demand in 2025, Tacoma Power and Seattle City Light ",
      "relied more heavily on net imports from BPAT, while AECI switched from a net exporter to ",
      "a net importer \u2014 driven primarily by SWPP. Rust connections increased dependence; ",
      "slate connections offset it. SCHEMATIC relationships (reported BA-pair interchange), NOT ",
      "physical transmission routes. Line width = |contribution| to the stress-vs-normal change, ",
      "in percentage points."
    ), width = 140),
    caption = source_caption,
    theme = theme(
      plot.title = element_text(family = fonts$title_1, face = "bold", size = rel(1.9),
                                color = colors$title, margin = margin(b = 6)),
      plot.subtitle = element_text(family = fonts$subtitle, size = rel(1.0),
                                   color = colors$subtitle, margin = margin(b = 10), lineheight = 1.2),
      # element_textbox_simple(), same reasoning as the panel-level SIKE
      # caption above — this text now includes the social-icon markup too.
      plot.caption = ggtext::element_textbox_simple(
        width = unit(1, "npc"), family = fonts$caption, size = rel(0.62),
        color = colors$caption, hjust = 0, lineheight = 1.3, margin = margin(t = 8)
      ),
      plot.background = element_rect(fill = colors$background, color = colors$background),
      # Larger bottom/right margin — the previous defaults let both the
      # SIKE note and the source line run past the figure edge.
      plot.margin = margin(t = 10, r = 30, b = 50, l = 20)
    )
  )

# --- 6. Export ----------------------------------------------------------------

dir.create(here("figures"), recursive = TRUE, showWarnings = FALSE)

ggsave(here("figures", "final_dependence_network.png"), final_figure,
       width = 13, height = 8, dpi = 320, bg = colors$background)

message("Saved: figures/final_dependence_network.png")

# --- Legibility / technical-hygiene review checklist -------------------------

message("\n=== REVIEW CHECKLIST ===\n",
        "- Do the state panels now read as recognizable geography (WA/OR for\n",
        "  NW; MO/KS/OK/AR/TN for Central), cropped cleanly at the panel edge\n",
        "  with NOTHING bleeding into the caption area below?\n",
        "- Legend is intentionally suppressed (subtitle carries the encoding) —\n",
        "  confirm no legend renders.\n",
        "- Do the fonts actually render (showtext + Google Fonts need network\n",
        "  access on first run to download) — check for fallback/tofu glyphs?\n",
        "- WASHINGTON / MISSOURI labels: visibly quieter than the BA codes, in\n",
        "  empty space, no contact with any arc, node label, or pp label, and\n",
        "  still legible at LinkedIn feed size? Nothing else in the backdrop labeled?\n",
        "- BPAT/TPWR and BPAT/SCL label nudges: do they actually clear their\n",
        "  lines now, AND stay inside the panel now that clip = 'on'?\n",
        "- SIKE note, source line, and social row: all present, single 'Source:',\n",
        "  no clipping at the figure's bottom-right edge?\n\n",
        "JAPANESE DESIGN PRINCIPLES (per your own framework):\n",
        "Kanso / Fukinsei / Shibumi / Yugen / Seijaku / Ma / Jo-ha-kyu \u2014 \n",
        "review the exported PNG against each, as in prior portfolio pieces.")

