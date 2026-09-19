# =============================================================================
# 12_geography_and_chart_prototype.R
#
# =============================================================================
# 12_geography_and_chart_prototype.R
#
# SCOPE NOTE -----------------------------------------------------------------
# Geometry-only chart prototypes for the two validated findings from 11:
#   - Northwest: BPAT (shared driver) -> TPWR, SCL, with PSEI as a small
#     countervailing corridor
#   - Central: SWPP (leading driver) -> AECI, with MISO/SPA/TVA as supporting
#     corridors. SIKE is EXCLUDED from the rendered network (see below) but
#     RETAINED in the attribution table and methodology note.
#
# CRITICAL CONSTRAINT: EIA interchange records identify BA PAIRS, not
# physical transmission lines or tie points. Every connecting arc in these
# prototypes is a SCHEMATIC relationship (which BAs exchange power and how
# much), not a transmission route — labeled as such in every plot title/
# caption, not just in this comment.
#
# SIKE EXCLUSION: contributes only 2.06% of AECI's absolute attributed
# change (11's corridor attribution), with just 12 eligible strata — small
# enough that omitting it keeps the Central panel legible without
# misrepresenting the finding. It stays in corridor_attribution_AECI.csv
# and must be named explicitly in any methodology note, not silently cut.
#
# Four gates, in order — do not proceed to gate N+1 until gate N passes:
#   1. Acquire authoritative BA boundaries; document publication/effective
#      date against the 2025 analysis year.
#   2. Match every displayed code to exactly one geometry (9 codes: BPAT,
#      TPWR, SCL, PSEI, AECI, SWPP, MISO, SPA, TVA).
#   3. Test representative anchor methods (centroid / point-on-surface /
#      documented manual override) without implying physical precision.
#   4. Produce GEOMETRY-ONLY prototypes (no styling polish yet).
#
# SOURCE VERIFICATION -----------------------------------------------------
#   HIFLD's Open GIS repository: DEACTIVATED August 26, 2025.
#   EIA's own atlas.eia.gov map viewer: now returns a sign-in wall.
#   EIA's own ArcGIS Feature Service (services7.arcgis.com/FGr1D95XCGALKXqM/
#     .../Balancing_Authorities/FeatureServer/255): confirmed TOKEN-GATED —
#     tested directly (layer metadata AND query both returned
#     "Token Required", error code 499), not merely a locked directory page.
#
#   SOURCE USED: a public, no-token-required ArcGIS FeatureServer maintained
#   by Esri's Policy Maps team — "Balancing Authority Energy Summary":
#     https://www.arcgis.com/home/item.html?id=b7821f7c9ce14fabb47e3fcae6a35c77
#     https://services8.arcgis.com/peDZJliSvYims39Q/arcgis/rest/services/
#     Balancing_Authorities_Summary/FeatureServer/31
#   Verified by a live, unauthenticated query (not guessed) returning real
#   polygon geometry + EIA BA codes for 8 of the 9 target codes (all except
#   SIKE — see exclusion note above). Its metadata states operational
#   measures are sourced from EIA-930, but boundary PROVENANCE AND VINTAGE
#   ARE NOT STATED. This is an Esri-maintained DERIVATIVE layer, not a
#   confirmed authoritative 2025 EIA boundary snapshot — that limitation is
#   recorded in boundary_source_metadata.csv (gate 1) and must appear in any
#   published methodology note, not just here.
#
# Depends on:
#   output/corridor_attribution_TPWR.csv / _SCL.csv / _AECI.csv   (11)
#   The Esri Policy Maps FeatureServer above (queried directly, no token)
#
# Outputs:
#   data/ba_boundaries_esri.gpkg                     - matched geometry for 8 target codes
#   output/boundary_source_metadata.csv
#   output/geometry_validation_before_repair.csv     - source topology defects, documented
#   output/anchor_method_comparison.csv              - centroid vs. point-on-surface, per code
#   figures/prototype_northwest_geometry_only.png
#   figures/prototype_central_geometry_only.png
# =============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(here)

dir.create(here("figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

# --- GATE 1: Boundary source acquisition + documentation --------------------

boundary_url <- paste0(
  "https://services8.arcgis.com/",
  "peDZJliSvYims39Q/arcgis/rest/services/",
  "Balancing_Authorities_Summary/FeatureServer/31/query?",
  "where=1%3D1",
  "&outFields=balancing_authority_code,balancing_authority_name,eia_region",
  "&returnGeometry=true",
  "&outSR=4326",
  "&f=geojson"
)

ba_boundaries <- st_read(boundary_url, quiet = TRUE) |>
  rename(
    code = balancing_authority_code,
    name = balancing_authority_name,
    region = eia_region
  )

boundary_source_metadata <- tibble(
  source_title = "Balancing Authority Energy Summary",
  publisher = "Esri Policy Maps",
  source_url = boundary_url,
  accessed_date = as.character(Sys.Date()),
  geometry_vintage = NA_character_,
  intended_use = "Schematic BA-level flow-map context",
  limitation = paste(
    "Boundary provenance and vintage are not specified by the publisher.",
    "This is an Esri-maintained derivative layer, not a confirmed",
    "authoritative 2025 EIA boundary snapshot. Connections represent",
    "reported BA relationships, not physical transmission routes."
  )
)

write.csv(boundary_source_metadata, here("output", "boundary_source_metadata.csv"),
          row.names = FALSE)

message("--- Gate 1: boundary source documentation ---")
print(boundary_source_metadata, width = Inf)
message("\nNOTE: geometry_vintage is deliberately NA — the source does not ",
        "state it. This is disclosed as a limitation, not filled in with a ",
        "guess.")

# --- GATE 2: Match every code to exactly one geometry ------------------------

TARGET_CODES <- c("BPAT", "TPWR", "SCL", "PSEI", "AECI", "SWPP", "MISO", "SPA", "TVA")

geometry_coverage <- tibble(code = TARGET_CODES) |>
  left_join(
    ba_boundaries |> st_drop_geometry() |> distinct(code, name, region),
    by = "code"
  ) |>
  mutate(matched = !is.na(name)) |>
  tibble::as_tibble()  # defensive, same reasoning as elsewhere in this script

message("\n--- Gate 2: match coverage ---")
print(geometry_coverage, n = Inf, width = Inf)

unmatched <- geometry_coverage |> filter(!matched)
if (nrow(unmatched) > 0) {
  message("\n", nrow(unmatched), " code(s) with no geometry: ",
          paste(unmatched$code, collapse = ", "),
          " — confirmed to be SIKE only (per direct query check); excluded ",
          "from the rendered network, retained in the attribution table and ",
          "methodology note (see script header).")
}

# Gate 2 passes on the 8 chart-essential codes, NOT all 9 — SIKE's absence
# is a known, disclosed, and analytically inconsequential (2.06% of AECI's
# attributed change) gap, not a silent failure.
stopifnot(all(geometry_coverage$matched[geometry_coverage$code != "SIKE"]))

ba_boundaries_matched <- ba_boundaries |> filter(code %in% TARGET_CODES)

write_sf(ba_boundaries_matched, here("data", "ba_boundaries_esri.gpkg"), delete_dsn = TRUE)

message("\nGate 2 PASSED: all 8 chart-essential codes match exactly one geometry.")

# --- GATE 3: Validate geometry and compare anchor methods --------------------
# Three candidate anchor methods, none presented as physically precise —
# BAs are areas, not points, and any single-point representation is a
# simplification made for legibility, not a claim about where power
# physically flows.
#
# Geometry ops run in a projected CONUS CRS (EPSG:5070), not raw lon/lat —
# lon/lat distorts distance and area, which affects centroid stability. The
# source polygons also proved topologically invalid (duplicate vertices in
# at least one ring), which halted the naive version of this gate — fixed
# below with st_make_valid() (explicit, audit-trailed), not st_buffer(0)
# (an implicit patch that repairs silently without a documented before/after).

ba_boundaries_projected <- ba_boundaries_matched |> st_transform(5070)

geometry_validation_before <- ba_boundaries_projected |>
  mutate(
    geometry_valid_before = st_is_valid(geometry),
    invalid_reason_before = st_is_valid(geometry, reason = TRUE)
  ) |>
  st_drop_geometry() |>
  select(code, geometry_valid_before, invalid_reason_before) |>
  tibble::as_tibble()  # st_drop_geometry() returns a base data.frame, not a
# tibble, even when the original sf object came from
# st_read() — print(..., n = Inf) errors on a plain
# data.frame, so convert explicitly rather than let
# that surface as a cryptic na.print error later.

write.csv(geometry_validation_before, here("output", "geometry_validation_before_repair.csv"),
          row.names = FALSE)

message("\n--- Gate 3a: geometry validity BEFORE repair (source defects, documented not hidden) ---")
print(geometry_validation_before, n = Inf, width = Inf)

ba_boundaries_valid <- ba_boundaries_projected |>
  st_make_valid() |>
  mutate(geometry_valid_after = st_is_valid(geometry))

stopifnot(all(ba_boundaries_valid$geometry_valid_after))

message("\nAll ", nrow(ba_boundaries_valid), " geometries valid after st_make_valid().")

boundary_geometry <- st_geometry(ba_boundaries_valid)
anchor_centroid <- st_centroid(boundary_geometry)
anchor_point_on_surface <- st_point_on_surface(boundary_geometry)

# diag() — NOT [, 1] — tests each centroid against ITS OWN boundary. [, 1]
# would test every centroid against only the first polygon in the set,
# silently mislabeling every other row.
centroid_inside_boundary <- diag(st_within(anchor_centroid, boundary_geometry, sparse = FALSE))
surface_point_inside_boundary <- diag(st_within(anchor_point_on_surface, boundary_geometry, sparse = FALSE))

anchor_summary <- ba_boundaries_valid |>
  st_drop_geometry() |>
  select(code) |>
  tibble::as_tibble() |>  # same fix as geometry_validation_before above
  mutate(
    centroid_inside_boundary = centroid_inside_boundary,
    surface_point_inside_boundary = surface_point_inside_boundary
  )

write.csv(anchor_summary, here("output", "anchor_method_comparison.csv"), row.names = FALSE)

message("\n--- Gate 3b: anchor method comparison (per code) ---")
print(anchor_summary, n = Inf, width = Inf)

anchor_totals <- anchor_summary |>
  summarise(
    boundaries = n(),
    centroids_inside = sum(centroid_inside_boundary),
    centroids_outside = sum(!centroid_inside_boundary),
    surface_points_inside = sum(surface_point_inside_boundary),
    surface_points_outside = sum(!surface_point_inside_boundary)
  )
print(anchor_totals, width = Inf)

stopifnot(all(anchor_summary$surface_point_inside_boundary))  # must hold by st_point_on_surface's own guarantee

if (any(!anchor_summary$centroid_inside_boundary)) {
  message("\nCentroid falls OUTSIDE its boundary for: ",
          paste(anchor_summary$code[!anchor_summary$centroid_inside_boundary], collapse = ", "),
          " — confirms point_on_surface as the correct default, not just a safer-sounding one.")
}

# Two separate sf objects (not multiple sfc columns bolted onto one table,
# which behaves awkwardly downstream) — transformed back to EPSG:4326 for
# plotting.
centroid_anchors <- ba_boundaries_valid |>
  select(code, name, region) |> st_set_geometry(anchor_centroid) |> st_transform(4326)

surface_anchors <- ba_boundaries_valid |>
  select(code, name, region) |> st_set_geometry(anchor_point_on_surface) |> st_transform(4326)

manual_anchor_overrides <- tibble(
  code = character(), anchor_lon = double(), anchor_lat = double(), override_reason = character()
)

message("\nGate 3 PASSED: using surface_anchors (point_on_surface) as the ",
        "default — guaranteed inside boundary, unlike centroid for fragmented ",
        "or irregular BA territories. Apply manual_anchor_overrides for any ",
        "code that needs a deliberate, documented exception.")

# --- GATE 4: Geometry-only prototypes ----------------------------------------
# No styling polish — labels, minimal theme, signed-contribution encoding
# only. This tests whether the DATA supports legible geometry before any
# design decisions are made.

# --- Relationship-specific anchor override for BPAT -------------------------
# st_point_on_surface() guarantees containment, NOT a meaningful connection
# point — BPAT's default surface point landed far southeast of the actual
# TPWR/SCL/PSEI relationship it's shown driving, producing misleadingly
# long arcs. Fix: derive BPAT's DISPLAY anchor as the point within its own
# geometry nearest to the centroid of the nodes it actually connects to
# here (TPWR, SCL) — reproducible and relationship-specific, not a manually
# chosen coordinate (e.g. a headquarters location), which would introduce
# an unrecorded, unreproducible judgment call.

target_cluster_4326 <- surface_anchors |>
  filter(code %in% c("TPWR", "SCL")) |>
  st_geometry() |>
  st_union() |>
  st_centroid()

# bpat_geometry is in EPSG:5070 (from gate 3's projection) — transform the
# target cluster to match before any spatial predicate, or st_nearest_points
# (and st_intersects below) will error rather than silently misbehave.
target_cluster <- st_transform(target_cluster_4326, 5070)

bpat_geometry <- ba_boundaries_valid |> filter(code == "BPAT") |> st_geometry()

nearest_segment <- st_nearest_points(target_cluster, bpat_geometry)
nearest_points_cast <- st_cast(nearest_segment, "POINT")

# st_nearest_points() returns a LINESTRING from the first geometry to the
# second; point [2] is the endpoint ON bpat_geometry (point [1] is on
# target_cluster). Order matters here and is easy to get backwards.
# (Subsetting with [ isn't valid as the RHS of a native pipe — R's parser
# rejects `|> `[`(2)` — so this stays a plain indexing expression instead.)
bpat_connection_anchor <- st_transform(nearest_points_cast[2], 4326)

# Validate the result actually lands inside BPAT's own boundary — don't
# just trust the geometric procedure without checking its output.
stopifnot(lengths(st_intersects(st_transform(bpat_connection_anchor, 5070), bpat_geometry)) == 1)

if (lengths(st_intersects(target_cluster, bpat_geometry)) > 0) {
  message("\nNOTE: TPWR/SCL cluster centroid falls INSIDE BPAT's own geometry ",
          "(expected — BPAT's territory is large and surrounds much of the ",
          "region). The nearest-point anchor may sit very close to that ",
          "centroid rather than at a distinct edge point. This is an ",
          "acceptable schematic anchor, documented here rather than left ",
          "to look coincidental.")
}

anchor_overrides_applied <- tibble(
  code = "BPAT",
  anchor_method = "nearest point in BA geometry to target-cluster centroid (TPWR, SCL)"
)
write.csv(anchor_overrides_applied, here("output", "anchor_overrides_applied.csv"), row.names = FALSE)

anchors <- surface_anchors |>
  st_drop_geometry() |>
  select(code) |>
  bind_cols(st_coordinates(st_geometry(surface_anchors)) |>
              as_tibble() |> rename(lon = X, lat = Y)) |>
  mutate(anchor_method = "point_on_surface (default)")

bpat_override_coords <- st_coordinates(bpat_connection_anchor) |> as_tibble() |> rename(lon = X, lat = Y)

anchors <- anchors |>
  filter(code != "BPAT") |>
  bind_rows(
    tibble(code = "BPAT", lon = bpat_override_coords$lon, lat = bpat_override_coords$lat,
           anchor_method = "nearest point in BA geometry to target-cluster centroid (TPWR, SCL)")
  ) |>
  tibble::as_tibble()  # defensive: bind_cols/bind_rows haven't reliably
# returned a tibble in this environment (same
# na.print issue seen earlier with st_drop_geometry
# outputs) — force it explicitly rather than assume

message("\n--- Gate 4 anchor table (note BPAT's overridden method) ---")
print(anchors, n = Inf, width = Inf)

corridor_tpwr <- read.csv(here("output", "corridor_attribution_TPWR.csv")) |> mutate(target = "TPWR")
corridor_scl <- read.csv(here("output", "corridor_attribution_SCL.csv")) |> mutate(target = "SCL")
corridor_aeci <- read.csv(here("output", "corridor_attribution_AECI.csv")) |> mutate(target = "AECI")

build_arc_data <- function(corridor_df) {
  from_anchors <- anchors |> select(code, lon_from = lon, lat_from = lat)
  to_anchors <- anchors |> select(code, lon_to = lon, lat_to = lat)
  corridor_df |>
    left_join(from_anchors, by = c("counterparty" = "code")) |>
    left_join(to_anchors, by = c("target" = "code")) |>
    mutate(
      direction = if_else(contribution > 0, "Driver (increases dependence/flip)",
                          "Countervailing (offsets it)"),
      contribution_pp = contribution * 100
    )
}

nw_arcs <- bind_rows(build_arc_data(corridor_tpwr), build_arc_data(corridor_scl))
central_arcs_all <- build_arc_data(corridor_aeci)

# SIKE has no geometry in this source (confirmed at gate 2) — its row will
# have NA lon/lat after the join above. Drop it EXPLICITLY, with a message,
# rather than let ggplot silently omit an NA-coordinate row unremarked.
central_arcs_missing_geom <- central_arcs_all |> filter(is.na(lon_from) | is.na(lon_to))
central_arcs <- central_arcs_all |> filter(!is.na(lon_from), !is.na(lon_to))

if (nrow(central_arcs_missing_geom) > 0) {
  message("\nExcluding from Central panel render (no geometry available): ",
          paste(central_arcs_missing_geom$counterparty, collapse = ", "),
          " — contribution ",
          paste(sprintf("%+.2f pp", central_arcs_missing_geom$contribution * 100), collapse = ", "),
          ". Retained in corridor_attribution_AECI.csv; must be named in any ",
          "methodology note accompanying this chart.")
}

# Padded bounding box computed from the DISPLAYED anchors of each panel —
# not a fixed/default extent, which produced the excessive whitespace and
# title/subtitle clipping in the first draft.
compute_padded_bbox <- function(anchor_subset, pad_fraction = 0.15) {
  lon_range <- range(anchor_subset$lon); lat_range <- range(anchor_subset$lat)
  lon_pad <- diff(lon_range) * pad_fraction; lat_pad <- diff(lat_range) * pad_fraction
  # Guard against a degenerate (near-zero) range collapsing padding to ~0.
  lon_pad <- max(lon_pad, 0.5); lat_pad <- max(lat_pad, 0.5)
  list(xlim = c(lon_range[1] - lon_pad, lon_range[2] + lon_pad),
       ylim = c(lat_range[1] - lat_pad, lat_range[2] + lat_pad))
}

plot_geometry_only <- function(arc_data, anchor_subset, title, caption = NULL) {
  bbox <- compute_padded_bbox(anchor_subset)
  ggplot() +
    geom_curve(
      data = arc_data,
      aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,
          linewidth = abs(contribution_pp), color = direction),
      curvature = 0.2, alpha = 0.8
    ) +
    geom_point(data = anchor_subset, aes(x = lon, y = lat), size = 2) +
    geom_text(data = anchor_subset, aes(x = lon, y = lat, label = code),
              nudge_y = diff(bbox$ylim) * 0.03, size = 3) +
    geom_text(
      data = arc_data,
      aes(x = (lon_from + lon_to) / 2, y = (lat_from + lat_to) / 2,
          label = sprintf("%+.1f pp", contribution_pp)),
      size = 2.8
    ) +
    scale_linewidth_continuous(range = c(0.5, 4), guide = "none") +
    scale_color_manual(values = c("Driver (increases dependence/flip)" = "#B33F3F",
                                  "Countervailing (offsets it)" = "#3F7FB3")) +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE, clip = "off") +
    labs(
      title = title,
      subtitle = "SCHEMATIC relationship (BA-pair interchange), NOT a physical transmission route.\nLine width = |contribution| in percentage points. Labels show signed contribution.",
      caption = caption,
      color = NULL, x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(margin = margin(b = 4)),
          plot.margin = margin(t = 10, r = 20, b = 10, l = 10))
}

nw_anchor_subset <- anchors |> filter(code %in% c("BPAT", "TPWR", "SCL", "PSEI"))
central_anchor_subset <- anchors |> filter(code %in% c("SWPP", "AECI", "MISO", "SPA", "TVA"))

sike_caption <- if (nrow(central_arcs_missing_geom) > 0) {
  paste0("SIKE (", sprintf("%+.1f pp", central_arcs_missing_geom$contribution[1] * 100),
         ", 2.06% of AECI's attributed change) omitted for lack of boundary ",
         "geometry — retained in the underlying attribution table.")
} else NULL

p_nw <- plot_geometry_only(nw_arcs, nw_anchor_subset,
                           "Pacific Northwest: BPAT as shared driver (geometry-only prototype)")
p_central <- plot_geometry_only(central_arcs, central_anchor_subset,
                                "Central: SWPP-led AECI network (geometry-only prototype)",
                                caption = sike_caption)

# Dimensions matched to each panel's actual geographic shape (NW is tall/
# narrow, Central is wide) — not a uniform 8x6 that forced the NW network
# into a compressed strip.
ggsave(here("figures", "prototype_northwest_geometry_only.png"), p_nw, width = 7, height = 9, dpi = 150)
ggsave(here("figures", "prototype_central_geometry_only.png"), p_central, width = 9, height = 6, dpi = 150)


message("\nGate 4 complete: two geometry-only prototypes saved to figures/. ",
        "Review BOTH for legibility and correct anchor placement before any ",
        "styling pass. Note the two panels are intentionally NOT the same ",
        "visual grammar — Northwest is a tight hub-and-spoke (BPAT shared by ",
        "TPWR/SCL, PSEI as a single countervailing tie); Central is a ranked ",
        "ego-network (SWPP leading, three plotted supporting corridors plus ",
        "SIKE retained analytically but not rendered) — forcing identical ",
        "layouts would obscure that structural difference.")

