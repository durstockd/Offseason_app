# =============================================================================
# tmu_session_cards.R  —  card + plot builders for the TMU Offseason App
# -----------------------------------------------------------------------------
# Same shape as the pitch card in the pitcher apps:
#
#     [ header row: name | session | hand | pitch count ]
#     [   left plot   |   movement   |   right plot     ]
#     [            stats table (tableGrob)              ]
#
# BULLPEN card:  left = % of each pitch thrown, right = locations.
#                Stats table drops Chase%, Whiff% and AVG — a pen has no
#                swings and no batted balls, so those columns can only ever
#                read "-".
# LIVE AB card:  left = usage vs LHH, right = locations vs RHH, the swing
#                columns come back, plus AVG against and CSW%, and the
#                pitcher's line (BF / H / K / BB) rides in the header.
#
# Also here: the Game Stats / Hitters plots for the live-AB tab (spray chart,
# running average).
#
# Sourced by app.R, after tmu_session_data.R.
# =============================================================================

library(ggplot2)
library(patchwork)
library(grid)
library(gridExtra)
library(scales)

# =============================== KNOBS =======================================

# CATCHER'S VIEW. TrackMan PlateLocSide is positive toward the 3B side (a RHH's
# inside). Plotted raw that is the PITCHER's view, which is how the older apps
# ended up labelled "catcher view" while showing the mirror image. With this
# TRUE the display axis is flipped so positive PlateLocSide draws on the LEFT,
# which is what a catcher (and everyone reading the card) actually sees.
CATCHER_VIEW <- TRUE

# Movement plot axis limits, fixed so every card is on the same scale.
MOVE_LIM <- 25

# Confidence level for the movement ellipses, and the minimum pitches of a type
# before one is drawn at all.
ELLIPSE_LEVEL <- 0.90
ELLIPSE_MIN_N <- 4

# =============================================================================

.disp_side <- function(ps) if (isTRUE(CATCHER_VIEW)) -ps else ps

.plate_segs <- data.frame(
  x    = c(0.71, 0.71,  0,   -0.71, -0.71),
  y    = c(0,    0.3,   0.5,  0.3,   0),
  xend = c(0.71, 0,    -0.71,-0.71,  0.71),
  yend = c(0.3,  0.5,   0.3,  0,     0)
)

.flip_name <- function(x) {
  vapply(strsplit(as.character(x), ",\\s*"), function(p) {
    if (length(p) >= 2 && !is.na(p[2])) paste(p[2], p[1]) else p[1]
  }, character(1))
}

# -----------------------------------------------------------------------------
# make_ellipse_polygons()
#   Eigendecomposition rather than MASS — never fails, no extra dependency.
#   A collinear cluster produces a sliver instead of NaN.
# -----------------------------------------------------------------------------
make_ellipse_polygons <- function(data, level = ELLIPSE_LEVEL,
                                  n_pts = 100, min_n = ELLIPSE_MIN_N) {
  chisq_val <- stats::qchisq(level, df = 2)
  result <- NULL
  for (g in unique(data$PitchGroup)) {
    d <- data[data$PitchGroup == g &
                !is.na(data$HorzBreak) & !is.na(data$InducedVertBreak), ]
    if (nrow(d) < min_n) next
    cx <- mean(d$HorzBreak); cy <- mean(d$InducedVertBreak)
    S <- tryCatch(stats::cov(data.frame(a = d$HorzBreak, b = d$InducedVertBreak)),
                  error = function(e) NULL)
    if (is.null(S)) next
    S[1, 1] <- max(S[1, 1], 0.01); S[2, 2] <- max(S[2, 2], 0.01)
    eig <- tryCatch(eigen(S, symmetric = TRUE), error = function(e) NULL)
    if (is.null(eig)) next
    vals <- pmax(eig$values, 0)
    r1 <- sqrt(chisq_val * vals[1]); r2 <- sqrt(chisq_val * vals[2])
    if (!is.finite(r1) || !is.finite(r2)) next
    ang <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])
    t <- seq(0, 2 * pi, length.out = n_pts + 1)
    ca <- cos(ang); sa <- sin(ang)
    xe <- r1 * cos(t); ye <- r2 * sin(t)
    result <- rbind(result, data.frame(
      HorzBreak        = cx + ca * xe - sa * ye,
      InducedVertBreak = cy + sa * xe + ca * ye,
      PitchGroup       = g, stringsAsFactors = FALSE))
  }
  result
}

# -----------------------------------------------------------------------------
# make_usage_pct_plot()
#   "% of each pitch thrown". Horizontal bars, one per pitch type, biggest at
#   the top, labelled with the percentage and the raw count.
# -----------------------------------------------------------------------------
make_usage_pct_plot <- function(df, title = "Pitch Usage") {
  # Untracked live-AB rows are kept for their result but carry no pitch
  # information worth a bar — they'd only ever add an "Untagged" phantom.
  if ("Tracked" %in% names(df)) df <- df[!is.na(df$Tracked) & df$Tracked, , drop = FALSE]
  d <- df %>%
    dplyr::filter(!is.na(PitchGroup)) %>%
    dplyr::count(PitchGroup, name = "n") %>%
    dplyr::mutate(pct = n / sum(n),
                  lab = paste0(scales::percent(pct, accuracy = 1), "  (", n, ")"))
  if (nrow(d) == 0) return(NULL)
  
  d$PitchGroup <- factor(d$PitchGroup, levels = d$PitchGroup[order(d$n)])
  
  ggplot(d, aes(x = pct, y = PitchGroup, fill = PitchGroup)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = lab), hjust = -0.06, size = 4.4,
              fontface = "bold", color = "black") +
    scale_fill_manual(values = pitch_colors, guide = "none") +
    scale_x_continuous(limits = c(0, max(d$pct) * 1.42),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          axis.text.y = element_text(face = "bold", size = 12),
          axis.text.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "none")
}

# -----------------------------------------------------------------------------
# make_side_usage_plot()
#   Live-AB card only: usage against one batter side.
# -----------------------------------------------------------------------------
make_side_usage_plot <- function(df, side = c("Left", "Right")) {
  side <- match.arg(side)
  d <- df %>% dplyr::filter(BatterSide == side, !is.na(PitchGroup))
  if (nrow(d) == 0) return(NULL)
  make_usage_pct_plot(d, title = paste0("Usage vs ",
                                        if (side == "Left") "LHH" else "RHH"))
}

# -----------------------------------------------------------------------------
# make_movement_plot()
#   HB vs iVB with 90% ellipses. highlight_uid draws a black ring around the
#   selected pitch so the manual tag override has something to point at.
# -----------------------------------------------------------------------------
make_movement_plot <- function(df, highlight_uid = NULL, title = "Pitch Movement") {
  d <- df %>% dplyr::filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
  if (nrow(d) == 0) return(NULL)
  d$PitchGroup <- order_pitch_factor(d$PitchGroup)
  
  ell <- make_ellipse_polygons(d)
  
  p <- ggplot(d, aes(x = HorzBreak, y = InducedVertBreak, color = PitchGroup)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)
  
  if (!is.null(ell) && nrow(ell) > 0) {
    ell$PitchGroup <- factor(ell$PitchGroup, levels = levels(d$PitchGroup))
    p <- p + geom_polygon(data = ell,
                          aes(x = HorzBreak, y = InducedVertBreak,
                              group = PitchGroup, fill = PitchGroup),
                          alpha = 0.13, color = NA, inherit.aes = FALSE)
  }
  
  p <- p + geom_point(size = 3.4, alpha = 0.9)
  
  if (!is.null(highlight_uid)) {
    hl <- d[d$PitchUID %in% highlight_uid, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + geom_point(data = hl, aes(x = HorzBreak, y = InducedVertBreak),
                          shape = 21, size = 6.4, stroke = 1.6,
                          color = "black", fill = NA, inherit.aes = FALSE)
    }
  }
  
  # Pitches the user has hand-overridden get a hollow square so a corrected
  # tag is visible on the card, not just in the table.
  if ("IsOverride" %in% names(d)) {
    ov <- d[isTRUE(d$IsOverride) | (!is.na(d$IsOverride) & d$IsOverride), ,
            drop = FALSE]
    if (nrow(ov) > 0) {
      p <- p + geom_point(data = ov, aes(x = HorzBreak, y = InducedVertBreak),
                          shape = 22, size = 5.2, stroke = 1.1,
                          color = "#1a1a2e", fill = NA, inherit.aes = FALSE)
    }
  }
  
  p +
    scale_color_manual(values = pitch_colors, drop = FALSE) +
    scale_fill_manual(values = pitch_colors, drop = FALSE, guide = "none") +
    scale_x_continuous(limits = c(-MOVE_LIM, MOVE_LIM), breaks = seq(-24, 24, 6)) +
    scale_y_continuous(limits = c(-MOVE_LIM, MOVE_LIM), breaks = seq(-24, 24, 6)) +
    labs(title = title, x = "Horizontal Break (in)",
         y = "Induced Vertical Break (in)", color = NULL) +
    coord_fixed() +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9))
}

# -----------------------------------------------------------------------------
# make_location_plot()
#   Every located pitch in one zone panel, coloured by pitch type. Catcher's
#   view — see the CATCHER_VIEW knob.
# -----------------------------------------------------------------------------
make_location_plot <- function(df, title = "Locations", highlight_uid = NULL) {
  d <- df %>% dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight))
  if (nrow(d) == 0) return(NULL)
  d$PitchGroup <- order_pitch_factor(d$PitchGroup)
  d$.x <- .disp_side(d$PlateLocSide)
  
  p <- ggplot(d, aes(x = .x, y = PlateLocHeight, color = PitchGroup)) +
    geom_rect(data = data.frame(xmin = SZ_XMIN, xmax = SZ_XMAX,
                                ymin = SZ_YMIN, ymax = SZ_YMAX),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = NA, color = "black", linewidth = 1.1) +
    geom_segment(data = .plate_segs,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, color = "black", linewidth = 0.9) +
    geom_point(size = 3.2, alpha = 0.88)
  
  if (!is.null(highlight_uid)) {
    hl <- d[d$PitchUID %in% highlight_uid, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + geom_point(data = hl, aes(x = .x, y = PlateLocHeight),
                          shape = 21, size = 6.4, stroke = 1.6,
                          color = "black", fill = NA, inherit.aes = FALSE)
    }
  }
  
  p +
    scale_color_manual(values = pitch_colors, guide = "none", drop = FALSE) +
    scale_x_continuous(limits = c(-2.5, 2.5)) +
    scale_y_continuous(limits = c(0, 5.5)) +
    labs(title = title, x = NULL, y = NULL) +
    coord_fixed() +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          axis.text = element_blank(),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9))
}

# -----------------------------------------------------------------------------
# pitcher_line_text() — "17 BF  5 H  4 K  2 BB  .294 AVG" for the live header.
#   NULL when the frame has no completed PAs, so the header just omits it.
# -----------------------------------------------------------------------------
pitcher_line_text <- function(df) {
  pa <- pa_table(df)
  if (is.null(pa) || !nrow(pa)) return(NULL)
  s <- summarise_pa(pa)
  paste0(s$PA, " BF  ", s$H, " H  ", s$K, " K  ", s$BB, " BB  ",
         fmt_avg(s$AVG), " AVG")
}

# -----------------------------------------------------------------------------
# make_card_header()
# -----------------------------------------------------------------------------
make_card_header <- function(pitcher_name, df, subtitle_extra = NULL) {
  hand <- unique(df$PitcherThrows); hand <- hand[!is.na(hand)]
  hl <- if (!length(hand)) "" else if (grepl("^L", hand[1])) "LHP" else "RHP"
  
  n_sess <- dplyr::n_distinct(df$SessionKey)
  when <- if (n_sess == 1) pretty_session_date(df$Date[1])
  else paste0(n_sess, " sessions")
  
  n_untr <- if ("Tracked" %in% names(df)) sum(!df$Tracked, na.rm = TRUE) else 0
  n_types <- if ("Tracked" %in% names(df))
    dplyr::n_distinct(df$PitchGroup[!is.na(df$Tracked) & df$Tracked])
  else dplyr::n_distinct(df$PitchGroup)
  sub <- paste(c(hl, when,
                 paste0(nrow(df), " Pitches",
                        if (n_untr > 0) paste0(" (", n_untr, " untracked)") else ""),
                 paste0(n_types, " Pitch Types"),
                 subtitle_extra),
               collapse = "   |   ")
  
  ggplot() +
    annotate("text", x = 0.5, y = 0.70, label = .flip_name(pitcher_name),
             size = 9, fontface = "bold", hjust = 0.5) +
    annotate("text", x = 0.5, y = 0.24, label = sub,
             size = 4.6, color = "gray25", hjust = 0.5) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void()
}

# -----------------------------------------------------------------------------
# make_card_table()
#   One row per pitch type plus an "All" row.
#
#   live = FALSE (bullpen) DROPS Chase%, Whiff% and AVG. In a pen nobody swings
#   and nothing is put in play, so those three can only ever print "-" — a
#   column of dashes reads like broken data rather than absent data.
# -----------------------------------------------------------------------------
make_card_table <- function(df, live = FALSE) {
  d <- df %>% dplyr::filter(!is.na(RelSpeed))
  if (nrow(d) == 0) return(NULL)
  
  throws <- unique(d$PitcherThrows); throws <- throws[!is.na(throws)]
  th <- if (!length(throws)) "R" else throws[1]
  hb_sign <- ifelse(grepl("^L", th), -1, 1)   # show break from the arm's side
  
  d <- d %>%
    dplyr::mutate(
      HB_display = HorzBreak * hb_sign,
      has_loc = !is.na(PlateLocSide) & !is.na(PlateLocHeight),
      in_zone = has_loc &
        PlateLocSide   >= SZ_XMIN & PlateLocSide   <= SZ_XMAX &
        PlateLocHeight >= SZ_YMIN & PlateLocHeight <= SZ_YMAX,
      # BULLPEN: a strike is a pitch in the box, full stop — the PitchCall tag
      # is ignored. LIVE AB: the tag is the only thing that can score a swing,
      # foul or ball in play, so it stays authoritative there.
      is_strike = if (live) PitchCall %in% STRIKE_CALLS else in_zone,
      swung = PitchCall %in% SWING_CALLS,
      whiff = PitchCall == "StrikeSwinging",
      chase = has_loc & !in_zone & swung,
      csw   = PitchCall %in% c("StrikeCalled", "StrikeSwinging")
    )
  
  # AVG against, by the pitch the PA ENDED on. Needs add_pa_outcomes() to have
  # run (app.R does it at load); without it the column is dashes.
  has_pa <- live && all(c("PAEnd", "PAResult") %in% names(d))
  if (has_pa) {
    d <- d %>% dplyr::mutate(
      pa_ab  = !is.na(PAEnd) & PAEnd & PAResult %in% AB_RESULTS,
      pa_hit = !is.na(PAEnd) & PAEnd & PAResult %in% HIT_RESULTS)
  }
  
  core <- function(x) {
    dplyr::tibble(
      Count    = nrow(x),
      Velocity = round(mean(x$RelSpeed, na.rm = TRUE), 1),
      Max      = {m <- suppressWarnings(max(x$RelSpeed, na.rm = TRUE))
      if (is.infinite(m)) NA_real_ else round(m, 1)},
      iVB      = round(mean(x$InducedVertBreak, na.rm = TRUE), 1),
      HB       = round(mean(x$HB_display, na.rm = TRUE), 1),
      Spin     = round(mean(x$SpinRate, na.rm = TRUE), 0),
      Axis     = {a <- mean_axis(x$SpinAxis)
      if (is.na(a)) "-" else tilt_from_axis(a)},
      VAA      = round(mean(x$VertApprAngle, na.rm = TRUE), 1),
      HAA      = round(mean(x$HorzApprAngle, na.rm = TRUE), 1),
      vRel     = round(mean(x$RelHeight, na.rm = TRUE), 2),
      hRel     = round(mean(x$RelSide,   na.rm = TRUE), 2),
      Ext      = round(mean(x$Extension, na.rm = TRUE), 2),
      # Denominator is LOCATED pitches on the bullpen card, so a pitch TrackMan
      # missed is not scored a ball for having no location. On the live card the
      # tag exists for every pitch, so it is over all of them.
      `Strike%`= if (live) scales::percent(mean(x$is_strike, na.rm = TRUE),
                                           accuracy = 1)
      else if (sum(x$has_loc) == 0) "-"
      else scales::percent(sum(x$in_zone) / sum(x$has_loc), accuracy = 1),
      `Zone%`  = if (sum(x$has_loc) == 0) "-"
      else scales::percent(sum(x$in_zone) / sum(x$has_loc), accuracy = 1),
      `Chase%` = if (sum(x$has_loc & !x$in_zone) == 0) "-"
      else scales::percent(sum(x$chase) / sum(x$has_loc & !x$in_zone),
                           accuracy = 1),
      `Whiff%` = if (sum(x$swung) == 0) "-"
      else scales::percent(sum(x$whiff) / sum(x$swung), accuracy = 1),
      `CSW%`   = scales::percent(mean(x$csw), accuracy = 1),
      AVG      = if (!has_pa || sum(x$pa_ab) == 0) "-"
      else fmt_avg(sum(x$pa_hit) / sum(x$pa_ab))
    )
  }
  
  by_type <- d %>%
    dplyr::group_by(PitchGroup) %>%
    dplyr::group_modify(~core(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`Pitch%` = scales::percent(Count / nrow(d), accuracy = 1)) %>%
    dplyr::arrange(match(as.character(PitchGroup), PITCH_ORDER)) %>%
    dplyr::rename(`Pitch Name` = PitchGroup)
  
  all_row <- core(d) %>%
    dplyr::mutate(`Pitch Name` = "All",
                  `Pitch%` = scales::percent(1, accuracy = 1))
  
  cols <- c("Pitch Name","Count","Pitch%","Velocity","Max","iVB","HB","Spin",
            "Axis","VAA","HAA","vRel","hRel","Ext")
  # On the bullpen card Strike% IS Zone% by construction, so only one is shown.
  cols <- if (live) c(cols, "Strike%", "Zone%", "Chase%", "Whiff%", "CSW%", "AVG")
  else      c(cols, "Strike%")
  
  out <- dplyr::bind_rows(by_type[, cols], all_row[, cols])
  out[] <- lapply(out, function(cc) {
    cc <- as.character(cc); cc[is.na(cc) | cc == "NA" | cc == "NaN"] <- "-"; cc
  })
  out
}

# Point height of one table row. Fixed rather than left to tableGrob's natural
# sizing, so make_session_card() can compute exactly how much of the card the
# table needs — the "All" row was being clipped off the bottom because
# wrap_elements() draws a grob at its natural size and lets the card crop it.
TABLE_ROW_PT <- 20
TABLE_PAD_IN <- 0.14

# Table font is FIT TO THE CARD WIDTH. The live table carries 20 columns and
# at a flat 11pt it ran off both edges of a 1600px card (it was already
# clipping "Pitch Name" at 18). Width is estimated from character counts —
# no graphics device needed, so it is safe to call inside renderPlot.
TABLE_FONT_MAX <- 11
TABLE_FONT_MIN <- 7

.est_table_width_in <- function(tbl, fs, pad_mm) {
  w <- vapply(names(tbl), function(cc) {
    head_w <- nchar(cc) * 0.50                    # bold header
    body_w <- max(nchar(as.character(tbl[[cc]])), 0) * 0.45
    max(head_w, body_w) * fs / 72
  }, numeric(1))
  sum(w) + length(w) * 2 * pad_mm / 25.4
}

.table_font_for <- function(tbl, width_in) {
  for (fs in seq(TABLE_FONT_MAX, TABLE_FONT_MIN, by = -0.5)) {
    pad <- 1.2 + 2.8 * (fs - TABLE_FONT_MIN) / (TABLE_FONT_MAX - TABLE_FONT_MIN)
    if (.est_table_width_in(tbl, fs, pad) <= width_in * 0.97)
      return(list(fs = fs, pad = pad))
  }
  list(fs = TABLE_FONT_MIN, pad = 1.2)
}

.table_grob <- function(tbl, width_in = Inf) {
  n  <- nrow(tbl)
  ft <- if (is.finite(width_in)) .table_font_for(tbl, width_in)
  else list(fs = TABLE_FONT_MAX, pad = 4)
  th <- gridExtra::ttheme_minimal(
    core = list(
      fg_params = list(fontsize = ft$fs,
                       fontface = c(rep("plain", n - 1), "bold")),
      bg_params = list(fill = c(rep(c("#ffffff", "#f4f6f8"), length.out = n - 1),
                                "#e6e9ec")),
      padding = grid::unit(c(ft$pad, 2), "mm")),
    colhead = list(fg_params = list(fontsize = ft$fs, fontface = "bold"),
                   bg_params = list(fill = "#d8dde2"),
                   padding = grid::unit(c(ft$pad, 2), "mm"))
  )
  g <- gridExtra::tableGrob(tbl, rows = NULL, theme = th)
  g$heights <- grid::unit(rep(TABLE_ROW_PT, nrow(g)), "pt")
  g
}

# Inches the table will occupy, header row included.
.table_height_in <- function(tbl) {
  if (is.null(tbl)) return(0)
  (nrow(tbl) + 1) * TABLE_ROW_PT / 72 + TABLE_PAD_IN
}

# -----------------------------------------------------------------------------
# make_session_card()
#   The card. Bullpen: usage% | movement | locations.
#              Live AB: usage vs LHH | movement | locations vs RHH.
#
#   card_width_in is the drawing width; the stats table shrinks its font to
#   fit it.
#
#   card_height_in MUST match the height the card is actually drawn at
#   (plotOutput height / res, or ggsave height). The three band heights are
#   computed in real inches from it: the table gets exactly the space its rows
#   need and the plots get the remainder. Passing relative weights instead is
#   what clipped the bold "All" row — a six-pitch arm needs a taller table than
#   a three-pitch arm, and fixed weights can't know that.
# -----------------------------------------------------------------------------
make_session_card <- function(pitcher_name, data, selected_pitches = "All",
                              live = FALSE, highlight_uid = NULL,
                              card_height_in = 7.5,
                              card_width_in = 1600 / 150) {
  
  df <- data %>% dplyr::filter(Pitcher == pitcher_name)
  if (nrow(df) == 0) return(NULL)
  if (!("All" %in% selected_pitches)) {
    df <- df %>% dplyr::filter(PitchGroup %in% selected_pitches)
  }
  if (nrow(df) == 0) return(NULL)
  
  move <- make_movement_plot(df, highlight_uid = highlight_uid)
  if (is.null(move)) return(NULL)
  
  if (live) {
    left  <- make_side_usage_plot(df, "Left")
    right <- make_location_plot(df %>% dplyr::filter(BatterSide == "Right"),
                                title = "Locations vs RHH",
                                highlight_uid = highlight_uid)
    if (is.null(left))  left  <- make_usage_pct_plot(df)
    if (is.null(right)) right <- make_location_plot(df, highlight_uid = highlight_uid)
  } else {
    left  <- make_usage_pct_plot(df)
    right <- make_location_plot(df, highlight_uid = highlight_uid)
  }
  
  if (is.null(left))  left  <- patchwork::plot_spacer()
  if (is.null(right)) right <- patchwork::plot_spacer()
  
  plots_row <- (left | move | right) +
    patchwork::plot_layout(widths = c(1.15, 1.25, 1.15))
  
  tbl <- make_card_table(df, live = live)
  table_plot <- if (is.null(tbl)) patchwork::plot_spacer()
  else patchwork::wrap_elements(full = .table_grob(tbl, width_in = card_width_in))
  
  header <- make_card_header(pitcher_name, df,
                             subtitle_extra = if (live) pitcher_line_text(df))
  
  header_in <- 1.10
  table_in  <- .table_height_in(tbl)
  plots_in  <- max(3.0, card_height_in - header_in - table_in)
  
  (header / plots_row / table_plot) +
    patchwork::plot_layout(heights = c(header_in, plots_in, table_in))
}

# =============================================================================
# ZONE ANALYSIS
# =============================================================================

# -----------------------------------------------------------------------------
# get_zone_bucket()
#   Nine in-zone tiles plus four outer quadrants, in DISPLAY coordinates.
#
#   Two things fixed relative to the older apps:
#   (1) the nine Zone_* branches now carry an OUTER bound on plate side. Without
#       `psd >= SZ_XMIN` a pitch two feet off the plate satisfied `psd < x1` and
#       was labelled an in-zone tile, which also made the trailing wide
#       branches dead code.
#   (2) column index is computed on the display axis, so with CATCHER_VIEW on,
#       column 1 really is the left-hand column of the drawn grid.
# -----------------------------------------------------------------------------
get_zone_bucket <- function(plate_side, plate_height) {
  psd <- .disp_side(plate_side)
  ph  <- plate_height
  
  x1 <- SZ_XMIN + (SZ_XMAX - SZ_XMIN) / 3
  x2 <- SZ_XMIN + 2 * (SZ_XMAX - SZ_XMIN) / 3
  y1 <- SZ_YMIN + (SZ_YMAX - SZ_YMIN) / 3
  y2 <- SZ_YMIN + 2 * (SZ_YMAX - SZ_YMIN) / 3
  ymid <- (SZ_YMIN + SZ_YMAX) / 2
  
  in_x <- !is.na(psd) & psd >= SZ_XMIN & psd <= SZ_XMAX
  in_y <- !is.na(ph)  & ph  >= SZ_YMIN & ph  <= SZ_YMAX
  
  col <- ifelse(!in_x, NA_integer_,
                ifelse(psd < x1, 1L, ifelse(psd < x2, 2L, 3L)))
  row <- ifelse(!in_y, NA_integer_,
                ifelse(ph >= y2, 1L, ifelse(ph >= y1, 2L, 3L)))
  
  out <- rep(NA_character_, length(psd))
  ok <- !is.na(col) & !is.na(row)
  out[ok] <- paste0("Zone_", row[ok], col[ok])
  
  # everything else that has a location is an outer quadrant
  rest <- !ok & !is.na(psd) & !is.na(ph)
  up   <- ph >= ymid
  lft  <- psd <  0
  out[rest &  up &  lft] <- "Outer_UL"
  out[rest &  up & !lft] <- "Outer_UR"
  out[rest & !up &  lft] <- "Outer_LL"
  out[rest & !up & !lft] <- "Outer_LR"
  out
}

# Strike % is deliberately absent from the bullpen list: with strikes derived
# from the zone, every in-zone tile reads 100% and every outer tile 0%, which
# adds nothing the tile's own position does not already show.
ZONE_METRICS_BULLPEN <- c("Pitch %")
ZONE_METRICS_LIVE    <- c("Pitch %", "Strike %", "Swing %", "Whiff %",
                          "Chase %", "Exit Velocity", "Hard Hit %")

# -----------------------------------------------------------------------------
# calculate_zone_stats()
#   Returns one row per bucket. Pitch % is the share of the pitcher's LOCATED
#   pitches that landed in that tile, so the thirteen tiles sum to 100%.
# -----------------------------------------------------------------------------
calculate_zone_stats <- function(df, metric_name, strike_from_zone = FALSE) {
  d <- df %>%
    dplyr::mutate(ZoneBucket = get_zone_bucket(PlateLocSide, PlateLocHeight)) %>%
    dplyr::filter(!is.na(ZoneBucket)) %>%
    add_session_flags(strike_from_zone = strike_from_zone)
  n_tot <- nrow(d)
  
  vals <- d %>%
    dplyr::group_by(ZoneBucket) %>%
    dplyr::summarise(
      Value = dplyr::case_when(
        metric_name == "Pitch %"       ~ 100 * dplyr::n() / n_tot,
        metric_name == "Strike %"      ~ 100 * mean(IsStrike, na.rm = TRUE),
        metric_name == "Swing %"       ~ 100 * mean(IsSwing,  na.rm = TRUE),
        metric_name == "Whiff %"       ~ ifelse(sum(IsSwing, na.rm = TRUE) == 0,
                                                NA_real_,
                                                100 * sum(IsWhiff, na.rm = TRUE) /
                                                  sum(IsSwing, na.rm = TRUE)),
        metric_name == "Chase %"       ~ ifelse(sum(!InZone, na.rm = TRUE) == 0,
                                                NA_real_,
                                                100 * sum(IsChase, na.rm = TRUE) /
                                                  sum(!InZone, na.rm = TRUE)),
        metric_name == "Exit Velocity" ~ mean(ExitSpeed[IsBIP], na.rm = TRUE),
        metric_name == "Hard Hit %"    ~ ifelse(sum(IsBIP, na.rm = TRUE) == 0,
                                                NA_real_,
                                                100 * mean(ExitSpeed[IsBIP] >= 95,
                                                           na.rm = TRUE)),
        TRUE ~ NA_real_),
      N = dplyr::n(),
      .groups = "drop")
  
  all_buckets <- data.frame(ZoneBucket = c(
    "Outer_UL","Outer_UR","Outer_LL","Outer_LR",
    "Zone_11","Zone_12","Zone_13",
    "Zone_21","Zone_22","Zone_23",
    "Zone_31","Zone_32","Zone_33"), stringsAsFactors = FALSE)
  
  all_buckets %>% dplyr::left_join(vals, by = "ZoneBucket")
}

# -----------------------------------------------------------------------------
# make_zone_grid_plot()
#   The thirteen-tile grid. Tile geometry is in an abstract 1-6 space; the
#   bucket labels already carry the display flip, so nothing is mirrored here.
# -----------------------------------------------------------------------------
make_zone_grid_plot <- function(df, metric_name, title = NULL,
                                strike_from_zone = FALSE) {
  d <- df %>% dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight))
  if (nrow(d) == 0) return(NULL)
  
  zone_df <- calculate_zone_stats(d, metric_name,
                                  strike_from_zone = strike_from_zone)
  zone_df$Label <- ifelse(
    is.na(zone_df$Value), "",
    if (metric_name == "Exit Velocity") sprintf("%.1f", zone_df$Value)
    else paste0(round(zone_df$Value, 0), "%"))
  
  strike_tiles <- data.frame(
    ZoneBucket = c("Zone_11","Zone_12","Zone_13",
                   "Zone_21","Zone_22","Zone_23",
                   "Zone_31","Zone_32","Zone_33"),
    xmin = c(2,3,4, 2,3,4, 2,3,4),
    xmax = c(3,4,5, 3,4,5, 3,4,5),
    ymin = c(4,4,4, 3,3,3, 2,2,2),
    ymax = c(5,5,5, 4,4,4, 3,3,3),
    cx   = c(2.5,3.5,4.5, 2.5,3.5,4.5, 2.5,3.5,4.5),
    cy   = c(4.5,4.5,4.5, 3.5,3.5,3.5, 2.5,2.5,2.5),
    stringsAsFactors = FALSE
  ) %>% dplyr::left_join(zone_df, by = "ZoneBucket")
  
  outer_polys <- dplyr::bind_rows(
    data.frame(x = c(1,3.5,3.5,2,2,1),   y = c(6,6,5,5,3.5,3.5), g = "Outer_UL"),
    data.frame(x = c(3.5,6,6,5,5,3.5),   y = c(6,6,3.5,3.5,5,5), g = "Outer_UR"),
    data.frame(x = c(1,3.5,3.5,2,2,1),   y = c(1,1,2,2,3.5,3.5), g = "Outer_LL"),
    data.frame(x = c(3.5,6,6,5,5,3.5),   y = c(1,1,3.5,3.5,2,2), g = "Outer_LR")
  ) %>%
    dplyr::mutate(ZoneBucket = g) %>%
    dplyr::left_join(zone_df[, c("ZoneBucket","Value","Label")], by = "ZoneBucket")
  
  outer_labels <- data.frame(
    ZoneBucket = c("Outer_UL","Outer_UR","Outer_LL","Outer_LR"),
    cx = c(1.65, 5.45, 1.65, 5.45),
    cy = c(5.15, 5.15, 1.85, 1.85), stringsAsFactors = FALSE
  ) %>% dplyr::left_join(zone_df[, c("ZoneBucket","Value","Label")], by = "ZoneBucket")
  
  # No home plate under the grid. It ate vertical room in every panel and,
  # once several grids are wrapped side by side, bought nothing — the outer
  # quadrants already read as the zone. ylim now stops at the bottom edge of
  # the grid (y = 1) so the tiles get that space back.
  
  mid <- if (all(is.na(zone_df$Value))) 0 else stats::median(zone_df$Value, na.rm = TRUE)
  
  ggplot() +
    geom_rect(data = strike_tiles,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Value),
              color = "grey55", linewidth = 1.1) +
    geom_polygon(data = outer_polys,
                 aes(x = x, y = y, group = g, fill = Value),
                 color = "grey55", linewidth = 1.1) +
    geom_text(data = strike_tiles, aes(x = cx, y = cy, label = Label),
              size = 6.6, fontface = "bold") +
    geom_text(data = outer_labels, aes(x = cx, y = cy, label = Label),
              size = 6.6, fontface = "bold") +
    scale_fill_gradient2(low = "#4f74b8", mid = "#f2f2f2", high = "#e07a7a",
                         midpoint = mid, na.value = "grey95") +
    coord_fixed(xlim = c(0.9, 6.1), ylim = c(0.9, 6.1), clip = "off") +
    labs(title = title, x = NULL, y = NULL) +
    theme_void(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 17,
                                    lineheight = 1.1,
                                    margin = margin(b = 4)),
          plot.margin = margin(4, 4, 4, 4),
          legend.position = "none")
}

# -----------------------------------------------------------------------------
# make_zone_grid_by_pitch()
#   One grid per pitch type, side by side — "pitch % for every pitch in the
#   zone". Each grid's percentages are within that pitch type, so each panel
#   sums to 100%.
# -----------------------------------------------------------------------------
make_zone_grid_by_pitch <- function(df, metric_name, ncol = NULL,
                                    strike_from_zone = FALSE) {
  types <- PITCH_ORDER[PITCH_ORDER %in% unique(df$PitchGroup)]
  types <- types[vapply(types, function(t)
    sum(df$PitchGroup == t & is.finite(df$PlateLocSide) &
          is.finite(df$PlateLocHeight)) > 0, logical(1))]
  if (!length(types)) return(NULL)
  
  plots <- lapply(types, function(t) {
    n <- sum(df$PitchGroup == t)
    make_zone_grid_plot(df %>% dplyr::filter(PitchGroup == t), metric_name,
                        title = paste0(t, "  (", n, ")"),
                        strike_from_zone = strike_from_zone)
  })
  plots <- Filter(Negate(is.null), plots)
  if (!length(plots)) return(NULL)
  
  # Balanced wrap: 4 types go 2x2 rather than 3-then-1, which left one grid
  # stranded on its own row with a page of white space beside it.
  if (is.null(ncol)) {
    n <- length(plots)
    ncol <- if (n <= 2) n else if (n <= 4) 2 else 3
  }
  patchwork::wrap_plots(plots, ncol = min(ncol, length(plots)))
}

# -----------------------------------------------------------------------------
# make_zone_table()
#   Pitch % by zone AND pitch type, as a table. Rows are the thirteen buckets,
#   columns the pitch types.
# -----------------------------------------------------------------------------
make_zone_table <- function(df) {
  d <- df %>%
    dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight)) %>%
    dplyr::mutate(Zone = get_zone_bucket(PlateLocSide, PlateLocHeight)) %>%
    dplyr::filter(!is.na(Zone))
  if (nrow(d) == 0) return(NULL)
  
  lv <- c("Zone_11","Zone_12","Zone_13","Zone_21","Zone_22","Zone_23",
          "Zone_31","Zone_32","Zone_33",
          "Outer_UL","Outer_UR","Outer_LL","Outer_LR")
  
  d %>%
    dplyr::count(Zone, PitchGroup, name = "n") %>%
    dplyr::group_by(PitchGroup) %>%
    dplyr::mutate(pct = paste0(round(100 * n / sum(n), 0), "%")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n) %>%
    tidyr::pivot_wider(names_from = PitchGroup, values_from = pct,
                       values_fill = "\u2014") %>%
    dplyr::mutate(Zone = factor(Zone, levels = lv)) %>%
    dplyr::arrange(Zone) %>%
    dplyr::mutate(Zone = as.character(Zone))
}
# =============================================================================
# LIVE-AB RESULTS PLOTS
# =============================================================================

RESULT_COLORS <- c(
  `1B` = "#7D3C98", `2B` = "#8E44AD", `3B` = "#A569BD", HR = "#4A235A",
  Out = "#21618C", FC = "#2E86C1", E = "#B9770E", SF = "#5D6D7E",
  SH = "#5D6D7E", BIP = "#7F8C8D"
)

# -----------------------------------------------------------------------------
# make_spray_chart()
#   Balls in play from pa_table(), placed by TrackMan Bearing (landing
#   direction, 0 = straightaway, negative = pull side for a RHH / 3B line) and
#   Distance. Falls back to launch Direction when Bearing is missing. Distance
#   is TrackMan's modelled carry, same as the Y'alls barrel board.
# -----------------------------------------------------------------------------
make_spray_chart <- function(pa, title = "Spray Chart") {
  if (is.null(pa) || !nrow(pa)) return(NULL)
  d <- pa[pa$BIP, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ang  <- ifelse(is.na(d$Bearing), d$Direction, d$Bearing)
  keep <- !is.na(ang) & !is.na(d$Distance)
  d <- d[keep, , drop = FALSE]; ang <- ang[keep]
  if (!nrow(d)) return(NULL)
  d$x <- d$Distance * sin(ang * pi / 180)
  d$y <- d$Distance * cos(ang * pi / 180)
  d$Result <- factor(d$PAResult, levels = names(RESULT_COLORS))
  
  arc <- function(r) {
    t <- seq(-45, 45, length.out = 60) * pi / 180
    data.frame(x = r * sin(t), y = r * cos(t), r = r)
  }
  arcs  <- rbind(arc(150), arc(250), arc(330))
  lines <- data.frame(x = c(0, 0), y = c(0, 0),
                      xend = 340 * sin(c(-45, 45) * pi / 180),
                      yend = 340 * cos(c(-45, 45) * pi / 180))
  
  ggplot(d, aes(x = x, y = y)) +
    geom_path(data = arcs, aes(group = r), color = "gray80", linetype = "dashed") +
    geom_segment(data = lines, aes(xend = xend, yend = yend),
                 color = "gray45", linewidth = 0.8) +
    geom_point(aes(fill = Result), shape = 21, size = 4.2, color = "black",
               stroke = 0.4, alpha = 0.92) +
    scale_fill_manual(values = RESULT_COLORS, drop = TRUE) +
    coord_fixed(xlim = c(-260, 260), ylim = c(-10, 420)) +
    labs(title = title, fill = NULL, x = NULL, y = NULL) +
    theme_void(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          legend.position = "bottom")
}

# -----------------------------------------------------------------------------
# make_running_avg_plot()
#   One hitter's AVG / OBP / SLG to date across the offseason, one point per
#   session, from hitter_session_log().
# -----------------------------------------------------------------------------
make_running_avg_plot <- function(log, batter) {
  if (is.null(log) || !nrow(log)) return(NULL)
  d <- log[log$Batter == batter, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$When <- suppressWarnings(as.Date(d$Date))
  if (all(is.na(d$When))) d$When <- as.Date("2026-01-01") + seq_len(nrow(d))
  
  long <- rbind(
    data.frame(When = d$When, Stat = "AVG", Value = d$`AVG to date`),
    data.frame(When = d$When, Stat = "OBP", Value = d$`OBP to date`),
    data.frame(When = d$When, Stat = "SLG", Value = d$`SLG to date`))
  long <- long[!is.na(long$Value), , drop = FALSE]
  if (!nrow(long)) return(NULL)
  # Drawn SLG first and AVG last so AVG stays visible when the two are equal
  # (every hit a single); the legend keeps the usual AVG / OBP / SLG order.
  long$Stat <- factor(long$Stat, levels = c("SLG", "OBP", "AVG"))
  long <- long[order(long$Stat), , drop = FALSE]
  long$Lab  <- fmt_avg(long$Value)
  
  last <- long[long$When == max(long$When), , drop = FALSE]
  
  ggplot(long, aes(x = When, y = Value, color = Stat, group = Stat)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_text(data = last, aes(label = Lab), hjust = -0.25, size = 4.2,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c(AVG = "#1a1a2e", OBP = "#2E86C1",
                                  SLG = "#C0392B"),
                       breaks = c("AVG", "OBP", "SLG")) +
    scale_y_continuous(labels = function(v) fmt_avg(v),
                       limits = c(0, max(0.5, max(long$Value) * 1.12))) +
    scale_x_date(date_labels = "%b %d",
                 expand = expansion(mult = c(0.05, 0.18))) +
    labs(title = paste0(.flip_name(batter), " \u2014 Offseason To Date"),
         x = NULL, y = NULL, color = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          legend.position = "bottom",
          panel.grid.minor = element_blank())
}