# =============================================================================
# app.R  —  TMU Offseason App  (Bullpens + Live ABs / Scrimmages)
# -----------------------------------------------------------------------------
# Same shape as the pitcher apps: a pitch card on the first tab, pitch
# sequence on the second, zone analysis on the third, manual tag override in
# the sidebar.
#
# BULLPENS tab
#   Pitch Card     — [ % of each pitch thrown | movement | locations ]
#                    Stats table without Chase%, Whiff% or AVG.
#   Pitch Sequence — every pitch he threw, in order.
#   Zone Analysis  — pitch % per zone, overall and per pitch type.
#   Download       — TMUBullpenReport.Rmd, rendered from what's on screen.
#
# LIVE ABs / SCRIMMAGES tab
#   Same three, plus batter side and count filters, with the swing columns,
#   AVG against and CSW% back in, and:
#   Game Stats     — his line (BF/H/K/BB/slash), results by pitch type,
#                    platoon splits, every PA, spray chart.
#   Hitters        — offseason hitting board + one hitter's running
#                    AVG / OBP / SLG across every session.
#   Download       — TMUPitcherReport.Rmd (the Y'alls pitcher report,
#                    converted), rendered from what's on screen.
#
# NO Stuff+. NO auto tagger — PitchGroup is the hand-entered TaggedPitchType,
# spelling standardised; a pitch left untagged takes TrackMan's AutoPitchType
# (UNTAGGED_FALLBACK_TO_AUTO in tmu_session_data.R), and anything overridden
# by hand in the sidebar beats both. TagSource says which one each pitch used.
#
# DATA: tmu_session_data.rds, built by build_tmu_session_data.R. With
# DATA_SOURCE = "auto" a folder whose CSVs changed since the build is re-read
# at startup, so new exports always show up.
#
# Folder layout:
#   Offseason App/
#     app.R
#     tmu_session_data.R
#     tmu_session_cards.R
#     build_tmu_session_data.R
#     tmu_session_data.rds
#     TMUBullpenReport.Rmd
#     TMUPitcherReport.Rmd
#     tag_overrides.csv
#     bullpens/
#     live_abs/
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

source("tmu_session_data.R")
source("tmu_session_cards.R")

# =============================== KNOBS =======================================

# Card render size. Fixed, NOT "100%": renderPlot sizes the device from the
# container, so in a narrow viewer pane the whole card scales down and the
# stats table runs off the right edge.
#
# CARD_RES and CARD_H_PX together give the drawing height in inches, which is
# handed to make_session_card() so it can budget the stats table exactly. Change
# one and the card re-budgets itself — do NOT change them independently of
# CARD_H_IN below.
CARD_W_PX  <- 1600
CARD_H_PX  <- 1180
CARD_RES   <- 150
CARD_W     <- paste0(CARD_W_PX, "px")
CARD_H     <- paste0(CARD_H_PX, "px")
CARD_W_IN  <- CARD_W_PX / CARD_RES
CARD_H_IN  <- CARD_H_PX / CARD_RES

# Reports behind the sidebar download button. Both .Rmd files sit next to
# app.R and are rendered from a temp copy, so the app folder is never
# written to.
REPORT_RMD <- c(bullpen = "TMUBullpenReport.Rmd", liveab = "TMUPitcherReport.Rmd")

# Hitters tab: default minimum PA for the board.
HITTER_MIN_PA_DEFAULT <- 1

# Live ABs: a game file (Yakkertech @ NKU) carries both teams. TRUE keeps the
# pitcher dropdown to TMU arms and the Hitters tab to TMU bats (PitcherTeam /
# BatterTeam == TMU_TEAM_CODE, or blank). Opponent pitches stay in the data,
# so PAs, counts and results are still built from the full game. FALSE shows
# everyone.
LIVEAB_TMU_ONLY <- TRUE

# Zone Analysis render height. The grids are coord_fixed, so height is what
# sizes them — the wrap centres itself in whatever width the pane gives it.
# Bump this if the tiles still read small on a large display.
ZONE_PLOT_H <- "1050px"

# Pitch types offered in the manual override dropdown.
OVERRIDE_CHOICES <- c("Four-Seam","Two-Seam","Sinker","Cutter","Slider",
                      "Sweeper","Curveball","Changeup","Splitter", UNTAGGED_LABEL)

# =============================================================================

APP_DIR <- normalizePath(getwd())

STORE <- tryCatch(load_session_store(), error = function(e) {
  message("Session store load failed (", conditionMessage(e),
          ") — reading CSVs directly.")
  tryCatch(load_session_store("csv"), error = function(e2) {
    message("CSV load failed too: ", conditionMessage(e2))
    list(bullpen = NULL, liveab = NULL)
  })
})

BULLPENS <- STORE$bullpen
# PA ids / results are built on the WHOLE live-AB frame, before any pitcher
# filter — a PA belongs to the session's pitch order, not to one arm.
LIVEABS  <- tryCatch(add_pa_outcomes(STORE$liveab), error = function(e) {
  message("PA outcome build failed: ", conditionMessage(e)); STORE$liveab })
rm(STORE)

BULLPEN_IDX <- build_session_index(BULLPENS)
LIVEAB_IDX  <- build_session_index(LIVEABS)

flip_name <- function(x) {
  vapply(strsplit(as.character(x), ",\\s*"), function(p) {
    if (length(p) >= 2 && !is.na(p[2])) paste(p[2], p[1]) else p[1]
  }, character(1))
}

tmu_rows <- function(d, col) {
  if (is.null(d) || !isTRUE(LIVEAB_TMU_ONLY) || !col %in% names(d)) return(d)
  d[is.na(d[[col]]) | d[[col]] == "" | d[[col]] == TMU_TEAM_CODE, , drop = FALSE]
}

first_non_na <- function(v) {
  v <- v[!is.na(v)]
  if (length(v)) v[[1]] else NA_character_
}

pa_result_code <- function(r) {
  dplyr::case_when(r %in% c("SF", "SH") ~ "SAC", r == "BIP" ~ "IP", TRUE ~ r)
}

fmt1 <- function(x) ifelse(is.na(.num(x)), "\u2014", sprintf("%.1f", .num(x)))
fmt0 <- function(x) ifelse(is.na(.num(x)), "\u2014", sprintf("%.0f", .num(x)))

# Result abbreviation, shared by the sequence table. On a live-AB frame the
# PA-ending pitch shows the PA result from add_pa_outcomes() instead, so a K
# inferred from the count reads K, not CS.
res_abbrev <- function(pitch_call, play_result = NA, pa_end = NULL,
                       pa_result = NULL) {
  per_pitch <- .res_abbrev_pitch(pitch_call, play_result)
  if (is.null(pa_end) || is.null(pa_result)) return(per_pitch)
  dplyr::case_when(
    !is.na(pa_end) & pa_end & pa_result %in% c("SF", "SH")  ~ "SAC",
    !is.na(pa_end) & pa_end & pa_result == "BIP"            ~ "IP",
    !is.na(pa_end) & pa_end & !is.na(pa_result) &
      pa_result != "Incomplete"                             ~ pa_result,
    TRUE                                                    ~ per_pitch)
}

.res_abbrev_pitch <- function(pitch_call, play_result = NA) {
  dplyr::case_when(
    pitch_call == "HitByPitch"                        ~ "HBP",
    pitch_call == "BallIntentional"                   ~ "IB",
    pitch_call %in% c("BallCalled","BallinDirt")      ~ "B",
    pitch_call == "StrikeCalled"                      ~ "CS",
    pitch_call == "StrikeSwinging"                    ~ "SW",
    grepl("^Foul", pitch_call)                        ~ "F",
    pitch_call == "InPlay" & play_result == "Single"  ~ "1B",
    pitch_call == "InPlay" & play_result == "Double"  ~ "2B",
    pitch_call == "InPlay" & play_result == "Triple"  ~ "3B",
    pitch_call == "InPlay" & play_result == "HomeRun" ~ "HR",
    pitch_call == "InPlay" & play_result == "Out"     ~ "Out",
    pitch_call == "InPlay"                            ~ "IP",
    TRUE                                              ~ "\u2014")
}

RES_COLORS <- c(B = "#C0392B", IB = "#C0392B", HBP = "#C0392B", BB = "#C0392B",
                CS = "#1E8449", SW = "#1E8449", K = "#1E8449", F = "#B9770E",
                `1B` = "#7D3C98", `2B` = "#7D3C98", `3B` = "#7D3C98",
                HR = "#7D3C98", Out = "#21618C", IP = "#21618C",
                FC = "#21618C", E = "#B9770E", SAC = "#21618C")

# -----------------------------------------------------------------------------
# stat_dt() — a DT table for summarise_pa()/summarise_pitches() output that
#   keeps the numbers NUMERIC (so columns sort properly) and only formats at
#   display time: AVG/OBP/SLG/OPS as .333, anything ending in % as 33%, EV to
#   one decimal. NA shows as a dash.
# -----------------------------------------------------------------------------
.JS_AVG <- DT::JS(
  "function(d,t){if(t!=='display')return d;",
  "if(d===null||d===''||d==='NA'||isNaN(d))return '\u2014';",
  "var s=Number(d).toFixed(3);return s.charAt(0)==='0'?s.substring(1):s;}")
.JS_PCT <- DT::JS(
  "function(d,t){if(t!=='display')return d;",
  "if(d===null||d===''||d==='NA'||isNaN(d))return '\u2014';",
  "return Math.round(Number(d)*100)+'%';}")
.JS_EV <- DT::JS(
  "function(d,t){if(t!=='display')return d;",
  "if(d===null||d===''||d==='NA'||isNaN(d))return '\u2014';",
  "return Number(d).toFixed(1);}")

stat_dt <- function(df, order_by = NULL, desc = TRUE, page_len = 50,
                    selection = "none", bold_last = FALSE, dom = "t") {
  df  <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  nm  <- names(df)
  idx <- function(p) which(p(nm)) - 1L          # DT columns are 0-based
  defs <- list(
    list(targets = idx(function(n) n %in% c("AVG","OBP","SLG","OPS") |
                         (grepl("to date$", n) & !grepl("^PA", n))),
         render = .JS_AVG),
    list(targets = idx(function(n) grepl("%$", n)), render = .JS_PCT),
    list(targets = idx(function(n) n %in% c("Avg EV","Max EV","Velo","EV")),
         render = .JS_EV),
    list(targets = "_all", className = "dt-center"))
  defs <- Filter(function(d) length(d$targets) > 0, defs)
  ord <- if (!is.null(order_by) && order_by %in% nm)
    list(list(match(order_by, nm) - 1L, if (desc) "desc" else "asc")) else list()
  dt <- DT::datatable(
    df, rownames = FALSE, selection = selection,
    options = list(pageLength = page_len, scrollX = TRUE, autoWidth = FALSE,
                   searching = FALSE, dom = dom, order = ord,
                   columnDefs = defs))
  if (bold_last && nrow(df) > 0) {
    dt <- DT::formatStyle(dt, names(df)[1], target = "row",
                          fontWeight = DT::styleRow(nrow(df), "bold"))
  }
  dt
}

# The pitcher's line for any frame: slash line + discipline, one row.
pitcher_line_row <- function(d) {
  pa <- pa_table(d)
  a  <- summarise_pa(pa)
  b  <- summarise_pitches(d)
  dplyr::bind_cols(
    a[, c("PA","AB","H","2B","3B","HR","BB","HBP","K","AVG","OBP","SLG",
          "K%","BB%")],
    b[, c("Pitches","Strike%","Whiff%","Chase%","CSW%","FPS%","Avg EV","HH%")]
  ) %>% dplyr::rename(BF = PA)
}

# Renders one of the report .Rmds from a temp copy. The frame the app is
# showing is written to an .rds and handed over as a param, so the report
# carries every override and filter exactly as on screen.
render_session_report <- function(mode, frame, file, extra_params = list()) {
  rmd <- REPORT_RMD[[mode]]
  src <- file.path(APP_DIR, rmd)
  if (!file.exists(src)) {
    stop(rmd, " is not in the app folder (", APP_DIR, ").")
  }
  if (is.null(frame) || !nrow(frame)) stop("Nothing to report for this filter.")
  
  tmp <- tempfile("tmu_report_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(src, tmp)
  data_path <- file.path(tmp, "report_data.rds")
  saveRDS(as.data.frame(frame), data_path)
  
  params <- if (mode == "bullpen") {
    list(data_file = data_path, team_name = TMU_TEAM_CODE,
         session_label = SESSION_LABELS[["bullpen"]])
  } else {
    list(app_dir = APP_DIR, rds_file = data_path, team_name = TMU_TEAM_CODE,
         session_label = SESSION_LABELS[["liveab"]])
  }
  params <- utils::modifyList(params, extra_params)
  
  out <- rmarkdown::render(
    input = file.path(tmp, rmd), params = params,
    output_file = "report.html", output_dir = tmp, intermediates_dir = tmp,
    knit_root_dir = tmp,
    # a fresh environment: a knit must never see (or leave behind) objects
    # in the app's global env
    envir = new.env(parent = globalenv()), quiet = TRUE)
  file.copy(out, file, overwrite = TRUE)
  invisible(file)
}

# =============================================================================
# UI
# =============================================================================

mode_sidebar <- function(p, idx, live) {
  ch <- session_choices(idx)
  tagList(
    selectizeInput(paste0(p, "_session"),
                   if (live) "Session(s)" else "Bullpen(s)",
                   choices  = c("All Sessions" = "All Sessions", ch),
                   selected = "All Sessions", multiple = TRUE,
                   options  = list(plugins = list("remove_button"))),
    selectInput(paste0(p, "_pitcher"), "Choose Pitcher", choices = NULL),
    selectizeInput(paste0(p, "_pitch"), "Pitch Type(s)",
                   choices = "All", selected = "All", multiple = TRUE,
                   options = list(plugins = list("remove_button"))),
    
    if (live) selectInput(paste0(p, "_side"), "Batter Side",
                          choices = c("All","Left","Right"), selected = "All"),
    if (live) selectInput(paste0(p, "_count"), "Count State",
                          choices = c("All Counts","Ahead","Even",
                                      "Behind","Two Strikes"),
                          selected = "All Counts"),
    
    tags$hr(),
    tags$details(
      tags$summary(style = "font-weight:600;cursor:pointer;margin-bottom:8px;",
                   HTML("&#9656; Manual Tag Override")),
      helpText("Click a dot in the movement plot below, pick what it should ",
               "be, then Apply. Corrections are saved to ", TAG_OVERRIDE_FILE,
               " and reload automatically \u2014 they are keyed on the pitch's ",
               "TrackMan UID, so replacing the export in ",
               SESSION_DIRS[[if (live) "liveab" else "bullpen"]],
               "/ with a fresh download keeps them."),
      textOutput(paste0(p, "_sel_text")),
      uiOutput(paste0(p, "_ovr_status")),
      selectInput(paste0(p, "_manual_tag"), "Change selected pitch to",
                  choices = OVERRIDE_CHOICES, selected = "Four-Seam"),
      actionButton(paste0(p, "_apply_ovr"),  "Apply Tag Change"),
      br(), br(),
      actionButton(paste0(p, "_clear_ovr"),  "Clear Selected Override"),
      br(), br(),
      actionButton(paste0(p, "_clear_all")," Clear All Overrides"),
      br(), br(),
      plotOutput(paste0(p, "_click_move"), height = "320px", width = "100%",
                 click = paste0(p, "_move_click"))
    ),
    
    tags$hr(),
    radioButtons(paste0(p, "_rpt_scope"), "Report covers",
                 choices = c("This pitcher" = "one",
                             "Every pitcher in the selected session(s)" = "all"),
                 selected = "one"),
    downloadButton(paste0(p, "_dl_report"),
                   if (live) "Download Pitcher Report" else "Download Bullpen Report"),
    helpText(style = "font-size:11px;",
             "HTML \u2014 open it and print (letter, portrait). One card per ",
             "pitcher per session, overrides included. ",
             if (live) "Side / count filters are ignored; the report is the full outing."),
    br(),
    downloadButton(paste0(p, "_dl_data"), "Download Corrected Data")
  )
}

# Live-AB only tabs.
game_stats_tab <- function(p) {
  tabPanel(
    "Game Stats", br(),
    helpText("His results for the sessions / side / count picked in the ",
             "sidebar. With a count filter on, PA results count the PAs that ",
             "ENDED in that count. Pitch-type filter is ignored here \u2014 ",
             "every pitch type is shown."),
    h4("Pitching Line"),
    DTOutput(paste0(p, "_gs_line")),
    br(),
    h4("Results by Pitch Type"),
    helpText("PA columns (AB / H / AVG / K / BB) count the pitch each PA ended on."),
    DTOutput(paste0(p, "_gs_types")),
    br(),
    fluidRow(
      column(6, h4("Platoon Splits"), DTOutput(paste0(p, "_gs_plat"))),
      column(6, h4("Spray Chart (Balls In Play Against)"),
             plotOutput(paste0(p, "_gs_spray"), height = "380px"))
    ),
    br(),
    h4("Every Plate Appearance"),
    DTOutput(paste0(p, "_gs_pas"))
  )
}

hitters_tab <- function(p) {
  tabPanel(
    "Hitters", br(),
    fluidRow(
      column(3, selectInput(paste0(p, "_hit_hand"), "vs Pitcher Hand",
                            choices = c("All", "Right", "Left"), selected = "All")),
      column(2, numericInput(paste0(p, "_hit_minpa"), "Min PA",
                             value = HITTER_MIN_PA_DEFAULT, min = 0, step = 1)),
      column(7, helpText("TMU hitters only. The board uses the Session(s) filter in the sidebar ",
                         "and ignores the pitcher, pitch-type, side and count ",
                         "filters. The trend chart always covers every session ",
                         "of the offseason. Click a row to open that hitter."))
    ),
    h4("Offseason Hitting"),
    DTOutput(paste0(p, "_hit_board")),
    downloadButton(paste0(p, "_hit_dl"), "Download Hitting CSV"),
    tags$hr(),
    fluidRow(column(4, selectInput(paste0(p, "_hitter"), "Hitter", choices = NULL))),
    fluidRow(
      column(7, plotOutput(paste0(p, "_hit_trend"), height = "400px")),
      column(5, plotOutput(paste0(p, "_hit_spray"), height = "400px"))
    ),
    h4("Session by Session"),
    helpText("\"to date\" columns are his running totals through that session."),
    DTOutput(paste0(p, "_hit_log")),
    br(),
    h4("Every Plate Appearance"),
    DTOutput(paste0(p, "_hit_pas"))
  )
}

mode_main <- function(p, live) {
  tabsetPanel(
    tabPanel(
      "Pitch Card", br(),
      h4(textOutput(paste0(p, "_summary_label"))),
      uiOutput(paste0(p, "_untagged_note")),
      plotOutput(paste0(p, "_card"), height = CARD_H, width = CARD_W)
    ),
    tabPanel(
      "Pitch Sequence", br(),
      h4("Every Pitch Thrown"),
      if (!live) helpText(
        "PT# is which pitch of that type it was \u2014 \"3/12\" is the 3rd of ",
        "12 sinkers. Click a row to select that pitch for a tag override."),
      helpText("Src: blank = your tag, TM = the tracking system's auto tag ",
               "(TrackMan / Yakkertech \u2014 you left it untagged), ",
               "OVR = manual override."),
      DTOutput(paste0(p, "_seq")),
      br(),
      fluidRow(
        column(5, h4("Selected Pitch \u2014 Location"),
               plotOutput(paste0(p, "_seq_loc"), height = "400px")),
        column(7, h4("Selected Pitch \u2014 Movement"),
               plotOutput(paste0(p, "_seq_move"), height = "400px"))
      )
    ),
    tabPanel(
      "Zone Analysis", br(),
      fluidRow(
        column(3, selectInput(paste0(p, "_zone_metric"), "Zone Metric",
                              choices = if (live) ZONE_METRICS_LIVE
                              else ZONE_METRICS_BULLPEN,
                              selected = "Pitch %")),
        column(3, br(), checkboxInput(paste0(p, "_zone_split"),
                                      "Split by pitch type", value = TRUE))
      ),
      helpText("Percentages are within whatever is drawn: the overall grid ",
               "sums to 100% across the thirteen tiles, and each per-pitch ",
               "grid sums to 100% within that pitch type."),
      plotOutput(paste0(p, "_zone_plot"), height = ZONE_PLOT_H)
    ),
    if (live) game_stats_tab(p),
    if (live) hitters_tab(p)
  )
}

ui <- fluidPage(
  # A DataTable rendered inside a hidden tab measures its columns against a
  # zero-width container, which is why the Pitch Sequence table came
  # up with the header and the body on different grids and a block of dead
  # space down the left. Re-adjusting every table whenever a tab is shown fixes
  # it for good; autoWidth = FALSE alone only reduces it.
  tags$head(tags$script(HTML("
    $(document).on('shown.bs.tab shown.bs.collapse', function () {
      if (window.jQuery && $.fn.dataTable) {
        $($.fn.dataTable.tables(true)).DataTable().columns.adjust();
      }
      $(window).trigger('resize');
    });
  "))),
  titlePanel("TMU Offseason \u2014 Bullpens & Live ABs"),
  tabsetPanel(
    id = "mode",
    tabPanel("Bullpens", br(),
             sidebarLayout(
               sidebarPanel(width = 3, mode_sidebar("bp", BULLPEN_IDX, FALSE)),
               mainPanel(width = 9, mode_main("bp", FALSE)))),
    tabPanel("Live ABs / Scrimmages", br(),
             sidebarLayout(
               sidebarPanel(width = 3, mode_sidebar("la", LIVEAB_IDX, TRUE)),
               mainPanel(width = 9, mode_main("la", TRUE))))
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # One factory, called twice. The two tab sets are fully independent in the
  # UI — separate filters, separate override tables — but share their
  # arithmetic, so they cannot drift apart on what counts as a strike.
  make_mode <- function(p, ALL, IDX, live) {
    
    IN <- function(x) input[[paste0(p, "_", x)]]
    
    # The load status says what the server actually found, so an empty tab
    # names its own cause instead of a generic "no sessions".
    empty_msg <- paste0(
      "No sessions loaded. ",
      session_load_status(if (live) "liveab" else "bullpen"))
    
    # ---- persistent overrides ----
    # Scope: the PitchUIDs this tab owns. A save rewrites only these rows on
    # disk, so the bullpen tab and the live-AB tab cannot erase each other.
    scope_uids <- if (is.null(ALL) || !nrow(ALL)) character(0)
    else unique(ALL$PitchUID)
    
    overrides  <- reactiveVal({
      disk <- read_tag_overrides()
      disk[disk$PitchUID %in% scope_uids, , drop = FALSE]
    })
    
    # TRUE after a successful write, FALSE after a failed one, NULL before any.
    ovr_saved  <- reactiveVal(NULL)
    
    # Merge this tab's overrides into whatever is on disk and write it back.
    persist_overrides <- function(ov) {
      ok <- write_tag_overrides(
        merge_tag_overrides(read_tag_overrides(), ov, scope_uids))
      ovr_saved(ok)
      ok
    }
    
    sel_uid    <- reactiveVal(NULL)
    
    # ---- session filter ----
    sess_data <- reactive({
      validate(need(!is.null(ALL) && nrow(ALL) > 0, empty_msg))
      sel <- IN("session")
      if (is.null(sel) || !length(sel) || "All Sessions" %in% sel) return(ALL)
      ALL %>% filter(SessionKey %in% sel)
    })
    
    observeEvent(IN("session"), {
      cur <- IN("session")
      if (is.null(cur) || !length(cur)) {
        updateSelectizeInput(session, paste0(p, "_session"), selected = "All Sessions")
      } else if ("All Sessions" %in% cur && length(cur) > 1) {
        updateSelectizeInput(session, paste0(p, "_session"),
                             selected = setdiff(cur, "All Sessions"))
      }
    }, ignoreInit = TRUE)
    
    # ---- pitcher list follows the session filter ----
    # Two things here are load-bearing, and both were the bug:
    #
    #   1. cur is read with isolate(). Reading input$<p>_pitcher reactively
    #      made this observer depend on the very input it writes to, so
    #      picking a pitcher re-fired it, which re-sent the choice list, which
    #      made the browser report a fresh selection, which re-fired it again.
    #      That is the loop that bounced the dropdown back to the first name.
    #
    #   2. choices are only re-sent when the roster actually changed. Handing
    #      the same list back to a selectInput rebuilds the <select> and the
    #      client momentarily reports option 1 (alphabetically first arm)
    #      before the intended selection lands.
    #
    # last_arms is a plain reactiveVal used as a memo; it is read with
    # isolate() so writing it cannot re-trigger this observer either.
    last_arms <- reactiveVal(NULL)
    
    observe({
      req(!is.null(ALL) && nrow(ALL) > 0)
      src  <- sess_data()
      if (live) src <- tmu_rows(src, "PitcherTeam")
      arms <- sort(unique(src$Pitcher))
      names(arms) <- flip_name(arms)
      req(length(arms) > 0)
      
      if (identical(arms, isolate(last_arms()))) return()
      last_arms(arms)
      
      cur <- isolate(IN("pitcher"))
      updateSelectInput(session, paste0(p, "_pitcher"), choices = arms,
                        selected = if (!is.null(cur) && cur %in% arms) cur
                        else unname(arms[1]))
    })
    
    # ---- tagged data: hand tags, plus manual overrides ----
    tagged_data <- reactive({
      d <- sess_data()
      req(IN("pitcher"))
      d <- d %>% filter(Pitcher == IN("pitcher"))
      req(nrow(d) > 0)
      
      ov <- overrides()
      apply_tag_overrides(d, ov)
    })
    
    # Pitch-type dropdown follows the pitcher — only what he actually threw.
    # Same memo guard as the pitcher list: this observer runs on every override
    # too, and re-sending an unchanged choice list to a selectize box for no
    # reason is what makes selections flicker.
    last_types <- reactiveVal(NULL)
    
    observe({
      d <- tryCatch(tagged_data(), error = function(e) NULL)
      req(!is.null(d), nrow(d) > 0)
      types <- PITCH_ORDER[PITCH_ORDER %in% unique(d$PitchGroup)]
      
      if (identical(types, isolate(last_types()))) return()
      last_types(types)
      
      cur <- isolate(IN("pitch"))
      keep <- if (!is.null(cur) && all(cur %in% c("All", types))) cur else "All"
      updateSelectizeInput(session, paste0(p, "_pitch"),
                           choices = c("All", types), selected = keep)
    })
    
    observeEvent(IN("pitch"), {
      cur <- IN("pitch")
      if (is.null(cur) || !length(cur)) {
        updateSelectizeInput(session, paste0(p, "_pitch"), selected = "All")
      } else if ("All" %in% cur && length(cur) > 1) {
        updateSelectizeInput(session, paste0(p, "_pitch"),
                             selected = setdiff(cur, "All"))
      }
    }, ignoreInit = TRUE)
    
    # ---- fully filtered (side / count too, on the live tab) ----
    fdata <- reactive({
      d <- tagged_data()
      if (live) {
        if (!is.null(IN("side")) && IN("side") != "All")
          d <- d %>% filter(BatterSide == IN("side"))
        if (!is.null(IN("count")) && IN("count") != "All Counts") {
          d <- d %>%
            mutate(.cb = case_when(
              is.na(Balls) | is.na(Strikes) ~ "Unknown",
              Strikes == 2                  ~ "Two Strikes",
              Balls   >  Strikes            ~ "Behind",
              Balls   <  Strikes            ~ "Ahead",
              TRUE                          ~ "Even")) %>%
            filter(.cb == IN("count")) %>% select(-.cb)
        }
      }
      d
    })
    
    # The card keeps every pitch type (the card's own selected_pitches argument
    # does the filtering), so the usage bars still show the full mix.
    card_source <- reactive({
      d <- fdata(); req(nrow(d) > 0); d
    })
    
    # ---- header ----
    output[[paste0(p, "_summary_label")]] <- renderText({
      d <- fdata(); req(nrow(d) > 0)
      paste0(if (live) "Sessions: " else "Bullpens: ",
             n_distinct(d$SessionKey),
             "  |  Pitches: ", nrow(d),
             "  |  Overrides: ", sum(d$IsOverride, na.rm = TRUE))
    })
    
    output[[paste0(p, "_untagged_note")]] <- renderUI({
      d <- fdata(); req(nrow(d) > 0)
      n_none <- sum(d$PitchGroup == UNTAGGED_LABEL, na.rm = TRUE)
      n_auto <- sum(d$TagSource == TAG_SOURCE_TRACKMAN, na.rm = TRUE)
      if (n_none == 0 && n_auto == 0) return(NULL)
      msg <- c(
        if (n_auto > 0) sprintf(
          "%d of %d pitches had no hand tag and are using the tracking system's auto tag (TrackMan or Yakkertech; marked TM in Pitch Sequence) \u2014 spot-check them and override any it got wrong.",
          n_auto, nrow(d)),
        if (n_none > 0) sprintf(
          "%d of %d pitches have no tag from you or TrackMan and are grouped as \"%s\".",
          n_none, nrow(d), UNTAGGED_LABEL))
      div(style = paste("background:#fff3cd;border-left:4px solid #b8860b;",
                        "padding:8px 12px;margin-bottom:10px;border-radius:4px;"),
          HTML(paste(msg, collapse = "<br>")))
    })
    
    # ---- the card ----
    # Built at an explicit height so make_session_card() can budget the stats
    # table in real inches instead of relative weights — that is what was
    # clipping the bold "All" row off the bottom.
    build_card <- function(h_in) {
      d <- card_source()
      card <- make_session_card(IN("pitcher"), d,
                                selected_pitches = IN("pitch"),
                                live = live, highlight_uid = sel_uid(),
                                card_height_in = h_in,
                                card_width_in = CARD_W_IN)
      validate(need(!is.null(card), "No plottable pitches for this filter."))
      card
    }
    
    card_plot <- reactive({ build_card(CARD_H_IN) })
    
    output[[paste0(p, "_card")]] <- renderPlot({ card_plot() }, res = CARD_RES)
    
    # ---- clickable movement plot (manual override) ----
    output[[paste0(p, "_click_move")]] <- renderPlot({
      d <- fdata() %>% filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
      validate(need(nrow(d) > 0, "No tracked movement."))
      if (!("All" %in% IN("pitch"))) d <- d %>% filter(PitchGroup %in% IN("pitch"))
      validate(need(nrow(d) > 0, "No pitches for this filter."))
      d$PitchGroup <- order_pitch_factor(d$PitchGroup)
      pl <- ggplot(d, aes(x = HorzBreak, y = InducedVertBreak, color = PitchGroup)) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
        geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
        geom_point(size = 3.6, alpha = 0.95) +
        scale_color_manual(values = pitch_colors, drop = FALSE) +
        coord_cartesian(xlim = c(-MOVE_LIM, MOVE_LIM),
                        ylim = c(-MOVE_LIM, MOVE_LIM), clip = "off") +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),
              plot.margin = margin(2, 2, 2, 2))
      hl <- d[d$PitchUID %in% sel_uid(), , drop = FALSE]
      if (nrow(hl) > 0) {
        pl <- pl + geom_point(data = hl, shape = 21, size = 6.6, stroke = 1.7,
                              color = "black", fill = NA)
      }
      pl
    })
    
    observeEvent(input[[paste0(p, "_move_click")]], {
      d <- fdata() %>% filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
      req(nrow(d) > 0)
      if (!("All" %in% IN("pitch"))) d <- d %>% filter(PitchGroup %in% IN("pitch"))
      req(nrow(d) > 0)
      near <- nearPoints(d, input[[paste0(p, "_move_click")]],
                         xvar = "HorzBreak", yvar = "InducedVertBreak",
                         maxpoints = 1, threshold = 20)
      if (nrow(near) == 1) sel_uid(near$PitchUID[1])
    })
    
    output[[paste0(p, "_sel_text")]] <- renderText({
      u <- sel_uid()
      if (is.null(u)) return("Selected pitch: none")
      d <- fdata() %>% filter(PitchUID == u)
      if (nrow(d) != 1) return("Selected pitch: none")
      paste0("Selected: pitch #", d$SeqNo[1], "  |  current tag: ",
             d$PitchGroup[1], " (", d$TagSource[1], ")  |  ",
             fmt1(d$RelSpeed[1]), " mph")
    })
    
    # ---- nearest-neighbour suggestion ----
    # Only ever a suggestion. It pre-selects the dropdown and shows what it
    # matched against; nothing changes until Apply is pressed.
    sel_suggestion <- reactive({
      u <- sel_uid()
      if (is.null(u)) return(NULL)
      d <- tryCatch(tagged_data(), error = function(e) NULL)
      if (is.null(d) || !nrow(d)) return(NULL)
      suggest_pitch_tag(d, u)
    })
    
    # A pitch the suggestion should speak up for: no tag at all, or a tag
    # TrackMan filled in rather than one entered by hand.
    needs_check <- function(row) {
      as.character(row$PitchGroup[1]) == UNTAGGED_LABEL ||
        identical(as.character(row$TagSource[1]), TAG_SOURCE_TRACKMAN)
    }
    
    # Open the dropdown on the suggestion when the selected pitch has no hand
    # tag (untagged or TrackMan-filled). A hand-tagged pitch is left alone —
    # re-tagging a deliberate hand tag should not lead the witness.
    observeEvent(sel_uid(), {
      u <- sel_uid(); req(!is.null(u))
      d <- tryCatch(tagged_data(), error = function(e) NULL)
      req(!is.null(d), nrow(d) > 0)
      row <- d[d$PitchUID == u, , drop = FALSE]
      req(nrow(row) == 1)
      if (!needs_check(row)) return()
      
      s <- sel_suggestion()
      if (is.null(s) || !(s$tag %in% OVERRIDE_CHOICES)) return()
      updateSelectInput(session, paste0(p, "_manual_tag"), selected = s$tag)
    })
    
    output[[paste0(p, "_ovr_status")]] <- renderUI({
      bits <- list()
      
      u <- sel_uid()
      s <- sel_suggestion()
      if (!is.null(u) && !is.null(s)) {
        d   <- tryCatch(tagged_data(), error = function(e) NULL)
        row <- if (!is.null(d) && nrow(d)) d[d$PitchUID == u, , drop = FALSE] else NULL
        if (!is.null(row) && nrow(row) == 1 && needs_check(row)) {
          # Distance is in pooled SDs across his own tagged pitches. A loose
          # match is labelled as one rather than dressed up.
          conf <- if (s$dist < 0.75) "close match"
          else if (s$dist < 1.5) "loose match"
          else "POOR match \u2014 check this one yourself"
          bits <- c(bits, list(div(
            style = paste("background:#e8f4ea;border-left:4px solid #2ECC71;",
                          "padding:6px 10px;margin:6px 0;border-radius:4px;",
                          "font-size:12px;"),
            HTML(sprintf(
              "Looks most like his <b>%s</b> (%s, %.2f SD, against %d hand-tagged pitches).%s<br>",
              s$tag, conf, s$dist, s$n_pool,
              if (identical(as.character(row$TagSource[1]), TAG_SOURCE_TRACKMAN))
                sprintf(" TrackMan called it <b>%s</b>%s.",
                        row$PitchGroup[1],
                        if (s$tag == as.character(row$PitchGroup[1])) " too"
                        else " \u2014 they disagree")
              else "")),
            HTML(sprintf("Nearest: %s mph / %s iVB / %s HB / %s spin",
                         fmt1(s$neighbor$RelSpeed),
                         fmt1(s$neighbor$InducedVertBreak),
                         fmt1(s$neighbor$HorzBreak),
                         fmt0(s$neighbor$SpinRate))),
            br(),
            tags$em("Suggestion only \u2014 nothing is applied until you press Apply.")
          )))
        }
      }
      
      saved <- ovr_saved()
      if (identical(saved, FALSE)) {
        bits <- c(bits, list(div(
          style = paste("background:#f8d7da;border-left:4px solid #a94442;",
                        "padding:6px 10px;margin:6px 0;border-radius:4px;",
                        "font-size:12px;"),
          HTML(paste0("<b>Not saved.</b> Could not write ", TAG_OVERRIDE_FILE,
                      " \u2014 the correction is live in this session only.")))))
      } else if (identical(saved, TRUE)) {
        bits <- c(bits, list(div(
          style = "color:#2b7a3d;font-size:12px;margin:6px 0;",
          HTML(paste0("Saved to ", TAG_OVERRIDE_FILE, ".")))))
      }
      
      if (!length(bits)) return(NULL)
      do.call(tagList, bits)
    })
    
    observeEvent(input[[paste0(p, "_apply_ovr")]], {
      u <- sel_uid(); req(!is.null(u))
      new <- IN("manual_tag"); req(!is.null(new), nzchar(new))
      ov <- overrides()
      if (u %in% ov$PitchUID) {
        ov$ManualPitchGroup[ov$PitchUID == u] <- new
      } else {
        ov <- rbind(ov, data.frame(PitchUID = u, ManualPitchGroup = new,
                                   stringsAsFactors = FALSE))
      }
      overrides(ov)
      persist_overrides(ov)
    })
    
    observeEvent(input[[paste0(p, "_clear_ovr")]], {
      u <- sel_uid(); req(!is.null(u))
      ov <- overrides() %>% filter(PitchUID != u)
      overrides(ov)
      persist_overrides(ov)
    })
    
    # Clears only THIS tab's corrections. Scoped on purpose: a global wipe from
    # the bullpen tab quietly taking the scrimmage corrections with it is the
    # kind of thing that surfaces three weeks later.
    observeEvent(input[[paste0(p, "_clear_all")]], {
      ov <- data.frame(PitchUID = character(),
                       ManualPitchGroup = character(),
                       stringsAsFactors = FALSE)
      overrides(ov)
      persist_overrides(ov)
      sel_uid(NULL)
    })
    
    # ---- pitch sequence ----
    seq_df <- reactive({
      d <- fdata() %>% arrange(SessionKey, SeqNo)
      if (!("All" %in% IN("pitch"))) d <- d %>% filter(PitchGroup %in% IN("pitch"))
      req(nrow(d) > 0)
      d
    })
    
    output[[paste0(p, "_seq")]] <- renderDT({
      d <- seq_df()
      base <- data.frame(
        `#`   = d$SeqNo,
        Pitch = as.character(d$PitchGroup),
        Src   = dplyr::case_when(d$TagSource == TAG_SOURCE_TRACKMAN ~ "TM",
                                 d$TagSource == TAG_SOURCE_OVERRIDE ~ "OVR",
                                 d$TagSource == TAG_SOURCE_NONE     ~ "\u2014",
                                 TRUE                               ~ ""),
        Res   = if (live) res_abbrev(d$PitchCall, d$PlayResult,
                                     d$PAEnd, d$PAResult)
        else res_abbrev(d$PitchCall, d$PlayResult),
        Velo  = fmt1(d$RelSpeed),
        Spin  = fmt0(d$SpinRate),
        Tilt  = ifelse(is.na(d$Tilt) | d$Tilt == "",
                       tilt_from_axis(d$SpinAxis), d$Tilt),
        iVB   = fmt1(d$InducedVertBreak),
        HB    = fmt1(d$HorzBreak),
        Ext   = fmt1(d$Extension),
        RelH  = fmt1(d$RelHeight),
        RelS  = fmt1(d$RelSide),
        VAA   = fmt1(d$VertApprAngle),
        check.names = FALSE, stringsAsFactors = FALSE)
      
      tbl <- if (live) {
        cbind(base[, 1, drop = FALSE],
              data.frame(Inn = fmt0(d$Inning),
                         Batter = flip_name(d$Batter),
                         S = ifelse(is.na(d$BatterSide) | d$BatterSide == "",
                                    "\u2014", substr(d$BatterSide, 1, 1)),
                         Cnt = ifelse(is.na(d$Balls) | is.na(d$Strikes), "\u2014",
                                      paste0(d$Balls, "-", d$Strikes)),
                         check.names = FALSE, stringsAsFactors = FALSE),
              base[, -1, drop = FALSE],
              data.frame(EV = ifelse(d$PitchCall == "InPlay",
                                     fmt1(d$ExitSpeed), "\u2014"),
                         LA = ifelse(d$PitchCall == "InPlay",
                                     fmt0(d$Angle), "\u2014"),
                         check.names = FALSE, stringsAsFactors = FALSE))
      } else {
        cbind(base[, 1, drop = FALSE],
              data.frame(`PT#` = paste0(d$PTNum, "/", d$PTTot),
                         check.names = FALSE, stringsAsFactors = FALSE),
              base[, -1, drop = FALSE])
      }
      
      datatable(tbl, rownames = FALSE, selection = "single",
                options = list(pageLength = 50, scrollX = TRUE,
                               autoWidth = FALSE, searching = FALSE)) %>%
        formatStyle("Pitch", color = styleEqual(names(pitch_colors),
                                                unname(pitch_colors)),
                    fontWeight = "bold") %>%
        formatStyle("Src", color = styleEqual(c("TM", "OVR"),
                                              c("#B9770E", "#21618C")),
                    fontWeight = "bold") %>%
        formatStyle("Res", color = styleEqual(names(RES_COLORS),
                                              unname(RES_COLORS)),
                    fontWeight = "bold")
    }, server = FALSE)
    
    # Selecting a row selects that pitch for the override panel too.
    observeEvent(input[[paste0(p, "_seq_rows_selected")]], {
      i <- input[[paste0(p, "_seq_rows_selected")]]
      d <- seq_df()
      req(length(i) == 1, i >= 1, i <= nrow(d))
      sel_uid(d$PitchUID[i])
    })
    
    output[[paste0(p, "_seq_loc")]] <- renderPlot({
      d <- seq_df()
      pl <- make_location_plot(d, title = NULL, highlight_uid = sel_uid())
      validate(need(!is.null(pl), "No tracked locations."))
      pl
    })
    
    output[[paste0(p, "_seq_move")]] <- renderPlot({
      d <- seq_df()
      pl <- make_movement_plot(d, highlight_uid = sel_uid(), title = NULL)
      validate(need(!is.null(pl), "No tracked movement."))
      pl
    })
    
    # ---- zone analysis ----
    zone_src <- reactive({
      d <- fdata()
      if (!("All" %in% IN("pitch"))) d <- d %>% filter(PitchGroup %in% IN("pitch"))
      req(nrow(d) > 0)
      d
    })
    
    output[[paste0(p, "_zone_plot")]] <- renderPlot({
      d <- zone_src()
      validate(need(any(is.finite(d$PlateLocSide) & is.finite(d$PlateLocHeight)),
                    "No tracked locations for this filter."))
      pl <- if (isTRUE(IN("zone_split"))) {
        make_zone_grid_by_pitch(d, IN("zone_metric"), strike_from_zone = !live)
      } else {
        make_zone_grid_plot(d, IN("zone_metric"),
                            title = paste0(flip_name(IN("pitcher")), "\n",
                                           IN("zone_metric"), " by Zone"),
                            strike_from_zone = !live)
      }
      validate(need(!is.null(pl), "Nothing to draw for this metric."))
      pl
    })
    
    # ---- downloads ----
    # Report: the frame is tagged_data() (this pitcher) or the whole session
    # selection with this tab's overrides applied (every pitcher). Side /
    # count filters are deliberately NOT applied — a report is the outing.
    report_frame <- function() {
      if (identical(IN("rpt_scope"), "all")) {
        apply_tag_overrides(sess_data(), overrides())
      } else {
        tagged_data()
      }
    }
    
    output[[paste0(p, "_dl_report")]] <- downloadHandler(
      filename = function() {
        who <- if (identical(IN("rpt_scope"), "all")) "AllPitchers"
        else gsub("[^A-Za-z0-9]+", "_", IN("pitcher"))
        paste0("TMU_", who, "_",
               if (live) "LiveAB_Report" else "Bullpen_Report", "_",
               format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        withProgress(message = "Building report\u2026", value = 0.3, {
          tryCatch(
            render_session_report(if (live) "liveab" else "bullpen",
                                  report_frame(), file),
            error = function(e) {
              showNotification(paste("Report failed:", conditionMessage(e)),
                               type = "error", duration = 12)
              stop(e)
            })
          setProgress(1)
        })
      }
    )
    
    output[[paste0(p, "_dl_data")]] <- downloadHandler(
      filename = function() {
        paste0(gsub("[^A-Za-z0-9]+", "_", IN("pitcher")), "_",
               if (live) "scrimmage" else "bullpen", "_corrected.csv")
      },
      content = function(file) readr::write_csv(fdata(), file)
    )
    
    if (!live) return(invisible(NULL))
    
    # =========================================================================
    # GAME STATS  (live only)
    # =========================================================================
    output[[paste0(p, "_gs_line")]] <- renderDT({
      d <- fdata(); req(nrow(d) > 0)
      keys <- unique(d$SessionKey)
      rows <- lapply(keys, function(k) {
        x  <- d[d$SessionKey == k, , drop = FALSE]
        dt <- first_non_na(x$Date)
        dplyr::bind_cols(
          data.frame(Session = pretty_session_date(dt), sort_date = dt,
                     check.names = FALSE, stringsAsFactors = FALSE),
          pitcher_line_row(x))
      })
      tbl <- dplyr::bind_rows(rows)
      tbl <- tbl[order(tbl$sort_date), , drop = FALSE]
      tbl$sort_date <- NULL
      multi <- length(keys) > 1
      if (multi) {
        tbl <- dplyr::bind_rows(tbl, dplyr::bind_cols(
          data.frame(Session = "Total", check.names = FALSE), pitcher_line_row(d)))
      }
      stat_dt(tbl, bold_last = multi)
    }, server = FALSE)
    
    output[[paste0(p, "_gs_types")]] <- renderDT({
      d  <- fdata(); req(nrow(d) > 0)
      pa <- pa_table(d)
      ty <- unique(as.character(d$PitchGroup))
      ty <- c(PITCH_ORDER[PITCH_ORDER %in% ty], sort(setdiff(ty, PITCH_ORDER)))
      one <- function(label, x, pp) {
        a <- summarise_pa(pp); b <- summarise_pitches(x)
        dplyr::bind_cols(
          data.frame(Pitch = label, check.names = FALSE, stringsAsFactors = FALSE),
          b[, c("Pitches","Swing%","Whiff%","CSW%","Chase%","Z-Contact%")],
          dplyr::rename(a[, c("PA","AB","H","HR","K","BB","AVG","SLG")],
                        `PA Ended` = PA),
          b[, c("BIP","Avg EV","Max EV","HH%","GB%","LD%","FB%")])
      }
      rows <- lapply(ty, function(t) {
        one(t, d[d$PitchGroup == t, , drop = FALSE],
            if (is.null(pa)) NULL else pa[pa$PitchGroup == t, , drop = FALSE])
      })
      tbl <- dplyr::bind_rows(c(rows, list(one("All", d, pa))))
      stat_dt(tbl, bold_last = TRUE) %>%
        formatStyle("Pitch", color = styleEqual(names(pitch_colors),
                                                unname(pitch_colors)),
                    fontWeight = "bold")
    }, server = FALSE)
    
    output[[paste0(p, "_gs_plat")]] <- renderDT({
      d <- fdata(); req(nrow(d) > 0)
      rows <- lapply(c("Left", "Right"), function(sd) {
        x <- d[!is.na(d$BatterSide) & d$BatterSide == sd, , drop = FALSE]
        if (!nrow(x)) return(NULL)
        dplyr::bind_cols(
          data.frame(Split = paste0("vs ", substr(sd, 1, 1), "HH"),
                     check.names = FALSE),
          pitcher_line_row(x)[, c("BF","AB","H","HR","K","BB","AVG","OBP",
                                  "SLG","Whiff%","Chase%")])
      })
      tbl <- dplyr::bind_rows(rows)
      validate(need(nrow(tbl) > 0, "No batter side charted for this filter."))
      stat_dt(tbl)
    }, server = FALSE)
    
    output[[paste0(p, "_gs_spray")]] <- renderPlot({
      pl <- make_spray_chart(pa_table(fdata()), title = NULL)
      validate(need(!is.null(pl), "No balls in play with a bearing and distance."))
      pl
    })
    
    pa_log_table <- function(pa, who = c("batter", "pitcher")) {
      who <- match.arg(who)
      tbl <- data.frame(
        Date   = pretty_session_date(pa$Date),
        Inn    = fmt0(pa$Inning),
        Name   = flip_name(if (who == "batter") pa$Batter else pa$Pitcher),
        S      = ifelse(is.na(pa$BatterSide), "\u2014", substr(pa$BatterSide, 1, 1)),
        P      = pa$Pitches,
        Count  = ifelse(is.na(pa$FinalCount), "\u2014", pa$FinalCount),
        Result = pa_result_code(pa$PAResult),
        `Last Pitch` = as.character(pa$PitchGroup),
        Velo   = round(pa$RelSpeed, 1),
        EV     = ifelse(pa$BIP, round(pa$ExitSpeed, 1), NA_real_),
        LA     = ifelse(pa$BIP, round(pa$Angle), NA_real_),
        Dist   = ifelse(pa$BIP, round(pa$Distance), NA_real_),
        check.names = FALSE, stringsAsFactors = FALSE)
      names(tbl)[names(tbl) == "Name"] <- if (who == "batter") "Batter" else "Pitcher"
      stat_dt(tbl, dom = "tip", page_len = 25) %>%
        formatStyle("Result", color = styleEqual(names(RES_COLORS),
                                                 unname(RES_COLORS)),
                    fontWeight = "bold") %>%
        formatStyle("Last Pitch", color = styleEqual(names(pitch_colors),
                                                     unname(pitch_colors)),
                    fontWeight = "bold")
    }
    
    output[[paste0(p, "_gs_pas")]] <- renderDT({
      pa <- pa_table(fdata())
      validate(need(!is.null(pa), "No completed plate appearances for this filter."))
      pa_log_table(pa, "batter")
    }, server = FALSE)
    
    # =========================================================================
    # HITTERS  (live only)
    # -------------------------------------------------------------------------
    # Board: sidebar session filter + pitcher-hand filter only.
    # Trend / log / spray: every session of the offseason (+ hand filter).
    # =========================================================================
    hit_frame <- function(d) {
      h <- IN("hit_hand")
      if (!is.null(d) && !is.null(h) && h != "All") {
        d <- d[!is.na(d$PitcherThrows) & d$PitcherThrows == h, , drop = FALSE]
      }
      d
    }
    
    hit_board <- reactive({
      d  <- hit_frame(sess_data())
      pa <- tmu_rows(pa_table(d), "BatterTeam")
      validate(need(!is.null(pa), "No completed plate appearances in the selected sessions."))
      bats <- sort(unique(pa$Batter[!is.na(pa$Batter) & pa$Batter != ""]))
      rows <- lapply(bats, function(b) {
        pp <- pa[!is.na(pa$Batter) & pa$Batter == b, , drop = FALSE]
        x  <- d[!is.na(d$Batter) & d$Batter == b, , drop = FALSE]
        a  <- summarise_pa(pp)
        sw <- summarise_pitches(x)
        dplyr::bind_cols(
          data.frame(Hitter = flip_name(b), key = b,
                     Sessions = dplyr::n_distinct(pp$SessionKey),
                     check.names = FALSE, stringsAsFactors = FALSE),
          a[, c("PA","AB","H","2B","3B","HR","BB","HBP","K",
                "AVG","OBP","SLG","OPS","K%","BB%")],
          sw[, c("Swing%","Whiff%","Chase%")],
          a[, c("Avg EV","Max EV","HH%")])
      })
      tbl <- dplyr::bind_rows(rows)
      mp  <- suppressWarnings(as.numeric(IN("hit_minpa")))
      if (length(mp) != 1 || is.na(mp)) mp <- 0
      tbl[tbl$PA >= mp, , drop = FALSE]
    })
    
    output[[paste0(p, "_hit_board")]] <- renderDT({
      tbl <- hit_board()
      validate(need(nrow(tbl) > 0, "No hitters meet the PA minimum."))
      stat_dt(tbl[, names(tbl) != "key", drop = FALSE], order_by = "AVG",
              selection = "single", dom = "tip")
    }, server = FALSE)
    
    output[[paste0(p, "_hit_dl")]] <- downloadHandler(
      filename = function() paste0("TMU_offseason_hitting_",
                                   format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        tbl <- hit_board()
        tbl$key <- NULL
        readr::write_csv(format_stat_cols(tbl), file)
      }
    )
    
    # Hitter picker: everyone with a completed PA anywhere in the offseason.
    all_hitters <- local({
      pa0 <- tryCatch(tmu_rows(pa_table(ALL), "BatterTeam"),
                      error = function(e) NULL)
      if (is.null(pa0)) character(0)
      else sort(unique(pa0$Batter[!is.na(pa0$Batter) & pa0$Batter != ""]))
    })
    observe({
      req(length(all_hitters) > 0)
      cur <- isolate(IN("hitter"))
      updateSelectInput(session, paste0(p, "_hitter"),
                        choices  = stats::setNames(all_hitters, flip_name(all_hitters)),
                        selected = if (!is.null(cur) && cur %in% all_hitters) cur
                        else all_hitters[1])
    })
    
    observeEvent(input[[paste0(p, "_hit_board_rows_selected")]], {
      i   <- input[[paste0(p, "_hit_board_rows_selected")]]
      tbl <- hit_board()
      req(length(i) == 1, i >= 1, i <= nrow(tbl))
      updateSelectInput(session, paste0(p, "_hitter"), selected = tbl$key[i])
    })
    
    season_pa <- reactive({ tmu_rows(pa_table(hit_frame(ALL)), "BatterTeam") })
    
    hitter_pa <- reactive({
      b  <- IN("hitter"); req(b)
      pa <- season_pa()
      validate(need(!is.null(pa), "No completed plate appearances."))
      pa[!is.na(pa$Batter) & pa$Batter == b, , drop = FALSE]
    })
    
    output[[paste0(p, "_hit_trend")]] <- renderPlot({
      pa <- hitter_pa()
      validate(need(nrow(pa) > 0, "No completed PAs for this hitter."))
      make_running_avg_plot(hitter_session_log(pa), IN("hitter"))
    })
    
    output[[paste0(p, "_hit_spray")]] <- renderPlot({
      pl <- make_spray_chart(hitter_pa(), title = "Spray Chart \u2014 Offseason")
      validate(need(!is.null(pl), "No balls in play with a bearing and distance."))
      pl
    })
    
    output[[paste0(p, "_hit_log")]] <- renderDT({
      pa  <- hitter_pa()
      validate(need(nrow(pa) > 0, "No completed PAs for this hitter."))
      log <- hitter_session_log(pa)
      tbl <- data.frame(Session = pretty_session_date(log$Date),
                        check.names = FALSE, stringsAsFactors = FALSE) %>%
        dplyr::bind_cols(log[, c("PA","AB","H","2B","3B","HR","BB","K",
                                 "AVG","OBP","SLG","Avg EV",
                                 "PA to date","AVG to date","OBP to date",
                                 "SLG to date","OPS to date")])
      stat_dt(tbl)
    }, server = FALSE)
    
    output[[paste0(p, "_hit_pas")]] <- renderDT({
      pa <- hitter_pa()
      validate(need(nrow(pa) > 0, "No completed PAs for this hitter."))
      pa_log_table(pa, "pitcher")
    }, server = FALSE)
  }
  
  make_mode("bp", BULLPENS, BULLPEN_IDX, live = FALSE)
  make_mode("la", LIVEABS,  LIVEAB_IDX,  live = TRUE)
}

shinyApp(ui, server)