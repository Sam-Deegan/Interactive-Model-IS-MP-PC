## ---------------------------------------------------------------------
## GENERATED COPY. Do not edit.
## Master: _apps/_toolkit/dublin_app.R
## Re-sync with: source("_apps/sync_toolkit.R")
## ---------------------------------------------------------------------

################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Dublin Toy-Model Toolkit: Shared Look and Machinery                        ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   The master copy lives in _apps/_toolkit/dublin_app.R. Every toy-model app
##   carries a copy at R/toolkit.R, because shinylive needs each app folder to
##   be self-contained. Edit the master, then run:
##
##     source("_apps/sync_toolkit.R")
##
##   to push the change into every app. Do not edit an app's R/toolkit.R: the
##   sync script overwrites it.
##
##   An app supplies four things and gets the rest from here:
##     T_03_01_controls_lst   one entry per slider: label, min, max, step, from
##     T_03_02_help_lst       one hover explanation per slider
##     T_03_03_equations_lst  the staged equations (see T_06)
##     T_03_04_notation_lst   the symbol key (see T_06)
##   These are named by the app in its own B_03, then passed to the builders.
##
## Inputs:
##   None.
##
## Outputs:
##   None written to disk.
##
## Style note:
##   Toolkit objects use the section letter T so they never collide with an
##   app's own A-G sections. Objects inside functions use plain snake_case.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## T: Toolkit ##################################################################
################################################################################
# Note: Palette, plot theme, CSS, control and tile builders, and the engine
#   behind the equations and notation tabs.

#### T_01: Palette #############################################################
# Note: The Dublin Beamer theme's colours, and nothing else.

###### T_01_01: Theme Colours ##################################################
# Note: Taken from dublin-theme.tex. The theme has no red and no amber on
#   purpose: dgGreen is "the one green", used as the single accent, and
#   everything else is the navy-blue ramp or a grey.

T_01_01_palette_vec <- c(
  navy   = "#04204C",
  blue   = "#0056A4",
  bluedk = "#003C77",
  light  = "#9FC4E0",
  green  = "#61B77C",
  ink    = "#212529",
  muted  = "#6C757D",
  rule   = "#D8E0E6",
  wash   = "#F2F6F9",
  ground = "#FFFFFF"
)

###### T_01_01a: The Zero Line #################################################
# Note: Between rule and muted. A reference line for zero has to be visible on
#   a gridded panel without competing with the model's own resting points.

T_01_01_zero_chr <- "#AEB8C2"

###### T_01_02: Series Colours #################################################
# Note: The convention the example deck states in its own figure notes: the
#   realised series in navy, the comparison dashed in green, bands in light
#   blue, reference lines grey dashed. Use these names rather than picking a
#   colour at each call, so every figure in every app reads the same way.

T_01_02_series_vec <- c(
  main       = T_01_01_palette_vec[["navy"]],
  compare    = T_01_01_palette_vec[["green"]],
  band       = T_01_01_palette_vec[["light"]],
  reference  = T_01_01_palette_vec[["muted"]],
  third      = T_01_01_palette_vec[["blue"]],
  fourth     = T_01_01_palette_vec[["muted"]]
)

###### T_01_03: Plot Text Size #################################################
# Note: Large enough to read when projected in a lecture theatre.

T_01_03_base_size_int <- 14L

###### T_01_04: The Ghost ######################################################
# Note: Every figure can draw itself TWICE: once at the sliders as they are
#   now, and once at the values the loaded worked example set. The second is
#   the GHOST, and it is what gives a student something to change things
#   against.
#
#   It is the SAME colour as the live curve at half opacity, not a second
#   series colour and not grey. A ghost is the same object at other settings,
#   so it must read as a faded copy of the line it belongs to; a different
#   colour would say it is a different object, which is what the comparison
#   series (dashed green) already means.
#
#   It is drawn BEFORE the live layers, so the live curve sits on top. When
#   nothing has been moved the two coincide exactly and the ghost is invisible
#   of its own accord, which is the correct behaviour: there is nothing to
#   compare against yet.

T_01_04_ghost_alpha_num <- 0.5

#### T_02: Plot Look ###########################################################
# Note: The theme function and the small formatters every app needs.

###### T_02_01: Plot Theme #####################################################
# Note: The look of the figures in the Dublin deck: a wash ground with no
#   border, horizontal hairlines only, navy titles and axis titles, muted
#   tick labels and captions, and a legend with no title along the bottom.

# Note: theme_bw() DOES carry gridlines — it is theme_grey() with a white
#   panel and a border, keeping both the major and the minor grid. Everything
#   below is what takes them away again, and puts the AXES back: dropping the
#   panel border without drawing the two spines leaves the plot floating.
#     grid = "h"     faint horizontals, for reading a value off a series
#     grid = "v"     faint verticals, for a HORIZONTAL bar chart, where the
#                    number the reader takes off a bar is on the x axis and
#                    horizontals would run along the bars and mark nothing
#     grid = "none"  nothing, for a diagram where no value is read off

T_02_01_theme_fn <- function(base_size = T_01_03_base_size_int,
                             grid = c("h", "v", "none")) {
  grid <- match.arg(grid)
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA),
      plot.background    = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA),
      legend.background  = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA),
      legend.key         = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA),
      panel.border       = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = if (grid == "v") {
        ggplot2::element_line(colour = T_01_01_palette_vec[["rule"]],
                              linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.major.y = if (grid == "h") {
        ggplot2::element_line(colour = T_01_01_palette_vec[["rule"]],
                              linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      axis.line          = ggplot2::element_line(
        colour = T_01_01_palette_vec[["muted"]], linewidth = 0.4),
      # Note: A secondary axis is how a reference level gets its symbol, but
      #   ggplot draws a spine on that side too and the panel comes back as a
      #   box. Blank the two extra spines; the ticks stay.
      axis.line.x.top    = ggplot2::element_blank(),
      axis.line.y.right  = ggplot2::element_blank(),
      axis.ticks         = ggplot2::element_line(
        colour = T_01_01_palette_vec[["muted"]], linewidth = 0.4),
      strip.background   = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA),
      strip.text         = ggplot2::element_text(
        colour = T_01_01_palette_vec[["navy"]], face = "bold", hjust = 0),
      # Note: Flush with the LEFT EDGE OF THE PLOT, not of the panel, so the
      #   title lines up with the y-axis title rather than floating over the
      #   figure.
      plot.title.position = "plot",
      plot.title         = ggplot2::element_text(
        colour = T_01_01_palette_vec[["navy"]], face = "bold", hjust = 0),
      plot.subtitle      = ggplot2::element_text(
        colour = T_01_01_palette_vec[["muted"]], size = ggplot2::rel(0.85)),
      # Note: Captions are switched OFF, not styled. A reading instruction
      #   baked into the image cannot be re-wrapped or re-sized once the
      #   figure is a PNG on a slide, and it duplicates the note the deck
      #   carries under \fig{}. The text belongs under the figure in the app
      #   and in the deck's figure note, so any caption = an app still passes
      #   is inert rather than rendered.
      plot.caption       = ggplot2::element_blank(),
      # Note: Axis titles are the size of the subtitle, not of the title, and
      #   bold so the axes read before the numbers do. Write them as the word
      #   then the symbol in brackets: "Inflation (pi[t])".
      axis.title         = ggplot2::element_text(
        colour = T_01_01_palette_vec[["navy"]], face = "bold",
        size = ggplot2::rel(0.85)),
      axis.text          = ggplot2::element_text(
        colour = T_01_01_palette_vec[["muted"]]),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(
        colour = T_01_01_palette_vec[["ink"]])
    )
}

###### T_02_01a: Angle of a Line on Screen #####################################
# Note: A label that follows its line has to be rotated by the line's angle AS
#   DRAWN, which is not atan(slope): it depends on how many data units the
#   panel gives an inch on each axis. Fixing the panel to a known aspect
#   ratio makes it computable, so a plot that labels a line sets
#   theme(aspect.ratio = a) and passes the same a here.
#
#   Then label the line with annotate("text"), rotated and lifted clear of it:
#     annotate("text", x = x0, y = y0,
#              angle = T_02_01a_angle_fn(slope, xlim, ylim),
#              label = "...", parse = TRUE, size = 3.2,
#              hjust = 0.5, vjust = -1.15, colour = <muted>)
#   vjust works in the ROTATED frame, so a negative value lifts the label
#   perpendicular to the line — above it, without breaking it.

T_02_01a_angle_fn <- function(slope, xlim, ylim, aspect = 1) {
  atan(slope * (diff(range(xlim)) / diff(range(ylim))) * aspect) * 180 / pi
}

###### T_02_02: Reference Lines and Their Labels ###############################
# Note: Two different kinds of line, drawn differently on purpose, and their
#   symbols put on the OPPOSITE axis rather than floating in the panel.
#
#   T_02_02_zero_fn()   ZERO, dashed and faint: the arithmetic origin. Where
#                       the axes cross tells the reader the sign of everything
#                       on the panel, and on a policy rule the horizontal zero
#                       IS the lower bound.
#   T_02_02_rest_fn()   A RESTING POINT, dotted and darker: r*, pi*, a zero
#                       output gap, a steady state. A model object, not
#                       arithmetic, and the difference should be visible
#                       without reading a note.
#   T_02_02_mark_y_fn() the level's SYMBOL, on the right-hand axis
#   T_02_02_mark_x_fn() the level's SYMBOL, along the top
#
#   Both line helpers go in BEFORE the geoms, so the curves are drawn over
#   them. A secondary axis brings a spine with it, so T_02_01_theme_fn blanks
#   axis.line.x.top and axis.line.y.right; the ticks stay, which is what keeps
#   the symbol anchored to a value.
#
#   Why it matters: a symbol tied to a value is what makes a dynamic
#   describable — "inflation is above pi*", "the gap is still negative".
#   The same symbol floating mid-panel is debris.

T_02_02_zero_fn <- function(h = TRUE, v = TRUE) {
  out <- list()
  # Note: NOT the rule colour. That is the gridline colour, so on a panel
  #   with grid = "h" the zero line was indistinguishable from the hairline
  #   behind it. Dashed and mid-grey reads as zero without competing with the
  #   dotted, darker resting points.
  if (h) out <- c(out, list(ggplot2::geom_hline(
    yintercept = 0, linetype = "dashed", linewidth = 0.3,
    colour = T_01_01_zero_chr)))
  if (v) out <- c(out, list(ggplot2::geom_vline(
    xintercept = 0, linetype = "dashed", linewidth = 0.3,
    colour = T_01_01_zero_chr)))
  out
}

T_02_02_rest_fn <- function(h = NULL, v = NULL) {
  out <- list()
  if (!is.null(h)) out <- c(out, list(ggplot2::geom_hline(
    yintercept = h, linetype = "dotted", linewidth = 0.4,
    colour = T_01_01_palette_vec[["muted"]])))
  if (!is.null(v)) out <- c(out, list(ggplot2::geom_vline(
    xintercept = v, linetype = "dotted", linewidth = 0.4,
    colour = T_01_01_palette_vec[["muted"]])))
  out
}

# Note: These build a WHOLE scale, so they replace any scale_*_continuous the
#   figure already had. Pass the primary axis's own labels, breaks and
#   expansion through rather than losing them — three apps silently lost a
#   percent-formatted axis to this before the arguments existed.

T_02_02_mark_y_fn <- function(at, lab, labels = ggplot2::waiver(),
                              breaks = ggplot2::waiver(),
                              expand = ggplot2::waiver(), ...) {
  ggplot2::scale_y_continuous(
    labels = labels, breaks = breaks, expand = expand, ...,
    sec.axis = ggplot2::dup_axis(name = NULL, breaks = at, labels = lab))
}

T_02_02_mark_x_fn <- function(at, lab, labels = ggplot2::waiver(),
                              breaks = ggplot2::waiver(),
                              expand = ggplot2::waiver(), ...) {
  ggplot2::scale_x_continuous(
    labels = labels, breaks = breaks, expand = expand, ...,
    sec.axis = ggplot2::dup_axis(name = NULL, breaks = at, labels = lab))
}

###### T_02_01b: Fold a Long Label #############################################
# Note: ggplot2 never wraps a caption, so a long one runs off the right-hand
#   edge and the end of the sentence is simply lost. Fold it onto as many
#   lines as it needs before the plot is drawn.

T_02_01b_fold_fn <- function(txt, width) {
  if (is.null(txt) || !is.character(txt) || !nzchar(txt)) return(txt)
  paste(strwrap(txt, width = width), collapse = "\n")
}

###### T_02_01c: Draw a Plot ###################################################
# Note: The last thing every figure passes through. It does two jobs.
#
#   It folds the title, because one carrying a couple of computed numbers
#   outgrows a narrow card easily.
#
#   And it LIFTS THE CAPTION OUT OF THE PLOT. A caption drawn inside the
#   graphics device takes its height from the panel, so a four-line note
#   squashes the figure it is explaining; set in HTML underneath the card it
#   costs the panel nothing, wraps to the card's own width, and can be
#   selected and copied. The caption is stashed against this output's id and
#   T_07_07d_cap_fn renders it below the plot.
#
#   The output id comes from shiny::getCurrentOutputInfo(), so no plot
#   function and no renderPlot body had to change to gain this.

T_02_01c_draw_fn <- function(p, cap_width = 95, title_width = 60) {
  if (!inherits(p, "ggplot")) return(p)
  p$labels$title <- T_02_01b_fold_fn(p$labels$title, title_width)

  cap <- p$labels$caption
  id  <- tryCatch(shiny::getCurrentOutputInfo()$name, error = function(e) NULL)
  if (!is.null(id)) {
    store <- T_02_01d_capstore_fn()
    if (!is.null(store)) {
      store[[id]] <- if (is.null(cap) || !nzchar(cap)) "" else cap
      p$labels$caption <- NULL
    }
  }
  p
}

###### T_02_01d: Where Lifted Captions Live ####################################
# Note: One reactiveValues per session, hung off the session object so two
#   readers of the same app never see each other's figures.

T_02_01d_capstore_fn <- function() {
  s <- shiny::getDefaultReactiveDomain()
  if (is.null(s)) return(NULL)
  if (is.null(s$userData$fig_caps)) {
    s$userData$fig_caps <- shiny::reactiveValues()
  }
  s$userData$fig_caps
}

###### T_02_02: Placeholder Panel ##############################################
# Note: What a figure shows when there is nothing to draw yet. Keeps the wash
#   ground so the card does not flash white.

T_02_02_placeholder_fn <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, size = 5, label = text,
                      colour = T_01_01_palette_vec[["muted"]]) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA),
      plot.background  = ggplot2::element_rect(
        fill = T_01_01_palette_vec[["wash"]], colour = NA)
    )
}

###### T_02_03: Equilibrium Point ##############################################
# Note: The open marker the deck uses for a point on a line: a white disc with
#   a coloured ring.

T_02_03_point_fn <- function(x, y, colour = T_01_02_series_vec[["main"]],
                             size = 3.4) {
  ggplot2::annotate("point", x = x, y = y, shape = 21, stroke = 1.3,
                    size = size, colour = colour,
                    fill = T_01_01_palette_vec[["ground"]])
}

###### T_02_03a: The Ghost Layers ##############################################
# Note: The same three marks as the live figure, at T_01_04_ghost_alpha_num.
#   Add them FIRST, before the live layers, so the current curve draws over
#   the ghost rather than under it.
#
#   Each takes its own data and mapping and sets inherit.aes = FALSE, because
#   a ghost is a second data set on the same panel and must not pick up the
#   plot's colour or linetype scales — doing so would put it in the
#   legend as a fourth series and force a scale entry for it.
#
#   T_02_03b_ghost_off_fn is the guard: it answers "is there anything to
#   compare against?". A NULL reference, or one identical to what is on the
#   sliders, means no — so a student who has not moved anything sees a
#   clean figure, and the ghost appears the moment they do.

T_02_03a_ghost_line_fn <- function(data, mapping,
                                   colour = T_01_02_series_vec[["main"]],
                                   linewidth = 1, ...) {
  ggplot2::geom_line(data = data, mapping = mapping, colour = colour,
                     alpha = T_01_04_ghost_alpha_num, linewidth = linewidth,
                     inherit.aes = FALSE, ...)
}

T_02_03a_ghost_path_fn <- function(data, mapping,
                                   colour = T_01_02_series_vec[["main"]],
                                   linewidth = 1, ...) {
  ggplot2::geom_path(data = data, mapping = mapping, colour = colour,
                     alpha = T_01_04_ghost_alpha_num, linewidth = linewidth,
                     inherit.aes = FALSE, ...)
}

T_02_03a_ghost_point_fn <- function(x, y, colour = T_01_02_series_vec[["main"]],
                                    size = 3.4) {
  ggplot2::annotate("point", x = x, y = y, shape = 21, stroke = 1.3,
                    size = size, colour = colour,
                    alpha = T_01_04_ghost_alpha_num,
                    fill = T_01_01_palette_vec[["ground"]])
}

T_02_03b_ghost_off_fn <- function(par, ref) {
  is.null(ref) || isTRUE(all.equal(par, ref))
}

###### T_02_04: Euro Formatter #################################################
# Note: Amounts are in billions unless the app says otherwise. The sign is
#   written as the escape "\u20ac", not as a literal character. Shinylive runs
#   under a C locale, and Shiny parses app.R with an encoding but sources
#   R/*.R without one, so a literal euro in THIS file reaches the page as
#   "<e2><82><ac>". The escape is ASCII in the source and so survives either
#   way. Same rule for any other non-ASCII character in a string here.

T_02_04_eur_fn <- function(x, digits = 0, unit = "bn") {
  paste0("\u20ac", formatC(x, format = "f", digits = digits, big.mark = ","),
         unit)
}

###### T_02_05: Number Formatter ###############################################
# Note: Fixed decimals without the currency sign.

T_02_05_num_fn <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

###### T_02_06: Percentage Formatter ###########################################
# Note: Integer percentages, as the style guide asks for.

T_02_06_pct_fn <- function(x, digits = 0) {
  paste0(formatC(x * 100, format = "f", digits = digits), "%")
}

#### T_03: Controls ############################################################
# Note: The slider-plus-typing-box control, and the server side that keeps the
#   two in step.

###### T_03_01: Control Builder ################################################
# Note: A label with a hover explanation, then a slider with a box beside it
#   for typing an exact value. "controls" and "help" are the app's own lists.

T_03_01_control_fn <- function(id, controls, help, defaults, min = NULL) {
  spec  <- controls[[id]]
  value <- defaults[[id]]
  shiny::tags$div(
    class = "ctl",
    shiny::tags$div(class = "ctl-label", shiny::HTML(spec$label)),
    shiny::tags$div(
      class = "ctl-row",
      shiny::tags$div(
        class = "ctl-slider",
        shiny::sliderInput(id, NULL,
                           min = if (is.null(min)) spec$min else min,
                           max = spec$max, value = value, step = spec$step,
                           width = "100%")),
      shiny::tags$div(
        class = "ctl-box",
        shiny::numericInput(paste0(id, "_box"), NULL, value = value,
                            step = spec$step, width = "100%"))
    ),
    T_03_05_note_fn(help[[id]])
  )
}

###### T_03_05: A Grey Note Under a Control ####################################
# Note: The explanation of what a control does, shown rather than hidden. It
#   used to sit behind a small tooltip; a reader has to know to hover before
#   it helps them, which defeats the point of writing it.

T_03_05_note_fn <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  shiny::tags$div(class = "ctl-note", shiny::HTML(txt))
}

###### T_03_02: Slider and Box in Step #########################################
# Note: The box holds the exact value the model uses. Moving the slider copies
#   its value into the box. Typing in the box moves the slider to the nearest
#   point it can show, and that position is not copied back, so a typed 0.83
#   stays 0.83 even though the slider moves in 0.05 steps. Call once from the
#   server.

T_03_02_sync_fn <- function(input, session, controls) {
  near <- function(slider_value, box_value, spec) {
    abs(slider_value - min(max(box_value, spec$min), spec$max)) <=
      spec$step / 2 + 1e-8
  }
  lapply(names(controls), function(id) {
    box  <- paste0(id, "_box")
    spec <- controls[[id]]

    shiny::observeEvent(input[[id]], {
      b <- input[[box]]
      if (!is.null(b) && !is.na(b) && near(input[[id]], b, spec)) return()
      shiny::updateNumericInput(session, box, value = input[[id]])
    }, ignoreInit = TRUE)

    box_typed <- shiny::debounce(shiny::reactive(input[[box]]), 500)

    shiny::observeEvent(box_typed(), {
      b <- box_typed()
      if (is.null(b) || is.na(b)) return()
      v <- min(max(b, spec$min), spec$max)
      if (!near(input[[id]], v, spec)) {
        shiny::updateSliderInput(session, id, value = v)
      }
    }, ignoreInit = TRUE)
  })
  invisible(NULL)
}

###### T_03_03: Set One Control ################################################
# Note: Moves the slider and the box together, for scenarios and reset.

T_03_03_set_fn <- function(session, controls, id, value) {
  spec <- controls[[id]]
  shiny::updateSliderInput(session, id,
                           value = min(max(value, spec$min), spec$max))
  shiny::updateNumericInput(session, paste0(id, "_box"), value = value)
  invisible(NULL)
}

###### T_03_04: Read a Control #################################################
# Note: The exact value: the typed box if it holds one, else the slider.

T_03_04_val_fn <- function(input, id) {
  b <- input[[paste0(id, "_box")]]
  if (is.null(b) || is.na(b)) input[[id]] else b
}

#### T_04: Tiles ###############################################################
# Note: The readouts above the figures.

###### T_04_01: Readout Tile ###################################################
# Note: A read-only tile. "class" takes good or bad for the accent stripe.

T_04_01_tile_fn <- function(label, value, note = NULL, class = "") {
  # Note: The value goes through HTML() like the label and the note. A plain
  #   text node is written out in the session's locale, and shinylive runs in
  #   C, so a euro sign or an en dash in the number came out as <e2><82><ac>
  #   or <e2><80><93> on screen. A tag object is passed straight through.
  shiny::tags$div(
    class = paste("stat-tile", class),
    shiny::tags$div(class = "stat-label", shiny::HTML(label)),
    shiny::tags$div(class = "stat-value",
                    if (is.character(value)) shiny::HTML(value) else value),
    if (!is.null(note)) shiny::tags$div(class = "stat-note",
                                        shiny::HTML(note))
  )
}

###### T_04_02: Typed Readout Tile #############################################
# Note: A tile whose number can be typed over, so the class can ask "what
#   would deliver this?" and let the app invert the algebra.

T_04_02_typed_fn <- function(id, label, value, hint, step = 0.05) {
  shiny::tags$div(
    class = "stat-tile",
    shiny::tags$div(class = "stat-label", shiny::HTML(label)),
    shiny::tags$div(class = "stat-input",
                    shiny::numericInput(id, NULL, value = round(value, 3),
                                        step = step, width = "100%")),
    shiny::tags$div(class = "stat-hint", shiny::HTML(hint))
  )
}

###### T_04_03: Tile Row #######################################################
# Note: Wraps tiles in the flex row. Pass tiles as a list; NULLs are dropped,
#   so an app can gate a tile on the stage with an if().

T_04_03_row_fn <- function(...) {
  shiny::tags$div(class = "stat-row mb-2", ...)
}

#### T_05: Scenarios ###########################################################
# Note: The scenario menu and the story panel under it.

###### T_05_01: Scenario Choices ###############################################
# Note: Grouped by stage for the drop-down, and numbered within each stage
#   ("Scenario 1: ..."), in the order they are defined, so adding one
#   renumbers the rest on its own.

T_05_01_choices_fn <- function(scenarios, stage_word = "Stage") {
  out <- c(
    list("Choose a scenario" = c("Custom (set sliders yourself)" = "custom")),
    lapply(
      split(scenarios, vapply(scenarios, `[[`, "", "stage")),
      function(grp) {
        stats::setNames(names(grp),
                        paste0("Scenario ", seq_along(grp), ": ",
                               vapply(grp, `[[`, "", "label")))
      }
    )
  )
  names(out)[-1] <- paste(stage_word, names(out)[-1])
  out
}

###### T_05_02: Scenario Story ################################################
# Note: One short paragraph, and the notation named in brackets where it comes
#   up — "(alpha)", "(beta<sub>pi</sub> = 0.8)" — so a
#   student reading the story can find the same symbol on the slider and in
#   the equations tab without a separate glossary.
#
#   An earlier version grouped the controls into cards headed "the shock",
#   "the adjustment mechanism" and "the outcome". It was accurate and it was
#   too much apparatus for three sentences of set-up; the prose carries it.
#
#   "key" is no longer needed. An app that still supplies one (a character
#   vector of control ids) gets the old definition list underneath, so the
#   unconverted apps keep working until their stories are rewritten.

T_05_02_story_fn <- function(scenario, controls, help) {
  if (is.null(scenario)) return(NULL)
  legacy <- if (is.null(scenario$key)) NULL else {
    ids <- unlist(scenario$key, use.names = FALSE)
    list(
      shiny::tags$div(class = "story-key", "What matters here"),
      shiny::tags$dl(lapply(ids, function(id) {
        shiny::tagList(
          shiny::tags$dt(shiny::HTML(controls[[id]]$label)),
          shiny::tags$dd(shiny::HTML(help[[id]])))
      }))
    )
  }
  shiny::tags$div(class = "story", shiny::HTML(scenario$story), legacy)
}

###### T_05_03: Apply a Scenario ###############################################
# Note: Sets the stage, then every control: the scenario's own values where it
#   names them, the defaults everywhere else, so nothing is left over from the
#   scenario before.

T_05_03_apply_fn <- function(session, scenario, controls, defaults,
                             stage_id = "stage") {
  if (is.null(scenario)) return(invisible(NULL))
  shiny::updateRadioButtons(session, stage_id, selected = scenario$stage)
  for (id in names(controls)) {
    T_03_03_set_fn(session, controls, id,
                   if (!is.null(scenario$values[[id]])) {
                     scenario$values[[id]]
                   } else {
                     defaults[[id]]
                   })
  }
  invisible(NULL)
}

###### T_05_04: Worked-Example Presets #########################################
# Note: Scenarios belong in the MAIN window, not the sidebar. A sidebar menu
#   listing every stage's scenarios is a second selector that silently moves
#   the first one: choose a scenario from another lecture and the lecture
#   changes under you. Here the presets are shown for the stage on screen and
#   no other, so the sidebar chooses the model and the main window chooses
#   what to run in it. One decision each.
#
#   The buttons are built once, at UI time, so their ids are stable and no
#   dynamic UI is needed; conditionalPanel does the revealing. A stage with no
#   scenarios shows no card at all.
#
#   There is no "custom" option. Every stage opens on its first example and
#   the sliders stay live, so a student plays by moving them, not by first
#   choosing to be allowed to. The sidebar Reset returns the defaults.
#
#   Wire it up in the app with, in the UI, right above the prompt:
#       T_05_04_presets_fn(B_03_04_scenarios_lst, B_03_03_stages_vec)
#   and in the server, one observer per preset plus one for Custom:
#       scenario <- shiny::reactiveVal("custom")
#       lapply(names(scenarios), function(k)
#         shiny::observeEvent(input[[paste0("preset_", k)]], ...))

T_05_04_presets_fn <- function(scenarios, stages, stage_word = "Stage",
                               stage_id = "stage",
                               title_id = "preset_title",
                               story_id = "scenario_story") {
  by_stage <- split(names(scenarios), vapply(scenarios, `[[`, "", "stage"))
  panels <- lapply(names(by_stage), function(st) {
    shiny::conditionalPanel(
      sprintf("input.%s == '%s'", stage_id, st),
      shiny::tags$div(
        class = "preset-row",
        lapply(by_stage[[st]], function(k)
          shiny::actionButton(
            paste0("preset_", k), scenarios[[k]]$label,
            class = "btn btn-outline-primary btn-sm preset-btn"))
      )
    )
  })
  shiny::conditionalPanel(
    sprintf("[%s].indexOf(input.%s) >= 0",
            paste0("'", names(by_stage), "'", collapse = ", "), stage_id),
    bslib::card(
      bslib::card_header(shiny::uiOutput(title_id, inline = TRUE)),
      shiny::tags$div(
        class = "preset-lead",
        "Load another example, or move any slider \u2014 they stay live."),
      panels,
      shiny::uiOutput(story_id)
    )
  )
}

###### T_05_05: Which Preset Is Loaded #########################################
# Note: The card header names the loaded preset, but a lecturer at the board
#   wants to see it without reading, so the button itself is marked. The
#   server is the single source of truth — a preset also loads on its own when
#   the stage changes, so a click handler would miss it — and this is the one
#   line of JS that applies the class without rebuilding the buttons. Mount it
#   beside the other scripts in the page head, and in the server call
#       session$sendCustomMessage("dgPreset", key)
#   wherever the loaded preset changes.

T_05_05_preset_js_chr <- "
Shiny.addCustomMessageHandler('dgPreset', function (key) {
  $('.preset-row .preset-btn').removeClass('is-loaded').blur();
  if (key) { $('#preset_' + key).addClass('is-loaded'); }
});
"

###### T_05_06: Preset Card Title ##############################################
# Note: renderUI, not renderText: the separator is a middle dot and renderText
#   escapes a non-ASCII character to <U+00B7> under a C locale, which is what
#   shinylive runs in.

T_05_06_preset_title_fn <- function(scenario, stage, stages,
                                    stage_word = "Stage") {
  nm  <- trimws(names(stages)[match(stage, stages)])
  shiny::HTML(if (is.null(scenario)) {
    paste0("Worked Examples &middot; ", stage_word, " ", nm)
  } else {
    paste0("Worked Example: ", scenario$label,
           " &middot; ", stage_word, " ", nm)
  })
}

###### T_05_07: Preset Card Styling ############################################
# Note: Its own constant rather than a block inside T_07_06, because each app
#   mounts its own CSS string and would otherwise not see this at all. Mount
#   it in the page head beside the app's own style tag:
#       shiny::tags$style(shiny::HTML(T_05_07_preset_css_chr))

T_05_07_preset_css_chr <- "
  .preset-lead { font-size: 0.82rem; color: #6C757D; margin: 0 0 0.55rem 0; }
  .preset-row { display: flex; flex-wrap: wrap; gap: 0.5rem;
                align-items: center; }
  .preset-btn { white-space: normal; text-align: left; }
  .preset-btn:focus { box-shadow: none; }
  .preset-btn.is-loaded { background: #04204C; border-color: #04204C;
                          color: #FFFFFF; }
  .preset-custom { color: #6C757D; text-decoration: none; }
  .preset-custom:hover { color: #04204C; text-decoration: underline; }
  .card .story { margin-top: 0.75rem; }
"

#### T_06: Equations and Notation ##############################################
# Note: The three-tab panel that shows the model as it stands at this stage.
#   An equations list is a list of items, each:
#     group    one of the names of the groups vector
#     label    what the row is called
#     versions named by the stage a version first applies, value is LaTeX
#     notes    named the same way, value is the explanation (HTML allowed)
#   The panel shows the latest version at or before the current stage, flags
#   items new or changed at this stage, and for changed items shows what they
#   were before.

###### T_05_04: Name of a Stage ################################################
# Note: The stage list is keyed "1", "2", ... with names like "Stage 2:
#   Population Growth". This pulls the descriptive half back out so the
#   equations card can be titled with what the stage ADDS rather than a fixed
#   "The Model So Far", which never changed and told the reader nothing.

T_05_04_stage_name_fn <- function(stages, stage) {
  hit <- names(stages)[match(stage, unname(stages))]
  if (is.na(hit)) return(paste("Stage", stage))
  hit
}

###### T_06_01: Inline Maths ###################################################
# Note: Wraps LaTeX for MathJax.

T_06_01_mj_fn <- function(tex) shiny::HTML(paste0("\\(", tex, "\\)"))

###### T_06_02: New or Changed Flag ############################################
# Note: The little badge beside a row's label.

T_06_02_flag_fn <- function(status) {
  if (status != "") {
    shiny::tags$span(class = paste0("eq-flag eq-", status), status)
  }
}

###### T_06_03: Items in Force #################################################
# Note: Every item that has appeared by this stage, with its current version,
#   whether it is new or changed here, and what it was before.

T_06_03_items_fn <- function(equations, stage) {
  items <- Filter(function(it) min(as.numeric(names(it$versions))) <= stage,
                  equations)
  lapply(items, function(it) {
    keys   <- as.numeric(names(it$versions))
    cur    <- max(keys[keys <= stage])
    cur_nm <- names(it$versions)[keys == cur]
    status <- if (cur != stage) "" else if (cur == min(keys)) "new" else
      "changed"
    was <- if (status == "changed") {
      it$versions[[names(it$versions)[keys == max(keys[keys < cur])]]]
    }
    list(group = it$group, label = it$label, tex = it$versions[[cur_nm]],
         status = status, was = was, note = it$notes[[cur_nm]])
  })
}

###### T_06_04: Equations Tab ##################################################
# Note: The equations alone, in their groups, two columns wide.

T_06_04_model_fn <- function(items, groups, empty_text) {
  group_col <- function(grp) {
    rows <- lapply(Filter(function(x) x$group == grp, items), function(x) {
      shiny::tags$tr(
        shiny::tags$td(class = "eq-label", shiny::HTML(x$label),
                       T_06_02_flag_fn(x$status)),
        shiny::tags$td(T_06_01_mj_fn(x$tex)))
    })
    shiny::tags$div(
      class = "eq-group",
      shiny::tags$div(class = "eq-group-title", groups[[grp]]),
      if (length(rows) == 0) {
        shiny::tags$div(class = "chg-note text-muted", empty_text)
      } else {
        shiny::tags$table(class = "eq-table", do.call(shiny::tagList, rows))
      }
    )
  }
  shiny::withMathJax(shiny::tagList(
    do.call(bslib::layout_columns, c(
      list(col_widths = bslib::breakpoints(sm = 12, md = c(6, 6, 6, 6),
                                           xl = c(7, 5, 7, 5))),
      lapply(names(groups), group_col)
    )),
    shiny::tags$div(
      class = "eq-legend",
      shiny::tags$span(class = "eq-flag eq-new", "new"), " and ",
      shiny::tags$span(class = "eq-flag eq-changed", "changed"),
      " mark what this stage adds to the one before. The Explanations tab",
      " says what each one does.")
  ))
}

###### T_06_05: Notation Tab ###################################################
# Note: Every symbol in force, in one column per group. A notation item is
#   list(grp = , sym = , txt = , from = ).

T_06_05_notation_fn <- function(notation, stage, columns, first_stage) {
  items <- Filter(function(x) x$from <= stage, notation)
  col <- function(grps, title) {
    its <- Filter(function(x) x$grp %in% grps, items)
    shiny::tags$div(
      shiny::tags$div(class = "eq-group-title", title),
      shiny::tags$table(class = "nota-table", lapply(its, function(x) {
        shiny::tags$tr(
          shiny::tags$td(T_06_01_mj_fn(x$sym)),
          shiny::tags$td(paste0(toupper(substr(x$txt, 1, 1)),
                                substring(x$txt, 2)),
                         if (x$from == stage && stage > first_stage) {
                           shiny::tags$span(class = "eq-flag eq-new", "new")
                         }))
      }))
    )
  }
  shiny::withMathJax(do.call(bslib::layout_columns, c(
    list(col_widths = bslib::breakpoints(
      sm = 12, lg = rep(floor(12 / length(columns)), length(columns)))),
    lapply(names(columns), function(title) col(columns[[title]], title))
  )))
}

###### T_06_06: Explanations Tab ###############################################
# Note: Every equation with its explanation, and what it was before.

T_06_06_explain_fn <- function(items, groups) {
  blocks <- lapply(names(groups), function(grp) {
    its <- Filter(function(x) x$group == grp, items)
    if (length(its) == 0) return(NULL)
    shiny::tagList(
      shiny::tags$tr(shiny::tags$td(colspan = "3", class = "eq-group-title",
                                    groups[[grp]])),
      lapply(its, function(x) {
        shiny::tags$tr(
          shiny::tags$td(class = "eq-label", shiny::HTML(x$label),
                         T_06_02_flag_fn(x$status)),
          shiny::tags$td(shiny::tags$div(T_06_01_mj_fn(x$tex)),
                         if (!is.null(x$was)) {
                           shiny::tags$div(class = "chg-was", "was ",
                                           T_06_01_mj_fn(x$was))
                         }),
          shiny::tags$td(class = "chg-note", shiny::HTML(x$note))
        )
      })
    )
  })
  shiny::withMathJax(shiny::tags$table(class = "eq-table eq-explain", blocks))
}

#### T_07: Page Furniture ######################################################
# Note: The bslib theme, the CSS, the title bar, the QR block and the footer.

###### T_07_01: Author Credit ##################################################
# Note: Shown in the title bar, the browser tab and the footer, so every app
#   carries its credit wherever it is linked from.

T_07_01_author_chr <- "Sam Deegan"

###### T_07_02: Author Website #################################################
# Note: Linked from the title bar and footer.

T_07_02_site_chr <- "https://sam-deegan.com"

###### T_07_03: Course Line ####################################################
# Note: Footer text naming the module the apps were built for.

T_07_03_course_chr <- "ECON42550 Macroeconomics, University College Dublin"

###### T_07_04: QR Code Source #################################################
# Note: The URL the browser uses for the QR image, or NULL if none is found.
#   Files in www/ are served automatically; the course's _shared/ is
#   registered here, for when the app is run from inside the course repo.

T_07_04_qr_fn <- function(file = "qr-sam-deegan.png",
                          shared = file.path("..", "..", "_shared")) {
  T_07_04b_qr_data_chr
}

###### T_07_04b: The QR Code Itself ############################################
# Note: The QR is embedded as a data URI rather than shipped as a file.
#   shinylive's export drops www/ entirely, so on the website the file was
#   simply never there and no QR ever appeared. Inlining it means there is
#   nothing to serve, nothing to miss from a bundle, and no need to publish
#   a folder with addResourcePath just to reach one small image.

T_07_04b_qr_data_chr <- paste0(
  "data:image/png;base64,",
  "iVBORw0KGgoAAAANSUhEUgAAAdAAAAHQCAIAAACeP6xXAAAWfmNhQlgAABZ+an",
  "VtYgAAAB5qdW1kYzJwYQARABCAAACqADibcQNjMnBhAAAAFlhqdW1iAAAAR2p1",
  "bWRjMm1hABEAEIAAAKoAOJtxA3VybjpjMnBhOmRlMmQ4MzYzLTBhODAtNGYxNi",
  "04N2RmLTUyYjM2M2RhOTk3OAAAAAOTanVtYgAAAClqdW1kYzJhcwARABCAAACq",
  "ADibcQNjMnBhLmFzc2VydGlvbnMAAAAAuGp1bWIAAABEanVtZGNib3IAEQAQgA",
  "AAqgA4m3ETYzJwYS5pbmdyZWRpZW50LnYzAAAAABhjMnNo0hKfCG5/dhwCI44U",
  "kfoP3wAAAGxjYm9yo2lkYzpmb3JtYXRpaW1hZ2UvcG5namluc3RhbmNlSUR4LH",
  "htcDppaWQ6NmU0Zjg5MmItNzJjYy00NzZkLTkzODQtNmM3NTNiNTdkNWQybHJl",
  "bGF0aW9uc2hpcGhwYXJlbnRPZgAAAeJqdW1iAAAAQWp1bWRjYm9yABEAEIAAAK",
  "oAOJtxE2MycGEuYWN0aW9ucy52MgAAAAAYYzJzaB64gqIRaZznepP1L6DK8lMA",
  "AAGZY2JvcqJnYWN0aW9uc4KiZmFjdGlvbmtjMnBhLm9wZW5lZGpwYXJhbWV0ZX",
  "JzoWtpbmdyZWRpZW50c4GiY3VybHgtc2VsZiNqdW1iZj1jMnBhLmFzc2VydGlv",
  "bnMvYzJwYS5pbmdyZWRpZW50LnYzZGhhc2hYILkt2/QqDpb6U8xrRx9NUio1I9",
  "zQC3ElJcRLqTKGEm5rpGZhY3Rpb254HWNvbS5hbnRocm9waWMuY2xhdWRlLnBy",
  "b3ZpZGVkanBhcmFtZXRlcnOheB9jb20uYW50aHJvcGljLm9yaWdpbi1jb25maW",
  "RlbmNlZ3Vua25vd25rZGVzY3JpcHRpb254ZkNsYXVkZSBwcm92aWRlZCB0aGlz",
  "IGZpbGUgYXQgdGhlIHJlcXVlc3Qgb2YgYSB1c2VyIGFuZCBtYXkgaGF2ZSBjcm",
  "VhdGVkIG9yIG1vZGlmaWVkIHRoZSBmaWxlIGNvbnRlbnRzLm1zb2Z0d2FyZUFn",
  "ZW50oWRuYW1lZkNsYXVkZXJhbGxBY3Rpb25zSW5jbHVkZWT1AAAAyGp1bWIAAA",
  "BAanVtZGNib3IAEQAQgAAAqgA4m3ETYzJwYS5oYXNoLmRhdGEAAAAAGGMyc2jn",
  "aA86bUAj1BTSzkCWMnhdAAAAgGNib3KlY2FsZ2ZzaGEyNTZjcGFkTQAAAAAAAA",
  "AAAAAAAABkaGFzaFggqiqx2c9zOedWG0/vhtxbPWkPZPa4G9D2oP8uxkYIbGpk",
  "bmFtZW5qdW1iZiBtYW5pZmVzdGpleGNsdXNpb25zgaJlc3RhcnQYIWZsZW5ndG",
  "gZFooAAAI+anVtYgAAACdqdW1kYzJjbAARABCAAACqADibcQNjMnBhLmNsYWlt",
  "LnYyAAAAAg9jYm9ypWNhbGdmc2hhMjU2aXNpZ25hdHVyZXhNc2VsZiNqdW1iZj",
  "0vYzJwYS91cm46YzJwYTpkZTJkODM2My0wYTgwLTRmMTYtODdkZi01MmIzNjNk",
  "YTk5NzgvYzJwYS5zaWduYXR1cmVqaW5zdGFuY2VJRHgseG1wOmlpZDo3ZjNjMT",
  "kxNi1hNDNiLTRmZWMtYTQwMy1hMzk5MTBjNDFhYjlyY3JlYXRlZF9hc3NlcnRp",
  "b25zg6JjdXJseC1zZWxmI2p1bWJmPWMycGEuYXNzZXJ0aW9ucy9jMnBhLmluZ3",
  "JlZGllbnQudjNkaGFzaFgguS3b9CoOlvpTzGtHH01SKjUj3NALcSUlxEupMoYS",
  "bmuiY3VybHgqc2VsZiNqdW1iZj1jMnBhLmFzc2VydGlvbnMvYzJwYS5hY3Rpb2",
  "5zLnYyZGhhc2hYIC5B22W+uG+VitRrrfVmzK3FsTB1S14IaJMXNIvR0NNDomN1",
  "cmx4KXNlbGYjanVtYmY9YzJwYS5hc3NlcnRpb25zL2MycGEuaGFzaC5kYXRhZG",
  "hhc2hYIAHTskPFNgui1FMDB0GwVqpnkoPL3S1kwxxiqQ2vQKurdGNsYWltX2dl",
  "bmVyYXRvcl9pbmZvo2RuYW1lb0FudGhyb3BpYyBGaWxlc2d2ZXJzaW9uZTEuMC",
  "4wa3NwZWNWZXJzaW9uZTIuNC4wAAAQOGp1bWIAAAAoanVtZGMyY3MAEQAQgAAA",
  "qgA4m3EDYzJwYS5zaWduYXR1cmUAAAAQCGNib3LShFkCEqIBJhghWQIKMIICBj",
  "CCAY2gAwIBAgIUQOWgCu7COdC+uIP6BkIFPWdVEwAwCgYIKoZIzj0EAwMwSTEX",
  "MBUGA1UEChMOQW50aHJvcGljLCBQQkMxLjAsBgNVBAMTJUFudGhyb3BpYyBDb2",
  "50ZW50IENyZWRlbnRpYWxzIFJvb3QgQ0EwHhcNMjYwODA3MTg0MzU2WhcNMjgw",
  "ODA2MTk0MzU2WjBEMRcwFQYDVQQKEw5BbnRocm9waWMsIFBCQzEpMCcGA1UEAx",
  "MgQW50aHJvcGljIENsYXVkZSBDb250ZW50IFNpZ25pbmcwWTATBgcqhkjOPQIB",
  "BggqhkjOPQMBBwNCAASYegpry1AYBRTVNL1CpTlbROnY3dey+UrsF9C3phYrAT",
  "N3ZHf93Mo8RQN0KOUuOn19P4oWNFWe5n2/She9N7eTo1gwVjAOBgNVHQ8BAf8E",
  "BAMCB4AwFQYDVR0lBA4wDAYKKwYBBAGD6F4CATAMBgNVHRMBAf8EAjAAMB8GA1",
  "UdIwQYMBaAFM5R4gSBTmRbI/jjxM+aPpzB11zCMAoGCCqGSM49BAMDA2cAMGQC",
  "MDFzHRSeAXrSy1WOzkbhPZ6Km2wGTmZ/2gK18k8BQGXyqz88Rdrz6CTX9flAnY",
  "NVxgIwcF9c3fVhqmJKpi+UhasNUMko69cyX6STPfta3Q8EjyzDjzoyrol46FP6",
  "VFHhvUcJoWNwYWRZDZ4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "AAAAAAAAAAAAAAAAD2WECntGzDxQemCudV5YhayOl4u5vvnGqlv8S/BBFT5ytT",
  "EZwa4QPNJjq//+GBjwP3NbegQ45DoqC4cDKyfrsm1dfevv1vAwAACGpJREFUeJ",
  "zt3MFtNEUURlGMTCAERLQERCAshgxGKuF359Xf56wtu7o9c1Wr7+v1ev0GwLzf",
  "P30AgKcQXICI4AJEBBcgIrgAEcEFiAguQERwASKCCxARXICI4AJEBBcgIrgAEc",
  "EFiAguQERwASKCCxARXICI4AJEBBcgIrgAEcEFiAguQERwASKCCxARXICI4AJE",
  "BBcgIrgAEcEFiAguQOR7+g/88edf03/iUf795++jnz99/6e//9T0eaY/b9Pv55",
  "Tv18+a/v+64QJEBBcgIrgAEcEFiAguQERwASKCCxARXICI4AJEBBcgIrgAEcEF",
  "iAguQERwASKCCxAZ38M9tW1vdNq2PdOn7cluO8+0pz3vtu+XGy5ARHABIoILEB",
  "FcgIjgAkQEFyAiuAARwQWICC5ARHABIoILEBFcgIjgAkQEFyAiuACRdXu4p7bt",
  "Xd6+N3p6/m3v/9TT9n9Pbfv/3v4+3XABIoILEBFcgIjgAkQEFyAiuAARwQWICC",
  "5ARHABIoILEBFcgIjgAkQEFyAiuAARwQWIXL+Hy3vT+6HT+7nbzn9q2/PyWW64",
  "ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyAiO",
  "ACROzh/uKm91i37b3efn5+bW64ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4",
  "ABHBBYgILkBEcAEiggsQEVyAiOACRK7fw7VP+t62/dlTt+/VbjvPqdvPv40bLk",
  "BEcAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4",
  "AJF1e7in+6e8N70ne/te7bRt78f367PccAEiggsQEVyAiOACRAQXICK4ABHBBY",
  "gILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYiM7+E+bf90m9vfv33e924//9O4",
  "4QJEBBcgIrgAEcEFiAguQERwASKCCxARXICI4AJEBBcgIrgAEcEFiAguQERwAS",
  "KCCxAZ38Pdtk96ep6nuX1f9fb93KedZ9v3cfp9uuECRAQXICK4ABHBBYgILkBE",
  "cAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQ+Xq9Xp8+Q2p6f9M+6W",
  "d53vduf95T2/ad3XABIoILEBFcgIjgAkQEFyAiuAARwQWICC5ARHABIoILEBFc",
  "gIjgAkQEFyAiuAARwQWIfH/6AP/X7fuw9knfO30/t38ennae2/ejT7nhAkQEFy",
  "AiuAARwQWICC5ARHABIoILEBFcgIjgAkQEFyAiuAARwQWICC5ARHABIoILEBnf",
  "w53eo9y217ltf9N53tt2nlPb9nNPbTvPNDdcgIjgAkQEFyAiuAARwQWICC5ARH",
  "ABIoILEBFcgIjgAkQEFyAiuAARwQWICC5ARHABIuN7uLw3vQd6+57stj3lU7e/",
  "n1Pb9oK37e264QJEBBcgIrgAEcEFiAguQERwASKCCxARXICI4AJEBBcgIrgAEc",
  "EFiAguQERwASKCCxCxh/vDtu2Bntq2H3rqaee/fc/31LbznHLDBYgILkBEcAEi",
  "ggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyAiOACRAQXILJuD/f2vc",
  "vpPdbT551+n573s79/2/dl2x7xtvfjhgsQEVyAiOACRAQXICK4ABHBBYgILkBE",
  "cAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBk3R7utj3Wp+293m76ebd9Prd9fm",
  "5/3mluuAARwQWICC5ARHABIoILEBFcgIjgAkQEFyAiuAARwQWICC5ARHABIoIL",
  "EBFcgIjgAkTW7eE+zba9zm17u/ZV39u2Bz39Prd9Pk+54QJEBBcgIrgAEcEFiA",
  "guQERwASKCCxARXICI4AJEBBcgIrgAEcEFiAguQERwASKCCxD5er1enz4Dg7bt",
  "t27bz33aeU5t28M9te19uuECRAQXICK4ABHBBYgILkBEcAEiggsQEVyAiOACRA",
  "QXICK4ABHBBYgILkBEcAEiggsQ+Z7+A9v2WG+3bd9zev/09Pdvez/Tbt/bfdoe",
  "sRsuQERwASKCCxARXICI4AJEBBcgIrgAEcEFiAguQERwASKCCxARXICI4AJEBB",
  "cgIrgAkfE93FPb9jqnbdsLvv39b3uf027/f03b9n7ccAEiggsQEVyAiOACRAQX",
  "ICK4ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYis28M9tW3/dNv+5q",
  "nT9zn9vLefZ9vv57PccAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQ",
  "EVyAiOACRAQXICK4ABHBBYhcv4fLzzrdk53eb912nlPb9nmfttd8avr9uOECRA",
  "QXICK4ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQ",
  "sYf7i5ve93zaXu30+bft2277f93ODRcgIrgAEcEFiAguQERwASKCCxARXICI4A",
  "JEBBcgIrgAEcEFiAguQERwASKCCxARXIDI9Xu403ugvDe937ptH3ba7fu22/aF",
  "t3HDBYgILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyAiO",
  "ACRAQXILJuD/dp+5jT7J/+rG3Pe3qebXu7284zzQ0XICK4ABHBBYgILkBEcAEi",
  "ggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyAyNfr9fr0GQAewQ0XIC",
  "K4ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyA",
  "iOACRAQXICK4ABHBBYgILkBEcAEiggsQEVyAiOACRAQXICK4ABHBBYgILkBEcA",
  "EiggsQEVyAyH+uT5dQ7LLGXgAAAABJRU5ErkJggg=="
)

###### T_07_05: bslib Theme ####################################################
# Note: Dublin colours, so the apps match the slides.

T_07_05_theme_fn <- function() {
  bslib::bs_theme(
    version   = 5,
    primary   = T_01_01_palette_vec[["blue"]],
    secondary = T_01_01_palette_vec[["muted"]],
    success   = T_01_01_palette_vec[["green"]],
    info      = T_01_01_palette_vec[["light"]],
    bg        = T_01_01_palette_vec[["ground"]],
    fg        = T_01_01_palette_vec[["ink"]],
    base_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", local = FALSE),
      "Segoe UI", "Helvetica", "Arial", "sans-serif")
  )
}

###### T_07_06: Extra CSS ######################################################
# Note: Stat tiles, prompt, scenario story, the slider-plus-box controls, bold
#   sidebar headings, and the tab strips: the selected tab is blue, like the
#   pills inside the equations tab. Only theme colours appear here.

T_07_06_css_chr <- "
  /* Headings inherit Bootstrap's --bs-body-line-height of 1.5, which is right
     for body copy and much too loose for a heading; bslib does not apply
     headings-line-height to .bslib-page-title or .card-header. Measured
     before this rule: page title 29.89px on a 44.83px line, card header
     14.40px on 22px. */
  h1, h2, h3, h4, h5, h6,
  .bslib-page-title, .card-header { line-height: 1.2; }
  .bslib-page-title { line-height: 1.15; letter-spacing: -0.01em; }
  .stat-row { display: flex; flex-wrap: wrap; gap: 0.75rem; }
  .stat-caption { font-size: 0.8rem; color: #6C757D; margin: 0.2rem 0; }
  .stat-slot { flex: 1 1 11rem; display: flex; }
  .stat-slot > * { flex: 1 1 auto; }
  .stat-input .form-group { margin-bottom: 0; }
  .stat-input input { font-size: 1.35rem; font-weight: 600; color: #04204C;
    padding: 0.05rem 0.4rem; border: 1px solid #D8E0E6; background: #FFFFFF; }
  .stat-hint { font-size: 0.72rem; color: #6C757D; font-style: italic; }
  .side-qr { text-align: center; margin-top: 1rem; font-size: 0.8rem; }
  .side-qr img { width: 110px; height: 110px; }
  .side-qr-name { font-weight: 700; color: #04204C; margin-top: 0.3rem; }
  .bslib-page-title { display: flex; align-items: center; gap: 0.3rem;
    width: 100%; }
  .title-qr { margin-left: auto; }
  .title-qr img { height: 40px; width: 40px; }
  .stat-tile { flex: 1 1 11rem; border: none;
    border-left: 5px solid #0056A4; border-radius: 4px;
    padding: 0.45rem 0.8rem; background: #F2F6F9; }
  .stat-tile.good { border-left-color: #61B77C; }
  .stat-tile.bad  { border-left-color: #04204C; }
  .stat-label { font-size: 0.8rem; color: #6C757D; }
  .stat-value { font-size: 1.5rem; font-weight: 600; color: #04204C; }
  .stat-note  { font-size: 0.78rem; color: #212529; }
  .eq-table td { padding: 0.15rem 0.9rem 0.15rem 0; vertical-align: middle;
    border-bottom: 1px solid #F2F6F9; }
  .eq-label { color: #6C757D; font-size: 0.85rem; white-space: nowrap; }
  .eq-group { overflow-x: auto; }
  .eq-group-title { font-weight: 700; color: #04204C; font-size: 0.9rem;
    border-bottom: 2px solid #D8E0E6; margin-bottom: 0.3rem; }
  .eq-flag { display: inline-block; font-size: 0.65rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.04em; color: #FFFFFF;
    padding: 0.05rem 0.35rem; border-radius: 3px; margin-left: 0.35rem; }
  .eq-new     { background: #61B77C; }
  .eq-changed { background: #0056A4; }
  .eq-legend  { font-size: 0.78rem; color: #6C757D; margin-top: 0.3rem; }
  .eq-explain td.chg-note { max-width: 32rem; }
  .eq-explain td.eq-group-title { padding-top: 0.6rem; }
  .nota-table { width: 100%; font-size: 0.88rem; }
  .nota-table td { padding: 0.25rem 0.6rem 0.25rem 0; vertical-align: top;
    border-bottom: 1px solid #F2F6F9; }
  .nota-table td:first-child { white-space: nowrap; width: 6.5rem; }
  .chg-was  { color: #6C757D; font-size: 0.85rem; }
  .chg-note { font-size: 0.85rem; }
  .prompt { background: #F2F6F9; border-left: 4px solid #61B77C;
    padding: 0.6rem 0.9rem; border-radius: 4px; font-size: 0.95rem; }
  .problem { background: #F2F6F9; border-left: 4px solid #04204C;
    padding: 0.6rem 0.9rem; border-radius: 4px; }
  .story { background: #F2F6F9; border-radius: 4px; font-size: 0.86rem;
    padding: 0.55rem 0.75rem; margin: -0.4rem 0 0.7rem 0; }
  .story-key { font-weight: 700; color: #04204C; margin-top: 0.45rem; }
  .story dl { margin: 0.2rem 0 0 0; }
  .story dt { font-weight: 600; color: #0056A4; }
  .story dd { margin: 0 0 0.3rem 0; }
  .narrative { border: 1px solid #D8E0E6; border-left: 4px solid #61B77C;
    border-radius: 4px; padding: 0.55rem 0.75rem; font-size: 0.88rem;
    margin-bottom: 0.7rem; background: #FFFFFF; }
  .nar-head { font-weight: 700; color: #04204C; margin-bottom: 0.2rem; }
  .nar-source { font-size: 0.86em; color: #6C757D; margin-top: 0.5rem; }
  .ctl { margin-bottom: 0.4rem; }
  .ctl-label { font-size: 0.88rem; font-weight: 600; color: #0056A4;
    margin-bottom: -0.25rem; }
  .ctl-help { color: #0056A4; cursor: help; font-size: 0.85rem; }
  .ctl-row { display: flex; gap: 0.5rem; align-items: center; }
  .ctl-slider { flex: 1 1 auto; min-width: 0; }
  .ctl-box { flex: 0 0 4.9rem; }
  .ctl .form-group { margin-bottom: 0; }
  .ctl-box input { padding: 0.15rem 0.35rem; font-size: 0.85rem;
    text-align: right; }
  .sidebar .accordion-button { font-weight: 700; color: #04204C; }
  .sidebar h6 { color: #04204C; font-weight: 700; margin-top: 0.5rem; }
  .title-credit { font-size: 0.8rem; font-weight: 400; margin-left: 0.8rem;
    opacity: 0.8; }
  .title-credit a { color: inherit; }
  .credit { font-size: 0.8rem; color: #6C757D; text-align: center;
    padding: 1rem 0 0.5rem 0; }
  .nav-tabs .nav-link { color: #6C757D; font-weight: 600;
    border-color: transparent; }
  .nav-tabs .nav-link:hover { color: #0056A4; background: #F2F6F9; }
  .nav-tabs .nav-link.active { color: #FFFFFF; font-weight: 700;
    background: #0056A4; border-color: #0056A4; }
  .site-nav { background: #FFFFFF; border-bottom: 1px solid #D8E0E6;
    padding: 1rem 2rem; margin: -0.5rem -0.5rem 0.75rem -0.5rem; }
  .site-nav-row { display: flex; align-items: center; gap: 2rem;
    max-width: 1200px; margin: 0 auto; }
  .site-nav ul { list-style: none; display: flex; justify-content: center;
    gap: 2rem; flex: 1 1 auto; margin: 0; padding: 0; flex-wrap: wrap; }
  .site-nav a.site-brand { display: flex; align-items: center; gap: 0.5rem;
    flex: 0 0 auto; color: #04204C; font-size: 0.95rem; font-weight: 600;
    padding-bottom: 0; }
  .site-nav a.site-brand:hover { color: #0056A4; }
  .site-nav a.site-brand img { width: 28px; height: 28px;
    border-radius: 50%; display: block; }
  .site-nav a { color: #04204C; text-decoration: none; font-weight: 500;
    font-size: 0.95rem; padding-bottom: 0.25rem; position: relative;
    transition: color 0.3s ease; }
  .site-nav a:hover, .site-nav a.active { color: #0056A4; }
  .site-nav a.active::after { content: ''; position: absolute;
    bottom: -0.5rem; left: 0; right: 0; height: 2px; background: #0056A4; }
  .page-title-wrap { display: block; width: 100%; }
  .page-byline { color: #6C757D; font-size: 0.95rem; font-weight: 500;
    margin: -0.35rem 0 0.6rem 0; }
  .page-byline a { color: #6C757D; text-decoration: none; }
  .page-byline a:hover { color: #0056A4; }
  @media (max-width: 700px) {
    .site-nav { padding: 0.6rem 0.75rem; }
    .site-nav ul { gap: 1rem; font-size: 0.85rem; }
    .site-nav-row { gap: 0.75rem; flex-wrap: wrap; }
  }
  #stage-label, #scenario-label { font-weight: 700; color: #04204C;
    font-size: 0.95rem; margin-bottom: 0.35rem; }
  #stage .radio label, #stage .shiny-options-group label { font-weight: 400;
    color: #212529; }
  .ctl-note { font-size: 0.78rem; color: #6C757D; line-height: 1.45;
    margin: 0.15rem 0 0.1rem 0; }
  .fig-head { display: flex; align-items: center; gap: 0.5rem; }
  .fig-note { color: #6C757D; font-size: 0.82rem; line-height: 1.45;
    padding: 0.15rem 0.15rem 0 0.15rem; }
  .fig-note p { margin: 0; }
  .fig-save { margin-left: auto; border: 1px solid #D8E0E6; background: #FFFFFF;
    color: #0056A4; font-size: 0.72rem; font-weight: 600; border-radius: 3px;
    padding: 0.1rem 0.5rem; cursor: pointer; line-height: 1.5;
    transition: background 0.2s ease, color 0.2s ease; }
  .fig-save:hover { background: #0056A4; color: #FFFFFF;
    border-color: #0056A4; }
  .fig-save:active { background: #003C77; border-color: #003C77; }
"

###### T_07_06b: The MathJax Library ###########################################
# Note: Load MathJax ourselves rather than relying on shiny::withMathJax().
#   withMathJax() emits the library tag as a singleton, and singletons are
#   suppressed inside renderUI — which is the only place this app calls it —
#   so the library never reached the page and every equation rendered as raw
#   \( ... \). Shiny's own default also points at mathjax.rstudio.com, which
#   no longer serves the file. Version 2.7.9 from cdnjs keeps the MathJax.Hub
#   API that withMathJax() and the hooks below call.

T_07_06b_mathjax_src_chr <- paste0(
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js",
  "?config=TeX-AMS-MML_HTMLorMML")

###### T_07_07: Re-Typeset MathJax #############################################
# Note: Two triggers. A formula laid out in a hidden tab gets the wrong size,
#   so re-render when a tab is shown; and the equations are rebuilt by
#   renderUI every time the stage changes, so typeset again whenever Shiny
#   delivers a new value. Both are no-ops until the library has loaded.

T_07_07_mathjax_js_chr <- paste(
  "(function() {",
  "  var pending = null;",
  "  function typeset() {",
  "    if (!window.MathJax || !MathJax.Hub) return;",
  "    clearTimeout(pending);",
  "    pending = setTimeout(function() {",
  "      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);",
  "    }, 50);",
  "  }",
  "  document.addEventListener('shown.bs.tab', typeset);",
  "  $(document).on('shiny:value shiny:visualchange', typeset);",
  "})();",
  sep = "\n"
)

###### T_07_07b: Save a Figure as PNG ##########################################
# Note: Shiny already renders every plot as a PNG <img> in the page, at the
#   browser's own pixel ratio (so 2x on a retina screen). Saving it is then
#   just a matter of pointing an <a download> at that image's src. Doing it
#   this way rather than with a downloadHandler means no R graphics device is
#   involved, so it behaves identically under shinylive in the browser, where
#   a server-side device may not exist at all.

T_07_07b_save_js_chr <- paste(
  "document.addEventListener('click', function(ev) {",
  "  var btn = ev.target.closest('.fig-save');",
  "  if (!btn) return;",
  "  var box = document.getElementById(btn.dataset.plot);",
  "  var img = box ? box.querySelector('img') : null;",
  "  if (!img || !img.src) { return; }",
  "  var a = document.createElement('a');",
  "  a.href = img.src;",
  "  a.download = (btn.dataset.name || 'figure') + '.png';",
  "  document.body.appendChild(a);",
  "  a.click();",
  "  document.body.removeChild(a);",
  "});",
  sep = "\n"
)

###### T_07_07c: Figure Card ###################################################
# Note: A card holding one figure, with a Save PNG button in its header. Use
#   in place of card(card_header(title), plotOutput(id, height)).

T_07_07c_figcard_fn <- function(id, title, height, file = NULL) {
  T_07_07e_add_fn(id)
  stem <- if (is.null(file)) {
    gsub("(^-|-$)", "",
         gsub("-+", "-", gsub("[^a-z0-9]+", "-", tolower(title))))
  } else {
    file
  }
  bslib::card(
    bslib::card_header(
      shiny::tags$div(
        class = "fig-head",
        shiny::tags$span(title),
        shiny::tags$button(type = "button", class = "fig-save",
                           `data-plot` = id, `data-name` = stem,
                           title = "Save this figure as a PNG",
                           "Save PNG")
      )
    ),
    shiny::plotOutput(id, height = height),
    shiny::uiOutput(paste0(id, "__cap"), class = "fig-note")
  )
}

###### T_07_07d: Wire Up the Lifted Captions ###################################
# Note: Called once from a server function. For every figure id it defines the
#   companion output that prints the caption T_02_01c_draw_fn lifted out of
#   the plot. The ids are collected as the UI is built, so an app never has to
#   list its own figures: adding a figcard is enough.

T_07_07d_cap_fn <- function(output) {
  ids <- T_07_07e_ids_fn()
  for (id in ids) {
    local({
      this <- id
      output[[paste0(this, "__cap")]] <- shiny::renderUI({
        store <- T_02_01d_capstore_fn()
        txt   <- if (is.null(store)) NULL else store[[this]]
        if (is.null(txt) || !nzchar(txt)) return(NULL)
        shiny::tags$p(txt)
      })
    })
  }
  invisible(ids)
}

###### T_07_07e: The Figure Register ###########################################
# Note: Every figcard records its id here as the UI is constructed, which
#   happens once when the app loads.

T_07_07e_env <- new.env(parent = emptyenv())
T_07_07e_env$ids <- character(0)

T_07_07e_ids_fn <- function() T_07_07e_env$ids

T_07_07e_add_fn <- function(id) {
  if (!id %in% T_07_07e_env$ids) {
    T_07_07e_env$ids <- c(T_07_07e_env$ids, id)
  }
  invisible(NULL)
}

###### T_07_08: Page Head ######################################################
# Note: The style block, the site's typeface and the MathJax refresh, for
#   tags$head(). Inter is the website's font; the stack falls back to the
#   system one if Google Fonts cannot be reached, so an app opened offline
#   still looks right.

T_07_08_head_fn <- function() {
  shiny::tags$head(
    shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    shiny::tags$link(
      rel = "stylesheet",
      href = paste0("https://fonts.googleapis.com/css2?family=Inter:",
                    "wght@300;400;500;600;700&display=swap")),
    shiny::tags$style(shiny::HTML(T_07_06_css_chr)),
    shiny::tags$script(src = T_07_06b_mathjax_src_chr),
    shiny::tags$script(shiny::HTML(T_07_07_mathjax_js_chr)),
    shiny::tags$script(shiny::HTML(T_07_07b_save_js_chr))
  )
}

###### T_07_08b: Site Navigation ###############################################
# Note: The website's own nav bar, so an app opened from sam-deegan.com feels
#   like a page of the site rather than a dead end: these apps are reached by
#   a full page load from Resources, so without this there is no way back but
#   the browser's back button. Deliberately NOT sticky (the site's is): these
#   pages are dense and a bar that never scrolls away costs too much height.
#   Links are absolute so they work from the site, from a local runApp, and
#   from a shinylive export sitting anywhere. target = "_top" is required:
#   shinylive runs the app inside an iframe, so without it a click loads the
#   whole website into the app's frame instead of navigating away from it.
#
#   The brand block at the left (27-09-2026) is the site's own: Sam's logo
#   as a 28px circle and his name, linking home, with the page links to its
#   right. The logo is www/sd-logo.png in each app (180 x 180). The sidebar
#   QR stays: it is what a phone is pointed at.

T_07_08b_nav_fn <- function(active = "Resources",
                            logo_src = "sd-logo.png") {
  pages <- c(Bio = "index.html", Papers = "papers.html",
             Teaching = "teaching.html", Experience = "experience.html",
             Presentations = "talks.html", Resources = "resources.html",
             Contact = "contact.html")
  shiny::tags$nav(
    class = "site-nav",
    shiny::tags$div(
      class = "site-nav-row",
      shiny::tags$a(
        href = T_07_02_site_chr, target = "_top", class = "site-brand",
        shiny::tags$img(src = logo_src, alt = T_07_01_author_chr),
        T_07_01_author_chr),
      shiny::tags$ul(
        lapply(names(pages), function(nm) {
          shiny::tags$li(shiny::tags$a(
            href   = paste0(T_07_02_site_chr, "/", pages[[nm]]),
            target = "_top",
            class  = if (identical(nm, active)) "active" else NULL,
            nm))
        })
      )
    )
  )
}

###### T_07_09: Title Bar ######################################################
# Note: Just the app's name. It used to carry "by Sam Deegan · sam-deegan.com"
#   with both halves linking to the same place, plus a 40px QR linking there
#   again: three links to one URL in a single heading, and a QR too small for
#   anyone to scan. The nav bar above now carries the site, and the sidebar
#   carries the one QR that is actually big enough to point a phone at.

T_07_09_title_fn <- function(name, qr_src = NULL) {
  # Note: bslib lays the page title out as a flex row, so a second element
  #   beside the heading lands to its right and wraps. The wrapper below is a
  #   block, which puts the byline under the name where the website has it.
  shiny::tags$div(
    class = "page-title-wrap",
    shiny::tags$h1(class = "bslib-page-title", name),
    shiny::tags$div(
      class = "page-byline",
      shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                    T_07_01_author_chr))
  )
}

###### T_07_10: Sidebar QR Block ###############################################
# Note: The QR code and name at the foot of the sidebar.

T_07_10_sideqr_fn <- function(qr_src) {
  if (is.null(qr_src)) return(NULL)
  shiny::tags$div(
    class = "side-qr",
    shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                  shiny::tags$img(src = qr_src,
                                  alt = paste("QR code for",
                                              T_07_02_site_chr))),
    shiny::tags$div(class = "side-qr-name", T_07_01_author_chr),
    shiny::tags$div(shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                                  sub("^https?://", "", T_07_02_site_chr)))
  )
}

###### T_07_11: Footer #########################################################
# Note: Who built it and what for. "extra" adds a sentence, such as whose
#   notation the app follows.

T_07_11_footer_fn <- function(extra = NULL) {
  shiny::tags$footer(
    class = "credit",
    "Built by ",
    shiny::tags$a(href = T_07_02_site_chr, target = "_blank",
                  T_07_01_author_chr),
    paste0(" for ", T_07_03_course_chr, "."),
    if (!is.null(extra)) paste0(" ", extra)
  )
}

###### T_07_12: Prompt Panel ###################################################
# Note: The guidance line above the figures. A loaded scenario is a worked
#   Example; the stage's own line is a Note. Labelling the two differently
#   tells the student at a glance which one they are reading.

T_07_12_prompt_fn <- function(scenario, stage, prompts) {
  on_stage <- !is.null(scenario) && scenario$stage == stage
  txt   <- if (on_stage) scenario$prompt else prompts[[stage]]
  label <- if (on_stage) "Example. " else "Note. "
  shiny::tags$div(class = "prompt", shiny::tags$strong(label), txt)
}

###### T_07_13: Problems Panel #################################################
# Note: Warnings shown when the calibration stops making sense.

T_07_13_problems_fn <- function(problems) {
  if (length(problems) == 0) return(NULL)
  shiny::tags$div(class = "problem", lapply(problems, shiny::tags$p))
}
