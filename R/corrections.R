# Manual corrections ---------------------------------------------------------
#
# A correction is one row of a data frame with columns:
#   action : "add_leaf", "remove_leaf", "healthy", "injured",
#            "chlorotic", "necrotic"
#   shape  : "circle" (brush stroke: x, y, radius) or
#            "rect" (xmin, xmax, ymin, ymax)
# Coordinates are pixels of the analysed (cropped) image.
# Corrections are applied in order, are stored in the result and are
# therefore fully reproducible.

#' Valid correction actions
#' @noRd
correction_actions <- function() {
  c("add_leaf", "remove_leaf", "healthy", "injured", "chlorotic", "necrotic")
}

#' Create correction rows (used by the Shiny app and documented in segment_leaf)
#' @noRd
new_correction <- function(action, shape = c("circle", "rect"), x = NA_real_, y = NA_real_,
                           radius = NA_real_, xmin = NA_real_, xmax = NA_real_,
                           ymin = NA_real_, ymax = NA_real_) {
  shape <- match.arg(shape)
  if (!action %in% correction_actions()) leaf_abort(sprintf("Unknown correction action '%s'.", action))
  data.frame(action = action, shape = shape, x = x, y = y, radius = radius,
             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             stringsAsFactors = FALSE)
}

#' Validate / normalise a corrections data frame
#' @noRd
as_corrections <- function(corrections) {
  empty <- new_correction("healthy")[0, ]
  if (is.null(corrections) || (is.data.frame(corrections) && !nrow(corrections))) return(empty)
  if (!is.data.frame(corrections)) leaf_abort("`corrections` must be a data frame.")
  if (!all(c("action", "shape") %in% names(corrections))) {
    leaf_abort("`corrections` needs at least the columns `action` and `shape`.")
  }
  for (cc in setdiff(names(empty), names(corrections))) corrections[[cc]] <- NA_real_
  corrections <- corrections[names(empty)]
  bad <- setdiff(unique(corrections$action), correction_actions())
  if (length(bad)) leaf_abort(sprintf("Unknown correction action(s): %s.", paste(bad, collapse = ", ")))
  if (!all(corrections$shape %in% c("circle", "rect"))) leaf_abort("Correction `shape` must be 'circle' or 'rect'.")
  corrections$action <- as.character(corrections$action)
  corrections$shape <- as.character(corrections$shape)
  corrections
}

#' Logical region covered by one correction
#' @noRd
correction_region <- function(row, dims) {
  m <- matrix(FALSE, dims[1], dims[2])
  if (row$shape == "rect") {
    x1 <- max(1, floor(min(row$xmin, row$xmax))); x2 <- min(dims[1], ceiling(max(row$xmin, row$xmax)))
    y1 <- max(1, floor(min(row$ymin, row$ymax))); y2 <- min(dims[2], ceiling(max(row$ymin, row$ymax)))
    if (x1 <= x2 && y1 <= y2) m[x1:x2, y1:y2] <- TRUE
  } else {
    r <- max(0.5, row$radius)
    x1 <- max(1, floor(row$x - r)); x2 <- min(dims[1], ceiling(row$x + r))
    y1 <- max(1, floor(row$y - r)); y2 <- min(dims[2], ceiling(row$y + r))
    if (x1 <= x2 && y1 <= y2) {
      xs <- x1:x2; ys <- y1:y2
      m[xs, ys] <- outer((xs - row$x)^2, (ys - row$y)^2, `+`) <= r^2
    }
  }
  m
}

#' Apply corrections to a segmentation (leaf mask + class map)
#'
#' Added leaf pixels are classified with the segmentation's own tissue rule;
#' removed leaf pixels become background; tissue actions only affect pixels
#' inside the (current) leaf mask.
#' @noRd
apply_corrections <- function(seg, arr, corrections, config) {
  corrections <- as_corrections(corrections)
  if (!nrow(corrections)) return(seg)
  d <- dim(seg$leaf_mask)
  leaf <- seg$leaf_mask
  cm <- seg$class_map
  mc <- isTRUE(seg$multiclass)
  tissue_method <- seg$tissue_method
  cfg_t <- config
  if (identical(tissue_method, "auto")) {
    tissue_method <- "lab"
    cfg_t$tissue$lab_healthy_hue_min <- seg$thresholds$tissue$lab_healthy_hue_min %||%
      config$tissue$lab_healthy_hue_min
  }
  for (i in seq_len(nrow(corrections))) {
    row <- corrections[i, ]
    reg <- correction_region(row, d)
    if (!any(reg)) next
    switch(row$action,
      add_leaf = {
        new_px <- reg & !leaf
        if (any(new_px)) {
          tis <- classify_leaf_tissue(arr, new_px, tissue_method, mc, cfg_t)
          cm[new_px] <- tis$class_map[new_px]
          leaf <- leaf | new_px
        }
      },
      remove_leaf = { leaf[reg] <- FALSE; cm[reg] <- 0L },
      healthy = { cm[reg & leaf] <- 1L },
      injured = {
        sel <- reg & leaf
        if (any(sel)) {
          cm[sel] <- if (mc) subclassify_injured(arr, sel, cfg_t)[sel] else 2L
        }
      },
      chlorotic = { cm[reg & leaf] <- if (mc) 2L else 2L },
      necrotic = { cm[reg & leaf] <- if (mc) 3L else 2L }
    )
  }
  cm[!leaf] <- 0L
  seg$leaf_mask <- leaf
  seg$class_map <- cm
  seg$healthy_mask <- cm == 1L
  seg$injury_mask <- cm >= 2L
  seg$perforation_mask[leaf] <- FALSE
  seg$corrections <- rbind(as_corrections(seg$corrections), corrections)
  seg$mode <- "manual"
  seg$metrics <- calculate_injury(cm, leaf, mc)
  seg
}

#' Multiclass labels (2 chlorotic, 3 necrotic, 4 other) for pixels in `sel`
#' using the CIELAB rules of classify_leaf_tissue()
#' @noRd
subclassify_injured <- function(arr, sel, config) {
  tc <- config$tissue
  out <- matrix(0L, dim(sel)[1], dim(sel)[2])
  idx <- which(sel)
  lab <- rgb_to_lab(arr[, , 1][idx], arr[, , 2][idx], arr[, , 3][idx])
  achromatic <- lab$chroma < tc$achromatic_chroma_max
  necrotic <- (achromatic & lab$L <= tc$dark_lightness_max) |
    (!achromatic & (lab$hue < tc$lab_necrotic_hue_max | lab$hue >= 300))
  chlorotic <- !achromatic & !necrotic & lab$hue >= tc$lab_necrotic_hue_max &
    lab$hue <= tc$lab_healthy_hue_max
  out[idx] <- ifelse(necrotic, 3L, ifelse(chlorotic, 2L, 4L))
  out
}
