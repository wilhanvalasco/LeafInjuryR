# LeafInjuryR 0.2.2

CRAN resubmission addressing the comments of the review of version 0.2.1.
Analysis results are unchanged.

- `DESCRIPTION`: references for the implemented methods (Otsu thresholding,
  connected-component labeling, mathematical morphology, excess green and
  excess green minus excess red color indices) added to the `Description`
  field.
- `DESCRIPTION`: Walter Baida Garcia Coutinho added as author.
- `validate_leaf()` example: the reference mask is now rebuilt from the
  public crop record (`res$crop`) instead of calling an internal function.
- The validation vignette builds its synthetic image with public code only.
- Graphical parameters, options and the working directory are always
  restored with `on.exit(..., add = TRUE)`, including in the Shiny
  application (`inst/shiny/app/modules/helpers.R`, `mod_results.R`).
- Shiny application: temporary folders are removed when the browser
  session ends; internal helpers are reached through a single documented
  access point instead of the triple-colon operator.
- New regression tests for mask reconstruction, validation metrics and
  restoration of global state.

# LeafInjuryR 0.2.0

This release introduces a streamlined public API, an improved image-processing workflow, a redesigned interactive interface, and expanded validation, reporting and reproducibility features.

## Breaking changes

- Simplified API with six public functions: `analyze_leaf()`, `analyze_leaf_batch()`, `crop_leaf()`, `segment_leaf()`, `validate_leaf()` and `run_leafinjury_app()`.
- Helper functions that were previously exported are now internal.
- `segment_leaf()` returns both the leaf mask and the tissue classification, and supports manual corrections through `mode = "manual"`.
- Processing options use the structure `params = list(group = list(...))`.

## New features and improvements

- Redesigned Shiny application with four sections: Image, Segmentation, Analysis and Results.
- Larger image viewer with zoom, fit-to-screen, full-screen mode and optional reference grid.
- Brush and rectangle tools for refining segmentation masks, with real-time update of the injury percentage.
- Slider for comparing original and processed images.
- Batch analysis of multiple images with previews and consolidated results.
- Self-contained HTML reports, Excel workbooks with three worksheets, and CSV exports.
- New `tissue_method = "relative"` for naturally pigmented or senescent leaves.
- Optional conversion of pixel measurements to cm² using `pixels_per_cm`.
- Validation against reference masks using IoU, Dice coefficient, sensitivity, specificity, precision and F1 score.

## Reproducibility

- MD5 checksums for input file identification.
- Recording of processing parameters and computational environment.
- Traceability of manual segmentation corrections.
- Consistent results across HTML, Excel and CSV exports.
- Fully local processing, without external services.

## Scope

LeafInjuryR quantifies visible leaf injury patterns from RGB images. Results are image-derived measurements and are not automated diagnoses of plant diseases, nutritional disorders or phytotoxicity.

# LeafInjuryR 0.1.0

- Initial implementation of image-based leaf injury quantification.
- Leaf segmentation and classification of healthy and injured tissues.
- Estimation of injured leaf area as a percentage of total leaf area.
