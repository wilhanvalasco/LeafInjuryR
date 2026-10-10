
# LeafInjuryR

<sub>Version 0.2.0 | Release notes</sub>

This release introduces a streamlined public API, an enhanced image-processing workflow, an improved interactive interface, and expanded capabilities for validation, reporting, and reproducibility.

## Breaking Changes

- **Simplified API:** Six public functions are available: `analyze_leaf()`, `analyze_leaf_batch()`, `crop_leaf()`, `segment_leaf()`, `validate_leaf()`, and `run_leafinjury_app()`.
- **Internal functions:** Previously exported helper functions have been moved to the internal namespace.
- **Updated segmentation:** `segment_leaf()` returns both the leaf mask and tissue classification, with support for manual corrections through `mode = "manual"`.
- **Parameter configuration:** Processing options use the standardized structure `params = list(group = list(...))`.

## Features and Improvements

- **Interactive interface:** Redesigned Shiny application with four sections: Image, Segmentation, Analysis, and Results.
- **Image visualization:** Enlarged viewer with zoom, fit-to-screen, full-screen mode, and optional reference grid.
- **Manual corrections:** Interactive brush and rectangle tools for refining segmentation masks, with real-time injury percentage updates.
- **Image comparison:** Interactive slider for comparing original and processed images.
- **Batch processing:** Automated analysis of multiple images with previews and consolidated results.
- **Automated reports:** Self-contained HTML reports, Excel workbooks with three worksheets, and CSV exports.
- **Tissue classification:** Added `tissue_method = "relative"` for naturally pigmented or senescent leaves.
- **Physical measurements:** Optional conversion of pixel measurements to cm² using `pixels_per_cm`.
- **Validation:** Comparison against reference masks using IoU, Dice coefficient, sensitivity, specificity, precision, and F1 score.

## Reproducibility and Traceability

- MD5 checksums for input file identification and integrity verification.
- Recording of processing parameters and computational environment.
- Traceability of manual segmentation corrections.
- Consistent results across HTML, Excel, and CSV exports.
- Fully local image processing without external services.

## Scientific Scope

LeafInjuryR quantifies visible leaf injury patterns from RGB images. Measurements represent image-derived tissue characteristics and do not constitute automated diagnoses of plant diseases, nutritional disorders, or phytotoxicity.

---

<sub>Version 0.1.0 | Initial release</sub>

- Initial implementation of image-based leaf injury quantification.
- Leaf segmentation and classification of healthy and injured tissues.
- Estimation of injured leaf area as a percentage of total leaf area.
- Foundational image-processing workflow developed in R.
