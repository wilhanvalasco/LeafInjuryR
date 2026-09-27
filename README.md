# LeafInjuryR

Quantification of **visual leaf injury patterns** from digital photographs of
detached leaves on a contrasting background, using classical and auditable
image processing in R. Includes a local Shiny interface for people who do not
program.

The pipeline separates three steps, which are also validated separately:

```
original image -> (crop) -> (normalise) -> leaf segmentation -> leaf mask
   -> pixels INSIDE the leaf mask -> tissue classification -> quantification -> QC
```

1. **Leaf segmentation**: which pixels belong to the leaf (including necrotic
   and chlorotic tissue).
2. **Tissue classification**: healthy / injured (optionally healthy /
   chlorotic / necrotic / other), computed only inside the leaf mask.
3. **Quantification**: `injured_percent = injured pixels / leaf pixels x 100`.
   Background pixels never enter the calculation.

> **Scope.** LeafInjuryR quantifies visual colour patterns. It does **not**
> diagnose pathogens, diseases, nutritional deficiencies or phytotoxicity.
> Software that works computationally is **not** a scientifically validated
> method: calibrate and validate against manual reference masks for your own
> species, camera and lighting (see `vignette("method-validation")`).

## Installation

EBImage belongs to Bioconductor and must be installed first:

```r
install.packages("BiocManager")
BiocManager::install("EBImage")

# LeafInjuryR from the source archive
install.packages("LeafInjuryR_0.1.0.tar.gz", repos = NULL, type = "source")
# or from the unpacked folder
# install.packages("path/to/LeafInjuryR", repos = NULL, type = "source")

# Optional, for the graphical interface
install.packages(c("shiny", "bslib", "DT"))
```

On Linux, EBImage needs the system libraries for FFTW, JPEG, PNG and TIFF
(for example on Debian/Ubuntu: `libfftw3-dev libjpeg-dev libpng-dev libtiff-dev`).

## Quick start

```r
library(LeafInjuryR)

res <- analyze_leaf(leaf_example_images()[2], crop = "auto")
res                # summary
plot(res)          # Original | Leaf mask | Classification | Overlay
res$metrics        # one-row data frame
res$quality        # "ok" / "warning" / "failed" + messages
analysis_report(res)

# Multiclass
analyze_leaf(leaf_example_images()[2], crop = "auto", multiclass = TRUE)$metrics

# Batch -> results.csv, masks/, overlays/
analyze_leaf_batch("folder_with_photos/", output_dir = "results", crop = "auto")

# Compare segmentation methods (agreement, not accuracy)
plot(compare_segmentation_methods(leaf_example_images()[2]))

# Graphical interface (local only)
run_leafinjury_app()
```

## Main functions

| Step | Function |
|---|---|
| Input | `read_leaf()`, `list_leaf_images()`, `leaf_example_images()` |
| Cropping | `auto_crop_leaf()`, `crop_leaf_manual()`, `uncrop_mask()` |
| Normalisation | `normalize_leaf_image()` |
| Segmentation | `segment_leaf()`, `clean_leaf_mask()` |
| Classification | `classify_leaf_tissue()` |
| Quantification | `calculate_injury()` |
| Full pipeline | `analyze_leaf()`, `analyze_leaf_batch()` |
| Quality control | `check_leaf_quality()` |
| Visualisation | `create_leaf_overlay()`, `plot()`, `class_map_to_image()` |
| Method comparison | `compare_segmentation_methods()`, `compare_tissue_methods()` |
| Validation | `evaluate_segmentation()`, `evaluate_leaf_analysis()`, `validate_ground_truth()` |
| Reproducibility | `leaf_config()`, `get_analysis_parameters()`, `analysis_report()` |
| Export | `export_leaf_analysis()`, `write_leaf_results()` |

All tunable parameters are centralised in `leaf_config()` with documented
defaults, and every result stores the configuration, package/R versions,
thresholds actually used and random seed.

## Methods (v0.1.0)

* Segmentation: `auto` (default; CIELAB chroma + darkness with Otsu thresholds,
  background polarity from the image border, k-means fallback), `lab`, `hsv`,
  `exg`, `otsu`, `adaptive` (illumination-surface corrected), `kmeans`.
* Tissue classification: `lab` (default; fixed CIELAB hue-angle thresholds),
  `hsv`, `exgr`, `vote`, `auto` (within-leaf Otsu, bounded).

Default thresholds are starting values, not validated constants.

## Photography recommendations

Uniform matte background with good contrast; diffuse light without hard
shadows or glare; constant camera, distance and lighting within a trial;
1-4 megapixels with the leaf filling 20-70 % of the frame.

## Privacy

All processing is local. There is no login, analytics, telemetry, external
API or automatic upload. Images never leave your computer.

## Documentation

`vignette("getting-started")`, `vignette("image-segmentation")`,
`vignette("batch-processing")`, `vignette("method-validation")`.

## Roadmap

**v0.1** - classical image processing; single analysis; batch analysis;
Shiny interface.

**v0.2** - improved multiclass segmentation; validation datasets;
calibration tools.

**v1.0** - validated stable API.

**Future** - machine learning; deep learning; mobile image acquisition;
scale/reference calibration.

## License

GPL (>= 3).
