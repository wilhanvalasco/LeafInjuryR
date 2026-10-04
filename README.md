# LeafInjuryR

Quantification of **visual leaf injury** from digital photographs of detached
leaves, with classical and auditable image processing in R and a local,
minimalist Shiny interface for people who do not program.

```
import -> crop -> segment leaf -> leaf mask -> classify tissue INSIDE the leaf
       -> (manual correction) -> quantify -> quality control -> reports
```

**Injury (%) = injured pixels / leaf-mask pixels x 100.** The background never
enters the calculation. Areas are in pixels; cm² are reported only when a valid
scale (`pixels_per_cm`) is supplied.

> **Scope.** LeafInjuryR quantifies visual colour patterns. It does **not**
> diagnose pathogens, diseases, nutritional deficiencies or phytotoxicity.
> Working software is **not** a validated method: calibrate and validate against
> manual reference masks for your species, camera and lighting.

## Installation

```r
install.packages("BiocManager")
BiocManager::install("wilhanvalasco/LeafInjuryR")   # also installs EBImage (Bioconductor)

# graphical interface
install.packages(c("shiny", "bslib", "DT"))
```

From a local copy (e.g. after unzipping): open `LeafInjuryR.Rproj` in RStudio and
run `devtools::install()`.

## Start the interface

```r
library(LeafInjuryR)
run_leafinjury_app()
```

Four areas, with the photograph as the dominant element (zoom, fit to window,
full screen, optional grid):

| Area | What you do |
|---|---|
| **Imagem** | import a photograph, automatic or manual crop (drag with the mouse), confirm or reset |
| **Segmentação** | automatic segmentation, manual correction with brush or rectangle (live injury %), mask/classification/overlay views, advanced settings |
| **Análise** | single or batch analysis, progress, indicators, original × processed comparison slider |
| **Resultados** | results table, comparison between images, validation, export HTML / Excel / CSV / ZIP |

## The six functions

| Function | Purpose |
|---|---|
| `analyze_leaf()` | single-image analysis (areas, injury %, chlorotic/necrotic when enabled, masks, overlay) |
| `analyze_leaf_batch()` | many images with the same settings; per-image error handling; HTML + Excel + CSV reports |
| `crop_leaf()` | automatic (leaf bounding box + margin) or manual crop, without resampling |
| `segment_leaf()` | automatic segmentation and tissue classification, or manual correction |
| `validate_leaf()` | quality control; IoU, Dice, sensitivity, specificity, precision, F1 only when reference masks exist |
| `run_leafinjury_app()` | local Shiny interface |

## Examples

```r
library(LeafInjuryR)
f <- system.file("extdata", "leaf_example_02.jpg", package = "LeafInjuryR")

# single image
res <- analyze_leaf(f, crop = "auto", multiclass = TRUE)
res$metrics[, c("leaf_pixels", "injured_percent", "chlorotic_percent", "necrotic_percent")]
plot(res)
validate_leaf(res)

# manual correction (brush stroke = circle, or rectangle)
img <- crop_leaf(f)
seg <- segment_leaf(img)
fix <- data.frame(action = "healthy", shape = "rect", xmin = 40, xmax = 140, ymin = 220, ymax = 300)
seg2 <- segment_leaf(img, mode = "manual", corrections = fix, segmentation = seg)
analyze_leaf(img, segmentation = seg2)

# batch: writes report.html, results.xlsx, results.csv, masks/, overlays/
analyze_leaf_batch("folder_with_photos/", output_dir = "results", crop = "auto")

# validation against manual reference masks (PNG, size of the original photo)
validate_leaf(res, reference_leaf_mask = "leaf_mask.png", reference_injury_mask = "injury_mask.png")
```

## Reports

Batch analyses (and single analyses exported from the app) produce three files
from the same results table:

* **report.html** – self-contained (opens offline): executive summary,
  statistics (mean, median, min, max, SD, CV, IQR), comparative chart, sortable
  table, gallery of original and processed images with the injury % of each
  photograph, parameters and MD5 checksums of the input files;
* **results.xlsx** – sheets *Resultados*, *Resumo*, *Parametros*;
* **results.csv** – one row per image, one column per variable.

## Methods (v0.2.0)

* Leaf segmentation: `auto` (default; Otsu on CIELAB chroma + darkness, background
  polarity from the image border, k-means fallback), `lab`, `hsv`, `exg`, `otsu`,
  `adaptive` (illumination-surface corrected), `kmeans`; morphology, connected
  components and background-aware hole filling.
* Tissue classification: `lab` (default; fixed thresholds, comparable across
  treatments), `relative` (distance to the leaf's own colour, for pigmented or
  senescent leaves), `hsv`, `exgr`, `vote`, `auto`.

All parameters have documented defaults (`?analyze_leaf`, section *Parameters*);
every result stores package version, date, methods, thresholds, manual
corrections and the full configuration. Defaults are starting values, not
validated constants.

## Privacy

Everything runs locally. No login, analytics, telemetry, external API or upload.

## Roadmap

**v0.1** classical image processing; single and batch analysis; Shiny interface.
**v0.2** six-function API; redesigned interface; manual correction; HTML/Excel/CSV reports.
**v0.3** validation datasets; calibration tools.
**v1.0** validated stable API.
**Future** machine learning; mobile acquisition; scale/reference calibration.

## License

GPL (>= 3).
