# LeafInjuryR 0.2.0

## Breaking changes
* The public API is reduced to six functions: `analyze_leaf()`,
  `analyze_leaf_batch()`, `crop_leaf()`, `segment_leaf()`, `validate_leaf()` and
  `run_leafinjury_app()`. Former exported helpers are internal (still available
  as `LeafInjuryR:::name()`).
* `segment_leaf()` now returns the leaf mask **and** the tissue classification,
  and supports manual corrections (`mode = "manual"`).
* Parameter overrides use `params = list(<group> = list(...))`.

## New
* Completely redesigned Shiny interface with four areas (Imagem, Segmentação,
  Análise, Resultados), large image viewer (zoom, fit, full screen, optional
  grid), manual correction with brush or rectangle and live injury %,
  original × processed comparison slider, batch preview and validation.
* Batch reports: self-contained HTML, Excel (3 sheets) and CSV from the same table.
* `tissue_method = "relative"` for naturally pigmented or senescent leaves.
* Optional physical areas (cm²) with `pixels_per_cm`.
* Traceability: MD5 checksum of input files, platform, manual corrections.

# LeafInjuryR 0.1.0

* First version.
