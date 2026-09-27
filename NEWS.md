# LeafInjuryR 0.1.0

* First version: modular package replacing the former single-file Shiny app.
* Separate leaf segmentation, tissue classification and quantification.
* Fixes the denominator of the former algorithm (injury is now relative to
  leaf pixels, not to the whole image).
* Segmentation methods: auto, lab, hsv, exg, otsu, adaptive, kmeans.
* Tissue methods: lab (default), hsv, exgr, vote, auto; optional multiclass.
* Quality control, validation metrics, ground-truth support, batch processing,
  CSV/PNG export, method comparison and a local Shiny interface.
