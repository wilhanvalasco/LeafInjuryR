# Static documentation tab

mod_documentation_ui <- function(id) {
  bslib::card(
    bslib::card_header("How LeafInjuryR works"),
    tags$div(style = "max-width: 900px;",
      tags$h5("Pipeline"),
      tags$pre("original image -> (crop) -> (normalise) -> leaf segmentation -> leaf mask\n",
               "  -> pixels INSIDE the leaf mask -> tissue classification -> quantification -> QC"),
      tags$p(tags$b("Leaf segmentation"), " decides which pixels belong to the leaf (including ",
             "necrotic and chlorotic tissue). ", tags$b("Tissue classification"),
             " decides which leaf pixels look healthy or injured. Background pixels never ",
             "enter the injury percentage: injured (%) = injured pixels / leaf pixels x 100."),
      tags$h5("Classes"),
      tags$p("Binary: healthy / injured. Multiclass: healthy / chlorotic / necrotic / other, ",
             "with injured = chlorotic + necrotic + other."),
      tags$h5("Quality flags"),
      tags$p("'ok', 'warning' (inspect the result; values are not changed) or 'failed' ",
             "(no valid result). The diagnostic score is a heuristic, not a probability."),
      tags$h5("Scientific scope"),
      tags$p("The software quantifies visual colour patterns. Similar colours can have ",
             "different biological causes; it does not diagnose pathogens, diseases, ",
             "nutritional deficiencies or phytotoxicity. Working software is not a validated ",
             "method: validate thresholds against manually drawn reference masks for your ",
             "species, camera and lighting (see vignette('method-validation'))."),
      tags$h5("Privacy"),
      tags$p("Everything runs locally in your R session. No login, no telemetry, no external ",
             "services; images never leave your computer."),
      tags$h5("Photography tips"),
      tags$ul(tags$li("Uniform, contrasting background (white or matte)."),
              tags$li("Diffuse light; avoid hard shadows and glare."),
              tags$li("Keep camera, distance and lighting constant across a trial."),
              tags$li("1-4 megapixels with the leaf filling 20-70% of the frame is sufficient."))
    )
  )
}
