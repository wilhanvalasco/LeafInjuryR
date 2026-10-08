#' LeafInjuryR: Quantification of Visual Leaf Injury from Digital Images
#'
#' @description
#' `LeafInjuryR` provides a reproducible and auditable image-processing
#' workflow for quantifying visual leaf injury from digital images.
#' The package separates leaf segmentation from tissue classification,
#' allowing healthy and injured leaf areas to be identified and quantified
#' independently.
#'
#' @details
#' The package provides six main functions covering the complete image-analysis
#' workflow:
#'
#' \describe{
#'   \item{\code{analyze_leaf()}}{
#'     Performs the complete analysis of a single leaf image.
#'   }
#'   \item{\code{analyze_leaf_batch()}}{
#'     Processes multiple images and generates consolidated HTML, Excel,
#'     and CSV reports.
#'   }
#'   \item{\code{crop_leaf()}}{
#'     Provides automatic or manual image cropping prior to analysis.
#'   }
#'   \item{\code{segment_leaf()}}{
#'     Performs automatic leaf segmentation with optional manual correction.
#'   }
#'   \item{\code{validate_leaf()}}{
#'     Performs quality control and validation against manually generated
#'     reference masks.
#'   }
#'   \item{\code{run_leafinjury_app()}}{
#'     Launches the interactive local Shiny interface.
#'   }
#' }
#'
#' The analytical workflow is divided into two independent stages. First,
#' leaf segmentation identifies which pixels belong to the leaf. Second,
#' tissue classification determines which pixels within the leaf mask
#' represent healthy or injured tissue.
#'
#' Leaf injury is calculated as:
#'
#' \deqn{Injury (\%) = \frac{N_{injured}}{N_{leaf}} \times 100}
#'
#' where \eqn{N_{injured}} is the number of pixels classified as injured
#' tissue and \eqn{N_{leaf}} is the total number of pixels within the
#' segmented leaf area.
#'
#' @section Scope:
#' `LeafInjuryR` quantifies visible colour patterns associated with leaf
#' injury. The package does not diagnose pathogens, diseases, nutritional
#' deficiencies, physiological disorders, or phytotoxicity.
#'
#' Quantitative results should therefore be interpreted as measurements of
#' visible injury rather than as a diagnostic classification. For scientific
#' applications, segmentation and classification performance should be
#' calibrated and validated against manually annotated reference images
#' representative of the intended imaging conditions.
#'
#' @section Privacy:
#' All image processing is performed locally. The package does not transmit
#' images, analytical results, or user information to external services and
#' does not implement telemetry.
#'
#' @seealso
#' \code{\link{analyze_leaf}},
#' \code{\link{analyze_leaf_batch}},
#' \code{\link{crop_leaf}},
#' \code{\link{segment_leaf}},
#' \code{\link{validate_leaf}},
#' \code{\link{run_leafinjury_app}}
#'
#' @author
#' Wilhan Valasco dos Santos
#'
#' @name LeafInjuryR-package
#' @aliases LeafInjuryR
#' @keywords internal
"_PACKAGE"
