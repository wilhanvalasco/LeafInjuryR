# Reports: HTML (self-contained), Excel (3 sheets) and CSV --------------------
# All three are generated from the same results table.

#' JPEG data URI of an image (down-scaled for reports and the Shiny viewer)
#' @noRd
image_data_uri <- function(image, max_dim = 560L, quality = 82L) {
  if (is.null(image)) return(NA_character_)
  img <- as_color_image(as_rgb_array(image))
  d <- dim(img)
  s <- min(1, max_dim / max(d[1:2]))
  if (s < 1) img <- EBImage::resize(img, w = max(1, round(d[1] * s)), h = max(1, round(d[2] * s)))
  f <- tempfile(fileext = ".jpg"); on.exit(unlink(f))
  EBImage::writeImage(img, f, type = "jpeg", quality = quality)
  paste0("data:image/jpeg;base64,", base64enc::base64encode(f))
}

#' Descriptive summary of a results table
#' @noRd
batch_summary <- function(results) {
  ok <- results$status == "ok"
  v <- if (!is.null(results$injured_percent)) results$injured_percent[ok] else numeric(0)
  v <- v[is.finite(v)]
  st <- function(f) if (length(v)) f(v) else NA_real_
  q <- if (length(v)) stats::quantile(v, c(0.25, 0.75), names = FALSE) else c(NA, NA)
  out <- data.frame(
    indicator = c("Images processed", "Analyses completed", "Analyses with error",
                  "Quality warnings", "Injury mean (%)", "Injury median (%)",
                  "Injury standard deviation (%)", "Injury coefficient of variation (%)",
                  "Injury minimum (%)", "Injury maximum (%)", "Injury first quartile (%)",
                  "Injury third quartile (%)", "Injury interquartile range (%)",
                  "Mean leaf area (pixels)"),
    value = c(nrow(results), sum(ok), sum(!ok),
              sum(results$quality_flag == "warning", na.rm = TRUE),
              st(mean), st(stats::median), if (length(v) > 1) stats::sd(v) else NA_real_,
              if (length(v) > 1 && mean(v) > 0) 100 * stats::sd(v) / mean(v) else NA_real_,
              st(min), st(max), q[1], q[2], q[2] - q[1],
              if (any(ok) && !is.null(results$leaf_pixels)) mean(results$leaf_pixels[ok]) else NA_real_),
    stringsAsFactors = FALSE)
  for (cl in c("chlorotic", "necrotic")) {
    col <- paste0(cl, "_percent")
    if (!is.null(results[[col]]) && any(ok)) {
      out <- rbind(out, data.frame(indicator = sprintf("%s mean (%%)", tools::toTitleCase(cl)),
                                   value = mean(results[[col]][ok], na.rm = TRUE)))
    }
  }
  out
}

#' Coerce a leaf_analysis to the batch structure used by the reports
#' @noRd
as_report_source <- function(x) {
  if (inherits(x, "leaf_batch")) return(x)
  if (!inherits(x, "leaf_analysis")) leaf_abort("Reports need a 'leaf_analysis' or 'leaf_batch'.")
  m <- x$metrics
  m$quality_messages <- paste(x$quality$quality_messages, collapse = " | ")
  m$status <- "ok"; m$error_message <- NA_character_
  m$output_stem <- tools::file_path_sans_ext(m$file %||% "leaf")
  if (is.na(m$output_stem)) m$output_stem <- "leaf"
  m$elapsed_sec <- x$timing$elapsed_sec
  results <- order_result_columns(m)
  thumbs <- list()
  thumbs[[m$output_stem]] <- list(
    original = if (!is.null(x$cropped)) image_data_uri(x$cropped) else NA_character_,
    processed = if (!is.null(x$overlay)) image_data_uri(x$overlay) else NA_character_)
  structure(list(results = results, summary = batch_summary(results),
                 parameters = x$parameters, settings = list(), thumbnails = thumbs,
                 files = m$file), class = "leaf_batch")
}

#' Write the reports of an analysis or batch
#' @return Named character vector of written files.
#' @noRd
write_leaf_reports <- function(x, dir, formats = c("html", "xlsx", "csv")) {
  b <- as_report_source(x)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- character(0)
  if ("csv" %in% formats) {
    files["csv"] <- file.path(dir, "results.csv")
    utils::write.csv(b$results, files["csv"], row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  if ("xlsx" %in% formats) {
    files["xlsx"] <- file.path(dir, "results.xlsx")
    writexl::write_xlsx(list(Resultados = b$results, Resumo = b$summary,
                             Parametros = report_parameters(b)), files["xlsx"])
  }
  if ("html" %in% formats) {
    files["html"] <- file.path(dir, "report.html")
    writeLines(report_html(b), files["html"], useBytes = TRUE)
  }
  files
}

#' Zip files (flat) into `zipfile`
#' @noRd
zip_files <- function(zipfile, files) {
  old <- setwd(dirname(files[1])); on.exit(setwd(old))
  ok <- tryCatch({
    suppressWarnings(utils::zip(zipfile, files = basename(files), flags = "-q"))
    file.exists(zipfile)
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) leaf_abort("ZIP file could not be created (no 'zip' utility found).")
  invisible(zipfile)
}

#' Parameter/traceability table
#' @noRd
report_parameters <- function(b) {
  p <- b$parameters
  trace <- data.frame(
    parameter = c("report_generated_at", "package_version", "R_version", "EBImage_version",
                  "platform", "images", "area_unit"),
    value = c(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), p$package_version %||% NA,
              p$R_version %||% NA, p$EBImage_version %||% NA, p$platform %||% NA,
              nrow(b$results), p$area_unit %||% "pixels"),
    stringsAsFactors = FALSE)
  rest <- p[setdiff(names(p), c("package_version", "R_version", "EBImage_version", "platform"))]
  out <- rbind(trace, flatten_list(rest))
  if (length(b$settings)) out <- rbind(out, flatten_list(list(settings = b$settings)))
  out
}

#' @noRd
html_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE)
}

#' @noRd
fmt_num <- function(x, d = 2) ifelse(is.na(x), "\u2014", formatC(as.numeric(x), format = "f", digits = d, big.mark = ","))

#' Horizontal bar chart (inline SVG) of injury per image
#' @noRd
svg_injury_bars <- function(files, values, flags) {
  ok <- is.finite(values)
  files <- files[ok]; values <- values[ok]; flags <- flags[ok]
  if (!length(values)) return("<p class='muted'>No completed analyses.</p>")
  o <- order(values, decreasing = TRUE); files <- files[o]; values <- values[o]; flags <- flags[o]
  rowh <- 26; lab <- 210; w <- 760; barw <- w - lab - 70
  h <- rowh * length(values) + 30
  mx <- max(5, ceiling(max(values) / 5) * 5)
  col <- ifelse(flags == "ok", "#4C9A6A", ifelse(flags == "warning", "#D9A441", "#B0B0B0"))
  ticks <- pretty(c(0, mx), 5); ticks <- ticks[ticks <= mx]
  g <- sprintf("<line x1='%.1f' y1='0' x2='%.1f' y2='%d' class='grid'/><text x='%.1f' y='%d' class='tick'>%s</text>",
               lab + ticks / mx * barw, lab + ticks / mx * barw, h - 22, lab + ticks / mx * barw, h - 6, ticks)
  bars <- sprintf(paste0("<g><title>%s: %.2f%%</title><text x='%d' y='%.1f' class='lbl'>%s</text>",
                         "<rect x='%d' y='%.1f' width='%.1f' height='%d' rx='3' fill='%s'/>",
                         "<text x='%.1f' y='%.1f' class='val'>%.1f%%</text></g>"),
                  html_escape(files), values, lab - 8, (seq_along(values) - 1) * rowh + 17,
                  html_escape(ifelse(nchar(files) > 28, paste0(substr(files, 1, 26), "\u2026"), files)),
                  lab, (seq_along(values) - 1) * rowh + 5, values / mx * barw, rowh - 10, col,
                  lab + values / mx * barw + 6, (seq_along(values) - 1) * rowh + 17, values)
  m <- mean(values)
  mean_line <- sprintf("<line x1='%.1f' y1='0' x2='%.1f' y2='%d' class='mean'/><text x='%.1f' y='%d' class='meanlbl'>mean %.1f%%</text>",
                       lab + m / mx * barw, lab + m / mx * barw, h - 22, lab + m / mx * barw + 4, 10, m)
  paste0("<svg viewBox='0 0 ", w, " ", h, "' class='chart' role='img' aria-label='Injury per image'>",
         paste(g, collapse = ""), paste(bars, collapse = ""), mean_line, "</svg>")
}

#' Build the self-contained HTML report
#' @noRd
report_html <- function(b) {
  r <- b$results; s <- b$summary; p <- b$parameters
  ok <- r$status == "ok"
  sv <- function(ind) s$value[s$indicator == ind]
  kpi <- function(label, value, cls = "") sprintf("<div class='kpi %s'><span>%s</span><b>%s</b></div>", cls, label, value)
  kpis <- paste0(
    kpi("Images", sv("Images processed")), kpi("Completed", sv("Analyses completed")),
    kpi("Errors", sv("Analyses with error"), if (sv("Analyses with error") > 0) "bad" else ""),
    kpi("Mean injury", paste0(fmt_num(sv("Injury mean (%)")), "%"), "accent"),
    kpi("Median injury", paste0(fmt_num(sv("Injury median (%)")), "%")))
  stats_tbl <- paste0("<table class='kv'>", paste(sprintf("<tr><td>%s</td><td>%s</td></tr>",
    html_escape(s$indicator), ifelse(grepl("Images|Analyses|warnings", s$indicator),
                                     fmt_num(s$value, 0), fmt_num(s$value))), collapse = ""), "</table>")
  cols <- intersect(c("file", "status", "leaf_pixels", "healthy_percent", "injured_percent",
                      "chlorotic_percent", "necrotic_percent", "other_percent", "leaf_area_cm2",
                      "injured_area_cm2", "quality_flag", "manual_corrections", "error_message"), names(r))
  num <- vapply(r[cols], is.numeric, logical(1))
  head_row <- paste(sprintf("<th data-col='%d' class='%s'>%s</th>", seq_along(cols) - 1,
                            ifelse(num, "num", ""), html_escape(gsub("_", " ", cols))), collapse = "")
  body <- vapply(seq_len(nrow(r)), function(i) {
    cells <- vapply(cols, function(cc) {
      v <- r[[cc]][i]
      if (is.numeric(v)) sprintf("<td class='num' data-v='%s'>%s</td>", ifelse(is.na(v), "", v),
                                 fmt_num(v, if (grepl("pixels|corrections", cc)) 0 else 2))
      else if (cc %in% c("quality_flag", "status")) sprintf("<td><span class='badge %s'>%s</span></td>", html_escape(v), html_escape(v))
      else sprintf("<td>%s</td>", html_escape(v))
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  gallery <- vapply(seq_len(nrow(r)), function(i) {
    th <- b$thumbnails[[r$output_stem[i]]]
    if (is.null(th) || r$status[i] != "ok") {
      return(sprintf("<figure class='card failed'><div class='ph'>%s</div><figcaption><b>%s</b><span class='badge failed'>failed</span></figcaption></figure>",
                     html_escape(r$error_message[i] %||% "error"), html_escape(r$file[i])))
    }
    sprintf(paste0("<figure class='card'><div class='pair'><img src='%s' alt='original' loading='lazy' onclick='zoom(this)'>",
                   "<img src='%s' alt='processed' loading='lazy' onclick='zoom(this)'></div>",
                   "<figcaption><b title='%s'>%s</b><span class='pct'>%s%%</span><span class='badge %s'>%s</span></figcaption></figure>"),
            th$original, th$processed, html_escape(r$file[i]), html_escape(r$file[i]),
            fmt_num(r$injured_percent[i]), html_escape(r$quality_flag[i]), html_escape(r$quality_flag[i]))
  }, character(1))
  params <- report_parameters(b)
  ptbl <- paste0("<table class='kv'>", paste(sprintf("<tr><td>%s</td><td>%s</td></tr>",
                 html_escape(params$parameter), html_escape(params$value)), collapse = ""), "</table>")
  trace <- if (!is.null(r$file_md5)) paste0("<table class='kv'>", paste(sprintf("<tr><td>%s</td><td><code>%s</code></td></tr>",
                 html_escape(r$file), html_escape(r$file_md5)), collapse = ""), "</table>") else ""
  msgs <- r$quality_messages %||% rep(NA, nrow(r))
  warn <- which(r$quality_flag == "warning" & !is.na(msgs) & nzchar(msgs))
  warn_html <- if (length(warn)) paste0("<ul class='warn'>", paste(sprintf("<li><b>%s</b>: %s</li>",
                 html_escape(r$file[warn]), html_escape(msgs[warn])), collapse = ""), "</ul>") else
                 "<p class='muted'>No quality warnings.</p>"
  paste0('<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
'<meta name="viewport" content="width=device-width, initial-scale=1">',
'<title>LeafInjuryR report</title><style>
:root{--bg:#F7F8F6;--card:#fff;--ink:#1F2A24;--muted:#6B756F;--line:#E3E7E4;--acc:#3E8E5E;--warn:#C98A1B;--bad:#C2410C}
*{box-sizing:border-box}body{margin:0;font:14px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",Inter,Roboto,Helvetica,Arial,sans-serif;background:var(--bg);color:var(--ink)}
header{background:#fff;border-bottom:1px solid var(--line);padding:22px 32px;position:sticky;top:0;z-index:5}
header h1{margin:0;font-size:20px;font-weight:600}header p{margin:2px 0 0;color:var(--muted);font-size:12.5px}
nav{display:flex;gap:18px;margin-top:10px;flex-wrap:wrap}nav a{color:var(--muted);text-decoration:none;font-size:13px}nav a:hover{color:var(--acc)}
main{max-width:1180px;margin:0 auto;padding:26px 24px 60px}section{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:22px 24px;margin-bottom:20px}
h2{font-size:15px;font-weight:600;margin:0 0 14px}.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px}
.kpi{border:1px solid var(--line);border-radius:10px;padding:12px 14px}.kpi span{display:block;color:var(--muted);font-size:12px}.kpi b{font-size:22px;font-weight:600}
.kpi.accent b{color:var(--acc)}.kpi.bad b{color:var(--bad)}.two{display:grid;grid-template-columns:2fr 1fr;gap:22px}@media(max-width:860px){.two{grid-template-columns:1fr}}
.chart{width:100%;height:auto}.chart .lbl{font-size:11px;text-anchor:end;fill:var(--ink)}.chart .val{font-size:11px;fill:var(--muted)}.chart .tick{font-size:10px;fill:var(--muted);text-anchor:middle}
.chart .grid{stroke:var(--line)}.chart .mean{stroke:var(--ink);stroke-dasharray:4 3}.chart .meanlbl{font-size:10px;fill:var(--ink)}
table{border-collapse:collapse;width:100%;font-size:12.5px}th,td{padding:7px 9px;border-bottom:1px solid var(--line);text-align:left;vertical-align:top}
th{position:sticky;top:0;background:#fff;cursor:pointer;font-weight:600;white-space:nowrap}th:hover{color:var(--acc)}td.num,th.num{text-align:right;font-variant-numeric:tabular-nums}
.scroll{overflow:auto;max-height:520px}.kv td:first-child{color:var(--muted);width:40%;word-break:break-word}.kv td{word-break:break-word}
.badge{display:inline-block;padding:1px 8px;border-radius:99px;font-size:11px;background:#EEF4F0;color:var(--acc)}.badge.warning{background:#FBF3E2;color:var(--warn)}.badge.failed{background:#FCEDE6;color:var(--bad)}
.gallery{display:grid;grid-template-columns:repeat(auto-fill,minmax(290px,1fr));gap:14px}.card{margin:0;border:1px solid var(--line);border-radius:10px;overflow:hidden;background:#fff}
.pair{display:grid;grid-template-columns:1fr 1fr;gap:2px;background:var(--line)}.pair img{width:100%;height:150px;object-fit:contain;background:#fafafa;cursor:zoom-in;display:block}
figcaption{display:flex;align-items:center;gap:8px;padding:8px 10px;font-size:12px}figcaption b{flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;font-weight:500}
.pct{font-weight:600;color:var(--acc)}.ph{height:150px;display:flex;align-items:center;justify-content:center;padding:10px;color:var(--bad);font-size:12px;text-align:center}
.muted{color:var(--muted)}.warn{margin:0;padding-left:18px;font-size:12.5px}details summary{cursor:pointer;color:var(--muted);margin-bottom:8px}
dialog{border:none;border-radius:12px;padding:0;max-width:94vw;max-height:94vh;background:#111}dialog img{max-width:94vw;max-height:90vh;display:block}dialog::backdrop{background:rgba(0,0,0,.6)}
.note{font-size:12px;color:var(--muted);margin-top:10px}code{font-size:11.5px}
</style></head><body>',
'<header><h1>LeafInjuryR &middot; analysis report</h1><p>Generated ', html_escape(format(Sys.time(), "%Y-%m-%d %H:%M")),
' &middot; LeafInjuryR ', html_escape(p$package_version %||% ""), ' &middot; areas in ', html_escape(p$area_unit %||% "pixels"), '</p>',
'<nav><a href="#summary">Summary</a><a href="#chart">Comparison</a><a href="#table">Results</a><a href="#gallery">Gallery</a><a href="#params">Parameters</a><a href="#trace">Traceability</a></nav></header><main>',
'<section id="summary"><h2>Executive summary</h2><div class="kpis">', kpis, '</div>',
'<p class="note">Injury (%) = injured pixels / leaf-mask pixels &times; 100. Results quantify visual colour patterns; they are not a diagnosis and are not validated unless compared with manual reference masks.</p></section>',
'<section id="chart"><h2>Injury per image</h2><div class="two"><div>', svg_injury_bars(r$file, r$injured_percent, r$quality_flag),
'</div><div>', stats_tbl, '</div></div><h2 style="margin-top:18px">Quality messages</h2>', warn_html, '</section>',
'<section id="table"><h2>Individual results</h2><div class="scroll"><table id="res"><thead><tr>', head_row, '</tr></thead><tbody>',
paste(body, collapse = ""), '</tbody></table></div><p class="note">Click a column header to sort.</p></section>',
'<section id="gallery"><h2>Gallery &mdash; original (left) and processed (right)</h2><div class="gallery">', paste(gallery, collapse = ""),
'</div><p class="note">Use the gallery to spot segmentation or classification errors. Click an image to enlarge.</p></section>',
'<section id="params"><h2>Parameters</h2><details><summary>Show all parameters used</summary>', ptbl, '</details></section>',
'<section id="trace"><h2>Traceability</h2><p class="muted">MD5 checksum of each input file (identifies the exact photograph analysed).</p>', trace, '</section>',
'</main><dialog id="dlg" onclick="this.close()"><img id="dlgimg" alt=""></dialog><script>
function zoom(el){var d=document.getElementById("dlg");document.getElementById("dlgimg").src=el.src;d.showModal();}
document.querySelectorAll("#res th").forEach(function(th){var asc=true;th.addEventListener("click",function(){
var tb=document.querySelector("#res tbody"),i=+th.dataset.col,rows=Array.from(tb.rows),num=th.classList.contains("num");
rows.sort(function(a,b){var x=a.cells[i],y=b.cells[i];var u=num?parseFloat(x.dataset.v):x.textContent,v=num?parseFloat(y.dataset.v):y.textContent;
if(num){u=isNaN(u)?-Infinity:u;v=isNaN(v)?-Infinity:v;return asc?u-v:v-u;}return asc?u.localeCompare(v):v.localeCompare(u);});
rows.forEach(function(r){tb.appendChild(r);});asc=!asc;});});
</script></body></html>')
}
