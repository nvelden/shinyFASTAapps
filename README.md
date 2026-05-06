# Shiny FASTA Apps

This repository contains browser-based Shinylive exports of four FASTA tools. Each app runs entirely in the browser and can be embedded in another website with an iframe.

On the first visit, each app can take up to 1-2 minutes to load while the browser downloads and caches the Shinylive/webR runtime and R packages. Later visits are usually faster.

## Shinylive Apps

- Dashboard: `https://nvelden.github.io/shinyFASTAapps/`
- Merge FASTA: `https://nvelden.github.io/shinyFASTAapps/merge_FASTA/`
- FASTA to CSV: `https://nvelden.github.io/shinyFASTAapps/FASTAtoCSV/`
- Filter FASTA: `https://nvelden.github.io/shinyFASTAapps/filterFASTA/`
- FASTA Parameters: `https://nvelden.github.io/shinyFASTAapps/paramFASTA/`

The dashboard links open each app in a simple styled iframe shell. Direct app URLs are still available for embedding elsewhere.

## Embed

Use the app URLs directly in iframes:

```html
<iframe
  src="https://nvelden.github.io/shinyFASTAapps/merge_FASTA/"
  title="Merge FASTA"
  style="width: 100%; height: 700px; border: 0;"
></iframe>
```

The apps include the existing iframe-resizer content script from each app's `www/` directory for hosts that use iframe-resizer.

## Local Development

Run an app locally with Shiny:

```r
shiny::runApp("merge_FASTA")
```

Export all apps to `docs/`:

```r
apps <- c("merge_FASTA", "FASTAtoCSV", "filterFASTA", "paramFASTA")
unlink("docs", recursive = TRUE, force = TRUE)
dir.create("docs")

for (app in apps) {
  stage <- file.path(".shinylive-export", app)
  unlink(stage, recursive = TRUE, force = TRUE)
  dir.create(stage, recursive = TRUE)
  file.copy(file.path(app, "app.R"), stage)
  file.copy(file.path(app, "R"), stage, recursive = TRUE)
  if (dir.exists(file.path(app, "www"))) {
    file.copy(file.path(app, "www"), stage, recursive = TRUE)
  }
  shinylive::export(stage, file.path("docs", app))
}
```
