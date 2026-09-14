# stats-webapps

Public Shiny apps for *Introduction to Statistics and Data Science*, published as
a small Quarto website. Every app runs client-side through
[shinylive](https://quarto-ext.github.io/shinylive/), so the site is static and
needs no Shiny server.

## Layout

```
apps/                 # source Shiny apps (app.R, plus shinylive ports)
  sap_sampling/       # sampling distribution of the mean, SAP customer ROE
  sap_bootstrap/      # bootstrap resampling of the Nucleus sample
  bagging/            # averaging regression trees over bootstrap resamples
  thaler/             # independent projects and total portfolio payoffs
index.qmd             # site index
sap_sampling.qmd      # site page embedding the sampling app
sap_bootstrap.qmd     # site page embedding the bootstrap app
bagging.qmd           # site page embedding the bagging app
thaler.qmd            # site page embedding the Thaler app
_extensions/quarto-ext/shinylive
```

Page URLs are `<site root>/sap_sampling.html`, `/sap_bootstrap.html`,
`/bagging.html`, and `/thaler.html`. The book links these pages mid-prose, so the stems are stable —
do not rename them.

## Publishing

Only sampling and Thaler are approved for website publication. Bootstrap and
bagging remain unpublished and require separate release authorization. The
render list in `_quarto.yml` contains only the approved pages. Preview held
apps outside `_site/` so a subsequent publication cannot include those
previews.

```
quarto publish gh-pages
```

Each app page declares `engine: knitr` in its front matter. Without it Quarto
sees only a `shinylive-r` cell, reaches for the Jupyter engine, and fails before
the shinylive filter ever runs.

## Editing and testing

The sampling and Thaler pages read their app scripts during rendering. Edit
`apps/sap_sampling/app_shinylive.R` and `apps/thaler/app.R`; their QMD wrappers
contain no separate copy of the app code. The sampling app also has a local
ggplot variant in `apps/sap_sampling/app.R`. Keep its behavior and labels aligned
with the browser version.

The bootstrap page reads `apps/sap_bootstrap/app.R`, which is the maintained
browser and native base-graphics implementation. The bagging page reads
`apps/bagging/app_shinylive.R`; `apps/bagging/app.R` is its native ggplot2
variant for local inspection. The QMD files contain only the shinylive wrapper,
so app changes belong in those maintained R scripts.

Run the app regression checks from the repository root:

```bash
Rscript tests/app-regression.R
Rscript tests/held-app-regression.R
```

Build private previews of the held pages from a disposable project copy outside
the repository. Copy `_quarto.yml`, `_extensions/`, the two held QMDs, and the
maintained app scripts into `/absolute/path/to/disposable-project`, preserving
their `apps/...` paths. In that copy, change only the temporary `_quarto.yml`
render list to `sap_bootstrap.qmd` and `bagging.qmd` and set
`project.output-dir` to `preview`, then run:

```bash
cd /absolute/path/to/disposable-project
R_LIBS=/path/to/browser-library quarto render
```

The rendered files will be in `/absolute/path/to/disposable-project/preview/`.
The held pages remain outside the publication render list. Publishing either
one requires a separate release authorization after review.

## Browser build dependencies

Render Thaler using a CRAN build library containing ggplot2 3.5.2 and scales
1.3.0, matching the versions available in the current WebAssembly repository.
Newer desktop versions can omit dependencies required by these browser versions.
A GitHub development installation of ggplot2 cannot supply this browser bundle.
Use a separate library to preserve the desktop environment:

```bash
R_LIBS=/path/to/browser-library quarto render thaler.qmd
R_LIBS=/path/to/browser-library quarto render sap_sampling.qmd
```

The app pages keep the experiment assumptions and controls concise; the notes
provide the walkthroughs. Browser testing must cover startup, plots, bulk
simulation, invalid batch counts, display toggles, and history resets.
