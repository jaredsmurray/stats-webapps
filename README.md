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

Only sampling and Thaler are currently approved for the website. Bootstrap and
bagging are held for functionality and copy review. The render list in
`_quarto.yml` contains only the approved pages. Preview held apps outside
`_site/` so a subsequent publication cannot include those previews.

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

Bootstrap and bagging still carry inline app code in their pages. Changes to
those apps must also update their corresponding page chunks.

Run the app regression checks from the repository root:

```bash
Rscript tests/app-regression.R
```

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
