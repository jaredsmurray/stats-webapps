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
  thaler/             # see "thaler" below
index.qmd             # site index
sap_sampling.qmd      # site page embedding the sampling app
sap_bootstrap.qmd     # site page embedding the bootstrap app
bagging.qmd           # site page embedding the bagging app
_extensions/quarto-ext/shinylive
```

Page URLs are `<site root>/sap_sampling.html`, `/sap_bootstrap.html`, and
`/bagging.html`. The book links these pages mid-prose, so the stems are stable —
do not rename them.

## Publishing

```
quarto publish gh-pages
```

Each app page declares `engine: knitr` in its front matter. Without it Quarto
sees only a `shinylive-r` cell, reaches for the Jupyter engine, and fails before
the shinylive filter ever runs.

## Deferred cleanup: duplicated app code

Each site page carries the app's code inline in a `shinylive-r` chunk, and
`apps/<name>/` carries the same code as a runnable Shiny app. The two copies are
kept in sync **by hand**. Generating the wrapper chunk from `app.R` at render
time is the intended fix and is deferred; until then, any change to an app has to
be made in both places.

Two related wrinkles that come with the duplication:

- The `sap_sampling` and `bagging` apps have base-graphics ports
  (`app_shinylive.R`) because the shinylive bundle can only include CRAN
  packages and this project's ggplot2 is a GitHub install. The ports stay as-is.
- Header comments inside the app files and chunks still cite the old monorepo
  paths (`webapps/bagging_app`, `bagging_app.qmd`, and so on). They were left
  byte-identical during the split; refresh them along with the dedup work.

## thaler

`apps/thaler/` is orphaned: no site page links to it and no chapter or deck
references it. It was carried over so nothing is lost, but it is **not** part of
the website and is not listed on the index. Keep-or-drop is still an open
decision; drop the directory if it turns out nothing needs it.
