# AutoPlotR

AI-assisted visualization for R. AutoPlotR profiles tabular data, plans an appropriate visualization, generates editable R code, renders the plot, and saves reproducible artifacts from a natural-language request.

[![R](https://img.shields.io/badge/R-%3E%3D%204.4-blue)](https://www.r-project.org/) [![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Overview

AutoPlotR is designed for data analysts and researchers who want a fast path from a dataset to a professional visualization. It uses an LLM-powered planner to choose a visualization strategy and a plotter agent to write runnable R code.

## Features

-   Natural-language plotting from a dataset or live chat session
-   Data profiling for column types, ranges, missing values, and sample values
-   Structured visualization planning before code generation
-   Editable `ggplot2` scripts and rendered PNG output
-   Live browser chat for file discovery, data loading, profiling, and plotting
-   Safety checks that block dangerous generated R calls before execution
-   Support for multiple LLM providers through [`ellmer`](https://ellmer.tidyverse.org/)

## Installation

Install from GitHub:

``` r
install.packages("remotes")
remotes::install_github("mehrdadameri/AutoPlotR", dependencies = TRUE)
```

Requirements:

-   R 4.4 or later
-   An API key for an LLM provider supported by `ellmer`
-   `shinychat` for browser-based live chat

## Setup

Run the setup helper once:

``` r
library(AutoPlotR)

ap_setup()
```

Or configure a provider programmatically:

``` r
ap_setup(
  provider = "openai",
  model = "gpt-4o-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  launch = FALSE
)
```

Use `ap_llm_providers()` to list provider IDs available through your installed `ellmer` version.

## Quick Start

Create a plot from a natural-language request:

``` r
library(AutoPlotR)

result <- ap_plot(
  mtcars,
  "Scatter plot of mpg versus wt, colored by cylinder count"
)

print(result)
```

AutoPlotR creates an output folder with the rendered plot, editable R script, data profile, visualization plan, diagnostics, and manifest.

You can also inspect the plan before rendering:

``` r
plan <- ap_plan(mtcars, "Boxplot of mpg by cylinder count")
print(plan)

result <- ap_plot(mtcars, plan = plan, output_dir = "plots")
```

## Live Chat

`ap_live()` starts a conversational plotting session. The agent can scan the working directory, load supported data files, detect data frames, profile columns, and generate plots.

``` r
library(AutoPlotR)

ap_live(root = "/path-to-your-working-directory")
```

Browser mode runs in a background R process so the R console remains available. If you create or load new data in RStudio while the chat is running, push it to the chat session with:

``` r
ap_push_env()
```

After the chat loads or modifies data, fetch it back into the R session with:

``` r
ap_fetch_data()
```

For a blocking terminal session:

``` r
ap_live(mtcars, launch_browser = FALSE)
```

## Supported Data Files

The live agent can load common tabular formats from the configured working directory:

-   CSV, TSV, and delimited text files
-   Excel workbooks, when `readxl` is installed
-   RDS and RData files

File access in live chat is restricted to the configured root directory.

## Output

A typical `ap_plot()` run creates:

``` text
autoplotr-output/
  autoplotr-plot.png
  autoplotr-plot.R
  data.rds
  profile.json
  plan.json
  diagnostics.json
  manifest.json
  trace.jsonl
```

The R script is intended to be readable and editable. The JSON files record the data profile, visualization plan, diagnostics, and reproducibility metadata.

## Safety

Generated plotting code is parsed before execution. AutoPlotR blocks calls that can modify the system, install packages, execute shell commands, delete files, change working directories, or run arbitrary code evaluation.

The safety checks reduce risk, but generated code should still be reviewed before use in sensitive environments.

## Configuration

AutoPlotR stores provider and model settings in a user configuration file created by `ap_setup()`. Visualization defaults can be customized with YAML rule files:

-   Package defaults: `inst/knowledge/visualization-rules.yml`
-   User overrides: `~/.config/AutoPlotR/visualization-rules.yml`
-   Project overrides: `./autoplotr-viz-rules.yml`

Project rules override user rules; user rules override package defaults.

## License

MIT © 2026 Mehrdad Ameri
