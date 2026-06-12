# AutoPlotR Visualization Design Guide

## Mission
AutoPlotR must generate scientifically honest, readable, reproducible, and publication-ready R visualizations. Every plot must be selected from the structure of the data and the analytical question, not from decoration or visual novelty.

The default plotting backend is `ggplot2`. Generated code must be plain, editable R code. The final script must create a `ggplot` object named `p` and save it with `ggplot2::ggsave()`.

## Agent operating checklist

* Choose the simplest plot that answers the user's analytical question.
* Use only columns present in the data profile, or create derived columns through explicit `transformations` assignments.
* Do not invent columns, units, thresholds, statistical tests, model fits, sample sizes, or group meanings.
* State assumptions in `design_rationale` when the request is underspecified but still has a valid standard interpretation.
* Ask concise clarification questions only when missing information prevents a valid plot or would change the scientific conclusion.
* Keep `package = "ggplot2"` unless the user explicitly requests another supported plotting backend.
* For computed variables, map aesthetics to the new left-hand-side column names created by `transformations`.
* The plotter must Validate required columns, create `p`, save with `ggplot2::ggsave()`, and avoid installation, deletion, network, shell, or global-state side effects.

## Non-negotiable principles

1. Match the plot to the task.

   * Relationship: scatter, line, contour, hex/bin2d.
   * Distribution: histogram, density, ECDF, boxplot, violin, raincloud-style point + interval.
   * Comparison: dot plot, bar plot, boxplot, point-range, slope chart.
   * Composition: stacked bar only when parts-of-whole are essential and categories are few.
   * Matrix or two categorical axes: heatmap/tile.
   * Time or ordered sequence: line, step, area only when accumulation is meaningful.
   * Spatial data: map only with valid spatial coordinates and projection.

2. Preserve data truth.

   * Never silently filter, aggregate, winsorize, normalize, log-transform, reorder, impute, or remove outliers.
   * Every transformation must be explicit in the plot plan and reflected in labels, caption, or code comments.
   * Use raw observations when feasible. When showing summaries, show uncertainty or sample size when relevant.
   * Do not use decorative 3D effects, exploded pies, fake gradients, pictograms, or visual effects that distort magnitude.
   * Do not truncate axes in a way that changes the perceived conclusion. Bar charts representing magnitude should normally start at zero.
   * Avoid dual y-axes unless explicitly requested and scientifically justified. Prefer faceting, indexing, normalization, or separate aligned panels.

3. Make decoding easy.

   * Prefer position on a common scale, aligned position, and length over area, volume, angle, or color-only encodings.
   * Use color to group, highlight, or encode a continuous value only when it improves interpretation.
   * Do not encode the main scientific result with color alone. Add labels, shapes, linetypes, facets, or direct annotations.
   * Use direct labels when they reduce legend lookup.
   * Keep legends concise, ordered, and close to the data.

4. Make plots accessible.

   * Use colorblind-safe palettes by default.
   * For continuous scales, prefer perceptually uniform palettes such as viridis/cividis/mako/inferno/plasma depending on context.
   * For diverging data with a meaningful midpoint, use a diverging palette centered at the scientifically meaningful value, usually zero, baseline, control, or null effect.
   * For unordered categories, use a qualitative palette with clearly separable colors. Avoid more than about 8–10 color-coded categories; use faceting or grouping instead.
   * Ensure all text is readable at export size.
   * Avoid excessive text rotation. Prefer horizontal labels, coord_flip, wrapping, or reordering.

5. Make output reproducible.

   * Use explicit package calls where useful: `ggplot2::ggplot()`, `ggplot2::ggsave()`.
   * Set output size and resolution explicitly.
   * Default static output: width = 7 in, height = 5 in, dpi = 300.
   * Save both a raster output for quick viewing and, when appropriate, a vector output for publication editing.
   * The script must run from a clean R session if the input data object/path exists.

## Data audit before plotting

Before choosing a plot, inspect:

* Variable types: numeric, integer, categorical, ordered factor, date/time, logical.
* Number of rows and missing values.
* Number of unique values per variable.
* Whether x is ordered, temporal, categorical, or continuous.
* Whether y is raw observation, count, proportion, summary statistic, model output, or transformed value.
* Grouping variables and number of groups.
* Units and measurement scale.
* Whether values can be negative, zero, bounded, compositional, circular, or log-scaled.
* Whether repeated measures, paired samples, nested data, or batch effects exist.

The plot plan must state:

* The user’s analytical question.
* Selected plot type.
* Required variables.
* Any aggregation or transformation.
* Encoding choices.
* Accessibility choices.
* Output filename and dimensions.

## Chart selection rules

### Scatter plot

Use when both x and y are numeric and the goal is association, correlation, clusters, outliers, or model fit.

Default:

* `geom_point()`
* Use `alpha` and smaller point size for overplotting.
* Use `geom_smooth()` only when trend estimation is requested or clearly helpful.
* Use `method = "lm"` only for linear trend questions; otherwise use LOESS/GAM carefully.
* For very large datasets, use `geom_hex()`, `geom_bin_2d()`, density contours, or sampling with a documented seed.
* Add marginal distributions only if they clarify the question.

Avoid:

* Connecting unordered points.
* Using bubble area for precise quantitative comparison.
* Using color for too many groups.
* Adding regression lines without saying what model was used.

### Line plot

Use when x is ordered, temporal, or sequential and y changes over x.

Default:

* Sort by x before plotting.
* Use `geom_line()` and optionally `geom_point()` when individual observations matter.
* Set `group` explicitly when multiple series exist.
* Use linetype or direct labels in addition to color when comparing series.
* Use ribbons or error bars for uncertainty if available.

Avoid:

* Connecting categories with no natural order.
* Using smoothed lines when raw temporal variation is important.
* Overplotting many lines; use facets, highlighting, or small multiples.

### Bar plot

Use for counts or aggregated categorical comparisons.

Default:

* Use `geom_bar()` for counts.
* Use `geom_col()` for precomputed values.
* Start magnitude bars at zero.
* Sort categories by value unless natural/order-specific ordering is required.
* Use horizontal bars for long category names.
* Label units and aggregation function clearly.

Avoid:

* Bar plots for raw distributions when boxplot/violin/jitter is more informative.
* Stacked bars with many categories.
* 3D bars.
* Error bars without defining the interval type.

### Dot plot / lollipop plot

Use instead of bars when comparing many categories or when zero baseline is not analytically central.

Default:

* Use points on a common numeric axis.
* Sort categories by value.
* Add reference lines for baseline, target, or control.
* Prefer this for ranking, model coefficients, fold changes, and effect sizes.

### Histogram

Use for one numeric distribution.

Default:

* Use `geom_histogram()`.
* Choose and specify `binwidth` or `bins`.
* Explore more than one binwidth during planning if distribution shape matters.
* Label y as count, density, or proportion.
* Use facets rather than overlapping many histograms.

Avoid:

* Defaulting blindly to 30 bins when the shape is important.
* Comparing many groups with opaque overlapping histograms.
* Hiding outliers by limiting the scale without annotation.

### Density plot

Use for smooth distribution comparison when sample size is adequate.

Default:

* Use `geom_density()`.
* Use alpha carefully if overlapping.
* Prefer facets or ridgelines for many groups.
* Make clear that density is a smoothed estimate.

Avoid:

* Density plots for very small n.
* Interpreting density peaks as exact observed values.
* Using density for bounded or discrete variables without care.

### ECDF plot

Use when comparing distributions without binning or smoothing.

Default:

* Use `stat_ecdf()`.
* Prefer for distribution comparison across groups when exact cumulative behavior matters.
* Good for skewed data, thresholds, percentiles, and survival-like interpretation.

### Boxplot

Use for comparing numeric distributions across groups.

Default:

* Use `geom_boxplot()`.
* Add jittered raw points when n is small or moderate.
* Show sample size per group when relevant.
* Order groups by median or known experimental design.
* Use notch only if the median comparison is meaningful and sample size supports it.

Avoid:

* Boxplots alone when n is tiny.
* Hiding multimodality; use violin, jitter, or histogram facets if shape matters.

### Violin plot

Use for comparing distribution shapes across groups.

Default:

* Use `geom_violin()` plus boxplot or median marker.
* Add jittered points if sample size allows.
* Use the same scale across groups.
* State if violin width is scaled by count or normalized.

Avoid:

* Violin plots for very small n.
* Overinterpreting smoothed shapes.

### Heatmap / tile plot

Use for a matrix: two categorical/ordered axes and numeric fill.

Default:

* Use `geom_tile()`.
* Use sequential palette for low-to-high values.
* Use diverging palette only with meaningful midpoint.
* Cluster/reorder rows or columns only if explicitly planned.
* Label missing values or encode them with a neutral missing-value color.
* Use `coord_equal()` when square cells matter.

Avoid:

* Rainbow palettes.
* Too many cell labels.
* Reordering without documenting the method.
* Using heatmaps when exact values are more important than pattern.

### Correlation heatmap

Use for pairwise correlations among numeric variables.

Default:

* Show correlation coefficient scale from -1 to 1.
* Use a diverging palette centered at 0.
* Cluster variables only if documented.
* Optionally mask upper/lower triangle.
* Avoid implying causality.

### Point-range / error-bar plot

Use for estimates with uncertainty.

Default:

* Use `geom_pointrange()`, `geom_errorbar()`, or `geom_linerange()`.
* Define interval type: SD, SE, CI, credible interval, IQR, min-max.
* Prefer point-range over bars with error bars.
* Include reference line for null effect when relevant.

Avoid:

* Error bars without interval definition.
* Mixing interval types.
* Showing only mean ± SE when raw distribution is important.

### Faceting / small multiples

Use when groups are too many for color or when comparing patterns across conditions.

Default:

* Use `facet_wrap()` for one grouping variable.
* Use `facet_grid()` for two structured grouping variables.
* Keep scales fixed for direct comparison.
* Use free scales only when pattern shape matters more than magnitude comparison, and make this explicit.

Avoid:

* Too many facets with unreadable panels.
* Free scales without warning.

## Scientific plot-specific rules

### Volcano plot

Use for differential expression or similar effect-size/significance results.

Required:

* x = effect size, usually `log2FoldChange`.
* y = `-log10(adjusted_p_value)` or equivalent.
* Use adjusted p-values when available.
* Add threshold lines only if thresholds are specified or standard for the analysis.
* Label only selected top features to avoid clutter.
* State thresholds in subtitle or caption.
* Use colorblind-safe colors and avoid red/green-only encoding.

### MA plot

Use for mean abundance versus fold-change.

Required:

* x = mean expression/abundance.
* y = log fold change.
* Add horizontal reference at 0.
* Highlight significant features only if adjusted p-values are available.
* Use alpha for dense points.

### PCA / dimension reduction plot

Use for sample-level clustering or batch/condition structure.

Required:

* x/y labels must include explained variance when available.
* Use point shape or outline in addition to color if groups matter.
* Do not overstate separation as classification unless validated.
* Use equal aspect ratio when appropriate.
* Label samples only when few enough to remain readable.

### Survival curve

Use for time-to-event data.

Required:

* Use step curves.
* Show risk table if possible.
* Include censoring marks when available.
* State model/test only if computed by the script or supplied.

### Time-series with uncertainty

Use for repeated measurements over time.

Required:

* One line per group or subject depending on the question.
* For group summaries, show uncertainty ribbon or interval.
* Do not connect observations across missing time gaps unless justified.
* Use date/time scales with readable breaks.

## Axis and scale rules

* Axis labels must include variable name and units when available.
* Titles should describe the relationship or comparison, not just list variables.
* Subtitles can describe filters, transformations, or grouping.
* Captions can record data source, transformation, sample size, or statistical interval.
* Use commas or SI-style labels for large numbers.
* Use percent labels only for proportions/fractions.
* Use log scales for multiplicative variation, heavy skew, or fold-change-style interpretation; label clearly.
* Never log-transform non-positive values without explicit handling.
* Use `coord_cartesian()` to zoom without dropping data from statistical calculations.
* Use `scale_*_continuous(limits = ...)` only when intentionally excluding data from the plot and document it.
* For bar charts, avoid truncated y-axis unless the plot is not representing magnitude from zero and the reason is explicit.
* Reverse axes only when domain convention requires it.

## Color rules

* Default continuous palette: viridis or cividis.
* Default categorical palette: colorblind-safe qualitative palette.
* Default diverging palette: blue-neutral-orange or another colorblind-conscious diverging palette centered on a meaningful midpoint.
* Never use rainbow/jet for scientific numeric data.
* Avoid red/green as the only distinction.
* Use neutral gray for context and stronger color only for focus/highlight.
* Do not use more colors than needed.
* Maintain consistent color mapping across related plots.
* Missing values must be visible or explicitly removed.

## Text, labels, and annotation

* Use concise, descriptive titles.
* Use sentence case unless project style requires otherwise.
* Prefer direct labels for a small number of series.
* Use `ggrepel` for crowded labels when available.
* Annotate thresholds, reference lines, important outliers, or experimental conditions.
* Do not annotate decorative commentary.
* Do not label every point unless the dataset is small and labels are readable.
* Keep font size readable at final export dimensions.

## Layout and theme

Default theme:

* Use `theme_minimal()` or `theme_classic()` depending on plot type.
* Use light gridlines only when they aid quantitative reading.
* Remove heavy borders, background fills, and unnecessary panel decorations.
* Keep aspect ratio appropriate to the analytical task.
* Use `coord_fixed()` when geometric distance or cell shape matters.
* Use consistent visual style across outputs.

Recommended base:

```r
base_theme <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title.position = "plot",
    legend.position = "right",
    panel.grid.minor = ggplot2::element_blank()
  )
```

## Code-generation standards

Every generated R script must:

1. Load required packages explicitly.
2. Validate required columns exist.
3. Validate variable types where possible.
4. Record transformations in comments.
5. Create a plot object named `p`.
6. Print `p` when running interactively.
7. Save with `ggplot2::ggsave()`.
8. Use explicit filename, width, height, units, and dpi.
9. Avoid hidden global state.
10. Avoid changing the input data object unless assigned to a new object.

Default save call:

```r
ggplot2::ggsave(
  filename = output_file,
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
```

For publication-ready vector output, also save PDF/SVG when requested:

```r
ggplot2::ggsave(
  filename = sub("\\.png$", ".pdf", output_file),
  plot = p,
  width = 7,
  height = 5,
  units = "in"
)
```

## Planner behavior

The planner must produce a concise plot plan before code generation:

* Goal: what question the plot answers.
* Data: variables used and their types.
* Plot: selected chart type and why.
* Encodings: x, y, color, fill, shape, size, linetype, facet.
* Transformations: aggregation, filtering, scaling, ordering, log transform.
* Accessibility: palette, labels, non-color redundancy.
* Output: file path, dimensions, dpi.

If the user request is ambiguous, choose the safest scientifically standard visualization and document the assumption in the plan. Do not ask follow-up questions unless the missing information prevents any valid plot.

## Validator rules

Hard failures:

* Required columns missing.
* Plot type incompatible with variable types.
* Invalid transformation, such as log scale with non-positive values and no handling.
* Bar chart with nonzero y-baseline unless explicitly justified.
* Silent filtering or aggregation.
* Missing `p` object.
* Missing `ggsave()`.
* Use of rainbow/jet palette for numeric scientific data.
* Use of 3D chart effects.
* Unlabeled axes.
* Output dimensions or dpi missing.

Warnings:

* Too many categories for color.
* Too many overlapping points without alpha/binning/faceting.
* Very small sample size for density/violin.
* Axis text rotation above 45 degrees.
* Legend too large.
* Facets too numerous.
* Unclear units.
* Missing uncertainty for summarized estimates.
* Possible misleading dual-axis request.

## Default decision table

* numeric x + numeric y: scatter.
* ordered/time x + numeric y: line.
* categorical x + numeric y raw observations: boxplot + jitter, or violin + boxplot.
* categorical x + numeric y aggregated values: dot plot or bar plot.
* one numeric variable: histogram; density if n is adequate.
* one categorical variable: bar count plot.
* two categorical variables + count/proportion: grouped, stacked, or tile depending on task.
* two categorical variables + numeric value: heatmap/tile.
* many numeric variables pairwise: correlation heatmap or pairs plot.
* model estimates + intervals: point-range forest plot.
* high-density numeric x/y: hexbin, bin2d, contour, or sampled scatter with documented seed.
* repeated measures over ordered x: line with group/subject structure, or summary line with uncertainty ribbon.

## Final quality checklist

Before returning code, verify:

* The plot answers the user’s question.
* The chart type matches the data structure.
* The labels are explicit and include units when available.
* The title describes the relationship or comparison.
* The palette is accessible.
* The main message is not encoded by color alone.
* Any aggregation/transformation is documented.
* Raw data are shown when scientifically useful.
* Uncertainty is shown for estimates when available.
* Overplotting is handled.
* Axes and scales are not misleading.
* The plot is reproducible from plain R code.
* The output is saved with explicit size and resolution.
