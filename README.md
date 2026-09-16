# ShinyXRF

**ShinyXRF** application allow exploration, transformation, and visualization of data from XRF (or other geochemical) analyses.

The application allows, in particular, to prepare data, select samples, produce CLR/ALR/ILR biplots, generate 2D plots, create ternary diagrams, and export results in CSV, PDF, or PNG formats.

---

## Table of Contents

* [Features](#features)
* [Prerequisites](#prerequisites)
* [Launching the Application](#launching-the-application)
* [Input File Format](#input-file-format)
* [Usage](#usage)
* [Available Exports](#available-exports)
* [Data Preparation Tips](#data-preparation-tips)
* [Troubleshooting](#troubleshooting)
* [Contributions](#contributions)

---

## Features

### Data Import and Preparation

* Import an Excel file in `.xlsx` or `.xls` format.
* Define information columns, for example, `1:3`.
* Select a sample column and a group column.
* Replace values less than or equal to zero with a substitution value linked to the detection limit.
* Calculate compositional transformations:
  * CLR;
  * ALR;
  * ILR.

### Visualizations

The application contains several tabs:

* **Summary**: Overview of the data, global information, and selection of samples to display, by group, by values.
* **CLR Biplot**: PCA biplot from CLR-transformed data.
* **ALR Biplot**: PCA biplot from ALR-transformed data.
* **ILR Biplot**: PCA biplot from ILR-transformed data.
* **Boxplots**: Boxplots for groups and for selected elements.
* **Correlation matrix**: Interactive correlation matrix of the elements.
* **2D Plot**: Interactive plot between two variables or two variable ratios.
* **Ternary plot (isopleuros)**: Ternary diagram based on three elements with the isopleuro package.
* **Ternary plot (ggtern)**: Customizable ternary diagram, with the possibility to use ratios, with the ggtern package.
* **Exports**: Download prepared and transformed tables.

### Graph Customization

* Display groups by colors or symbols.
* Choose color palettes.
* Choose shape palettes.
* Export figures in vector PDF or PNG.
* Adjust width, height, and DPI of exports.

---

## Prerequisites

Before launching the application, you need:

* R;
* RStudio (recommended but not mandatory);
* The R packages listed below.

Required packages:

```r
required_packages <- c(
  "shiny",
  "readxl",
  "nexus",
  "dimensio",
  "isopleuros",
  "ggplot2",
  "plotly",
  "ggtern",
  "grid",
  "DT"
)
```

---

## Launching the Application

### From RStudio

1. Open the `ShinyXRF.R` file.
2. Click **Run App**.
3. Load an Excel file from the interface.

### From the R Console

From the directory containing `ShinyXRF.R`:

```r
shiny::runApp()
```

---

## Input File Format

The input file must be an Excel file in `.xlsx` or `.xls` format.

### Expected Structure

The application assumes the file contains:

1. Information columns;
2. A column identifying the samples;
3. A column indicating the group;
4. Numeric columns for chemical, granulometric, or magnetic variables.

Simplified example:

| Sample | Groupe | Profondeur | Fe | Mn | Ca | P | 
|--------|--------|-------------|----|----|----|---|
| ECH_001 | A | 1 | 1200 | 32 | 540 | 90 |
| ECH_002 | B | 2 | 980 | 28 | 610 | 75 |

### Information Columns

The **Information Columns** field allows you to specify which columns should not be treated as chemical variables.

Accepted examples:

```text
1:3
```

```text
1,2,5
```

```text
1:3,7
```

All other columns are considered numeric variables to be prepared for compositional processing.

---

## Usage

### 1. Load the Excel File

In the sidebar, use the **Input Excel File** field to import the data.

### 2. Define Global Parameters

Available parameters:

* **Information Columns**: Descriptive columns to exclude from chemical variables.
* **LOD Factor**: Factor used to replace values less than or equal to zero.
* **ALR Denominator**: Element to be used as the denominator for the ALR transformation.
* **Group Display**: Colors or symbols.
* **Palette**: Color or symbol palette.

### 3. Choose Main Columns

Once the file is loaded, select:

* The sample column;
* The group column.

These columns are used for identification, tooltips, legends, and graphical groupings.

### 4. Select Samples

In the **Summary** tab, all samples are selected by default.

You can:

* Select or deselect rows by clicking on it;
* Use **Select All** or **Deselect All**;
* Filter for groups;
* Filter for a range of values for an element.

Plots only use the selected samples.

### 5. Produce a CLR Biplot

In the **CLR Biplot** tab:

1. Choose the elements to include;
2. Choose the PCA axes to display;
3. View the interactive plot;
4. Optionally enable `dimensio` plots.

It is recommended to select at least three elements.

### 6. Produce a ALR Biplot

In the Parameters column, select the element which will be used as denominator

In the **ALR Biplot** tab:

1. Choose the elements to include;
2. Choose the PCA axes to display;
3. View the interactive plot;

It is recommended to select at least three elements.

### 7. Produce a ILR Biplot

In the **ILR Biplot** tab:

1. Choose the elements to include;
2. Choose the PCA axes to display;
3. View the interactive plot;

It is recommended to select at least three elements.

### 8. Produce boxplots

In the **Boxplots** tab:

1. Choose the elements to include;
2. View the interactive plot;
3. Optionally enable indivudal points to be displayed.

### 9. Produce correlation matrix

In the **Correlation matrix** tab:

1. Choose the elements to include;
2. Choose the correlation method (Pearson, Spearman, Kendall);
3. View the interactive plot;

### 10. Produce a 2D Plot

In the **2D Plot** tab, choose:

* A variable, a sum of variables, or a ratio for the X-axis;
* A variable, a sum of variables, or a ratio for the Y-axis;
* The logarithmic axes option if necessary;
* The option to connect points;
* Axis scales.

Ratios are constructed as:

```text
numerator / denominator
```

### 11. Produce an Isopleuros Ternary Diagram

In the **Ternary plot (isopleuros)** tab, select *exactly* three elements.

The diagram is built from the three selected variables and colored or symbolized according to the group.

### 8. Produce a ggtern Ternary Diagram

In the **ggtern Ternary** tab, define the three vertices of the triangle.

Each vertex can be:

* A single element;
* An element/element ratio;
* An element/number ratio.

---

## Available Exports

### Graphic Exports

For each plot, the application offers:

* A **vector PDF** export;
* A **PNG** export.

Before exporting, you can adjust:

* The width in inches;
* The height in inches;
* The DPI resolution for PNGs.

### Table Exports

In the **Exports** tab, the following files are available:

| File | Content |
|------|---------|
| `prepared_data.csv` | Data after preparation and LOD value replacement |
| `clr.csv` | CLR-transformed data |
| `alr.csv` | ALR-transformed data |

---

## Data Preparation Tips

To avoid errors during launch or calculations:

* Ensure numeric columns do not contain text;
* Avoid merged cells in Excel;
* Use a first row containing only column names;
* Avoid empty column names;
* Ensure the group column does not contain only missing values;
* Ensure texture columns are expressed in consistent units;
* Avoid denominators equal to zero in ratios.

---

## Troubleshooting

### Message: `Missing R Packages`

Install the packages indicated in the error message:

```r
install.packages(c("package_name"))
```

Then restart the application.

### Message: `Missing Column(s)`

The name of a selected column does not exist or was modified during import.

Possible solutions:

* Check the spelling of the column name;
* Ensure the Excel file contains the expected column;
* Reload the file;
* Reselect the columns in the interface.

### Message: `Select at Least One Sample`

In the **Summary** tab, select at least one row from the table.

### Message: `The Two CLR Axes Must Be Different`

In the **CLR Biplot** tab, choose two different PCA axes, for example:

```text
X Axis = 1
Y Axis = 2
```

### Message: `ALR Index Must Be <= ...`

The ALR denominator index exceeds the number of available chemical variables.

Solution:

* Check the information columns;
* Ensure chemical variables are located outside the information columns.

### The Ternary Diagram Does Not Display

Check that:

* Exactly three elements are selected;
* The three columns are numeric;
* The values are not all missing;
* Groups are specified.


## Contributions
* Alain Queffelec coded initial scripts for CLR (thanks to some plotting functions by Julien Le Guirriec) and ternary diagrams;
* Morgann Pauvert created an initial version of the Shiny app based on these scripts;
* Alain Queffelec refined, extended, and translated the Shiny app.
