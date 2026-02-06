# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Data science portfolio site (GitHub Pages) for an M.S. in Data Science program. The landing page (`index.html`) is a static HTML page linking to self-contained project directories organized by domain. Deployed at chloedbarker.github.io.

## Repository Structure

- `index.html` — Portfolio landing page (vanilla HTML/CSS, no framework)
- `machine-learning/` — Crab age prediction (Shiny app), employee attrition (R Markdown), sale price prediction (R Markdown)
- `healthcare-analytics/` — Heart disease prediction, hospital length-of-stay prediction (both R Markdown)
- `databases-sql/` — Water quality project (Jupyter/Python + MySQL)
- `data-storytelling/` — Presentations and infographics (PowerPoint/PDF, no code)
- `capstone/` — Featured capstone project (placeholder)
- `statistical-modeling/` — Empty, not yet populated

## Languages & Key Libraries

**R (primary):** tidyverse, dplyr, ggplot2, plotly, caret, shiny, bslib, DT, naniar, olsrr, GGally, patchwork, gtsummary

**Python:** Used in `databases-sql/water-quality-project/` via Jupyter notebook

**SQL:** MySQL for the water quality database project

## Running Projects

**R Markdown files (.Rmd):** Render with `rmarkdown::render("path/to/file.Rmd")` or Knit in RStudio.

**Shiny app** (`machine-learning/crab-age-prediction/app.R`): Run with `shiny::runApp("machine-learning/crab-age-prediction")` from the repo root in R.

**Jupyter notebook** (`databases-sql/water-quality-project/Semester_Project_Code.ipynb`): Run with `jupyter notebook`.

## Architecture Notes

- Each project directory is fully self-contained with its own data files, code, and output (reports, presentations).
- The site has no build step — HTML files are served directly by GitHub Pages.
- `index.html` uses a CSS grid card layout linking to section index pages, which in turn link to individual projects.
- Database credentials are excluded via `.gitignore` (`db_config.json`); a `db_config.example.json` template exists in the water quality project.
- RStudio project file (`chloedbarker.github.io.Rproj`) uses 2-space indentation and UTF-8 encoding.
