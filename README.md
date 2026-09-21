# Chloe Barker – Data Science Portfolio

This repository contains my data science portfolio: applied machine learning, healthcare analytics, decision-support apps, data engineering, geospatial analysis, and executive communication projects completed during and after my M.S. in Data Science.

## Technical Skills
- Python, R, SQL (MySQL)
- Statistical modeling & machine learning
- Data cleaning, feature engineering, and model evaluation
- Data visualization, decision support, and data storytelling
- Applied analytics across healthcare, HR, real estate, finance, and public-sector data

## Project Areas
- **Featured Work**
  - Award-winning capstone research and deployed decision-support applications

- **Predictive Modeling & Decision Systems**
  - Applied classification, regression, forecasting, thresholding, and business-value analysis

- **Data Engineering, SQL & Geospatial Systems**
  - Relational database design, ETL workflows, SQL querying, and interactive geospatial maps

- **Unsupervised Learning & Pattern Discovery**
  - Clustering, association-rule mining, validation metrics, and pattern interpretation

- **Executive Communication & Data Storytelling**
  - Visual narratives, product analyses, infographics, and presentations designed for decision-makers

## Preview and deployment checks

This is a static GitHub Pages site; it requires no build step or application server in production.

```sh
python3 -m http.server 8000 --bind 127.0.0.1
python3 scripts/check-site.py
```

Open `http://127.0.0.1:8000`. With Node.js, Playwright, and Chrome available, run `node scripts/check-browser.cjs` (set `NODE_PATH` if Playwright is installed outside the repository). The browser check covers all 19 portfolio pages at mobile, tablet, and desktop widths, local images, JavaScript errors, project search and filters, project navigation, sidebar scrolling, and map controls.

The homepage uses `style.css`; project pages use `assets/style.css` and `assets/project.css`. `assets/theme.css` loads last on every portfolio page and provides shared typography, accessible controls, dark surfaces, green actions, responsive media, and project header colors. Generated map and analysis HTML files retain their own visualization styles.

Before publishing, run both checks and merge the reviewed `spotify-redesign` changes into the branch configured as the GitHub Pages source. Verify the published homepage, one nested case study, PDF downloads, and external demo apps. External services (Hugging Face, Shiny, video, fonts, and icon CDNs) have separate availability and are not guaranteed by the local checks.
