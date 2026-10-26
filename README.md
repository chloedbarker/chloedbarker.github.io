# Chloe Barker · Data Science Portfolio

[View portfolio](https://chloedbarker.github.io/) · [LinkedIn](https://www.linkedin.com/in/chloedbarker/) · [Resume](resume/Chloe_Barker_Resume.pdf)

I’m a **Data Scientist | DevOps Support Analyst at AAA Auto Club Enterprises** (October 2026–present) and an M.S. Data Science graduate from SMU. This portfolio brings together my academic and independent work in machine learning, decision support, data engineering, and communication.

## Selected work

| Project | Focus | Explore |
| --- | --- | --- |
| Groundwater lithium & dementia prevalence | County-level geospatial research, statistical modeling, sensitivity analysis; SMU Best Capstone Presentation | [Case study](https://chloedbarker.github.io/capstone/) · [Source](capstone/) |
| Diabetes care decision support | Readmission risk modeling, threshold analysis, and retrieval of supporting guidance | [Case study](https://chloedbarker.github.io/predictive-analytics/diabetes-readmission-project/) · [Source](predictive-analytics/diabetes-readmission-project/) |
| Texas water quality | Relational database design, SQL workflows, and interactive maps | [Case study](https://chloedbarker.github.io/databases-sql/water-quality-project/) · [Source](databases-sql/water-quality-project/) |

More work: [predictive analytics](predictive-analytics/) · [unsupervised learning](machine-learning/) · [data storytelling](data-storytelling/).

## Technical focus

Python, R, SQL, PySpark, scikit-learn, statistical modeling, feature engineering, model evaluation, geospatial analysis, and data visualization. Project pages explain the problem, approach, results, and limitations; reported modeled outcomes are not production business results.

## Repository guide

```text
index.html              Portfolio homepage
style.css               Homepage layout and components
assets/                 Shared styles, homepage JavaScript, images, and icons
capstone/               Award-winning capstone research
predictive-analytics/   Classification, regression, forecasting, and apps
databases-sql/          Database and geospatial projects
machine-learning/      Clustering and pattern discovery
data-storytelling/     Visual narratives and presentations
certifications/        Credentials and organization images
resume/                Resume and project overview PDFs
scripts/               Static-site and browser validation
```

The website is plain HTML, CSS, and JavaScript, hosted on GitHub Pages. No build step is required. Individual analyses and apps have their own dependencies; see their project folders before running them. The website checks do not rerun analytical models.

## Preview locally

From the repository root, with Python 3 installed:

```sh
python3 -m http.server 8000 --bind 127.0.0.1
```

Open [localhost:8000](http://127.0.0.1:8000). In another terminal, check local links, assets, fragment targets, image alt attributes, and page landmarks:

```sh
python3 scripts/check-site.py
```

For browser checks, use Node.js 20+, the `playwright` package, and Google Chrome. With the preview server running:

```sh
npm install --no-save --package-lock=false playwright
node scripts/check-browser.cjs
```

The browser check visits all 19 portfolio pages at four viewport widths and checks overflow, local images, JavaScript errors, search, filters, navigation, sidebar scrolling, and map controls. If Playwright is installed elsewhere, set `NODE_PATH` to its `node_modules` directory.

## Maintenance

- Update career details in `index.html` and keep the downloadable resume in sync.
- Homepage interactions live in `assets/portfolio.js`. Project pages use `assets/style.css` and `assets/project.css`; `assets/theme.css` supplies shared overrides. Generated maps and analysis exports retain their own styles.
- Run both checks before publishing. GitHub Actions also runs the static checks on pushes and pull requests.
- Publish through the branch configured in this repository’s GitHub Pages settings. After deployment, verify the homepage, a nested case study, PDF downloads, and external demos.
- Keep credentials in local environment files or ignored configuration files. The water-quality project includes an example configuration.

External demos, fonts, and icon services are independently hosted; local checks do not guarantee their availability. Source datasets and third-party assets retain their original terms.
