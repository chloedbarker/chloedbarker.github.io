# Redesign content and branch review

Compared the working `spotify-redesign` site with fetched `origin/main` at `6f5c2fd` on September 20, 2026. The local `main` branch was behind that remote reference. No branch switch, merge, commit, push, or deployment was performed.

## Main-branch comparison

The four commits unique to current main added the LLY realization chart, selected it as the forecasting thumbnail, and refined the desktop sidebar. No new prose changes appeared in those commits. Imported the chart and thumbnail selection, and adapted compact laptop sidebar spacing to the redesign. Retained the redesign's full-width content, accessible sidebar scrolling, compressed images, metadata, navigation, and user-approved styling.

## Content corrections

- Capstone: removed claims that a homogeneity filter proved or rescued a biological signal. The source paper's table reports full-sample sodium-adjusted R² = 0.0823 and subset R² = 0.2257; these use different samples. The subset lithium coefficient has p = 0.7296. Negative coefficients are described as negative, not protective.
- Heart disease: described cross-sectional status classification, not future clinical risk prediction. Identified the AUROC gain from 0.841 to 0.845 as small and not demonstrated statistically meaningful. Corrected the claim that sensitivity was low for every model.
- Hospital stay: distinguished hospital-level associations from patient-level intervention effects. Acknowledged that BIC favors simple MLR while the reported prediction errors and AIC favor complex MLR.
- Housing: corrected “roughly halved” error to approximately 35% (0.229 to 0.150 RMSLE), identified adjusted R² and log-price scale, and qualified diagnostic and interaction interpretations.
- Attrition: identified the Frito-Lay analysis as a course case study and cost inputs as assumptions. Removed implications of measured savings or proven retention effectiveness.
- Diabetes: distinguished a deployed public-data prototype from validated clinical use. Qualified calibration, RAG citations, intervention benefit, and modeled net value.
- Clustering: distinguished weak evidence under tested settings from proof that no clusters exist. Corrected increasing minimum support from 2% to 3%, the K-Means ARI label, and the interpretation of association rules.
- Crab age: removed unsupported non-invasive age-assessment claims, since the predictors include shucked and viscera weights. Limited metrics to dataset-label evaluation.
- Forecasting: limited model comparisons to the reported horizon and target scale; removed unsupported causal explanations of performance.
- Database: replaced absolute claims about unqueryable data, perfect integrity, and eliminating all manual effort with concrete descriptions of integration and checks.
- Storytelling: corrected Delphi training versus external validation, Butterfly's cardiac/lung-exam denominator, and nutrition outcome definitions. Removed unsupported Butterfly price and training-data counts. Distinguished proposed product capabilities from implemented functionality.

## Evidence consulted

Local project PDFs, the capstone model table, project code and README material, the resume, and the following primary sources:

- [Delphi-2M paper](https://www.nature.com/articles/s41586-025-09529-3): approximately 400,000 UK Biobank training participants; 1.9 million Danish individuals for external validation without retraining.
- [Butterfly's Hennepin EMS case study](https://www.butterflynetwork.com/hennepin-enhances-emergency-response): manufacturer-reported deployment and care-decision figures; 10% refers to cardiac and lung exams.
- [Huang et al. (2012)](https://pubmed.ncbi.nlm.nih.gov/22677895/): ischemic heart disease mortality, not general cardiovascular disease incidence.
- [Pan et al. (2012)](https://pubmed.ncbi.nlm.nih.gov/22412075/): cohort-based red-meat/mortality associations.
- [Satija et al. (2016)](https://pmc.ncbi.nlm.nih.gov/articles/4907448/): plant-based diet quality and type 2 diabetes associations.

## Remaining limits and items for owner review

This is a content consistency review, not an independent reproduction of all analyses or verification of personal history. Original PDF slides and reports were preserved; some retain stronger wording than the revised website.

- The resume lists a Gmail contact address; the website uses the existing iCloud address. Neither was changed because both may be intentional.
- The resume's diabetes bullet mentions 71,518 encounters, whereas the newer project documentation reports 101,766 raw encounters and 99,340 eligible encounters. The website follows the project documentation; the resume may describe an earlier version or subset and should be reconciled before sharing.
- The resume lists additional roles not featured on the homepage. The homepage was treated as a curated summary; those roles were not added without a design/content request.
- Third-party app uptime, clinical effectiveness, and realized business impact are not established by local site tests.

## Validation

Run `python3 scripts/check-site.py` and the Playwright check documented in README.md. The latter covers all 19 portfolio pages at four widths, local image loading, JavaScript errors, navigation, filtering/card sizing, non-interactive skills hover behavior, sidebar scrolling, and map controls.
