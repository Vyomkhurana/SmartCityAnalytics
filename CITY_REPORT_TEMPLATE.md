# City Report Template (For Teacher Submission)

Use this template to explain that your analysis and recommendations are based on real data.

## 1. City Context
- City Name:
- Country/State:
- Study Period (start/end dates):
- Why this city was selected:

## 2. Data Sources (Real)
- Traffic data source (URL + owner):
- Air quality data source (URL + owner):
- Energy data source (URL + owner):
- Weather data source (URL + owner):
- Data refresh frequency (hourly/daily/etc.):

## 3. Data Quality Notes
- Missing values handling:
- Outlier handling:
- Time alignment strategy:
- Known limitations of the datasets:

## 4. Model and Analysis Outputs
Reference generated outputs in `outputs/` and `outputs/plots/`:
- Correlation insights:
- Peak-hour traffic findings:
- AQI hotspot findings:
- Energy demand findings:
- Anomaly findings:
- Clustering/PCA findings:

## 5. City-Specific Recommendations
Write recommendations only supported by your real data findings.

Example structure:
- Recommendation 1:
  - Evidence file(s):
  - Expected impact:
  - Risk/limitation:
- Recommendation 2:
  - Evidence file(s):
  - Expected impact:
  - Risk/limitation:

## 6. Reproducibility Steps
1. Place real files in `data/raw/`.
2. Run `source("run_pipeline.R")` or `./run_project.ps1`.
3. Open dashboard and export visuals for report.

## 7. Academic Integrity Statement
State clearly:
- No synthetic data was used for final conclusions.
- All recommendations are based on listed real sources.
