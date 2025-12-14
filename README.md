# Co-inertia Analysis Project

A presentation and tutorial on co-inertia analysis in ecology, demonstrating how to analyze relationships between two data tables sharing the same individuals.

## 📊 View the Presentation

**[View the presentation online](https://antoinelucasfra.github.io/co-inertia_project/)**

## Authors

Antoine Lucas, Chloé Tellier, Julien Petot

## Project Structure

```
co-inertia_project/
├── presentation.qmd           # Quarto presentation (revealjs)
├── scripts/
│   └── coinertia_analysis.R   # Standalone R script with complete analysis
├── .github/
│   └── workflows/
│       └── publish.yml        # GitHub Actions for auto-deployment
├── README.md
└── co_inertia_project.Rproj
```

## Requirements

```r
install.packages(c("ade4", "adegraphics"))
```

You also need [Quarto](https://quarto.org/docs/get-started/) installed.

## Generating the Presentation Locally

```bash
quarto render presentation.qmd
```

Or in R:
```r
quarto::quarto_render("presentation.qmd")
```

The output is a **self-contained HTML file** (`presentation.html`) with no external dependencies.

## Deployment

The presentation is automatically deployed to GitHub Pages when you push to the `main` branch.

### Manual Setup (one-time)

1. Go to your repository on GitHub
2. Navigate to **Settings** → **Pages**
3. Under "Build and deployment", select **GitHub Actions** as the source
4. Push to `main` branch - the workflow will automatically build and deploy

## Running the Analysis

The `scripts/coinertia_analysis.R` file contains the complete analysis code that can be run independently.

## Built With

- [Quarto](https://quarto.org/) - Scientific publishing system
- [ade4](https://cran.r-project.org/package=ade4) - Multivariate data analysis
