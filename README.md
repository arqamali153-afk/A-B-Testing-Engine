# 📊 Bayesian A/B Testing Engine

**[Live Dashboard](https://arqamresearch.shinyapps.io/A_B_Engine/)** | **[Sample PDF Report](Executive_Report.pdf)**

A custom end-to-end A/B testing application built to bridge the gap between rigorous statistical theory and practical business decision-making. 

Unlike standard frequentist calculators that rely on confusing p-values and ignore underlying data skews, this engine utilizes Bayesian probability and automated risk detection to provide actionable, mathematically sound business verdicts.

## 🚀 Video Demonstration


https://github.com/user-attachments/assets/dd531855-8f8b-42f1-85a6-8d7ee5ad2399


## 🧠 Core Features
*   **Bayesian Probability Engine:** Calculates the exact probability of a variation winning using Beta distributions and Monte Carlo simulations.
*   **Simpson’s Paradox Detection:** Automatically analyzes daily cumulative traffic to flag severe allocation skews that could render the overall mathematical verdict misleading.
*   **Guardrail Metrics:** Tracks secondary negative events (e.g., increased error rates or refunds) to ensure a "winning" variation does not damage other business areas.
*   **Automated LaTeX Reporting:** Compiles the live mathematical verdict, data visualizations, and risk alerts into a clean, presentation-ready PDF via RMarkdown.

## 🛠️ Technology Stack
*   **Language:** R
*   **Framework:** Shiny
*   **Reporting:** RMarkdown, LaTeX
*   **Visualizations:** ggplot2, Plotly

## 📂 Repository Structure
*   `app.R`: The core Shiny application and UI/Server logic.
*   `report.Rmd`: The parameterized RMarkdown file used for PDF generation.
*   `portfolio_ab_data.csv`: A sample dataset featuring a built-in Simpson's Paradox scenario for testing purposes.
*   `Executive_Report.pdf`: A sample automated LaTeX report generated directly by the engine.
*   `your_video_filename.mp4`: A brief video demonstration showcasing the interactive features and Bayesian probability calculations.
