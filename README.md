# Analysis of WTA & ATP Match Statistics (2020-2024)

## Project Overview

This project explores tennis match statistics from the ATP (Men's) and WTA (Women's) tours from 2020 to 2024. The primary goal is to use unsupervised learning (PCA and K-Means Clustering) to:
1.  Identify gender-based performance patterns.
2.  Understand the statistical factors contributing to player success (Win Rate).
3.  Investigate "mis-clustered" players (outliers) to understand stylistic differences between the tours.

**Key Findings:**
*   **Different Styles:** The analysis effectively distinguishes between ATP and WTA players with 85.7% accuracy based solely on match statistics.
*   **Serve Dominance:** The primary differentiator is serve power/volume (PC1).
*   **Winning Factor:** While serve power defines the "style" of the tour, the ability to minimize break point opportunities (PC2 - Pressure/Resilience) is the strongest predictor of winning matches in both tours.

## Data Source

The project relies on match data located in the `data/` directory.
*   **Location:**
    *   `data/ATP/`: Contains CSV files for ATP matches (2020-2024), players, and rankings.
    *   `data/WTA/`: Contains CSV files for WTA matches (2020-2024), players, and rankings.
*   **Format:** CSV files sourced from public tennis databases.

## How to Build and Run the Container

This project uses Docker to ensure a reproducible analysis environment.

1.  **Clone Repo:**
    Run the following command in the root of the repository:
    ```bash
    git clone https://github.com/QinwenZ2344/Bios611_final_proj.git
    cd Bios611_final_proj
    ```

2.  **Build the Docker Image:**
    Run the following command in the root of the repository:
    ```bash
    docker build . -t qinwen
    ```

3.  **Run the Container:**
    To run the analysis and ensure that generated reports/plots are saved back to your local machine, mount the current directory to `/work` inside the container:
    ```bash
    docker run -v "$(pwd):/home/rstudio" -e PASSWORD='yourpassword' -p 8787:8787 qinwen
    ```

    *   `-v "$(pwd):/work"`: You can use this to mount your current folder to `/work` inside the container so you can access the output files. 

## Building the Report

Once inside the Docker container, you can generate the final reports using the provided `Makefile`.

*   **Generate All Reports (HTML & PDF):**
    ```bash
    make all
    ```

*   **Generate Only HTML:**
    ```bash
    make report.html
    ```

*   **Generate Only PDF:**
    ```bash
    make report.pdf
    ```

*   **Clean Up Generated Files:**
    ```bash
    make clean
    ```

## Developer Instructions

The project is organized using a `Makefile` to orchestrate the analysis pipeline.

### Pipeline Structure
The analysis flow is as follows:
1.  **`make plots`**: Runs the R scripts in `scripts/` to process data and generate visualizations.
    *   `scripts/player_cluster.r`: Performs PCA and K-Means clustering; generates PCA plots.
    *   `scripts/player_rank.r`: Generates ranking analysis plots.
    *   `scripts/miscluster_analysis.r`: Analyzes outliers and generates detailed finding documents.
2.  **`make report.html`**: Compiles `report.Rmd` using `rmarkdown`, integrating the generated plots.
3.  **`make report.pdf`**: Converts the HTML report to PDF using `scripts/html2pdf.r`.

### Directory Layout
*   `scripts/`: Contains all R scripts for data processing and analysis.
*   `plots/`: Destination for generated figures.
*   `documents/`: Destination for intermediate text-based findings.
*   `data/`: Source data files.
