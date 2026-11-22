# Offshore Wind & Marine Ecosystem Explorer (EcoWindSim)

## Overview

The **Offshore Wind & Marine Ecosystem Explorer** (EcoWindSim) is an advanced interactive decision-support tool for exploring the complex interactions between planned floating offshore wind installations and marine ecosystems in the Celtic Sea region. This application combines real bathymetric data, published oceanographic observations, transparent scenario modeling, and machine-learning surrogates to assess potential environmental impacts.

## Version

**Version 01** - November 2026 Data Audit Release

This version includes comprehensive data corrections, verified Round 5 project information, real ETOPO1 bathymetry, and trained ML surrogate models.

## Key Features

### 1. **Interactive Mapping**
- Real-time visualization of Round 5 floating wind farm locations
- Marine Protected Areas (MPAs) from JNCC
- Predicted tidal-mixing fronts
- Seabird colonies and monitoring platforms
- Dynamic bathymetry and oceanographic overlays

### 2. **Ocean Dynamics Analysis**
- M2 tidal current patterns (depth-scaled estimates)
- Tidal mixing and stratification (Simpson-Hunter parameter)
- Seasonal frontal systems
- Water column structure and density profiles

### 3. **Autonomous Monitoring**
- Illustrative Autosub Long Range (Boaty McBoatface) mission planning
- Sensor data visualization
- Plankton imagery gallery with seasonal phenology
- Platform status register

### 4. **Ecosystem Impact Assessment**
- Plankton community dynamics (phytoplankton and zooplankton)
- Predator movement patterns and displacement
- Biodiversity metrics by distance
- Food web connectivity analysis

### 5. **ML/AI Prediction Engine**
- Multiple model types: Random Forest, Gradient Boosting, Neural Network, Ensemble
- 5-fold cross-validation with permutation feature importance
- Monte-Carlo projection with nested prediction intervals
- Scenario comparison and uncertainty analysis

### 6. **Report Generation**
- Comprehensive assessment reports (HTML/Word/PDF)
- Customizable sections and audience targeting
- Full reproducibility with parameter snapshots
- Optional audit logging

## Data Sources & Provenance

All data sources are documented with provenance classes:

- **OBSERVED**: Published measurements or official registers
- **DERIVED**: Computed from observed datasets with stated methods
- **SCHEMATIC**: Representative geometry (replaced when official data available)
- **ILLUSTRATIVE**: Representative values for explanation only

### Primary Data Sources

1. **NOAA ETOPO1** - Global Relief Model (bathymetry)
2. **British Oceanographic Data Centre (BODC)** - Oceanographic reference data
3. **Continuous Plankton Recorder Survey (MBA)** - Chlorophyll-a ranges
4. **JNCC** - Marine Protected Area boundaries
5. **The Crown Estate** - Round 5 project register and PDA boundaries
6. **CMEMS** - Recommended for observed ocean fields (not embedded)

## Technical Requirements

### R Version
- R >= 4.0.0

### Required Packages
```r
shiny, bslib, leaflet, plotly, DT, dplyr, ggplot2, viridis, 
shinydashboard, shinyWidgets, raster, ncdf4, lubridate

Optional Packages

randomForest
, gbm, nnet (for ML models)
rmarkdown (for report generation)
digest (for audit logging)
ragg (for high-quality graphics)
marmap (for extended bathymetry)

Installation

# Install required packages
install.packages(c("shiny", "bslib", "leaflet", "plotly", "DT", 
                   "dplyr", "ggplot2", "viridis", "shinydashboard", 
                   "shinyWidgets", "raster", "ncdf4", "lubridate"))

# Install optional ML packages
install.packages(c("randomForest", "gbm", "nnet"))

# Install report generation packages
install.packages("rmarkdown")

# Run the application
shiny::runApp("app.R")

Quick Start

    Launch the application and click "ENTER EXPLORER" on the cover page
    Select study area using the Area of Interest selector in the sidebar
    Configure wind farm parameters (number of farms, turbines, water depth)
    Run simulation by clicking the "Run Simulation" button
    Explore results across different tabs (Map, Ocean Dynamics, Ecosystem Impacts)
    Run ML prediction for long-term projections (optional)
    Generate report using the Reports tab

Application Structure

├── Home (Cover Page)
├── Welcome (Introduction)
├── Round 
5 Awards (Official project data)
├── Area Explorer (Study area selection and statistics)
├── Map Explorer (Interactive Celtic Sea map)
├── Ocean Dynamics (Physical oceanography)
│   ├── Current Analysis
│   ├── Tidal Patterns
│   ├── Frontal Systems
│   └── Water Column
├── Autonomous Monitoring (Platform data)
│   ├── AUV Tracks
│   ├── Sensor Data
│   ├── Plankton Imagery
│   └── Platform Status
├── Simulation Results (Scenario modeling)
│   ├── Response Timeline
│   ├── Detailed Results
│   ├── Impact Assessment
│   └── Future Projections
├── Ecosystem Impacts (Biological responses)
│   ├── Plankton Distributions
│   ├── Predator Movements
│   ├── Biodiversity Metrics
│   └── Connectivity
├── Ecological Assessment (Multi-trophic)
│   ├── Plankton Response
│   ├── Fish Recruitment
│   ├── Marine Mammals
│   ├── Seabirds
│   └── Impact Matrix
├── ML/AI Predictions (Machine learning)
│   ├── Impact Forecast
│   ├── Uncertainty Analysis
│   ├── Feature Importance
│   ├── Model Performance
│   └── Scenario Comparison
├── Reports (Document generation)
└── Documentation (References and methods)

Key Scientific Methods
Physical Oceanography

    M2 tidal current: Continuity estimate U = Q·a(x,y)/h with regional amplification
    Stratification: Simpson-Hunter parameter S = log10(h/U³)
    Frontal position: Contour S = 2.7 (mixed vs. stratified)
    Seawater density: UNESCO EOS-80 equation of state

Ecosystem Response Function

The impact index is computed by a documented response function:

impact 
= 0.30P - 0.25P·tanh(S - 2.7) + 0.15P(N - 5)/5 
         + 0.10(Chl - 2) - 0.18·dSST - 0.05P(W - 10)/10
         + 0.40P(f_strat - 0.45) - 0.08P(D - 0.3)

where P = ln(1 + GW/1.5)

Coefficients are assumptions reflecting literature-reported effects, not fitted values.
ML Surrogate Models

    Training: 450 synthetic scenarios from the response function
    Validation: 5-fold cross-validation
    Features: Oceanographic, plankton, climate, layout, seasonal
    Projection: 300 Monte-Carlo covariate paths + residual error

Known Limitations

    Impact index: Uncalibrated response function; coefficients encode direction of effects, not magnitudes
    ML models: Trained on synthetic data; skill measures emulation of app assumptions, not field validation
    Tidal currents: Depth-scaled estimate on ETOPO1, not hydrodynamic model output
    Round 5 outlines: Schematic unless official Crown Estate layer downloaded at start-up
    Reproducibility: Results are reproducible for identical scenarios (seed-based), but other modules contain unseeded illustrative variability
    Validation: No validation against independent observations has been performed

Data Audit (Version 01)

Major corrections implemented in November 2026:

    Fixed Round 5 project locations and assignments
    Replaced synthetic ocean fields with ETOPO1-derived data
    Verified MPA boundaries and monitoring platform locations
    Corrected tidal/stratification parameters and thresholds
    Trained real ML models with cross-validation
    Made simulations parameter- and area-coupled
    Corrected 50+ literature references

See Documentation > Data Audit tab for complete details.
Citation

If you use this application in research or policy work, please cite:

Offshore Wind & Marine Ecosystem Explorer (EcoWindSim), Version 10. Data sources: NOAA ETOPO1 (doi:10.7289/V5C8276M), JNCC MPAs, The Crown Estate Round 5, CPR Survey (MBA), and published literature as documented in the application.
Support & Feedback. 
Owner: Nexos Environmental Solutions (NES) Plymouth, UK
For technical issues, questions, or suggestions:

    Review the User Manual (see USER_MANUAL.md)
    Check the Documentation tab within the application
    Verify data sources and methods in the Data Audit section

License & Disclaimer

Decision-support prototype. Several outputs are simulated or illustrative. This application is not suitable for:

    Regulatory submission
    Evidence in consenting decisions
    Policy decisions without independent validation

Always verify results with current observational data and consult domain experts before use in decision-making.
Acknowledgments

Data providers: NOAA, BODC, MBA (CPR Survey), JNCC, The Crown Estate, Cefas, National Oceanography Centre, Marine Institute Ireland, Met Office.

Scientific basis: This application builds on published research by Simpson & Hunter (1974), Pingree & Griffiths (1978), Brown et al. (2003), Carpenter et al. (2016), De Mesel et al. (2015), Holt et al. (2016), and many others documented throughout.

Version 01 | November 2026 | Built with Shiny for R
