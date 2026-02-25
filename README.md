## For detailed methodological rationale, see docs/methodological_notes.md

# Cohort Simulation Framework for Respiratory Virus Testing Patterns

## Overview

This repository implements a synthetic cohort simulation framework designed to investigate how healthcare-seeking behavior and testing strategies shape observed respiratory virus surveillance data.

The framework explicitly separates:

1. Latent infection and symptom processes  
2. Healthcare-seeking behavior (visit process)  
3. Testing decision mechanisms  
4. Observed outcomes among tested individuals  

By modeling these components independently, the simulation enables methodological evaluation of selection bias and observation mechanisms in testing-based surveillance systems.

All data generated in this project are fully synthetic.  
No real-world individual-level data are used.

---

## Objectives

- Demonstrate how selective healthcare utilization affects testing-based surveillance metrics  
- Evaluate how testing coverage influences observed test positivity  
- Provide a reproducible synthetic data pipeline for methodological research  
- Support teaching and computational epidemiology workflows  

---

## Conceptual Framework

The simulation follows a cohort-based design in which individuals transition through:

Infection → Symptom status → Healthcare visit → Testing → Observed outcome

Only individuals who seek care and are selected for testing contribute to observed surveillance data, introducing a selection mechanism that can bias naïve estimates.

---

## Methods

### Synthetic Population

A synthetic cohort is generated with individual-level attributes such as:

- Infection status  
- Symptom status  
- Healthcare-seeking propensity  

Population parameters are informed by ranges commonly reported in respiratory virus cohort studies but are not calibrated to any specific dataset.

### Visit Process

Healthcare utilization is modeled as a stochastic process conditional on symptom status, representing the observation gateway.

### Testing Mechanism

Testing is applied probabilistically among healthcare visitors, allowing simulation of:

- Symptom-based testing  
- Partial testing coverage  
- Strategy-dependent sampling  

### Observed Outcomes

Outcomes are defined among the tested population, enabling comparison between:

- True infection prevalence  
- Observed test positivity  

### Statistical Models

The simulated datasets are structured to support:

- Logistic regression for testing probability  
- Poisson or negative binomial models for testing rates  
- Time-to-event models for healthcare-seeking behavior (optional extension)  

---

## Reproducibility

- Fully synthetic data generation  
- Script-based workflow  
- Fixed random seeds for reproducibility  

Run the simulation:

## Reference

The conceptual design of this simulation framework is informed by analytical strategies commonly used in cohort studies 
of respiratory virus testing patterns reported in the literature.

For background reading:

[1] TAYE B W, SARNA M, LE H, et al. Respiratory Viral Testing Rate Patterns in Young Children Attending Tertiary Care Across Western Australia: A Population-Based Birth Cohort Study [J]. Influenza Other Respir Viruses, 2024, 18(9): e70005.

```r
source("scripts/run.R")



