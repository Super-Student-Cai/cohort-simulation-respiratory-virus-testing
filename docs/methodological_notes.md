# Methodological Design and Simulation Framework

## 1. Scope and Purpose

This document provides a detailed methodological description of the synthetic cohort simulation framework implemented in this repository.

The framework is designed to evaluate how healthcare-seeking behavior and testing strategies influence observed respiratory virus surveillance data by explicitly modeling the observation process.

This is a methodological simulation study.  
No real-world individual-level data are used.

---

## 2. Conceptual Model

The simulation separates the data-generating process into four sequential components:

1. Latent infection process  
2. Symptom manifestation  
3. Healthcare-seeking behavior (visit process)  
4. Testing decision and observation  

Observed surveillance data arise only from the subset of individuals who:

- become infected (or not)  
- develop symptoms (or not)  
- seek care  
- are selected for testing  

This layered structure introduces selection mechanisms that can bias naïve estimates of testing rates and positivity.

---

## 3. Synthetic Cohort Generation

A closed synthetic cohort of size *N* is generated.

Each individual is assigned baseline attributes (e.g., identifiers and stochastic propensities).  
No demographic calibration to a specific population is performed.

Random seeds are fixed to ensure reproducibility.

---

## 4. Infection and Symptom Processes

### 4.1 Infection

Infection status is generated as a Bernoulli process:

Infection ~ Bernoulli(p_infection)

where *p_infection* is user-defined.

### 4.2 Symptom Status

Symptom manifestation is conditional on infection:

Symptom | Infection ~ Bernoulli(p_symptom)

Optionally, asymptomatic individuals may have a lower probability of symptoms to reflect imperfect symptom sensitivity.

This separation allows the framework to distinguish:

- True infection prevalence  
- Symptom-defined clinical population  

---

## 5. Healthcare-Seeking Behavior (Visit Process)

Healthcare utilization is modeled as a stochastic process conditional on symptom status.

Visit probabilities are specified as:

Visit | Symptom = 1 ~ Bernoulli(p_visit_sym)  
Visit | Symptom = 0 ~ Bernoulli(p_visit_asym)

with:

p_visit_sym > p_visit_asym

This step defines the **observation gateway**, since only visitors become eligible for testing.

This mechanism introduces selection bias analogous to real-world surveillance systems.

---

## 6. Testing Mechanism

Testing is applied probabilistically among individuals who seek care.

Test | Visit = 1 ~ Bernoulli(p_test_visit)

This allows simulation of:

- Symptom-based testing  
- Partial testing coverage  
- Strategy-dependent sampling  

Testing is independent of infection status conditional on visit in the base model, but the framework can be extended to symptom-dependent testing probabilities.

---

## 7. Observed Outcomes

Observed test positivity is defined among tested individuals:

Positivity = Infected among Tested / Tested

This differs from true prevalence:

True prevalence = Infected / N

The framework enables direct comparison between these quantities to quantify observation bias.

---

## 8. Data Structure

The simulated dataset contains individual-level indicators for:

- Infection status  
- Symptom status  
- Healthcare visit  
- Testing status  
- Observed test result  

This structure supports both individual-level and aggregated analyses.

---

## 9. Statistical Model Interface

The generated data can be analyzed using:

### 9.1 Logistic Regression

To model testing probability:

logit(P(Test = 1)) = f(Symptom, Visit)

### 9.2 Count Models

Poisson or negative binomial models for:

- Testing counts  
- Visit counts  

### 9.3 Optional Time-to-Event Models

The framework can be extended to simulate time to healthcare visit and analyzed using Cox models.

---

## 10. Parameterization Strategy

Parameter values are user-defined and intended to fall within plausible ranges reported in cohort-based respiratory virus surveillance studies.

No parameters are calibrated to match a specific empirical dataset.

The simulation is designed for conceptual and methodological exploration rather than empirical inference.

---

## 11. Reproducibility

Reproducibility is ensured through:

- Fixed random seeds  
- Script-based execution  
- Deterministic parameter specification  

The entire pipeline can be executed via:

```r
source("scripts/run.R")