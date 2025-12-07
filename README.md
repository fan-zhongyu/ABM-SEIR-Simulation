# ABM-SEIR Simulation Platform
> ** 2025 Fall Brown PHP2560 Final Project**
> Zhongyu Fan(zhongyu_fan@brown.edu), Yifei Feng, Qi Qi
## 1. Background and Methods

This application simulates the dynamics of the COVID-19 pandemic using an **Agent-Based Model (ABM)** framework. Unlike aggregate equation-based models, ABM simulates individual agents interacting within a virtual environment. The simulation logic is rooted in the **SEIR compartmental framework** and inspired by the methodology presented in *COVID-ABS: An agent-based model of COVID-19 epidemic* (Silva et al., 2020).

### Methodological Approach: Monte Carlo Simulations
> Agent-based models are inherently stochastic. A single simulation run represents only one possible trajectory of the epidemic. To ensure statistical robustness, this application utilizes **Monte Carlo methods**. For each scenario, the simulation is executed over $N_{reps}$ independent repetitions. The results are aggregated to compute the Mean and 95% Confidence Intervals (CI).

---

![Workflow of the Agent-Based Model Simulation](www/flow.png)
*Figure 1: Workflow of the Agent-Based Model Simulation.*

---

## 2. Key Concepts & Parameters

### Epidemiological Compartments
* **Susceptible (S):** Vulnerable individuals. Probability of infection depends on local prevalence.
* **Exposed (E):** Infected but not yet infectious (Latent period).
* **Infected (I):** Infectious individuals capable of transmitting the virus.
* **Recovered (R):** Immune individuals removed from the transmission chain.

[More compartment SEIR model information available here](https://doi.org/10.1038/s41592-020-0856-2).

### Core Parameters
In our simulation, Transmission Rate ($\beta$), Incubation Rate ($\sigma$), and Recovery Rate ($\gamma$) are the main input parameters.

* **Transmission Rate ($\beta$):** Controls the speed of spread. Related to the Basic Reproduction Number by $R_0 = \frac{\beta}{\gamma}$.
* **Incubation Rate ($\sigma$):** Inverse of the incubation period: $\sigma = \frac{1}{T_{inc}}$. (e.g., 5-day incubation $\to \sigma = 0.2$).
* **Recovery Rate ($\gamma$):** Inverse of the infectious period: $\gamma = \frac{1}{T_{inf}}$. (e.g., 5-day infectiousness $\to \gamma = 0.2$).

## 3. Mathematical Framework

The simulation operates in discrete time steps ($\Delta t = 1 \text{ day}$). Transitions are stochastic (Bernoulli trials) based on the following probabilities.

### A. The Effective Transmission Rate
The raw transmission rate $\beta$ is reduced by interventions (Masks and Social Distancing):

$$\beta_{\text{eff}} = \beta \times (1 - \text{MaskEffect}) \times (1 - \text{SocialDistancing})$$

### B. The Force of Infection ($\lambda$)
The probability of a Susceptible agent meeting an Infected agent depends on the Contact Rate ($c$) and current Prevalence:

$$\lambda = \beta_{\text{eff}} \times c \times \frac{I_{\text{total}}}{N}$$

### C. State Transition Probabilities
Probabilities per time step derived from exponential decay assumptions:

$$P(S \to E) = 1 - e^{-\lambda \Delta t}$$
$$P(E \to I) = 1 - e^{-\sigma \Delta t}$$
$$P(I \to R) = 1 - e^{-\gamma \Delta t}$$

## 4. Statistical Analysis

Simulation analysis involves fitting a linear regression model to the aggregate dataset ($N = \text{Scenarios} \times \text{Repetitions}$). The model quantifies the impact of interventions on outcomes Peak Prevalence and Day of Peak.

### Model Specification with Interactions
To capture the potential effects of combined interventions, the model includes pairwise interaction terms between all selected predictors (Social Distancing, Mask Usage, and Lockdown). The full model takes the form:

$$Y = \beta_0 + \underbrace{\beta_1 SD + \beta_2 Mask + \beta_3 Lockdown}_{\text{Main Effects}} + \underbrace{\beta_{ij} (X_i \times X_j)}_{\text{Interaction Terms}} + \epsilon$$

### Statistical Interpretation: Interaction Effects
* **The Baseline Trap:** In models with interactions, the main effect coefficient represents the outcome only when the interacting variable is zero.
* **Avoid Isolated Interpretation:** When an interaction term is significant, coefficients cannot be interpreted in isolation.
* **Focus on Net Effect:** Evaluate true effectiveness based on the net effect under specific conditions.

## 5. Limitations & Future Work

While this simulation provides valuable insights, the following limitations should be considered:

* **Computational Constraints (Population Size):** Capped at 5,000 individuals due to memory constraints in Shiny.
* **Scenario Scope:** Based on predefined scenarios from Silva et al. (2020), potentially overlooking broader variability.
* **Modeling Assumptions:** OLS Linear Regression is an approximation and may not fully capture non-linear viral dynamics.

## 6. References & Data Sources

* Kucharski, A. J., et al. (2020). Early dynamics of transmission and control of COVID-19: a mathematical modelling study. *The Lancet Infectious Diseases*. [DOI Link](https://doi.org/10.1016/S1473-3099(20)30144-4)
* Li, Q., et al. (2020). Early Transmission Dynamics in Wuhan, China. *NEJM*. [DOI Link](https://www.nejm.org/doi/full/10.1056/NEJMoa2001316)
* Wu, J. T., Leung, K., & Leung, G. M. (2020). Nowcasting and forecasting the potential spread. *The Lancet*. [DOI Link](https://doi.org/10.1016/S0140-6736(20)30260-9)
* WHO. (2020). Statement on IHR Emergency Committee meeting. [View Statement](https://www.who.int/news-room/detail/23-01-2020-statement-on-the-meeting-of-the-international-health-regulations-(2005)-emergency-committee-regarding-the-outbreak-of-novel-coronavirus-(2019-ncov))
* WHO-China Joint Mission. (2020). Final Report. [View PDF](https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf)
* Liu, T., et al. (2020). Transmission dynamics of 2019-nCoV. *bioRxiv*. [DOI Link](https://www.biorxiv.org/content/10.1101/2020.01.25.919787v2)
* Rocklöv, J., et al. (2020). COVID-19 outbreak on the Diamond Princess. *Journal of Travel Medicine*. [DOI Link](https://doi.org/10.1093/jtm/taaa030)
* Backer, J. A., et al. (2020). Incubation period of 2019-nCoV. *Eurosurveillance*. [DOI Link](https://doi.org/10.2807/1560-7917.ES.2020.25.5.2000062)
* Read, J. M., et al. (2020). Novel coronavirus 2019-nCoV: early estimation. *medRxiv*. [DOI Link](https://doi.org/10.1101/2020.01.23.20018549)
* Bi, Q., et al. (2020). Epidemiology and transmission of COVID-19 in Shenzhen. *The Lancet Infectious Diseases*. [DOI Link](https://doi.org/10.1016/S1473-3099(20)30287-5)
* Tang, B., et al. (2020). Estimation of the Transmission Risk. *Journal of Clinical Medicine*. [DOI Link](https://www.mdpi.com/2077-0383/9/2/462)
* Goh, G. (2020). Epidemic Calculator. [Interactive Tool](https://gabgoh.github.io/COVID/?utm_source=catalyzex.com)
* Silva, P. C. L., et al. (2020). COVID-ABS: An agent-based model of COVID-19 epidemic. *Chaos, Solitons & Fractals*. [DOI Link](https://doi.org/10.1016/j.chaos.2020.110088)
