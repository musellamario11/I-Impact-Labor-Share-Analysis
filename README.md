# I-Impact-Labor-Share-Analysis
Econometric analysis on CPS 2016-2025 microdata using Fixed-Effects models in R
**Project Overview**

This project investigates the relationship between Artificial Intelligence (AI) exposure and wage inequality in the United States over the period 2016–2025. 
Building on a task-based framework, the analysis examines how AI-driven technological change affects the wage gap between high-skilled and low-skilled workers and the distribution of labor income.

**Key Empirical Findings**

• AI and Wage Inequality  
The results provide evidence of a positive and statistically significant association between AI exposure and the skill premium (wage gap).

• Magnitude of the Effect  
In the preferred specification (Two-Way Fixed Effects with controls), the estimated coefficient (β = 1.62) indicates that higher AI exposure is associated with a larger hourly wage gap, ceteris paribus.

• Contribution to Inequality Dynamics  
AI exposure is estimated to be associated with approximately 21.6% of the observed increase in the wage gap over the sample period, suggesting a non-negligible role of AI in shaping recent inequality trends.

• Economic Mechanisms  
The findings are consistent with a task-based interpretation:  
– Reinstatement effect: AI complements high-skill labor, increasing productivity  
– Displacement effect: AI substitutes routine and low-skill tasks  
This combination contributes to widening wage inequality and declining labor share in highly exposed sectors.

**Methodology**

The empirical analysis relies on a Two-Way Fixed Effects (TWFE) panel model:

WageGap_it = β AI_Exposure_it + γX_it + α_i + δ_t + ε_it

• Dependent Variable: Skill premium (difference in weighted mean hourly wages between high- and low-skill workers)  
• Key Independent Variable: AI Occupational Exposure Index (Felten et al.)  
• Controls: High-skill employment share, log total employment  
• Fixed Effects: Industry (α_i) and year (δ_t)  
• Estimation: Weighted using ASECWT to ensure national representativeness  

**Data & Construction**

• Source: IPUMS-CPS microdata (2016–2025)  
• Sample: 470,000+ individual observations aggregated into an industry-year panel  
• Harmonization: Crosswalks between OCC (occupation), IND1990 (industry), and SOC-based exposure indices  
• Additional Indices: Automation Risk (Frey & Osborne), Routine Task Intensity (Autor & Dorn)

**Sectoral Heterogeneity**

The impact of AI is heterogeneous across industries:

• High-exposure sectors: Finance, Insurance, Professional Services  
• Low-exposure sectors: Agriculture, Mining, Wood Products  

Counterfactual simulations suggest that, in the absence of AI diffusion, the wage gap would have been significantly lower, particularly in cognitively intensive sectors.

**Conclusion**

The analysis suggests that AI acts as a form of skill-biased technological change, contributing to rising wage inequality. 
While AI adoption enhances productivity, its benefits are unevenly distributed, raising important policy questions regarding labor market adjustment, education, and technological governance.
